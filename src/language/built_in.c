#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "built_in.h"
#include "char_buffer.h"
#include "file.h"
#include "object.h"
#include "program.h"
#include "vm.h"

/** Defines a built-in function that can be used without any imports. */
#define DEFINE_NATIVE_FUNC(name) \
    static Obj *native_##name(Vm *vm, Obj *callee, Obj **args)

/** Returns an error signal without reporting a message. */
#define RETURN_ERROR_SIGNAL() return NULL;

/** Reports a runtime error and returns the "native errored" signal. */
#define RETURN_ERROR(vm, ...) \
    do { \
        RUNTIME_ERROR(vm, __VA_ARGS__); \
        RETURN_ERROR_SIGNAL(); \
    } while (false)

/** 
 * Checks and returns an error with a message if the parameter at index was not of an expected type.
 * 
 * The index is for the argument in parameters which was of an unexpected type.
 * The expected type is an obj type enum of what type the built-in expected that argument to be.
 */
#define PARAM_TYPE_CHECK(vm, args, index, expectedType) \
    do { \
        if (args[(index)]->type != (expectedType)) { \
            RETURN_ERROR( \
                vm, "Expected argument %"PRIu32" to be %s, got %s instead.", \
                index + 1, obj_type_str(expectedType), obj_type_str(args[(index)]->type) \
            ); \
        } \
    } while (false)

/** The default return of a native that doesn't explicitly return anything. */
#define DEFAULT_RETURN(vm) return AS_OBJ(new_null_obj((vm)->program))

/** Returns an object in a native function (also performs the conversion to the base obj struct). */
#define RETURN_OBJ(object) return AS_OBJ(object)

/** Prints out an iterable and all of its objects. Adds newline after each element if it's set. */
static void print_destructure(Vm *vm, Obj *toPrint, const bool addNewline) {
    GC_FREEZE(&vm->program->gc);
    IteratorObj *iterator = new_iterator_obj(vm->program, toPrint);
    Obj *current;
    while ((current = iterate(vm->program, iterator))) {
        print_obj(current, false);
        if (addNewline) {
            putchar('\n');
        }
    }
    GC_END_FREEZE(&vm->program->gc);
}

/** Built-in printing function with some boolean flag settings. */
DEFINE_NATIVE_FUNC(print) {
    UNUSED_VARIABLE(callee);
    PARAM_TYPE_CHECK(vm, args, 1, OBJ_BOOL);
    PARAM_TYPE_CHECK(vm, args, 2, OBJ_BOOL);

    Obj *toPrint = args[0];
    const bool addNewline = AS_PTR(BoolObj, args[1])->boolean;
    const bool destructurePrint = AS_PTR(BoolObj, args[2])->boolean;
    if (destructurePrint) {
        if (!is_iterable(toPrint)) {
            RETURN_ERROR(
                vm, "Expected iterable in a destructuring print, got %s instead.",
                obj_type_str(toPrint->type)
            );
        }
        print_destructure(vm, toPrint, addNewline);
    } else {
        print_obj(toPrint, false);
        if (addNewline) {
            putchar('\n');
        }
    }
    DEFAULT_RETURN(vm);
}

/** Takes a condition and a string message to print if the assertion failed. */
DEFINE_NATIVE_FUNC(assert) {
    UNUSED_VARIABLE(callee);
    PARAM_TYPE_CHECK(vm, args, 1, OBJ_STRING);

    if (!as_bool(vm->program, args[0])->boolean) {
        // Store the message in a buffer since the string gets freed when doing a runtime error.
        CharBuffer assertMessage = create_char_buffer();
        buffer_append_string(&assertMessage, AS_PTR(StringObj, args[1])->string);
        RUNTIME_ERROR(vm, assertMessage.text);
        free_char_buffer(&assertMessage);
        RETURN_ERROR_SIGNAL();
    }
    DEFAULT_RETURN(vm);
}

/** Grab the current time and returns it. */
DEFINE_NATIVE_FUNC(time) {
    UNUSED_VARIABLE(callee);
    UNUSED_VARIABLE(args);

    const ZmxFloat currentTime = (ZmxFloat)clock() / CLOCKS_PER_SEC;
    RETURN_OBJ(new_float_obj(vm->program, currentTime));
}

/** Opens and returns the passed file in a mode. Errors if it failed to open it. */
DEFINE_NATIVE_FUNC(open) {
    UNUSED_VARIABLE(callee);
    PARAM_TYPE_CHECK(vm, args, 0, OBJ_STRING);
    PARAM_TYPE_CHECK(vm, args, 1, OBJ_STRING);
    StringObj *path = AS_PTR(StringObj, args[0]);
    StringObj *modeStr = AS_PTR(StringObj, args[1]);

    if (path->length > MAX_PATH_LENGTH) {
        RETURN_ERROR(vm, "File paths cannot exceed %u in size.", MAX_PATH_LENGTH);
    }
    const FileMode mode = parse_file_mode(modeStr->string, modeStr->length);
    if (mode == FILE_INVALID) {
        RETURN_ERROR(vm, "Invalid file mode: '%s'.", modeStr->string);
    }

    FILE *stream = fopen(path->string, modeStr->string);
    if (stream == NULL) {
        RETURN_ERROR(vm, "Couldn't open file at path '%s': %s", path->string, strerror(errno));
    }
    RETURN_OBJ(new_file_obj(vm->program, stream, path, modeStr));
}

/** File class: attempts to read and return the whole file as one string. Can error. */
DEFINE_NATIVE_FUNC(file_read) {
    UNUSED_VARIABLE(args);
    FileObj *file = AS_PTR(FileObj, callee);
    if (!file->isOpen) {
        RETURN_ERROR(vm, "Cannot do file operation on closed file.");
    }
    if (file->mode == FILE_WRITE_ONLY) {
        StringObj *modeStr = new_string_obj(vm->program, "mode", strlen("mode"));
        Obj *mode = table_get(&file->fields, AS_OBJ(modeStr));
        RETURN_ERROR(vm, "Can't read file with mode '%s'.", AS_PTR(StringObj, mode)->string);
    }

    const long fileSize = get_file_size(file->stream, file->path->string);
    char *readString = ARRAY_ALLOC(fileSize + 1, char); // +1 for NUL.
    fread(readString, 1, fileSize, file->stream);
    if (ferror(file->stream)) {
        RETURN_ERROR(
            vm, "Failed to read file at path '%s': %s", file->path->string, strerror(errno)
        );
    }

    readString[fileSize] = '\0';
    StringObj *contents = new_string_obj(vm->program, readString, fileSize);
    free(readString);
    RETURN_OBJ(contents);
}

/** FIle class: writes to a file, creates one if it doesn't exist. */
DEFINE_NATIVE_FUNC(file_write) {
    UNUSED_VARIABLE(args);
    PARAM_TYPE_CHECK(vm, args, 0, OBJ_STRING);
    FileObj *file = AS_PTR(FileObj, callee);
    StringObj *text = AS_PTR(StringObj, args[0]);
    if (!file->isOpen) {
        RETURN_ERROR(vm, "Cannot do file operation on closed file.");
    }
    if (file->mode == FILE_READ_ONLY) {
        RETURN_ERROR(vm, "Can't write to file with mode '%s'.", "TODO: add mode string here.");
    }

    if (fputs(text->string, file->stream) == EOF) {
        RETURN_ERROR(vm, "Failed to write to file '%s': %s", file->path->string, strerror(errno));
    }
    DEFAULT_RETURN(vm);
}

/** 
 * File class: manually closes the file's stream.
 * 
 * This only closes the stream and sets a flag indicating that file operations should no longer
 * be done on the file object, but doesn't free the object.
 */
DEFINE_NATIVE_FUNC(file_close) {
    UNUSED_VARIABLE(args);
    FileObj *file = AS_PTR(FileObj, callee);

    if (fclose(file->stream) != 0) {
        RETURN_ERROR(vm, "Failed to close file at '%s'.", file->path->string);
    }
    file->isOpen = false;
    DEFAULT_RETURN(vm);
}

/** Returns an empty func parameters struct with no optionals or even mandatory parameters. */
static FuncParams no_args() {
    FuncParams params = {
        .minArity = 0, .maxArity = 0,
        .names = CREATE_DA(), .values = CREATE_DA()
    };
    return params;
}

/** Returns func parameters which have no optionals, only named mandatory parameters. */
static FuncParams no_optionals(ZmxProgram *program, const u32 arity, ...) {
    FuncParams params = {
        .minArity = arity, .maxArity = arity,
        .names = CREATE_DA(), .values = CREATE_DA()
    };
    
    va_list args;
    va_start(args, arity);
    for (u32 i = 0; i < arity; i++) {
        char *name = va_arg(args, char *);
        APPEND_DA(&params.names, AS_OBJ(new_string_obj(program, name, strlen(name))));
        APPEND_DA(&params.values, NULL);
    }
    va_end(args);
    return params;
}

/** Returns a native function's parameters with some optionals. */
static FuncParams with_optionals(ZmxProgram *program, const u32 minArity, const u32 maxArity, ...) {
    FuncParams params = {
        .minArity = minArity, .maxArity = maxArity,
        .names = CREATE_DA(), .values = CREATE_DA()
    };

    va_list args;
    va_start(args, maxArity);
    for (u32 i = 0; i < maxArity; i++) {
        char *name = va_arg(args, char *);
        APPEND_DA(&params.names, AS_OBJ(new_string_obj(program, name, strlen(name))));
        APPEND_DA(&params.values, va_arg(args, Obj *));
    }
    va_end(args);
    return params;
}

/** Loads a function into the passed table. */
static void load_native_method(
    ZmxProgram *program, Table *loadingTable, const char *name,
    NativeFunc func, const FuncParams params
) {
    StringObj *nameObj = new_string_obj(program, name, strlen(name));
    NativeFuncObj *native = new_native_func_obj(program, nameObj, func, params);
    
    table_set(loadingTable, AS_OBJ(nameObj), AS_OBJ(native));
}

/** Loads a native function into the program, as if it was a method of the program. */
static void load_native_func(
    ZmxProgram *program, const char *name, NativeFunc func, const FuncParams params
) {
    load_native_method(program, &program->builtIn.funcs, name, func, params);
}

/** Loads all built-in functions. */
static void load_funcs(ZmxProgram *program) {
    char *assertMsg = "Assert failed.";
    load_native_func(
        program, "print", native_print,
        with_optionals(
            program, 1, 3,
            "value", NULL,
            "newline", new_bool_obj(program, true), "destructure", new_bool_obj(program, false)
        )
    );
    load_native_func(
        program, "assert", native_assert,
        with_optionals(
            program, 1, 2,
            "condition", NULL, "message", new_string_obj(program, assertMsg, strlen(assertMsg))
        )
    );
    load_native_func(program, "time", native_time, no_args());
    load_native_func(program, "open", native_open, no_optionals(program, 2, "path", "mode"));
}

/** Loads the file class's information. */
static void load_file_class(ZmxProgram *program) {
    char *nameChars = "file";
    StringObj *name = new_string_obj(program, nameChars, strlen(nameChars));
    ClassObj *fileClass = new_class_obj(program, name, false);

    load_native_method(program, &fileClass->methods, "close", native_file_close, no_args());
    load_native_method(program, &fileClass->methods, "read", native_file_read, no_args());
    load_native_method(
        program, &fileClass->methods, "write", native_file_write, no_optionals(program, 1, "text")
    );
    program->builtIn.fileClass = fileClass;
}

/** Loads all built-in classes. */
static void load_classes(ZmxProgram *program) {
    load_file_class(program);
}

/** 
 * Loads all built-ins into the built-in struct inside the program.
 * 
 * The whole thing runs under a frozen GC for simplicity, and the fact that we know statically
 * that we won't allocate a ton of memory (unlike an arbitrary user's code).
 */
void load_built_ins(ZmxProgram *program) {
    GC_FREEZE(&program->gc);
    load_funcs(program);
    load_classes(program);
    GC_END_FREEZE(&program->gc);
}

/** Returns a built-ins struct which is empty (nothing loaded yet). */
BuiltIns empty_built_ins() {
    BuiltIns builtIn = {0};
    return builtIn;
}

/** Frees the memory allocated and owned by built-ins (excludeing objs - they're program owned). */
void free_built_ins(BuiltIns *builtIn) {
    free_table(&builtIn->funcs);
}
