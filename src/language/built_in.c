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

#if OS == WINDOWS_OS
    #include <windows.h>
#elif OS == UNIX_OS
    #include <unistd.h>
#endif

/** Defines a built-in function that can be used without any imports. */
#define DEFINE_NATIVE_FUNC(name) \
    static Obj *native_##name(ThreadObj *thread, Obj *callee, Obj **args)

/** Returns an error signal without reporting a message. */
#define RETURN_ERROR_SIGNAL() return NULL;

/** Reports a runtime error and returns the "native errored" signal. */
#define RETURN_ERROR(thread, ...) \
    do { \
        RUNTIME_ERROR(thread, __VA_ARGS__); \
        RETURN_ERROR_SIGNAL(); \
    } while (0)

/** 
 * Checks and returns an error with a message if the parameter at index was not of an expected type.
 * 
 * The index is for the argument in parameters which was of an unexpected type.
 * The expected type is an obj type enum of what type the built-in expected that argument to be.
 */
#define PARAM_TYPE_CHECK(thread, args, index, expectedType) \
    do { \
        if (args[(index)]->type != (expectedType)) { \
            RETURN_ERROR( \
                thread, "Expected argument %"PRIu32" to be %s, got %s instead.", \
                index + 1, obj_type_str(expectedType), obj_type_str(args[(index)]->type) \
            ); \
        } \
    } while (0)

/** The default return of a native that doesn't explicitly return anything. */
#define DEFAULT_RETURN(thread) return AS_OBJ((thread)->vm->program->internedNull)

/** Returns an object in a native function (also performs the conversion to the base obj struct). */
#define RETURN_OBJ(object) return AS_OBJ(object)

/** Prints out an iterable and all of its objects. Adds newline after each element if it's set. */
static void print_destructure(VulnerableObjs *vulnObjs, Obj *toPrint, const bool addNewline) {
    IteratorObj *iterator = new_iterator_obj(vulnObjs, toPrint);
    bool hasAllocated = false;
    Obj *current;
    mutex_lock(&vulnObjs->program->printLock);
    while ((current = iterate(vulnObjs, iterator, &hasAllocated))) {
        print_obj(current, false);
        if (addNewline) {
            putchar('\n');
        }

        if (hasAllocated) {
            OPT_DROP_UNROOTED(vulnObjs);
        }
    }
    mutex_unlock(&vulnObjs->program->printLock);
}

/** Built-in printing function with some boolean flag settings. */
DEFINE_NATIVE_FUNC(print) {
    UNUSED_VARIABLE(callee);
    PARAM_TYPE_CHECK(thread, args, 1, OBJ_BOOL);
    PARAM_TYPE_CHECK(thread, args, 2, OBJ_BOOL);

    Obj *toPrint = args[0];
    const bool addNewline = AS_PTR(BoolObj, args[1])->boolean;
    const bool destructurePrint = AS_PTR(BoolObj, args[2])->boolean;
    if (destructurePrint) {
        if (!is_iterable(toPrint)) {
            RETURN_ERROR(
                thread, "Expected iterable in a destructuring print, got %s instead.",
                obj_type_str(toPrint->type)
            );
        }
        print_destructure(&thread->vulnObjs, toPrint, addNewline);
    } else {
        mutex_lock(&thread->vm->program->printLock);
        print_obj(toPrint, false);
        if (addNewline) {
            putchar('\n');
        }
        mutex_unlock(&thread->vm->program->printLock);
    }
    DEFAULT_RETURN(thread);
}

/** Takes a condition and a string message to print if the assertion failed. */
DEFINE_NATIVE_FUNC(assert) {
    UNUSED_VARIABLE(callee);
    PARAM_TYPE_CHECK(thread, args, 1, OBJ_STRING);

    if (!as_bool(&thread->vulnObjs, args[0])->boolean) {
        // Store the message in a buffer since the string gets freed when doing a runtime error.
        CharBuffer assertMessage = create_char_buffer();
        buffer_append_string(&assertMessage, AS_PTR(StringObj, args[1])->string);
        RUNTIME_ERROR(thread, assertMessage.text);
        free_char_buffer(&assertMessage);
        RETURN_ERROR_SIGNAL();
    }
    DEFAULT_RETURN(thread);
}

/** Sleeps for a given amount of miliseconds. */
DEFINE_NATIVE_FUNC(sleep) {
    UNUSED_VARIABLE(callee);
    PARAM_TYPE_CHECK(thread, args, 0, OBJ_INT);
    const ZmxInt sleepTimeMs = AS_PTR(IntObj, args[0])->number;

#if OS == WINDOWS_OS
    Sleep(sleepTimeMs);
#elif OS == UNIX_OS
    usleep(sleepTimeMs * 1000); // Multiply by 1000 because this is microsecond sleep.
#endif
    DEFAULT_RETURN(thread);
}

/** Grab the current time and returns it. */
DEFINE_NATIVE_FUNC(time) {
    UNUSED_VARIABLE(callee);
    UNUSED_VARIABLE(args);

    const ZmxFloat currentTime = (ZmxFloat)clock() / CLOCKS_PER_SEC;
    RETURN_OBJ(new_float_obj(&thread->vulnObjs, currentTime));
}

/** Opens and returns the passed file in a mode. Errors if it failed to open it. */
DEFINE_NATIVE_FUNC(open) {
    UNUSED_VARIABLE(callee);
    PARAM_TYPE_CHECK(thread, args, 0, OBJ_STRING);
    PARAM_TYPE_CHECK(thread, args, 1, OBJ_STRING);
    StringObj *path = AS_PTR(StringObj, args[0]);
    StringObj *modeStr = AS_PTR(StringObj, args[1]);

    if (path->length > MAX_PATH_LENGTH) {
        RETURN_ERROR(thread, "File paths cannot exceed %u in size.", MAX_PATH_LENGTH);
    }
    const FileMode mode = parse_file_mode(modeStr->string, modeStr->length);
    if (mode == FILE_INVALID) {
        RETURN_ERROR(thread, "Invalid file mode: '%s'.", modeStr->string);
    }

    FILE *stream = fopen(path->string, modeStr->string);
    if (stream == NULL) {
        RETURN_ERROR(thread, "Couldn't open file at path '%s': %s", path->string, strerror(errno));
    }
    RETURN_OBJ(new_file_obj(&thread->vulnObjs, stream, path, modeStr));
}

/** File class: attempts to read and return the whole file as one string. Can error. */
DEFINE_NATIVE_FUNC(File_read) {
    UNUSED_VARIABLE(args);
    FileObj *file = AS_PTR(FileObj, callee);
    if (!file->isOpen) {
        RETURN_ERROR(thread, "Cannot do file operation on closed file.");
    }
    if (file->mode == FILE_WRITE_ONLY) {
        Obj *mode = table_string_get(&thread->vulnObjs, &file->fields, "mode");
        RETURN_ERROR(thread, "Can't read file with mode '%s'.", AS_PTR(StringObj, mode)->string);
    }

    const long fileSize = get_file_size(file->stream, file->path->string);
    char *readString = ARRAY_ALLOC(fileSize + 1, char); // +1 for NUL.
    fread(readString, 1, fileSize, file->stream);
    if (ferror(file->stream)) {
        RETURN_ERROR(
            thread, "Failed to read file at path '%s': %s", file->path->string, strerror(errno)
        );
    }

    readString[fileSize] = '\0';
    StringObj *contents = new_string_obj(&thread->vulnObjs, readString, fileSize);
    free(readString);
    RETURN_OBJ(contents);
}

/** FIle class: writes to a file, creates one if it doesn't exist. */
DEFINE_NATIVE_FUNC(File_write) {
    PARAM_TYPE_CHECK(thread, args, 0, OBJ_STRING);
    FileObj *file = AS_PTR(FileObj, callee);
    StringObj *text = AS_PTR(StringObj, args[0]);
    if (!file->isOpen) {
        RETURN_ERROR(thread, "Cannot do file operation on closed file.");
    }
    if (file->mode == FILE_READ_ONLY) {
        RETURN_ERROR(thread, "Can't write to file with mode '%s'.", "TODO: add mode string here.");
    }

    if (fputs(text->string, file->stream) == EOF) {
        RETURN_ERROR(
            thread, "Failed to write to file '%s': %s", file->path->string, strerror(errno)
        );
    }
    DEFAULT_RETURN(thread);
}

/** 
 * File class: manually closes the file's stream.
 * 
 * This only closes the stream and sets a flag indicating that file operations should no longer
 * be done on the file object, but doesn't free the object.
 */
DEFINE_NATIVE_FUNC(File_close) {
    UNUSED_VARIABLE(args);
    FileObj *file = AS_PTR(FileObj, callee);

    if (fclose(file->stream) != 0) {
        RETURN_ERROR(thread, "Failed to close file at '%s'.", file->path->string);
    }
    file->isOpen = false;
    DEFAULT_RETURN(thread);
}

/** Thread class: the initializer function for a thread object. Returns a created thread object. */
DEFINE_NATIVE_FUNC(Thread) {
    UNUSED_VARIABLE(callee);
    PARAM_TYPE_CHECK(thread, args, 1, OBJ_BOOL); // The "is daemon" boolean.

    Obj *runnable = args[0];
    const bool isDaemon = AS_PTR(BoolObj, args[1])->boolean;
    if (runnable->type != OBJ_FUNC && runnable->type != OBJ_METHOD && runnable->type != OBJ_CLASS) {
        RETURN_ERROR(
            thread, "Expected runnable to be a user-defined callable, got %s instead.",
            obj_type_str(runnable->type)
        );
    }
    if (runnable->type == OBJ_CLASS && AS_PTR(ClassObj, runnable)->init == NULL) {
        RETURN_ERROR(thread, "Class runnable must have an initializer.");
    }

    ThreadObj *createdThread = vm_push_thread(
        &thread->vulnObjs, thread->vm,
        runnable, AS_PTR(RuntimeFuncObj, get_runnable_func(runnable))->module, false, isDaemon
    );
    RETURN_OBJ(createdThread);
}

/** Thread class: begins executing the thread with arguments: a list of args and a map of kwargs. */
DEFINE_NATIVE_FUNC(Thread_run) {
    PARAM_TYPE_CHECK(thread, args, 0, OBJ_LIST);
    PARAM_TYPE_CHECK(thread, args, 1, OBJ_MAP);
    ListObj *argList = AS_PTR(ListObj, args[0]);
    MapObj *kwargMap = AS_PTR(MapObj, args[1]);

    ThreadObj *threadToRun = AS_PTR(ThreadObj, callee);
    FuncObj *runnableFunc = get_runnable_func(threadToRun->runnable);
    PUSH_PROTECTED(&threadToRun->vulnObjs, argList);
    PUSH_PROTECTED(&threadToRun->vulnObjs, kwargMap);

    const bool validCall = call_is_valid(
        thread, runnableFunc->name->string, derive_runtime_params(runnableFunc),
        argList->items.length, kwargMap
    );
    if (get_thread_state(threadToRun) != THREAD_NOT_STARTED) {
        RETURN_ERROR(thread, "Cannot run thread multiple times.");
    }
    if (!validCall) {
        RETURN_ERROR_SIGNAL();
    }
    run_thread(threadToRun, thread_entry); // Thread runs independently now.

    DEFAULT_RETURN(thread);
}

/** Thread class: joins the passed thread. Errors by default unless bypass is on. */
DEFINE_NATIVE_FUNC(Thread_join) {
    PARAM_TYPE_CHECK(thread, args, 0, OBJ_BOOL);
    const bool bypass = AS_PTR(BoolObj, args[0])->boolean;
    ThreadObj *threadToJoin = AS_PTR(ThreadObj, callee);
    if (!threadToJoin->isDaemon) {
        join_thread(threadToJoin);
    } else if (!bypass) {
        RETURN_ERROR(thread, "Can't join daemon thread."); // Daemon and not bypassed.
    }
    DEFAULT_RETURN(thread);
}

/** Lock class: the initializer function for a lock object's creation. */
DEFINE_NATIVE_FUNC(Lock) {
    UNUSED_VARIABLE(callee);
    UNUSED_VARIABLE(args);
    RETURN_OBJ(new_lock_obj(&thread->vulnObjs));
}

/** Lock class: attempts to obtain a lock before continuing code execution. */
DEFINE_NATIVE_FUNC(Lock_obtain) {
    UNUSED_VARIABLE(args);

    LockObj *lock = AS_PTR(LockObj, callee);
    mutex_lock(&lock->lock);
    lock->isHeld = true;
    DEFAULT_RETURN(thread);
}

/** Lock class: releases a held lock so other threads can start obtaining it. */
DEFINE_NATIVE_FUNC(Lock_release) {
    UNUSED_VARIABLE(args);

    LockObj *lock = AS_PTR(LockObj, callee);
    if (!lock->isHeld) {
        RETURN_ERROR(thread, "Attempted to release lock not currently in use.");
    }

    mutex_unlock(&lock->lock);
    lock->isHeld = false;
    DEFAULT_RETURN(thread);
}

/** Lock class: returns a boolean of whether or not the lock is currently in use. */
DEFINE_NATIVE_FUNC(Lock_held) {
    UNUSED_VARIABLE(args);
    RETURN_OBJ(new_bool_obj(&thread->vulnObjs, AS_PTR(LockObj, callee)->isHeld));
}

/** Returns an empty func parameters struct with no optionals or even mandatory parameters. */
static FuncParams no_params() {
    FuncParams params = {.minArity = 0, .maxArity = 0, .names = CREATE_DA(), .values = CREATE_DA()};
    return params;
}

/** Returns func parameters which have no optionals, only named mandatory parameters. */
static FuncParams mandatory_params(VulnerableObjs *vulnObjs, const u32 arity, ...) {
    FuncParams params = {
        .minArity = arity, .maxArity = arity, .names = CREATE_DA(), .values = CREATE_DA()
    };
    
    va_list args;
    va_start(args, arity);
    for (u32 i = 0; i < arity; i++) {
        char *name = va_arg(args, char *);
        PUSH_DA(&params.names, AS_OBJ(new_string_obj(vulnObjs, name, strlen(name))));
        PUSH_DA(&params.values, NULL);
    }
    va_end(args);
    return params;
}

/** Returns a native function's parameters with some optionals. */
static FuncParams with_optional_params(
    VulnerableObjs *vulnObjs, const u32 minArity, const u32 maxArity, ...
) {
    FuncParams params = {
        .minArity = minArity, .maxArity = maxArity, .names = CREATE_DA(), .values = CREATE_DA()
    };

    va_list args;
    va_start(args, maxArity);
    for (u32 i = 0; i < maxArity; i++) {
        char *name = va_arg(args, char *);
        PUSH_DA(&params.names, AS_OBJ(new_string_obj(vulnObjs, name, strlen(name))));
        PUSH_DA(&params.values, va_arg(args, Obj *));
    }
    va_end(args);
    return params;
}

/** Loads a function into the passed table. */
static void load_native_method(
    VulnerableObjs *vulnObjs, Table *loadingTable, const char *name,
    NativeFunc func, const FuncParams params
) {
    StringObj *nameObj = new_string_obj(vulnObjs, name, strlen(name));
    NativeFuncObj *native = new_native_func_obj(vulnObjs, nameObj, func, params);
    
    table_set(loadingTable, AS_OBJ(nameObj), AS_OBJ(native));
}

/** Loads a native function into the program, as if it was a method of the program. */
static void load_native_func(
    VulnerableObjs *vulnObjs, const char *name, NativeFunc func, const FuncParams params
) {
    load_native_method(vulnObjs, &vulnObjs->program->builtIn.funcs, name, func, params);
}

/** Loads all built-in functions. */
static void load_funcs(VulnerableObjs *vulnObjs) {
    char *assertMsg = "Assert failed.";
    load_native_func(
        vulnObjs, "print", native_print,
        with_optional_params(
            vulnObjs, 1, 3,
            "value", NULL,
            "newline", new_bool_obj(vulnObjs, true), "destructure", new_bool_obj(vulnObjs, false)
        )
    );
    load_native_func(
        vulnObjs, "assert", native_assert,
        with_optional_params(
            vulnObjs, 1, 2,
            "condition", NULL, "message", new_string_obj(vulnObjs, assertMsg, strlen(assertMsg))
        )
    );
    load_native_func(
        vulnObjs, "sleep", native_sleep, mandatory_params(vulnObjs, 1, "milliseconds")
    );
    load_native_func(vulnObjs, "time", native_time, no_params());
    load_native_func(vulnObjs, "open", native_open, mandatory_params(vulnObjs, 2, "path", "mode"));

    load_native_func(
        vulnObjs, "Thread", native_Thread,
        with_optional_params(
            vulnObjs, 1, 2, "runnable", NULL, "daemon", new_bool_obj(vulnObjs, false)
        )
    );
    load_native_func(vulnObjs, "Lock", native_Lock, no_params());
}

/** Returns an initialized native class without methods or attributes with a terminated C-string. */
static ClassObj *initial_native_class(VulnerableObjs *vulnObjs, char *nameChars) {
    return new_class_obj(vulnObjs, new_string_obj(vulnObjs, nameChars, strlen(nameChars)), false);
}

/** Loads the file class's information. */
static void load_file_class(VulnerableObjs *vulnObjs) {
    ClassObj *fileClass = initial_native_class(vulnObjs, "File");

    load_native_method(vulnObjs, &fileClass->methods, "close", native_File_close, no_params());
    load_native_method(vulnObjs, &fileClass->methods, "read", native_File_read, no_params());
    load_native_method(
        vulnObjs, &fileClass->methods, "write", native_File_write,
        mandatory_params(vulnObjs, 1, "text")
    );
    vulnObjs->program->builtIn.fileClass = fileClass;
}

/** Loads the thread class's information. */
static void load_thread_class(VulnerableObjs *vulnObjs) {
    ClassObj *threadClass = initial_native_class(vulnObjs, "Thread");

    // Safe to pass empty list and map because this only ever gets called once per thread.
    load_native_method(
        vulnObjs, &threadClass->methods, "run", native_Thread_run,
        with_optional_params(
            vulnObjs, 0, 2, "args", new_list_obj(vulnObjs, (ObjArray)CREATE_DA()),
            "kwargs", new_map_obj(vulnObjs, create_table())
        )
    );
    load_native_method(
        vulnObjs, &threadClass->methods, "join", native_Thread_join,
        with_optional_params(vulnObjs, 0, 1, "bypass", new_bool_obj(vulnObjs, false))
    );
    vulnObjs->program->builtIn.threadClass = threadClass;
}

/** Loads the lock class's information. */
static void load_lock_class(VulnerableObjs *vulnObjs) {
    ClassObj *lockClass = initial_native_class(vulnObjs, "Lock");

    load_native_method(vulnObjs, &lockClass->methods, "obtain", native_Lock_obtain, no_params());
    load_native_method(vulnObjs, &lockClass->methods, "release", native_Lock_release, no_params());
    load_native_method(vulnObjs, &lockClass->methods, "held", native_Lock_held, no_params());
    vulnObjs->program->builtIn.lockClass = lockClass;
}

/** Loads all built-in classes. */
static void load_classes(VulnerableObjs *vulnObjs) {
    load_file_class(vulnObjs);
    load_thread_class(vulnObjs);
    load_lock_class(vulnObjs);
}

/** 
 * Loads all built-ins into the built-in struct in the program inside the passed vulnerabes.
 * 
 * The native functions loading dont have to drop their created objects off of the unrooted array
 * because this happens here after every set of built-in functionality is loaded.
 */
void load_built_ins(VulnerableObjs *vulnObjs) {
    load_funcs(vulnObjs);
    load_classes(vulnObjs);
    DROP_ALL_UNROOTED(vulnObjs);
}

/** Returns a built-ins struct which is empty (nothing loaded yet). */
BuiltIns empty_built_ins() {
    BuiltIns builtIn = {0};
    return builtIn;
}

/** Frees the memory allocated and owned by built-ins (excluding objs - they're program owned). */
void free_built_ins(BuiltIns *builtIn) {
    free_table(&builtIn->funcs);
}
