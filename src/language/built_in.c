#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "built_in.h"
#include "char_buffer.h"
#include "object.h"
#include "program.h"
#include "vm.h"

/** Defines a built-in function that can be used without any imports. */
#define NATIVE_FUNC(name) \
    static Obj *native_##name(Vm *vm, Obj **args)

/** Returns an error signal without reporting a message. */
#define RETURN_ERROR_SIGNAL() return NULL;

/** Reports a runtime error and returns the "native errored" signal. */
#define RETURN_ERROR(vm, ...) \
    do { \
        RUNTIME_ERROR(vm, __VA_ARGS__); \
        RETURN_ERROR_SIGNAL(); \
    } while (false)

/** 
 * Checks and returns an error with a message if the parameter at index wasn't of an expected type.
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
    GC_PUSH_PROTECTION(&vm->program->gc);
    IteratorObj *iterator = new_iterator_obj(vm->program, toPrint);
    Obj *current;
    while ((current = iterate(vm->program, iterator))) {
        print_obj(current, false);
        if (addNewline) {
            putchar('\n');
        }
    }
    GC_POP_PROTECTION(&vm->program->gc);
}

/** Built-in printing function with some boolean flag settings. */
NATIVE_FUNC(print) {
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
NATIVE_FUNC(assert) {
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
NATIVE_FUNC(time) {
    UNUSED_VARIABLE(args);

    const ZmxFloat currentTime = (ZmxFloat)clock() / CLOCKS_PER_SEC;
    RETURN_OBJ(new_float_obj(vm->program, currentTime));
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

/** Loads a native function into the passed table. */
static void load_native_func(
    ZmxProgram *program, const char *name, NativeFunc func, const FuncParams params
) {
    GC_PUSH_PROTECTION(&program->gc);
    StringObj *nameObj = new_string_obj(program, name, strlen(name));
    NativeFuncObj *native = new_native_func_obj(program, nameObj, func, params);
    
    table_set(&program->builtIn, AS_OBJ(nameObj), AS_OBJ(native));
    GC_POP_PROTECTION(&program->gc);
}

/** Loads all built-in objects into the program. */
void load_built_ins(ZmxProgram *program) {
    GC_PUSH_PROTECTION(&program->gc);
    char *assertMsg = "Assert failed.";
    load_native_func(
        program, "assert", native_assert,
        with_optionals(
            program, 1, 2,
            "condition", NULL, "message", new_string_obj(program, assertMsg, strlen(assertMsg))
        )
    );
    load_native_func(
        program, "print", native_print,
        with_optionals(
            program, 1, 3,
            "value", NULL,
            "newline", new_bool_obj(program, true), "destructure", new_bool_obj(program, false)
        )
    );
    load_native_func(program, "time", native_time, no_optionals(program, 0));
    GC_POP_PROTECTION(&program->gc);
}
