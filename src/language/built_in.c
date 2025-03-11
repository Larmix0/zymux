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
    static Obj *native_##name(Vm *vm, Obj **args, const u32 argAmount)

/** Reports a runtime error and returns the "native errored" signal. */
#define RETURN_ERROR(vm, ...) \
    do { \
        runtime_error(vm, __VA_ARGS__); \
        return NULL; \
    } while (false)

/** Returns an error signal without reporting a message. */
#define RETURN_ERROR_SIGNAL(vm) return NULL;

/** Returns an error where no arguments were expected, but some were provided nonetheless. */
#define RETURN_ERROR_TAKES_NO_ARGS(vm, nativeName, argAmount) \
    RETURN_ERROR(vm, nativeName " expected no arguments, got %"PRIu32" instead.", (argAmount))

/** Returns an error where 1 argument was expected, but it got 0 or 2+ arguments. */
#define RETURN_ERROR_TAKES_ONE_ARG(vm, nativeName, argAmount) \
    RETURN_ERROR(vm, nativeName " expected 1 argument, got %"PRIu32" instead.", (argAmount))

/** Returns an error where multiple arguments were expected, but the wrong amount was provided. */
#define RETURN_ERROR_INCORRECT_MULTIPLE_ARGS(vm, nativeName, expectedAmount, actualAmount) \
    RETURN_ERROR( \
        vm, nativeName " expected %"PRIu32" arguments, got %"PRIu32" instead.", \
        (expectedAmount), (actualAmount) \
    )

/** The default return of a native that doesn't explicitly return anything. */
#define DEFAULT_RETURN(vm) return AS_OBJ(new_null_obj((vm)->program))

/** Built-in printing function. */
NATIVE_FUNC(print) {
    if (argAmount != 1) {
        RETURN_ERROR_TAKES_ONE_ARG(vm, "print", argAmount);
    }
    print_obj(args[0], false);
    putchar('\n');
    DEFAULT_RETURN(vm);
}

/** Takes a condition and a string message to print if the assertion failed. */
NATIVE_FUNC(assert) {
    if (argAmount != 2) {
        RETURN_ERROR_INCORRECT_MULTIPLE_ARGS(vm, "assert", 2, argAmount);
    }
    if (args[1]->type != OBJ_STRING) {
        RETURN_ERROR(
            vm, "Expected argument 2 to be string, got %s instead.", obj_type_str(args[1]->type)
        );
    }

    if (!as_bool(vm->program, args[0])->boolean) {
        // Store the message in a buffer since the string gets freed when doing a runtime error.
        CharBuffer assertMessage = create_char_buffer();
        buffer_append_string(&assertMessage, AS_PTR(StringObj, args[1])->string);
        runtime_error(vm, assertMessage.text);
        free_char_buffer(&assertMessage);
        RETURN_ERROR_SIGNAL(vm);
    }
    DEFAULT_RETURN(vm);
}

/** Grab the current time and returns it. */
NATIVE_FUNC(time) {
    UNUSED_PARAMETER(args);
    if (argAmount != 0) {
        RETURN_ERROR_TAKES_NO_ARGS(vm, "time", argAmount);
    }

    const ZmxFloat currentTime = (ZmxFloat)clock() / CLOCKS_PER_SEC;
    return AS_OBJ(new_float_obj(vm->program, currentTime));
}

/** Loads a native function into the passed table. */
static void load_native_func(ZmxProgram *program, const char *name, NativeFunc func) {
    GC_PUSH_PROTECTION(&program->gc);
    StringObj *nameObj = new_string_obj(program, name, strlen(name));
    NativeFuncObj *native = new_native_func_obj(program, func, nameObj);
    
    table_set(&program->builtIn, AS_OBJ(nameObj), AS_OBJ(native));
    GC_POP_PROTECTION(&program->gc);
}

/** Loads all built-in objects into the program. */
void load_built_ins(ZmxProgram *program) {
    load_native_func(program, "print", native_print);
    load_native_func(program, "assert", native_assert);
    load_native_func(program, "time", native_time);
}
