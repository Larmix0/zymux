#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.h"
#include "char_buffer.h"
#include "compiler.h"
#include "dynamic_array.h"
#include "emitter.h"
#include "file.h"
#include "object.h"
#include "vm.h"

#if DEBUG_RUNTIME
    #include <stdio.h>

    #include "debug_runtime.h"
#endif

/** Reads the current instruction and increments IP to prepare for the next one. */
#define READ_INSTR(vm) (*(vm)->frame->ip++)

// TODO: turn the read_number function fully to a macro for performance?
// Pretty considerable since it's also used by the const reader.
/** Reads the upcoming number in the bytecode. Prefer this over the function in the VM. */
#define READ_NUMBER(vm) \
    ((vm)->frame->ip += (vm)->instrSize, \
    read_number( \
        &(vm)->frame->func->bytecode, \
        (vm)->frame->ip - (vm)->frame->func->bytecode.data - (vm)->instrSize, &(vm)->instrSize \
    ))

/** Reads the upcoming number and uses it to index an object in the current constant pool. */
#define READ_CONST(vm) ((vm)->frame->func->constPool.data[READ_NUMBER(vm)])

#define CREATE_STACK() {.objects = NULL, .capacity = 0}
#define PUSH(vm, obj) \
    do { \
        if ((vm)->stack.capacity < STACK_LENGTH(vm) + 1) { \
            reallocate_stack(vm); \
        } \
        *(vm)->frame->sp++ = (obj); \
    } while (false)
#define POP(vm) (*(--(vm)->frame->sp))
#define DROP(vm) (--(vm)->frame->sp)
#define DROP_AMOUNT(vm, amount) ((vm)->frame->sp -= (amount))
#define PEEK(vm) (*((vm)->frame->sp - 1))
#define PEEK_DEPTH(vm, depth) (*((vm)->frame->sp - (depth) - 1))
#define FREE_STACK(vm) (free((vm)->stack.objects))

#define BIN_LEFT(vm) (PEEK_DEPTH(vm, 1)) /** Left hand side object of a binary in the stack. */
#define BIN_RIGHT(vm) (PEEK_DEPTH(vm, 0)) /** Right hand side object of a binary in the stack. */

/** Resolves to a boolean of whether or not either of the binary numbers in the stack are floats. */
#define BIN_HAS_FLOAT(vm) (BIN_LEFT(vm)->type == OBJ_FLOAT || BIN_RIGHT(vm)->type == OBJ_FLOAT)

/** Writes a consistent runtime error message for all failed binary operations. */
#define BIN_OP_ERROR(vm, opString) \
    (runtime_error( \
        vm, "Can't use '%s' between %s and %s.", \
        opString, obj_type_str(BIN_LEFT(vm)->type), obj_type_str(BIN_RIGHT(vm)->type)) \
    )

/** 
 * Pushes resultNum and drops the last 2 elements (from the binary operation).
 * 
 * The resultNum passed is a C number (integer or float) which will be pushed
 * as the appropriate integer or float object after the binary operands have been dropped.
 * 
 * Used by passing a binary operation but not popping its operands from the stack while doing that.
 * 
 * Prints opString as the bugged operation if an error occurs.
 */
#define MATH_BIN_OP(vm, opString, resultNum) \
    do { \
        if (!IS_NUM(BIN_LEFT(vm)) || !IS_NUM(BIN_RIGHT(vm))) { \
            return BIN_OP_ERROR(vm, opString); \
        } \
        Obj *resultObj = BIN_HAS_FLOAT(vm) ? AS_OBJ(new_float_obj((vm)->program, resultNum)) \
            : AS_OBJ(new_int_obj((vm)->program, resultNum)); \
        DROP_AMOUNT(vm, 2); \
        PUSH(vm, resultObj); \
    } while (false)

/** 
 * Pushes resultBool and drops the last 2 elements (from the binary operation).
 * 
 * The resultBool passed is a resulting C boolean which will be pushed as a bool object
 * after the binary operands have been dropped.
 * 
 * Allows operands of any type
 * 
 * Used by passing a binary operation but not popping its operands from the stack while doing that.
 */
#define BOOL_BIN_OP(vm, resultBool) \
    do { \
        Obj *resultObj = AS_OBJ(new_bool_obj(vm->program, resultBool)); \
        DROP_AMOUNT(vm, 2); \
        PUSH(vm, resultObj); \
    } while (false)

/** BOOL_BIN_OP() wrapper which errors if the operands of the operation aren't both numbers. */
#define NUM_BOOL_BIN_OP(vm, opString, resultBool) \
    do { \
        if (!IS_NUM(BIN_LEFT(vm)) || !IS_NUM(BIN_RIGHT(vm))) { \
            return BIN_OP_ERROR(vm, opString); \
        } \
        BOOL_BIN_OP(vm, resultBool); \
    } while (false)

/** 
 * Binary operation where the operation is a bitwise one (only allowed on ints and bools).
 * 
 * Takes an opString, which is the operation's name as a string (like "left shift" for example),
 * and resultNum, which is the number result of the binary bitwise performed outside.
 */
#define BITWISE_BIN_OP(vm, opString, resultNum) \
    do { \
        if (!IS_BITWISABLE(BIN_LEFT(vm)) || !IS_BITWISABLE(BIN_RIGHT(vm))) { \
            return BIN_OP_ERROR(vm, opString); \
        } \
        Obj *resultObj = AS_OBJ(new_int_obj((vm)->program, resultNum)); \
        DROP_AMOUNT(vm, 2); \
        PUSH(vm, resultObj); \
    } while (false)

/** Reports a runtime error and always returns false to be used for convenience. */
bool runtime_error(Vm *vm, const char *format, ...) {
    const u32 bytecodeIdx = vm->frame->ip - vm->frame->func->bytecode.data - 1;
    va_list args;
    va_start(args, format);
    zmx_user_error(
        vm->program, vm->frame->func->positions.data[bytecodeIdx], "Runtime error", format, &args
    );
    va_end(args);
    return false;
}

/** 
 * Special reallocation function for the VM's stack.
 * 
 * The thing about the stack is there are a bunch of things that point in the middle of it
 * (like the stack pointer for example), but reallocation of memory might move the address
 * of the allocated memory and free the original, old one.
 * This in turn means that every pointer that points to an object inside the stack needs
 * to also be updated to the same index relative to the potentially new stack address.
 */
static void reallocate_stack(Vm *vm) {
    INCREASE_CAPACITY(vm->stack.capacity);
    Obj **originalStack = vm->stack.objects;
    vm->stack.objects = REALLOC(
        vm->stack.objects, vm->stack.capacity * sizeof(*vm->stack.objects)
    );
    if (vm->stack.objects == originalStack) {
        // Stack address didn't change, no need to move pointers in the stack to the new location.
        return;
    }
    
    for (u32 i = 0; i < vm->callStack.length; i++) {
        vm->callStack.data[i].sp = &vm->stack.objects[vm->callStack.data[i].sp - originalStack];
        vm->callStack.data[i].bp = &vm->stack.objects[vm->callStack.data[i].bp - originalStack];
    }
}

/** Pushes a new execution stack frame onto the passed VM. */
static void push_stack_frame(Vm *vm, FuncObj *func, Obj **bp, Obj **sp) {
    StackFrame frame = {.func = func, .ip = func->bytecode.data, .bp = bp, .sp = sp};
    APPEND_DA(&vm->callStack, frame);
    vm->frame = &LAST_ITEM_DA(&vm->callStack);
}

/** Pops the top frame on the passed VM and restores the previous one or NULL if there isn't any. */
static void pop_stack_frame(Vm *vm) {
    DROP_DA(&vm->callStack);
    vm->frame = vm->callStack.length == 0 ? NULL : &LAST_ITEM_DA(&vm->callStack);
}

/** Returns a VM initialized from the passed func. */
Vm create_vm(ZmxProgram *program, FuncObj *func) {
    Vm vm = {
        .program = program, .instrSize = INSTR_ONE_BYTE, .globals = create_table(),
        .callStack = CREATE_DA(), .openCaptures = CREATE_DA(),
        .frame = NULL, .stack = CREATE_STACK()
    };
    // Push the frame which requires the stack after the VM itself and its stack were initialized.
    push_stack_frame(&vm, func, vm.stack.objects, vm.stack.objects);
    return vm;
}

/** Frees all memory that the passed VM allocated. */
void free_vm(Vm *vm) {
    free_table(&vm->globals);
    FREE_DA(&vm->callStack);
    FREE_DA(&vm->openCaptures);
    FREE_STACK(vm);
}

/** Performs an assignment on some indexed array element. */
static bool list_assign_subscr(Vm *vm, ListObj *callee, Obj *subscript, Obj *value) {
    if (subscript->type != OBJ_INT) {
        return runtime_error(
            vm, "List indexing expected integer, got %s instead.", obj_type_str(subscript->type)
        );
    }
    const ZmxInt index = AS_PTR(IntObj, subscript)->number;
    if (IS_WITHIN_LENGTH(callee->items.length, index)) {
        callee->items.data[index] = value;
        return true;
    } else {
        return runtime_error(vm, "Index " ZMX_INT_FMT " is out of range.", index);
    }
    DROP_AMOUNT(vm, 2);
}

/** 
 * Assigns a key-value pair to a map.
 * If the key already exists, just change the value, otherwise add a new entry completely.
 */
static bool map_assign_subscr(Vm *vm, MapObj *callee, Obj *subscript, Obj *value) {
    if (!is_hashable(subscript)) {
        return runtime_error(vm, "%s key is not hashable.", obj_type_str(subscript->type));
    }
    table_set(&callee->table, subscript, value);
    DROP_AMOUNT(vm, 2);
    return true;
}

/** 
 * Performs a subscript get on an array using an integer.
 * 
 * Deletes the subscript and the callee of the subscript, and pushes the element on the callee
 * at the index of the integer subscript.
 */
static bool array_get_item(
    Vm *vm, ListObj *list, StringObj *str, Obj *subscript, const ZmxInt length, const bool isList
) {
    const ZmxInt index = AS_PTR(IntObj, subscript)->number;
    if (!IS_WITHIN_LENGTH(length, index)) {
        return runtime_error(vm, "Index " ZMX_INT_FMT " is out of range.", index);
    }
    Obj *result = isList ? list->items.data[index] 
        : AS_OBJ(new_string_obj(vm->program, str->string + index, 1));
    DROP_AMOUNT(vm, 2);
    PUSH(vm, result);
    return true;
}

/** 
 * Performs a subscript get on an array using range, slicing it.
 * 
 * Creates a subarray from the subscripted using the subscript range as a filter for which
 * elements of the array are included in the sliced array.
 * 
 * Because we're creating the subarray from a range, we have to iterate with an iterator,
 * which is pushed onto the stack so it doesn't get GCed.
 * 
 * After exhausting the iterator, we push the created subarray after
 * popping the original subscript, subscripted object, and the temporary iterator from the stack.
 */
static bool array_slice(
    Vm *vm, ListObj *list, StringObj *str, Obj *subscript, const ZmxInt length, const bool isList
) {
    ObjArray filteredList = CREATE_DA();
    CharBuffer filteredStr = create_char_buffer();

    IteratorObj *iterator = new_iterator_obj(vm->program, subscript);
    PUSH(vm, AS_OBJ(iterator)); // So iterator doesn't get GCed.
    Obj *current;
    while ((current = iterate(vm->program, iterator))) {
        const ZmxInt index = AS_PTR(IntObj, current)->number;
        if (!IS_WITHIN_LENGTH(length, index)) {
            FREE_DA(&filteredList);
            free_char_buffer(&filteredStr);
            return runtime_error(vm, "Slice goes out of range.");
        }
        if (isList) {
            APPEND_DA(&filteredList, list->items.data[index]);
        } else {
            buffer_append_char(&filteredStr, str->string[index]);
        }
    }
    Obj *result = isList ? AS_OBJ(new_list_obj(vm->program, filteredList))
        : AS_OBJ(new_string_obj(vm->program, filteredStr.text, filteredStr.length));
    if (!isList) {
        FREE_DA(&filteredList); // Only free if it's unused.
    }
    free_char_buffer(&filteredStr); // Always free here since new string copies the buffer anyway.
    DROP_AMOUNT(vm, 3); // Pop the subscript, the thing being subscripted, and the iterator.
    PUSH(vm, result);
    return true;
}

/** 
 * Gets a subscript from the passed array of ordered elements and pushes it without assigning.
 * 
 * Array subscripts are either an integer that captures one of their elements, or a range
 * which creates a subarray from the range's start, end, and step as a filter.
 * 
 * Returns true if successful, false if it errored.
 */
static bool array_get_subscr(Vm *vm, Obj *callee, Obj *subscript) {
    ASSERT(
        callee->type == OBJ_LIST || callee->type == OBJ_STRING,
        "Expected list or string for array subscript."
    );
    const bool isList = callee->type == OBJ_LIST;
    ListObj *list = isList ? AS_PTR(ListObj, callee) : NULL;
    StringObj *str = isList ? NULL : AS_PTR(StringObj, callee);
    const ZmxInt length = isList ? list->items.length : str->length;

    if (subscript->type == OBJ_INT) {
        return array_get_item(vm, list, str, subscript, length, isList);
    } else if (subscript->type == OBJ_RANGE) {
        return array_slice(vm, list, str, subscript, length, isList);
    }
    return runtime_error(
        vm, "Can't subscript %s with %s.", obj_type_str(callee->type), obj_type_str(subscript->type)
    );
}

/** 
 * Performs a subscript get on a map, which only accepts hashable subscripts for keys.
 * 
 * Deletes the subscript and the callee of the subscript, and pushes the corresponding value
 * of the key subscript.
 */
static bool map_get_subscr(Vm *vm, MapObj *callee, Obj *subscript) {
    if (!is_hashable(subscript)) {
        return runtime_error(vm, "%s key is not hashable.", obj_type_str(subscript->type));
    }
    Obj *result = table_get(&callee->table, subscript);
    if (result == NULL) {
        return runtime_error(
            vm, "Key '%s' does not exist in map.", as_string(vm->program, subscript)->string
        );
    }
    DROP_AMOUNT(vm, 2);
    PUSH(vm, result);
    return true;
}

/** 
 * Destructures an iterable which is expected to have "amount" of elements to destructure.
 * 
 * Creates an iterator to wrap the iterable at the top of the stack, and manually protects it
 * then starts a while loop iteration which loads all iterated elements on the stack.
 * 
 * If the amount of iterations ("i") is less than the expected number of iterations ("amount")
 * then the iterable didn't have enough elements,
 * and if the iterator wasn't fully exhausted ("current" not being NULL) then it had too many
 * elements instead. Both are an error.
 */
static bool destructure(Vm *vm, const u32 amount) {
    if (!is_iterable(PEEK(vm))) {
        return runtime_error(
            vm, "Expected iterable for destructuring, got %s instead.",
            obj_type_str(PEEK(vm)->type)
        );
    }
    GC_PUSH_PROTECTION(&vm->program->gc);
    IteratorObj *iterator = new_iterator_obj(vm->program, PEEK(vm));
    GC_PROTECT_OBJ(&vm->program->gc, AS_OBJ(iterator)); // Protect iterator while iterating.
    DROP(vm); // Now safe to pop iterable, as the protected iterator holds it.

    Obj *current;
    u32 i = 0;
    while ((current = iterate(vm->program, iterator)) && i < amount) {
        PUSH(vm, current);
        i++;
    }
    GC_POP_PROTECTION(&vm->program->gc);
    if (i < amount || current) {
        // One of the finishing conditions hasn't been met, so either too few or too many elements.
        return runtime_error(vm, "Expected %"PRIu32" elements in iterable.", amount);
    }
    return true;
}

/** Attempts to call the passed object. Returns whether or not it managed to call it. */
static bool call(Vm *vm, Obj *callee, Obj **args, const u32 argAmount) {
    switch (callee->type) {
    case OBJ_FUNC: {
        FuncObj *func = AS_PTR(FuncObj, callee);
        if (func->arity != argAmount) {
            return runtime_error(
                vm, "%s() expected %"PRIu32" %s, but got %"PRIu32" instead.",
                func->name->string, func->arity,
                func->arity == 1 ? "argument" : "arguments", argAmount
            );
        }
        push_stack_frame(vm, func, args - 1, vm->frame->sp);
        return true;
    }
    case OBJ_CLASS: {
        ClassObj *cls = AS_PTR(ClassObj, callee);
        PEEK_DEPTH(vm, argAmount) = AS_OBJ(new_instance_obj(vm->program, cls));
        if (cls->init != NULL) {
            return call(vm, AS_OBJ(cls->init), args, argAmount);
        } else if (argAmount != 0) {
            return runtime_error(vm, "Expected 0 arguments, but got %"PRIu32" instead.", argAmount);
        }
        return true;
    }
    case OBJ_METHOD: {
        MethodObj *method = AS_PTR(MethodObj, callee);
        PEEK_DEPTH(vm, argAmount) = AS_OBJ(method->instance);
        return call(vm, AS_OBJ(method->func), args, argAmount);
    }
    case OBJ_NATIVE_FUNC: {
        Obj *nativeReturn = AS_PTR(NativeFuncObj, callee)->func(vm, args, argAmount);
        if (nativeReturn == NULL) {
            return false;
        }
        PEEK_DEPTH(vm, argAmount) = nativeReturn;
        DROP_AMOUNT(vm, argAmount);
        return true;
    }
    default:
        return runtime_error(vm, "Can't call object of type %s.", obj_type_str(PEEK(vm)->type));
    }
}

/** 
 * Closes as many "open" captures that have now been deleted off of the stack.
 * 
 * Iterates over all of the open captures, and then closes any whose stack location corresponds
 * to that of a variable that's about to be popped.
 * 
 * Closing a capture is simply setting the final value of the local to be popped
 * on the captured object so it can be used later outside the stack frame.
 * 
 * pops is how many variables are going to be popped and need to be checked to see
 * if they hold open captures
 */
static void close_captures(Vm *vm, const u32 pops) {
    for (i64 i = (i64)vm->openCaptures.length - 1; i >= 0; i--) {
        CapturedObj *toClose = vm->openCaptures.data[i];
        if (toClose->stackLocation >= STACK_LENGTH(vm) - pops) {
            toClose->isOpen = false;
            toClose->captured = vm->stack.objects[toClose->stackLocation];
            DROP_DA(&vm->openCaptures);
        }
    }
}

/** 
 * Executes a return from the topmost call.
 * 
 * Uses the object at the top of the stack as the return value, by popping the whole function,
 * and replacing the callee object with that returned object (which also got popped).
 * 
 * Also, closes all alive variables that are gonna be deleted from the stack after the return.
 */
static void call_return(Vm *vm) {
    const u32 arity = vm->frame->func->arity;
    Obj *returned = PEEK(vm);
    if (vm->openCaptures.length > 0) {
        close_captures(vm, vm->frame->sp - vm->frame->bp);
    }
    
    pop_stack_frame(vm);
    DROP_AMOUNT(vm, arity); // Remove the args which were added before invoking the function.
    PEEK(vm) = returned;
}

/** Captures a variable by creating a capture object and placing it in the appropriate places. */
static void capture_variable(Vm *vm, Obj *capturedObj, const u32 stackLocation) {
    ClosureObj *closure = AS_PTR(ClosureObj, vm->frame->func);
    CapturedObj *capture = new_captured_obj(vm->program, capturedObj, stackLocation);
    APPEND_DA(&closure->captures, capture);
    APPEND_DA(&vm->openCaptures, capture);
}

/** Set (assign or create if one doesn't exist) a value on some property inside an object. */
static bool set_property(Vm *vm, Obj *originalObj, StringObj *name, Obj *value) {
    switch (originalObj->type) {
    case OBJ_INSTANCE:
        table_set(&AS_PTR(InstanceObj, originalObj)->fields, AS_OBJ(name), value);
        DROP(vm); // Pop the object being accessed, leaving only the value on the top of the stack.
        break;
    default:
        return runtime_error(
            vm, "Can't set property on object of type %s.", obj_type_str(originalObj->type)
        );  
    }
    return true;
}

/** Access a property inside some object that might have multiple properties inside it. */
static bool get_property(Vm *vm, Obj *originalObj, StringObj *name) {
    switch (originalObj->type) {
    case OBJ_INSTANCE: {
        InstanceObj *instance = AS_PTR(InstanceObj, originalObj);
        Obj *field = table_get(&instance->fields, AS_OBJ(name));
        if (field != NULL) {
            PEEK(vm) = field;
            return true;
        }
        Obj *method = table_get(&instance->cls->methods, AS_OBJ(name));
        if (method != NULL) {
            PEEK(vm) = AS_OBJ(new_method_obj(vm->program, instance, AS_PTR(FuncObj, method)));
            return true;
        }
        return runtime_error(vm, "No property called '%s'.", name->string);
    }
    case OBJ_ENUM: {
        EnumObj *enumObj = AS_PTR(EnumObj, originalObj);
        Obj *propertyIdx = table_get(&enumObj->lookupTable, AS_OBJ(name));
        if (propertyIdx == NULL) {
            return runtime_error(vm, "No enum member called '%s'.", name->string);
        }
        ASSERT(propertyIdx->type == OBJ_INT, "Expected enum lookup table value to be int.");
        PEEK(vm) = enumObj->members.data[AS_PTR(IntObj, propertyIdx)->number];
        break;
    }
    default:
        return runtime_error(
            vm, "Object of type %s doesn't have properties.", obj_type_str(originalObj->type)
        );
    }

    return true;
}

/** 
 * Executes all the bytecode in the passed VM's function object.
 * 
 * Returns whether or not executing the VM was successful (no errors).
 */
static bool execute_vm(Vm *vm) {
    while (true) {
#if DEBUG_RUNTIME
        print_runtime_state(
            vm->frame->func, vm->stack.objects, STACK_LENGTH(vm),
            vm->frame->ip - vm->frame->func->bytecode.data, vm->instrSize
        );
#endif
        const OpCode opcode = READ_INSTR(vm);
        switch (opcode) {
        case OP_LOAD_CONST:
            PUSH(vm, READ_CONST(vm));
            break;
        case OP_ARG_16: vm->instrSize = INSTR_TWO_BYTES; break;
        case OP_ARG_32: vm->instrSize = INSTR_FOUR_BYTES; break;
        case OP_TRUE: PUSH(vm, AS_OBJ(new_bool_obj(vm->program, true))); break;
        case OP_FALSE: PUSH(vm, AS_OBJ(new_bool_obj(vm->program, false))); break;
        case OP_NULL: PUSH(vm, AS_OBJ(new_null_obj(vm->program))); break;
        case OP_ADD:
            if (BIN_LEFT(vm)->type == OBJ_STRING && BIN_RIGHT(vm)->type == OBJ_STRING) {
                Obj *result = AS_OBJ(concatenate(
                    vm->program, AS_PTR(StringObj, BIN_LEFT(vm)), AS_PTR(StringObj, BIN_RIGHT(vm))
                ));
                DROP_AMOUNT(vm, 2);
                PUSH(vm, result);
            } else {
                MATH_BIN_OP(vm, "+", NUM_VAL(BIN_LEFT(vm)) + NUM_VAL(BIN_RIGHT(vm)));
            }
            break;
        case OP_SUBTRACT:
            MATH_BIN_OP(vm, "-", NUM_VAL(BIN_LEFT(vm)) - NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_MULTIPLY:
            MATH_BIN_OP(vm, "*", NUM_VAL(BIN_LEFT(vm)) * NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_DIVIDE:
            if (IS_NUM(BIN_LEFT(vm)) && IS_NUM(BIN_RIGHT(vm)) && NUM_VAL(BIN_RIGHT(vm)) == 0) {
                return runtime_error(vm, "Can't divide by 0.");
            }
            MATH_BIN_OP(vm, "/", NUM_VAL(BIN_LEFT(vm)) / NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_MODULO:
            if (IS_NUM(BIN_LEFT(vm)) && IS_NUM(BIN_RIGHT(vm)) && NUM_VAL(BIN_RIGHT(vm)) == 0) {
                return runtime_error(vm, "Can't modulo by 0.");
            }
            // Modulo doesn't handle floats in C, so use fmod() if there's a float in the operation.
            MATH_BIN_OP(
                vm, "%",
                BIN_HAS_FLOAT(vm) ? fmod(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm)))
                    : AS_PTR(IntObj, BIN_LEFT(vm))->number % AS_PTR(IntObj, BIN_RIGHT(vm))->number
            );
            break;
        case OP_EXPONENT:
            MATH_BIN_OP(vm, "**", pow(NUM_VAL(BIN_LEFT(vm)), NUM_VAL(BIN_RIGHT(vm))));
            break;
        case OP_LSHIFT: 
            BITWISE_BIN_OP(vm, "<<", BIT_VAL(BIN_LEFT(vm)) << BIT_VAL(BIN_RIGHT(vm)));
            break;
        case OP_RSHIFT:
            BITWISE_BIN_OP(vm, ">>", BIT_VAL(BIN_LEFT(vm)) >> BIT_VAL(BIN_RIGHT(vm)));
            break;
        case OP_BITWISE_OR:
            BITWISE_BIN_OP(vm, "|", BIT_VAL(BIN_LEFT(vm)) | BIT_VAL(BIN_RIGHT(vm)));
            break;
        case OP_BITWISE_AND:
            BITWISE_BIN_OP(vm, "&", BIT_VAL(BIN_LEFT(vm)) & BIT_VAL(BIN_RIGHT(vm)));
            break;
        case OP_XOR:
            BITWISE_BIN_OP(vm, "^", BIT_VAL(BIN_LEFT(vm)) ^ BIT_VAL(BIN_RIGHT(vm)));
            break;
        case OP_EQUAL:
            BOOL_BIN_OP(vm, equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm)));
            break;
        case OP_NOT_EQUAL:
            BOOL_BIN_OP(vm, !equal_obj(BIN_LEFT(vm), BIN_RIGHT(vm)));
            break;
        case OP_GREATER:
            NUM_BOOL_BIN_OP(vm, ">", NUM_VAL(BIN_LEFT(vm)) > NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_GREATER_EQ:
            NUM_BOOL_BIN_OP(vm, ">=", NUM_VAL(BIN_LEFT(vm)) >= NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_LESS:
            NUM_BOOL_BIN_OP(vm, "<", NUM_VAL(BIN_LEFT(vm)) < NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_LESS_EQ:
            NUM_BOOL_BIN_OP(vm, "<=", NUM_VAL(BIN_LEFT(vm)) >= NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_MINUS:
            if (PEEK(vm)->type == OBJ_INT) {
                const ZmxInt negated = -AS_PTR(IntObj, POP(vm))->number;
                PUSH(vm, AS_OBJ(new_int_obj(vm->program, negated)));
            } else if (PEEK(vm)->type == OBJ_FLOAT) {
                const ZmxFloat negated = -AS_PTR(FloatObj, POP(vm))->number;
                PUSH(vm, AS_OBJ(new_float_obj(vm->program, negated)));
            } else {
                return runtime_error(
                    vm, "Can't negate object of type %s.", obj_type_str(PEEK(vm)->type)
                );
            }
            break;
        case OP_NOT: {
            BoolObj *asBool = new_bool_obj(vm->program, !as_bool(vm->program, POP(vm))->boolean);
            PUSH(vm, AS_OBJ(asBool));
            break;
        }
        case OP_TILDE: {
            ZmxInt flipped;
            if (PEEK(vm)->type == OBJ_INT) {
                flipped = ~AS_PTR(IntObj, PEEK(vm))->number;
            } else if (PEEK(vm)->type == OBJ_BOOL) {
                flipped = ~((ZmxInt)AS_PTR(BoolObj, PEEK(vm))->boolean);
            } else {
                return runtime_error(vm, "Can't apply '~' to %s.", obj_type_str(PEEK(vm)->type));
            }
            PEEK(vm) = AS_OBJ(new_int_obj(vm->program, flipped));
            break;
        }
        case OP_IS: {
            ObjType checkedType;
            switch ((DataType)READ_NUMBER(vm)) {
            case TYPE_INT: checkedType = OBJ_INT; break;
            case TYPE_FLOAT: checkedType = OBJ_FLOAT; break;
            case TYPE_BOOL: checkedType = OBJ_BOOL; break;
            case TYPE_STRING: checkedType = OBJ_STRING; break;
            TOGGLEABLE_DEFAULT_UNREACHABLE();
            }
            PEEK(vm) = AS_OBJ(new_bool_obj(vm->program, PEEK(vm)->type == checkedType));
            break;
        }
        case OP_AS: {
            Obj *converted;
            switch ((DataType)READ_NUMBER(vm)) {
            case TYPE_INT: converted = AS_OBJ(as_int(vm->program, PEEK(vm))); break;
            case TYPE_FLOAT: converted = AS_OBJ(as_float(vm->program, PEEK(vm))); break;
            case TYPE_BOOL: converted = AS_OBJ(as_bool(vm->program, PEEK(vm))); break;
            case TYPE_STRING: converted = AS_OBJ(as_string(vm->program, PEEK(vm))); break;
            TOGGLEABLE_DEFAULT_UNREACHABLE();
            }
            if (converted == NULL) {
                if (PEEK(vm)->type == OBJ_STRING) {
                    return runtime_error(
                        vm, "Can't convert string '%s' to number.",
                        AS_PTR(StringObj, PEEK(vm))->string
                    );
                }
                return runtime_error(
                    vm, "Can't convert %s to a number.", obj_type_str(PEEK(vm)->type)
                );
            }
            PEEK(vm) = AS_OBJ(converted);
            break;
        }
        case OP_FINISH_STRING: {
            const u32 amount = READ_NUMBER(vm);
            StringObj *string = new_string_obj(vm->program, "", 0);

            // Build a string from the deepest/oldest till the outermost/newest one in the stack.
            for (u32 i = 1; i <= amount; i++) {
                string = concatenate(
                    vm->program, string, AS_PTR(StringObj, PEEK_DEPTH(vm, amount - i))
                );
            }
            DROP_AMOUNT(vm, amount);
            PUSH(vm, AS_OBJ(string));
            break;
        }
        case OP_RANGE: {
            Obj *step = PEEK(vm);
            Obj *end = PEEK_DEPTH(vm, 1);
            Obj *start = PEEK_DEPTH(vm, 2);
            if (start->type != OBJ_INT || end->type != OBJ_INT || step->type != OBJ_INT) {
                return runtime_error(
                    vm, "Range takes 3 integers, got %s, %s, and %s instead.",
                    obj_type_str(start->type), obj_type_str(end->type), obj_type_str(step->type)
                );
            }
            if (AS_PTR(IntObj, step)->number < 0) {
                return runtime_error(vm, "Range's step can't be a negative number.");
            }
            RangeObj *range = new_range_obj(
                vm->program, NUM_VAL(start), NUM_VAL(end), NUM_VAL(step)
            );
            DROP_AMOUNT(vm, 3);
            PUSH(vm, AS_OBJ(range));
            break;
        }
        case OP_TERNARY: {
            Obj *falseExpr = POP(vm);
            Obj *trueExpr = POP(vm);
            Obj *condition = POP(vm);
            PUSH(vm, as_bool(vm->program, condition)->boolean ? trueExpr : falseExpr);
            break;
        }
        case OP_LIST: {
            const u32 length = READ_NUMBER(vm);
            ObjArray items = CREATE_DA();
            for (u32 i = 1; i <= length; i++) {
                APPEND_DA(&items, PEEK_DEPTH(vm, length - i));
            }
            Obj *list = AS_OBJ(new_list_obj(vm->program, items));
            DROP_AMOUNT(vm, length);
            PUSH(vm, list);
            break;
        }
        case OP_MAP: {
            // x2 the amount of entries to account for each entry being 2: a key and value.
            const u32 length = READ_NUMBER(vm) * 2;
            Table entries = create_table();
            for (u32 i = 0; i < length; i += 2) {
                Obj *value = PEEK_DEPTH(vm, i);
                Obj *key = PEEK_DEPTH(vm, i + 1);
                if (!is_hashable(key)) {
                    return runtime_error(vm, "Can't hash %s key.", obj_type_str(key->type));
                }
                table_set(&entries, key, value);
            }
            Obj *map = AS_OBJ(new_map_obj(vm->program, entries));
            DROP_AMOUNT(vm, length);
            PUSH(vm, map);
            break;
        }
        case OP_GET_BUILT_IN: {
            Obj *value = table_get(&vm->program->builtIn, READ_CONST(vm));
            PUSH(vm, value);
            break;
        }
        case OP_DECLARE_GLOBAL:
            table_set(&vm->globals, READ_CONST(vm), PEEK(vm));
            DROP(vm);
            break;
        case OP_ASSIGN_GLOBAL:
            table_set(&vm->globals, READ_CONST(vm), PEEK(vm));
            break;
        case OP_GET_GLOBAL: {
            Obj *value = table_get(&vm->globals, READ_CONST(vm));
            PUSH(vm, value);
            break;
        }
        case OP_ASSIGN_LOCAL:
            vm->frame->bp[READ_NUMBER(vm)] = PEEK(vm);
            break;
        case OP_GET_LOCAL: {
            Obj *value = vm->frame->bp[READ_NUMBER(vm)];
            PUSH(vm, value);
            break;
        }
        case OP_CAPTURE:
            ASSERT(vm->frame->func->isClosure, "Tried to capture inside non-closure.");
            capture_variable(vm, PEEK(vm), STACK_LENGTH(vm) - 1);
            break;
        case OP_CAPTURE_AT: {
            ASSERT(vm->frame->func->isClosure, "Tried to capture inside non-closure.");
            const u32 at = READ_NUMBER(vm);
            capture_variable(vm, vm->frame->bp[at], vm->frame->bp + at - vm->stack.objects);
            break;
        }
        case OP_ASSIGN_CAPTURED: {
            ASSERT(vm->frame->func->isClosure, "Tried to assign captured inside non-closure.");
            ClosureObj *closure = AS_PTR(ClosureObj, vm->frame->func);
            CapturedObj *capture = closure->captures.data[READ_NUMBER(vm)];

            if (capture->isOpen) {
                vm->stack.objects[capture->stackLocation] = PEEK(vm);
            } else {
                capture->captured = PEEK(vm);
            }
            break;
        }
        case OP_GET_CAPTURED: {
            ASSERT(vm->frame->func->isClosure, "Tried to get captured inside non-closure.");
            ClosureObj *closure = AS_PTR(ClosureObj, vm->frame->func);
            CapturedObj *capture = closure->captures.data[READ_NUMBER(vm)];

            if (capture->isOpen) {
                PUSH(vm, vm->stack.objects[capture->stackLocation]);
            } else {
                PUSH(vm, capture->captured);
            }
            break;
        }
        case OP_SET_PROPERTY: {
            Obj *originalObj = PEEK(vm);
            StringObj *name = AS_PTR(StringObj, READ_CONST(vm));
            Obj *value = PEEK_DEPTH(vm, 1);
            if (!set_property(vm, originalObj, name, value)) {
                return false;
            }
            break;
        }
        case OP_GET_PROPERTY: {
            Obj *originalObj = PEEK(vm);
            StringObj *name = AS_PTR(StringObj, READ_CONST(vm));
            if (!get_property(vm, originalObj, name)) {
                return false;
            }
            break;
        }
        case OP_DESTRUCTURE: {
            const u32 amount = READ_NUMBER(vm);
            if (!destructure(vm, amount)) {
                return false;
            }
            break;
        }
        case OP_CALL: {
            const u32 argAmount = READ_NUMBER(vm);
            if (!call(vm, PEEK_DEPTH(vm, argAmount), vm->frame->sp - argAmount, argAmount)) {
                return false;
            }
            break;
        }
        case OP_ASSIGN_SUBSCR: {
            Obj *subscript = PEEK(vm);
            Obj *callee = PEEK_DEPTH(vm, 1);
            Obj *value = PEEK_DEPTH(vm, 2);
            if (callee->type == OBJ_LIST) {
                if (!list_assign_subscr(vm, AS_PTR(ListObj, callee), subscript, value)) {
                    return false;
                }
            } else if (callee->type == OBJ_MAP) {
                if (!map_assign_subscr(vm, AS_PTR(MapObj, callee), subscript, value)) {
                    return false;
                }
            } else {
                return runtime_error(
                    vm, "Can't subscript assign to %s.", obj_type_str(callee->type)
                );
            }
            break;
        }
        case OP_GET_SUBSCR: {
            Obj *subscript = PEEK(vm);
            Obj *callee = PEEK_DEPTH(vm, 1);
            if (callee->type == OBJ_LIST || callee->type == OBJ_STRING) {
                if (!array_get_subscr(vm, callee, subscript)) {
                    return false;
                }
            } else if (callee->type == OBJ_MAP) {
                if (!map_get_subscr(vm, AS_PTR(MapObj, callee), subscript)) {
                    return false;
                }
            } else {
                runtime_error(vm, "Can't subscript object of type %s.", callee->type);
            }
            break;
        }
        case OP_CLOSURE: {
            ClosureObj *closure = new_closure_obj(
                vm->program, AS_PTR(FuncObj, READ_CONST(vm)), false
            );
            if (vm->frame->func->isClosure) {
                ClosureObj *frameClosure = AS_PTR(ClosureObj, vm->frame->func);
                for (u32 i = 0; i < frameClosure->captures.length; i++) {
                    APPEND_DA(&closure->captures, frameClosure->captures.data[i]);
                }
            }
            PUSH(vm, AS_OBJ(closure));
            break;
        }
        case OP_CREATE_CLASS: {
            ClassObj *cls = new_class_obj(vm->program, AS_PTR(StringObj, READ_CONST(vm)));
            PUSH(vm, AS_OBJ(cls));
            break;
        }
        case OP_ADD_METHODS: {
            const u32 amount = READ_NUMBER(vm);
            ClassObj *cls = AS_PTR(ClassObj, PEEK_DEPTH(vm, amount));
            for (u32 i = 0; i < amount; i++) {
                // Order of appending doesn't matter since we're using a hash table.
                FuncObj *method = AS_PTR(FuncObj, PEEK_DEPTH(vm, i));
                table_set(&cls->methods, AS_OBJ(method->name), AS_OBJ(method));
            }
            DROP_AMOUNT(vm, amount);
            break;
        }
        case OP_ADD_INIT: {
            ClassObj *cls = AS_PTR(ClassObj, PEEK_DEPTH(vm, 1));
            cls->init = AS_PTR(FuncObj, POP(vm));
            break;
        }
        case OP_JUMP: {
            const u32 jump = READ_NUMBER(vm);
            vm->frame->ip += jump;
            break;
        }
        case OP_JUMP_BACK: {
            const u32 jump = READ_NUMBER(vm);
            vm->frame->ip -= jump;
            break;
        }
        case OP_JUMP_IF: {
            const u32 jump = READ_NUMBER(vm);
            if (as_bool(vm->program, PEEK(vm))->boolean) {
                vm->frame->ip += jump;
            }
            break;
        }
        case OP_JUMP_IF_NOT: {
            const u32 jump = READ_NUMBER(vm);
            if (!(as_bool(vm->program, PEEK(vm))->boolean)) {
                vm->frame->ip += jump;
            }
            break;
        }
        case OP_MAKE_ITER: {
            if (!is_iterable(PEEK(vm))) {
                return runtime_error(
                    vm, "Can't iterate over object of type %s.", obj_type_str(PEEK(vm)->type)
                );
            }
            IteratorObj *iterator = new_iterator_obj(vm->program, PEEK(vm));
            PEEK(vm) = AS_OBJ(iterator);
            break;
        }
        case OP_ITER_OR_JUMP: {
            ASSERT(PEEK(vm)->type == OBJ_ITERATOR, "Must iterate with an iterator.");
            IteratorObj *iterator = AS_PTR(IteratorObj, PEEK(vm));
            const u32 jump = READ_NUMBER(vm);
            
            Obj *element = iterate(vm->program, iterator);
            if (element == NULL) {
                vm->frame->ip += jump;
            } else {
                PEEK_DEPTH(vm, 1) = element;
            }
            break;
        }
        case OP_POP_JUMP_IF_NOT: {
            const u32 jump = READ_NUMBER(vm);
            if (!(as_bool(vm->program, PEEK(vm))->boolean)) {
                vm->frame->ip += jump;
            }
            DROP(vm);
            break;
        }
        case OP_POP_JUMP_BACK_IF: {
            const u32 jump = READ_NUMBER(vm);
            if (as_bool(vm->program, PEEK(vm))->boolean) {
                vm->frame->ip -= jump;
            }
            DROP(vm);
            break;
        }
        case OP_POP_LOCAL:
            if (vm->openCaptures.length > 0) {
                close_captures(vm, 1);
            }
            DROP(vm);
            break;
        case OP_POP_LOCALS: {
            const u32 pops = READ_NUMBER(vm);
            if (vm->openCaptures.length > 0) {
                close_captures(vm, pops);
            }
            DROP_AMOUNT(vm, pops);
            break;
        }
        case OP_POP_CAPTURES:
            ASSERT(vm->frame->func->isClosure, "Tried to pop captured inside non-closure.");
            DROP_AMOUNT_DA(&AS_PTR(ClosureObj, vm->frame->func)->captures, READ_NUMBER(vm));
            break;
        case OP_RETURN:
            ASSERT(vm->callStack.length > 1, "Tried to return top-level or a nonexistent level.");
            call_return(vm);
            break;
        case OP_CLOSURE_RETURN:
            ASSERT(vm->frame->func->isClosure, "Tried to closure return from non-closure.");
            ClosureObj *closure = AS_PTR(ClosureObj, vm->frame->func);
            DROP_AMOUNT_DA(&closure->captures, READ_NUMBER(vm));
            call_return(vm);
            break;
        case OP_COPY_TOP: {
            Obj *top = PEEK(vm);
            PUSH(vm, top);
            break;
        }
        case OP_END:
            return true;
        TOGGLEABLE_DEFAULT_UNREACHABLE();
        }
    }
}

/** 
 * Main function for interpreting a virtual machine.
 * 
 * Wrapper around execute_vm() for debugging and loading built-in names.
 * Returns whether or not we encountered a runtime error during execution.
 */
bool interpret(Vm *vm) {
#if DEBUG_RUNTIME
    printf("-------------------- VM START --------------------\n");
#endif
    const bool succeeded = execute_vm(vm);
#if DEBUG_RUNTIME
    printf("-------------------- VM END --------------------\n");
#endif
    return succeeded;
}

/** Simply executes the passed source string and frees all used memory except the program's. */
bool interpret_source(ZmxProgram *program, char *source) {
    Compiler compiler = compile_source(program, source);
    if (compiler.func == NULL) {
        return false; // Failed to compile.
    }
    Vm vm = create_vm(program, compiler.func);
    program->gc.vm = &vm;
    program->gc.compiler = NULL;
    free_compiler(&compiler);

    interpret(&vm);
    program->gc.vm = NULL;
    free_vm(&vm);
    return !program->hasErrored;
}
