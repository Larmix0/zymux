#include <math.h>
#include <stdio.h>
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
    #include "debug_runtime.h"
#endif

/** 
 * Returns an error if an uncaught error happens in the interpreter loop (no do-while protection).
 * 
 * This is not protected with a do-while loop as it's meant exclusively for the interpreter loop,
 * and it uses 'break' to immediately end executing the instruction regardless of whether or not
 * the error was caught.
 * However, protecting this macro with do-while would exit only the macro on 'break'.
 */
#define VM_LOOP_FINISH_ERROR(vm) \
    if (vm->program->hasErrored) { \
        return false; \
    } \
    break;

/** 
 * Raises a runtime error in the interpreter loop's switch statement (no do-while protection).
 * 
 * This is not protected with a do-while loop as it's meant exclusively for the interpreter loop,
 * and the exit line will use 'break' to get out of the instruction, which is meant to be
 * the loop switch's break, not a do-while.
 * 
 * This is done so that when an error is caught it stops executing the erroneous
 * instruction immediately, and goes to the catch point. 
 */
#define VM_LOOP_RUNTIME_ERROR(vm, ...) \
    RUNTIME_ERROR(vm, __VA_ARGS__); \
    VM_LOOP_FINISH_ERROR(vm); \

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
    VM_LOOP_RUNTIME_ERROR( \
        vm, "Can't use '%s' between %s and %s.", \
        opString, obj_type_str(BIN_LEFT(vm)->type), obj_type_str(BIN_RIGHT(vm)->type) \
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
            BIN_OP_ERROR(vm, opString); \
            break; \
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
            BIN_OP_ERROR(vm, opString); \
            break; \
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
            BIN_OP_ERROR(vm, opString); \
            break; \
        } \
        Obj *resultObj = AS_OBJ(new_int_obj((vm)->program, resultNum)); \
        DROP_AMOUNT(vm, 2); \
        PUSH(vm, resultObj); \
    } while (false)

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
    vm->stack.objects = REALLOC(vm->stack.objects, vm->stack.capacity * sizeof(*vm->stack.objects));
    if (vm->stack.objects == originalStack) {
        // Stack address didn't change, no need to move pointers in the stack to the new location.
        return;
    }
    
    for (u32 i = 0; i < vm->callStack.length; i++) {
        vm->callStack.data[i].sp = &vm->stack.objects[vm->callStack.data[i].sp - originalStack];
        vm->callStack.data[i].bp = &vm->stack.objects[vm->callStack.data[i].bp - originalStack];
    }
}

/** Creates and returns a new catch state in the current VM and its stack frame. */
static CatchState create_catch_state(Vm *vm, u8 *ip, const bool saveError) {
    const bool isClosure = vm->frame->func->isClosure;
    const u32 capturesAmount = isClosure ? AS_PTR(ClosureObj, vm->frame->func)->captures.length : 0;

    CatchState catchState = {
        .func = vm->frame->func, .ip = ip,
        .localsAmount = STACK_LENGTH(vm), .capturesAmount = capturesAmount,
        .openCapturesAmount = vm->openCaptures.length, .saveError = saveError
    };
    return catchState;
}

/** 
 * Prints a stack trace for a runtime error.
 * 
 * Prints in an order that makes the most recently executing function appear closest to the bottom.
 * TODO: extend this by adding file information when imports are added.
 */
static void print_stack_trace(Vm *vm, const u32 bytecodeIdx) {
    StringObj *mainNameObj = new_string_obj(vm->program, MAIN_NAME, strlen(MAIN_NAME));

    fprintf(stderr, RED "Stack trace:\n" DEFAULT_COLOR);
    for (u32 i = 0; i < vm->callStack.length; i++) {
        FuncObj *func = vm->callStack.data[i].func;
        fprintf(stderr, INDENT);

        if (equal_obj(AS_OBJ(func->name), AS_OBJ(mainNameObj))) {
            fprintf(stderr, "%s: ", func->name->string);
        } else {
            fprintf(stderr, "%s(): ", func->name->string);
        }
        fprintf(stderr, "line %d\n", func->positions.data[bytecodeIdx].line);
    }
    fputc('\n', stderr);
}

/** 
 * Catches a runtime error by setting its catching state as the current one executing.
 * 
 * Prints the caught error out if debugging is enabled, and pushes the passed error message onto
 * the stack if the catch stores the message as a variable.
 */
static void catch_error(Vm *vm, StringObj *errorMessage) {
    CatchState catch = POP_DA(&vm->catches);
    const u32 localPops = STACK_LENGTH(vm) - catch.localsAmount;
    DROP_AMOUNT(vm, localPops);
    const u32 openCapturePops = vm->openCaptures.length - catch.openCapturesAmount;
    DROP_AMOUNT_DA(&vm->openCaptures, openCapturePops);

    u32 capturePops = 0;
    if (catch.func->isClosure) {
        CapturedObjArray *captures = &AS_PTR(ClosureObj, catch.func)->captures;
        capturePops = captures->length - catch.capturesAmount;
        DROP_AMOUNT_DA(captures, capturePops);
    }

    vm->frame->func = catch.func;
    vm->frame->ip = catch.ip;
    if (catch.saveError) {
        PUSH(vm, AS_OBJ(errorMessage));
    }

#if DEBUG_RUNTIME
    print_caught_runtime_error(
        vm->program, catch.func, localPops, capturePops, openCapturePops, errorMessage->string
    );
#endif
}

/** 
 * Base function that tries to raise a runtime error with an object error message.
 * 
 * Sets a catch state and doesn't error if there's one. Meant only as a base error function,
 * so prefer to use macros that wrap this instead of directly calling it.
 */
void base_runtime_error(Vm *vm, StringObj *errorMessage) {
    if (vm->catches.length != 0) {
        catch_error(vm, errorMessage);
    } else {
        const u32 bytecodeIdx = vm->frame->ip - vm->frame->func->bytecode.data - 1;
        print_stack_trace(vm, bytecodeIdx);
        zmx_user_error(
            vm->program, vm->frame->func->positions.data[bytecodeIdx],
            "Runtime error", errorMessage->string, NULL
        );
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
        .catches = CREATE_DA(), .callStack = CREATE_DA(), .openCaptures = CREATE_DA(),
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
    FREE_DA(&vm->catches);
    FREE_STACK(vm);
}

/** Performs an assignment on some indexed array element. */
static bool list_assign_subscr(Vm *vm, ListObj *callee, Obj *subscript, Obj *value) {
    if (subscript->type != OBJ_INT) {
        RUNTIME_ERROR(
            vm, "List element assign expected integer subscript, got %s instead.",
            obj_type_str(subscript->type)
        );
        return false;
    }
    const ZmxInt index = AS_PTR(IntObj, subscript)->number;
    if (!IS_WITHIN_LENGTH(callee->items.length, index)) {
        RUNTIME_ERROR(vm, "Index " ZMX_INT_FMT " is out of range.", index);
        return false;
    }
    callee->items.data[index] = value;
    DROP_AMOUNT(vm, 2);
    return true;
}

/** 
 * Assigns a key-value pair to a map.
 * 
 * If the key already exists, just change the value, otherwise add a new entry completely.
 * Pop the callee and the subscript off of the stack afterwards.
 */
static bool map_assign_subscr(Vm *vm, MapObj *callee, Obj *subscript, Obj *value) {
    if (!is_hashable(subscript)) {
        RUNTIME_ERROR(vm, "%s key is not hashable.", obj_type_str(subscript->type));
        return false;
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
static bool array_get_item(Vm *vm, Obj *callee, Obj *subscript, const ZmxInt length) {
    const ZmxInt index = AS_PTR(IntObj, subscript)->number;
    if (!IS_WITHIN_LENGTH(length, index)) {
        RUNTIME_ERROR(vm, "Index " ZMX_INT_FMT " is out of range.", index);
        return false;
    }
    Obj *result;
    switch (callee->type) {
    case OBJ_LIST:
        result = AS_PTR(ListObj, callee)->items.data[index];
        break;
    case OBJ_STRING:
        result = AS_OBJ(new_string_obj(vm->program, AS_PTR(StringObj, callee)->string + index, 1));
        break;
    case OBJ_ENUM:
        result = AS_PTR(EnumObj, callee)->members.data[index];
        break;
    default: UNREACHABLE_ERROR(); // Assumes this was called with a subscriptable.
    }
    DROP_AMOUNT(vm, 2);
    PUSH(vm, result);
    return true;
}

/** 
 * Covers one iteration of creating an array slice.
 * 
 * Errors if the slice is out of bounds, and otherwise appends the element iterated.
 */
static bool slice_iteration(
    Vm *vm, Obj *callee, const ZmxInt index, const ZmxInt length, ObjArray *slice
) {
    if (!IS_WITHIN_LENGTH(length, index)) {
        FREE_DA(slice);
        RUNTIME_ERROR(vm, "Slice goes out of range.");
        return false;
    }
    Obj *slicedItem;
    switch (callee->type) {
    case OBJ_LIST:
        slicedItem = AS_PTR(ListObj, callee)->items.data[index];
        break;
    case OBJ_STRING:
        slicedItem = AS_OBJ(new_string_obj(
            vm->program, AS_PTR(StringObj, callee)->string + index, 1)
        );
        break;
    case OBJ_ENUM: {
        EnumObj *enumObj = AS_PTR(EnumObj, callee);
        EnumMemberObj *member = AS_PTR(EnumMemberObj, enumObj->members.data[index]);
        slicedItem = AS_OBJ(new_enum_member_obj(vm->program, enumObj, member->name, slice->length));
        break;
    }
    default:
        UNREACHABLE_ERROR();
    }
    APPEND_DA(slice, slicedItem);
    return true;
}

/** Returns a created object from the slice that was made on the callee depending on its type. */
static Obj *obj_from_slice(Vm *vm, Obj *callee, const ObjArray slice) {
    Obj *sliceResult;
    switch (callee->type) {
    case OBJ_LIST: {
        ObjArray list = CREATE_DA();
        for (u32 i = 0; i < slice.length; i++) {
            APPEND_DA(&list, slice.data[i]);
        }
        sliceResult = AS_OBJ(new_list_obj(vm->program, list));
        break;
    }
    case OBJ_STRING: {
        StringObj *string = new_string_obj(vm->program, "", 0);
        for (u32 i = 0; i < slice.length; i++) {
            string = concatenate(vm->program, string, AS_PTR(StringObj, slice.data[i]));
        }
        sliceResult = AS_OBJ(string);
        break;
    }
    case OBJ_ENUM: {
        EnumObj *enumObj = new_enum_obj(vm->program, AS_PTR(EnumObj, callee)->name);
        for (u32 i = 0; i < slice.length; i++) {
            EnumMemberObj *member = AS_PTR(EnumMemberObj, slice.data[i]);
            APPEND_DA(&enumObj->members, AS_OBJ(member));
            table_set(
                &enumObj->lookupTable,
                AS_OBJ(member->name), AS_OBJ(new_int_obj(vm->program, (ZmxInt)i))
            );
        }
        sliceResult = AS_OBJ(enumObj);
        break;
    }
    default:
        UNREACHABLE_ERROR(); // Shouldn't have been called.
    }
    return sliceResult;
}

/** 
 * Performs a subscript get on an array using range, slicing it.
 * 
 * Creates a subarray from the subscripted using the subscript range as a filter for which
 * elements of the array are included in the sliced array.
 * 
 * Because we're creating the subarray from a range, we have to iterate with an iterator.
 * After exhausting the iterator, we push the created subarray after
 * popping the original subscript and subscripted object from the stack.
 */
static bool slice(Vm *vm, Obj *callee, Obj *subscript, const ZmxInt length) {
    ObjArray slice = CREATE_DA();

    GC_PUSH_PROTECTION(&vm->program->gc);
    IteratorObj *iterator = new_iterator_obj(vm->program, subscript);
    Obj *current;
    while ((current = iterate(vm->program, iterator))) {
        const ZmxInt index = AS_PTR(IntObj, current)->number;
        if (!slice_iteration(vm, callee, index, length, &slice)) {
            return false;
        }
    }
    Obj *result = obj_from_slice(vm, callee, slice);
    DROP_AMOUNT(vm, 2);
    PUSH(vm, result);

    FREE_DA(&slice);
    GC_POP_PROTECTION(&vm->program->gc);
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
        callee->type == OBJ_LIST || callee->type == OBJ_STRING || callee->type == OBJ_ENUM,
        "Expected list, string, or enum for array subscript."
    );
    ZmxInt length;
    switch (callee->type) {
        case OBJ_LIST: length = AS_PTR(ListObj, callee)->items.length; break;
        case OBJ_STRING: length = AS_PTR(StringObj, callee)->length; break;
        case OBJ_ENUM: length = AS_PTR(EnumObj, callee)->members.length; break;
        default: UNREACHABLE_ERROR();
    } 

    if (subscript->type == OBJ_INT) {
        return array_get_item(vm, callee, subscript, length);
    } else if (subscript->type == OBJ_RANGE) {
        return slice(vm, callee, subscript, length);
    }
    RUNTIME_ERROR(
        vm, "Can't subscript %s with %s.", obj_type_str(callee->type), obj_type_str(subscript->type)
    );
    return false;
}

/** 
 * Performs a subscript get on a map, which only accepts hashable subscripts for keys.
 * 
 * Deletes the subscript and the callee of the subscript, and pushes the corresponding value
 * of the key subscript.
 */
static bool map_get_subscr(Vm *vm, MapObj *callee, Obj *subscript) {
    if (!is_hashable(subscript)) {
        RUNTIME_ERROR(vm, "%s key is not hashable.", obj_type_str(subscript->type));
        return false;
    }
    Obj *result = table_get(&callee->table, subscript);
    if (result == NULL) {
        RUNTIME_ERROR(
            vm, "Key '%s' doesn't exist in map.", as_string(vm->program, subscript)->string
        );
        return false;
    }
    DROP_AMOUNT(vm, 2);
    PUSH(vm, result);
    return true;
}

/** Set (assign or create if one doesn't exist) a value on some property inside an object. */
static bool set_property(Vm *vm, Obj *originalObj, StringObj *name, Obj *value) {
    switch (originalObj->type) {
    case OBJ_INSTANCE:
        table_set(&AS_PTR(InstanceObj, originalObj)->fields, AS_OBJ(name), value);
        DROP(vm); // Pop the object being accessed, leaving only the value on the top of the stack.
        break;
    default:
        RUNTIME_ERROR(
            vm, "Can't set property on object of type %s.", obj_type_str(originalObj->type)
        );
        return false;  
    }
    return true;
}

/** Access a property inside some object that might have multiple properties inside it. */
static bool get_property(Vm *vm, Obj *originalObj, StringObj *name) {
    switch (originalObj->type) {
    case OBJ_INSTANCE: {
        InstanceObj *instance = AS_PTR(InstanceObj, originalObj);
        Obj *field = table_get(&instance->fields, AS_OBJ(name));
        if (field) {
            PEEK(vm) = field;
            return true;
        }
        Obj *method = table_get(&instance->cls->methods, AS_OBJ(name));
        if (method) {
            PEEK(vm) = AS_OBJ(new_method_obj(vm->program, instance, AS_PTR(FuncObj, method)));
            return true;
        }
        RUNTIME_ERROR(vm, "No property called '%s'.", name->string);
        return false;
    }
    case OBJ_ENUM: {
        EnumObj *enumObj = AS_PTR(EnumObj, originalObj);
        Obj *propertyIdx = table_get(&enumObj->lookupTable, AS_OBJ(name));
        if (propertyIdx == NULL) {
            RUNTIME_ERROR(vm, "No enum member called '%s'.", name->string);
            return false;
        }
        ASSERT(propertyIdx->type == OBJ_INT, "Expected enum lookup table value to be int.");
        PEEK(vm) = enumObj->members.data[AS_PTR(IntObj, propertyIdx)->number];
        break;
    }
    default:
        RUNTIME_ERROR(
            vm, "Object of type %s doesn't have properties.", obj_type_str(originalObj->type)
        );
        return false;
    }

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
        RUNTIME_ERROR(
            vm, "Expected iterable for destructuring, got %s instead.", obj_type_str(PEEK(vm)->type)
        );
        return false;
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
        RUNTIME_ERROR(vm, "Expected %"PRIu32" elements in iterable.", amount);
        return false;
    }
    return true;
}

/** 
 * Attempts to iterate an element of a for loop instruction.
 * 
 * Returns the iterated object, or NULL if exhausted (jumps forward if exhausted).
 */
static Obj *for_iter_or_jump(Vm *vm) {
    ASSERT(PEEK(vm)->type == OBJ_ITERATOR, "Must iterate with an iterator.");
    IteratorObj *iterator = AS_PTR(IteratorObj, PEEK(vm));
    const u32 jump = READ_NUMBER(vm);
    
    Obj *element = iterate(vm->program, iterator);
    if (element == NULL) {
        vm->frame->ip += jump;
    }
    return element;
}

/** 
 * Checks that the pass passed arity is valid with the argument amount + keyword arguments.
 * 
 * Errors and returns false if the arity check fails, otherwise returns true.
 */
static bool check_arity(
    Vm *vm, const u32 minArity, const u32 maxArity, const u32 positionalAmount
) {
    MapObj *keywordArgs = AS_PTR(MapObj, PEEK(vm));
    char *argString = minArity == 1 ? "argument" : "arguments";
    const u32 allAmount = positionalAmount + keywordArgs->table.count;
    const bool tooFewArgs = positionalAmount < minArity;
    const bool tooManyArgs = allAmount > maxArity ;
    const bool noOptionals = minArity == maxArity;
    if (tooFewArgs && noOptionals) {
        RUNTIME_ERROR(
            vm, "Expected %"PRIu32" positional %s, got %"PRIu32" instead.",
            minArity, argString, positionalAmount
        );
        return false;
    } else if (tooManyArgs && noOptionals) {
        RUNTIME_ERROR(
            vm, "Expected %"PRIu32" %s, got %"PRIu32" instead.", minArity, argString, allAmount
        );
        return false;
    } else if (tooFewArgs) {
        RUNTIME_ERROR(
            vm, "Expected at least %"PRIu32" positional %s, but only got %"PRIu32".",
            minArity, argString, positionalAmount  
        );
        return false;
    } else if (tooManyArgs) {
        RUNTIME_ERROR(
            vm, "Expected %"PRIu32" to %"PRIu32" arguments, got %"PRIu32" instead.",
            minArity, maxArity, allAmount
        );
        return false;
    }
    return true;
}

/** 
 * Flattens all keyword arguments on the stack to prepare for a call.
 * 
 * First iterates through all positionally provided optional arguments and checks if they're
 * mentioned explicitly again in kwargs (repeated),
 * then iterates through the ones which weren't positionally
 * provided and tries to push an element from the kwargs map then pop it.
 * Otherwise uses the default value of that parameter.
 * Also performs a check that all kwargs have been used at the end to see if some kwarg names were
 * invalid.
 * 
 * Returns whether or not an error occurred while flattening the keyword arguments.
 */
static bool flatten_kwargs(Vm *vm, const FuncParams params, const u32 argAmount) {
    MapObj *keywordArgs = AS_PTR(MapObj, POP(vm));
    const u32 positionalOptionals = argAmount - params.minArity;
    for (u32 i = 0; i < positionalOptionals; i++) {
        if (table_get(&keywordArgs->table, params.optionalNames.data[i])) {
            // Argument was already provided positionally, but exists again in keyword args.
            RUNTIME_ERROR(vm, "Positional argument repeated as keyword argument.");
            return false;
        }
    }
    for (u32 i = positionalOptionals; i < params.optionalNames.length; i++) {
        Obj *argValue = table_get(&keywordArgs->table, params.optionalNames.data[i]);
        if (argValue) {
            PUSH(vm, argValue);
            table_delete(&keywordArgs->table, params.optionalNames.data[i]);
        } else {
            PUSH(vm, params.optionalValues.data[i]);
        }
    }
    if (keywordArgs->table.count > 0) {
        // Not exhausted, which means some keyword arguments had names not in the parameters.
        RUNTIME_ERROR(vm, "Keyword arguments must only have optional parameter names.");
        return false;
    }
    return true;
}

/** Attempts to call the passed object. Returns whether or not it managed to call it. */
static bool call(Vm *vm, Obj *callee, Obj **args, const u32 argAmount) {
    switch (callee->type) {
    case OBJ_FUNC: {
        FuncObj *func = AS_PTR(FuncObj, callee);
        if (
            !check_arity(vm, func->params.minArity, func->params.maxArity, argAmount)
            || !flatten_kwargs(vm, func->params, argAmount)
        ) {
            return false;
        }
        push_stack_frame(vm, func, args - 1, vm->frame->sp);
        return true;
    }
    case OBJ_CLASS: {
        ClassObj *cls = AS_PTR(ClassObj, callee);
        PEEK_DEPTH(vm, argAmount) = AS_OBJ(new_instance_obj(vm->program, cls));
        if (cls->init) {
            return call(vm, AS_OBJ(cls->init), args, argAmount);
        } else if (argAmount != 0) {
            RUNTIME_ERROR(vm, "Expected 0 arguments, got %"PRIu32" instead.", argAmount);
            return false;
        }
        return true;
    }
    case OBJ_METHOD: {
        MethodObj *method = AS_PTR(MethodObj, callee);
        PEEK_DEPTH(vm, argAmount) = AS_OBJ(method->instance);
        return call(vm, AS_OBJ(method->func), args, argAmount);
    }
    case OBJ_NATIVE_FUNC: {
        NativeFuncObj *native = AS_PTR(NativeFuncObj, callee);
        if (
            !check_arity(vm, native->params.minArity, native->params.maxArity, argAmount)
            || !flatten_kwargs(vm, native->params, argAmount)
        ) {
            return false;
        }
        Obj *nativeReturn = native->func(vm, args);
        if (nativeReturn == NULL) {
            return false;
        }
        const u32 arity = native->params.maxArity;
        PEEK_DEPTH(vm, arity) = nativeReturn;
        DROP_AMOUNT(vm, arity);
        return true;
    }
    default:
        RUNTIME_ERROR(vm, "Can't call object of type %s.", obj_type_str(callee->type));
        return false;
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
    const u32 arity = vm->frame->func->params.maxArity;
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
                VM_LOOP_RUNTIME_ERROR(vm, "Can't divide by 0.");
            }
            MATH_BIN_OP(vm, "/", NUM_VAL(BIN_LEFT(vm)) / NUM_VAL(BIN_RIGHT(vm)));
            break;
        case OP_MODULO:
            if (IS_NUM(BIN_LEFT(vm)) && IS_NUM(BIN_RIGHT(vm)) && NUM_VAL(BIN_RIGHT(vm)) == 0) {
                VM_LOOP_RUNTIME_ERROR(vm, "Can't modulo by 0.");
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
                VM_LOOP_RUNTIME_ERROR(
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
                VM_LOOP_RUNTIME_ERROR(vm, "Can't apply '~' to %s.", obj_type_str(PEEK(vm)->type));
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
                    VM_LOOP_RUNTIME_ERROR(
                        vm, "Can't convert string '%s' to number.",
                        AS_PTR(StringObj, PEEK(vm))->string
                    );
                }
                VM_LOOP_RUNTIME_ERROR(
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
                VM_LOOP_RUNTIME_ERROR(
                    vm, "Range takes 3 integers, got %s, %s, and %s instead.",
                    obj_type_str(start->type), obj_type_str(end->type), obj_type_str(step->type)
                );
            }
            if (AS_PTR(IntObj, step)->number < 0) {
                VM_LOOP_RUNTIME_ERROR(vm, "Range's step can't be a negative number.");
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
                    VM_LOOP_RUNTIME_ERROR(vm, "Can't hash %s key.", obj_type_str(key->type));
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
                VM_LOOP_FINISH_ERROR(vm);
            }
            break;
        }
        case OP_GET_PROPERTY: {
            Obj *originalObj = PEEK(vm);
            StringObj *name = AS_PTR(StringObj, READ_CONST(vm));
            if (!get_property(vm, originalObj, name)) {
                VM_LOOP_FINISH_ERROR(vm);
            }
            break;
        }
        case OP_DESTRUCTURE: {
            const u32 amount = READ_NUMBER(vm);
            if (!destructure(vm, amount)) {
                VM_LOOP_FINISH_ERROR(vm);
            }
            break;
        }
        case OP_FOR_ASSIGN_VARS: {
            const u32 amount = READ_NUMBER(vm);
            if (!destructure(vm, amount)) {
                VM_LOOP_FINISH_ERROR(vm);
            }
            for (u32 i = 0; i < amount; i++) {
                // +1 to go over the iterator.
                PEEK_DEPTH(vm, amount + i + 1) = PEEK_DEPTH(vm, i);
            }
            DROP_AMOUNT(vm, amount);
            break;
        }
        case OP_MAKE_ITER: {
            if (!is_iterable(PEEK(vm))) {
                VM_LOOP_RUNTIME_ERROR(
                    vm, "Can't iterate over object of type %s.", obj_type_str(PEEK(vm)->type)
                );
            }
            IteratorObj *iterator = new_iterator_obj(vm->program, PEEK(vm));
            PEEK(vm) = AS_OBJ(iterator);
            break;
        }
        case OP_FOR_ITER_ASSIGN: {
            Obj *iterated = for_iter_or_jump(vm);
            if (iterated) {
                PEEK_DEPTH(vm, 1) = iterated;
            }
            break;
        }
        case OP_FOR_ITER_LOAD: {
            Obj *iterated = for_iter_or_jump(vm);
            if (iterated) {
                PUSH(vm, iterated);
            }
            break;
        }
        case OP_CALL: {
            const u32 argAmount = READ_NUMBER(vm) + 1; // +1 to account for keyword args.
            if (!call(vm, PEEK_DEPTH(vm, argAmount), vm->frame->sp - argAmount, argAmount - 1)) {
                VM_LOOP_FINISH_ERROR(vm);
            }
            break;
        }
        case OP_ASSIGN_SUBSCR: {
            Obj *subscript = PEEK(vm);
            Obj *callee = PEEK_DEPTH(vm, 1);
            Obj *value = PEEK_DEPTH(vm, 2);
            if (callee->type == OBJ_LIST) {
                if (!list_assign_subscr(vm, AS_PTR(ListObj, callee), subscript, value)) {
                    VM_LOOP_FINISH_ERROR(vm);
                }
            } else if (callee->type == OBJ_MAP) {
                if (!map_assign_subscr(vm, AS_PTR(MapObj, callee), subscript, value)) {
                    VM_LOOP_FINISH_ERROR(vm);
                }
            } else {
                VM_LOOP_RUNTIME_ERROR(
                    vm, "Can't subscript assign to %s.", obj_type_str(callee->type)
                );
            }
            break;
        }
        case OP_GET_SUBSCR: {
            Obj *subscript = PEEK(vm);
            Obj *callee = PEEK_DEPTH(vm, 1);
            if (
                callee->type == OBJ_LIST || callee->type == OBJ_STRING || callee->type == OBJ_ENUM
            ) {
                if (!array_get_subscr(vm, callee, subscript)) {
                    VM_LOOP_FINISH_ERROR(vm);
                }
            } else if (callee->type == OBJ_MAP) {
                if (!map_get_subscr(vm, AS_PTR(MapObj, callee), subscript)) {
                    VM_LOOP_FINISH_ERROR(vm);
                }
            } else {
                VM_LOOP_RUNTIME_ERROR(
                    vm, "Can't subscript object of type %s.", obj_type_str(callee->type)
                );
            }
            break;
        }
        case OP_FUNC_OPTIONALS: {
            ObjArray optionalValues = AS_PTR(ListObj, PEEK(vm))->items;
            ObjArray optionalNames = AS_PTR(ListObj, PEEK_DEPTH(vm, 1))->items;
            FuncObj *func = AS_PTR(FuncObj, PEEK_DEPTH(vm, 2));
            for (u32 i = 0; i < optionalNames.length; i++) {
                APPEND_DA(&func->params.optionalNames, optionalNames.data[i]);
                APPEND_DA(&func->params.optionalValues, optionalValues.data[i]);
            }
            DROP_AMOUNT(vm, 2);
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
        case OP_POP_JUMP_IF_NOT: {
            const u32 jump = READ_NUMBER(vm);
            if (!(as_bool(vm->program, PEEK(vm))->boolean)) {
                vm->frame->ip += jump;
            }
            DROP(vm);
            break;
        }
        case OP_POP_JUMP_IF: {
            const u32 jump = READ_NUMBER(vm);
            if (as_bool(vm->program, PEEK(vm))->boolean) {
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
        case OP_START_TRY: {
            const bool saveErrorMessage = AS_PTR(BoolObj, POP(vm))->boolean;
            u8 *catchIp = vm->frame->func->bytecode.data + READ_NUMBER(vm);
            APPEND_DA(&vm->catches, create_catch_state(vm, catchIp, saveErrorMessage));
            break;
        }
        case OP_FINISH_TRY:
            DROP_DA(&vm->catches);
            break;
        case OP_RAISE: {
            StringObj *message = AS_PTR(StringObj, POP(vm));
            VM_LOOP_RUNTIME_ERROR(vm, message->string);
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
