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
 * Returns an error if a caught error happens in the interpreter loop.
 * 
 * Finishes up an already reported error by exiting the VM interpreter loop, and sets
 * an exit code if the errored thread is the main one (exit code is dictated by the main one only).
 */
#define INTERP_LOOP_FINISH_ERROR(thread) \
    do { \
        if ((thread)->isMain) { \
            set_exit_code(thread, false, 1); \
        } \
        return; \
    } while (0)

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
#define INTERP_LOOP_RUNTIME_ERROR(thread, ...) \
    RUNTIME_ERROR(thread, __VA_ARGS__); \
    INTERP_LOOP_FINISH_ERROR(thread); \

/** Reads the current instruction and increments IP to prepare for the next one. */
#define READ_INSTR(thread) (*(thread)->frame->ip++)

// TODO: turn the bytecode_number function fully to a macro for performance?
// Pretty considerable since it's also used by the const reader.
/** Reads the upcoming number in the bytecode and can be assigned to it. */
#define READ_NUMBER(thread) \
    ((thread)->frame->ip += (thread)->instrSize, \
    bytecode_number( \
        &(thread)->frame->func->bytecode, \
        (thread)->frame->ip - (thread)->frame->func->bytecode.data - (thread)->instrSize, \
        &(thread)->instrSize \
    ))

/** Reads the upcoming number and uses it to index an object in the current constant pool. */
#define READ_CONST(thread) ((thread)->frame->func->constPool.data[READ_NUMBER(thread)])

/** Pushes an object to the thread's stack. */
#define PUSH(thread, obj) \
    do { \
        if ((thread)->stack.capacity < (thread)->stack.length + 1) { \
            reallocate_stack(thread); \
        } \
        *(thread)->frame->sp++ = AS_OBJ(obj); \
        (thread)->stack.length++; \
    } while (0)

/** Drops one object from the thread's stack. */
#define DROP(thread) (--(thread)->stack.length, --(thread)->frame->sp)

/** Drops an amount of objects from the thread's stack. */
#define DROP_AMOUNT(thread, amount) \
    ((thread)->stack.length -= (amount), (thread)->frame->sp -= (amount))
    
/** 
 * Drops all objects at a certain depth then appends the passed object safely at it.
 * 
 * This function automatically places the passed object in the unrooted array to be cleared
 * out of it safely next instruction, whether the object is already in unrooted or not doesn't
 * matter.
 */
#define DROP_PUSH_DEPTH(thread, obj, depth) \
    do { \
        Obj *toPush_ = AS_OBJ(obj); \
        PUSH_UNROOTED(&(thread)->vulnObjs, toPush_); \
        DROP_AMOUNT(thread, depth); \
        PUSH(thread, toPush_); \
    } while (0)

/** Resolves to the topmost object in the stack. */
#define PEEK(thread) (*((thread)->frame->sp - 1))

/** Resolves to the object at a certain depth from the top of the stack (0 being the very top). */
#define PEEK_DEPTH(thread, depth) (*((thread)->frame->sp - (depth) - 1))

#define BIN_LEFT(thread) (PEEK_DEPTH(thread, 1)) /** LHS object of a binary op in the stack. */
#define BIN_RIGHT(thread) (PEEK_DEPTH(thread, 0)) /** RHS object of a binary op in the stack. */

/** Resolves to a boolean of whether or not either of the binary numbers in the stack are floats. */
#define BIN_HAS_FLOAT(thread) \
    (BIN_LEFT(thread)->type == OBJ_FLOAT || BIN_RIGHT(thread)->type == OBJ_FLOAT)

/** Writes a consistent runtime error message for all failed binary operations. */
#define BIN_OP_ERROR(thread, opString) \
    INTERP_LOOP_RUNTIME_ERROR( \
        thread, "Can't use '%s' between %s and %s.", \
        opString, obj_type_str(BIN_LEFT(thread)->type), obj_type_str(BIN_RIGHT(thread)->type) \
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
#define MATH_BIN_OP(thread, opString, resultNum) \
    do { \
        if (!IS_NUM(BIN_LEFT(thread)) || !IS_NUM(BIN_RIGHT(thread))) { \
            BIN_OP_ERROR(thread, opString); \
            break; \
        } \
        Obj *resultObj = BIN_HAS_FLOAT(thread) ? \
            AS_OBJ(new_float_obj(&(thread)->vulnObjs, resultNum)) \
            : AS_OBJ(new_int_obj(&(thread)->vulnObjs, resultNum)); \
        DROP_PUSH_DEPTH(thread, resultObj, 2); \
    } while (0)

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
#define BOOL_BIN_OP(thread, resultBool) \
    DROP_PUSH_DEPTH(thread, new_bool_obj(&(thread)->vulnObjs, resultBool), 2)

/** BOOL_BIN_OP() wrapper which errors if the operands of the operation aren't both numbers. */
#define NUM_BOOL_BIN_OP(thread, opString, resultBool) \
    do { \
        if (!IS_NUM(BIN_LEFT(thread)) || !IS_NUM(BIN_RIGHT(thread))) { \
            BIN_OP_ERROR(thread, opString); \
            break; \
        } \
        BOOL_BIN_OP(thread, resultBool); \
    } while (0)

/** 
 * Binary operation where the operation is a bitwise one (only allowed on ints and bools).
 * 
 * Takes an opString, which is the operation's name as a string (like "left shift" for example),
 * and resultNum, which is the number result of the binary bitwise performed outside.
 */
#define BITWISE_BIN_OP(thread, opString, resultNum) \
    do { \
        if (!IS_BITWISABLE(BIN_LEFT(thread)) || !IS_BITWISABLE(BIN_RIGHT(thread))) { \
            BIN_OP_ERROR(thread, opString); \
            break; \
        } \
        Obj *resultObj = AS_OBJ(new_int_obj(&(thread)->vulnObjs, resultNum)); \
        DROP_PUSH_DEPTH(thread, resultObj, 2); \
    } while (0)

static void interpreter_loop(ThreadObj *thread);

/** 
 * Special reallocation function for the thread's stack.
 * 
 * The thing about the stack is there are a bunch of things that point in the middle of it
 * (like the stack pointer for example), but reallocation of memory might move the address
 * of the allocated memory and free the original, old one.
 * This in turn means that every pointer that points to an object inside the stack needs
 * to also be updated to the same index relative to the potentially new stack address.
 */
static void reallocate_stack(ThreadObj *thread) {
    INCREASE_CAPACITY(thread->stack.capacity);
    Obj **originalStack = thread->stack.objects;
    thread->stack.objects = REALLOC(
        thread->stack.objects, thread->stack.capacity * sizeof(*thread->stack.objects)
    );
    if (thread->stack.objects == originalStack) {
        // Stack address didn't change, no need to move pointers in the stack to the new location.
        return;
    }
    
    for (u32 i = 0; i < thread->callStack.length; i++) {
        StackFrame *frame = &thread->callStack.data[i];
        frame->sp = &thread->stack.objects[frame->sp - originalStack];
        frame->bp = &thread->stack.objects[frame->bp - originalStack];
    }
}

/** Creates and returns a new catch state in the current thread and its stack frame. */
static CatchState create_catch_state(ThreadObj *thread, u8 *ip, const bool saveError) {
    FuncObj *func = thread->frame->func;
    const u32 capturesAmount = FLAG_IS_SET(func->flags, FUNC_CLOSURE_BIT)
        ? AS_PTR(RuntimeFuncObj, func)->captures.length : 0;

    CatchState catchState = {
        .func = func, .ip = ip, .localsAmount = thread->stack.length,
        .capturesAmount = capturesAmount, .openCapturesAmount = thread->openCaptures.length,
        .frameAmount = thread->callStack.length, .module = thread->module,
        .saveError = saveError
    };
    return catchState;
}

/** 
 * Performs a thread-safe pop operation on the thread's stack, but is a bit slower.
 * 
 * This is for safely grabbing and popping an element off of the stack by first placing it
 * in the unrooted vulnerables for the instruction/function execution.
 * 
 * This is a more expensive stack operation which should be reserved for places that
 * aren't hot in the VM (don't execute often).
 * This is because it has function overhead, places an object in the unrooted array
 * (push and peak operations), and only then does it do the object pop operation as well.
 * 
 * The safety of the returned object lasts until it's dropped from the unrooted array,
 * which is after every VM instruction.
 */
static Obj *safe_pop(ThreadObj *thread) {
    Obj *popped = PEEK(thread);
    PUSH_UNROOTED(&thread->vulnObjs, popped); // Now safe until the next instruction in the VM.
    DROP(thread);
    return popped;
}

/** Pushes a new execution stack frame onto the passed thread. */
static void push_stack_frame(ThreadObj *thread, FuncObj *func, Obj **bp, Obj **sp) {
    StackFrame frame = {.func = func, .ip = func->bytecode.data, .bp = bp, .sp = sp};
    PUSH_DA(&thread->callStack, frame);
    thread->frame = &LAST_ITEM_DA(&thread->callStack);
}

/** 
 * Pops the top frame on the thread and restores the previous one or NULL if there isn't any.
 * 
 * Also drops from the stack all local variables in the function, except the passed arguments
 * because those are considered a part of the caller's (previous function) stack.
 */
static void pop_stack_frame(ThreadObj *thread) {
    DROP_DA(&thread->callStack);
    thread->frame = thread->callStack.length == 0 ? NULL : &LAST_ITEM_DA(&thread->callStack);
    thread->stack.length = thread->frame->sp - thread->stack.objects;    
}

/** Safely sets a key value pair in the globals table of a module. */
static void set_global(ModuleObj *module, Obj *name, Obj *value) {
    mutex_lock(&module->globalsLock);
    table_set(&module->globalsUnsafe, name, value);
    mutex_unlock(&module->globalsLock);
}

/** 
 * Safely grabs a value from the globals table using the passed name as a key.
 * 
 * Can return NULL if the global doesn't exist in the passed module.
 */
static Obj *get_global(ModuleObj *module, Obj *name) {
    mutex_lock(&module->globalsLock);
    Obj *global = table_get(&module->globalsUnsafe, name);
    mutex_unlock(&module->globalsLock);
    return global;
}

/** Safely retrieves the length of the threads array in the passed VM. */
u32 vm_threads_length(Vm *vm) {
    mutex_lock(&vm->threadsLock);
    const u32 length = vm->threadsUnsafe.length;
    mutex_unlock(&vm->threadsLock);
    return length;
}

/** Safely retrieves a thread at a specific index in the VM's array of threads. */
ThreadObj *vm_thread_at(Vm *vm, const u32 index) {
    mutex_lock(&vm->threadsLock);
    ASSERT(index <= vm->threadsUnsafe.length, "Index goes over the size of the threads array.");
    Obj *thread = vm->threadsUnsafe.data[index];
    mutex_unlock(&vm->threadsLock);
    return AS_PTR(ThreadObj, thread);
}

/** Returns the main thread inside the VM (the VM must at least have one thread). */
ThreadObj *get_main_thread(Vm *vm) {
    ThreadObj *mainThread = vm_thread_at(vm, 0);
    ASSERT(mainThread, "No main thread stored in the VM.");
    return AS_PTR(ThreadObj, mainThread);
}

/** 
 * Safely creates a new thread object, pushing it into the VM's threads array and returning it.
 */
ThreadObj *vm_push_thread(
    VulnerableObjs *vulnObjs, Vm *vm, Obj *runnable, ModuleObj *module,
    const bool isMain, const bool isDaemon
) {
    ThreadObj *thread = new_thread_obj(
        vulnObjs, vm, runnable, module, vm_threads_length(vm), isMain, isDaemon
    );

    mutex_lock(&vm->threadsLock);
    PUSH_DA(&vm->threadsUnsafe, AS_OBJ(thread));
    mutex_unlock(&vm->threadsLock);
    return thread;
}

/** Safely returns the length of the modules array in the VM. */
static u32 vm_modules_length(Vm *vm) {
    mutex_lock(&vm->modulesLock);
    const u32 length = vm->modulesUnsafe.length;
    mutex_unlock(&vm->modulesLock);
    return length;
}

/** 
 * Safely pushes a new module to the VM's modules array.
 * 
 * Uses the amount of modules created (length) as an ID for the module,
 * and sets that module as the one which the thread is now using.
 */
static void push_module(ThreadObj *thread, StringObj *path, ModuleObj *importedBy) {
    Vm *vm = thread->vm;
    ModuleObj *module = new_module_obj(&thread->vulnObjs, path, importedBy, vm_modules_length(vm));
    thread->module = module; // Now executing this module.

    mutex_lock(&vm->modulesLock);
    PUSH_DA(&vm->modulesUnsafe, AS_OBJ(module));
    mutex_unlock(&vm->modulesLock);
}

/** 
 * Prints a stack trace for a runtime error.
 * 
 * Prints in an order that makes the most recently executing function appear at the bottom.
 */
static void print_stack_trace(ThreadObj *thread) {
    fprintf(stderr, RED "Stack trace:\n" DEFAULT_COLOR);
    for (u32 i = 0; i < thread->callStack.length; i++) {
        StackFrame *frame = &thread->callStack.data[i];
        fprintf(stderr, INDENT);

        if (FLAG_IS_SET(frame->func->flags, FUNC_CLOSURE_BIT)) {
            fprintf(stderr, "%s: ", frame->func->name->string);
        } else {
            fprintf(stderr, "%s(): ", frame->func->name->string);
        }
        const u32 bytecodeIdx = frame->ip - frame->func->bytecode.data;
        fprintf(stderr, "line %d\n", frame->func->positions.data[bytecodeIdx].line);
    }
    fputc('\n', stderr);
}

/** 
 * Restores the frame and module contexts of the thread after catching an error.
 * 
 * Before it pops the frames, it stored how many need to be popped in a separate const,
 * because the length inside the thread itself changes as the frames get popped.
 */
static void restore_frame_contexts(ThreadObj *thread, CatchState *catch) {
    thread->module = catch->module;
    const u32 framePops = thread->callStack.length - catch->frameAmount;
    for (u32 i = 0; i < framePops; i++) {
        pop_stack_frame(thread);
    }
}

/** 
 * Catches a runtime error by setting its catching state as the current one executing.
 * 
 * Prints the caught error out if debugging is enabled, and pushes the passed error message onto
 * the stack if the catch stores the message as a variable.
 */
static void catch_error(ThreadObj *thread, StringObj *errorMessage) {
    CatchState catch = POP_DA(&thread->catches);
    restore_frame_contexts(thread, &catch);

    const u32 localPops = thread->stack.length - catch.localsAmount;
    DROP_AMOUNT(thread, localPops);
    const u32 openCapturePops = thread->openCaptures.length - catch.openCapturesAmount;
    DROP_AMOUNT_DA(&thread->openCaptures, openCapturePops);

    u32 capturePops = 0;
    if (FLAG_IS_SET(thread->frame->func->flags, FUNC_CLOSURE_BIT)) {
        ObjArray *captures = &AS_PTR(RuntimeFuncObj, thread->frame->func)->captures;
        capturePops = captures->length - catch.capturesAmount;
        DROP_AMOUNT_DA(captures, capturePops);
    }

    thread->frame->ip = catch.ip;
    if (catch.saveError) {
        PUSH(thread, errorMessage);
    }

#if DEBUG_RUNTIME
    print_caught_runtime_error(
        &thread->vulnObjs, catch.func,
        localPops, capturePops, openCapturePops, errorMessage->string
    );
#endif
}

/** Sets whether the VM exited manually and an exit code if valid to do so. */
static void set_exit_code(ThreadObj *thread, const bool manuallyExited, const ZmxInt exitCode) {
    if (thread->isMain) {
        thread->vm->program->cli->manuallyExited = manuallyExited;
        thread->vm->program->cli->exitCode = exitCode;
    }
}

/** 
 * Base function that tries to raise a runtime error with an object error message.
 * 
 * Sets a catch state and doesn't error if there's one. Meant only as a base error function,
 * so prefer to use macros that wrap this instead of directly calling it.
 */
void base_runtime_error(ThreadObj *thread, StringObj *errorMessage) {
    if (thread->catches.length != 0) {
        // Catches, doesn't cause a runtime error.
        catch_error(thread, errorMessage);
        return;
    }

    print_stack_trace(thread);
    const u32 bytecodeIdx = thread->frame->ip - thread->frame->func->bytecode.data;
    zmx_user_error(
        thread->vm->program, AS_PTR(RuntimeFuncObj, thread->frame->func)->module->path->string,
        thread->frame->func->positions.data[bytecodeIdx],
        "Runtime error", errorMessage->string, NULL
    );
    set_exit_code(thread, false, 1);
}

/** 
 * Starts running the VM with the current thread as the main one using the passed function.
 * 
 * Also, sets the VM currently being GCed to it (the VM being initialized).
 */
void init_vm(Vm *vm, ZmxProgram *program, RuntimeFuncObj *func) {
    vm->program = program;
    program->gc.vm = vm;
    
    VulnerableObjs *startupVulns = &program->gc.startupVulnObjs;
    INIT_DA(&vm->threadsUnsafe);
    init_mutex(&vm->threadsLock);
    INIT_DA(&vm->modulesUnsafe);
    init_mutex(&vm->modulesLock);

    vm_push_thread(startupVulns, vm, AS_OBJ(func), NULL, true, false);
    ThreadObj *mainThread = get_main_thread(vm);
    push_stack_frame(
        mainThread, AS_PTR(FuncObj, func), mainThread->stack.objects, mainThread->stack.objects
    );
    push_module(mainThread, program->currentFile, NULL);
    func->module = mainThread->module; // Now set the module for main func.

    DROP_ALL_UNROOTED(startupVulns);
}

/** Frees all memory that the passed VM allocated. */
void free_vm(Vm *vm) {
    FREE_DA(&vm->threadsUnsafe);
    destroy_mutex(&vm->threadsLock);

    FREE_DA(&vm->modulesUnsafe);
    destroy_mutex(&vm->modulesLock);
}

/** Performs an assignment on some indexed array element. */
static bool list_assign_subscr(ThreadObj *thread, ListObj *callee, Obj *subscript, Obj *value) {
    if (subscript->type != OBJ_INT) {
        RUNTIME_ERROR(
            thread, "List element assign expected integer subscript, got %s instead.",
            obj_type_str(subscript->type)
        );
        return false;
    }
    const ZmxInt index = AS_PTR(IntObj, subscript)->number;
    if (!IS_WITHIN_LENGTH(callee->items.length, index)) {
        RUNTIME_ERROR(thread, "Index " ZMX_INT_FMT " is out of range.", index);
        return false;
    }
    callee->items.data[index] = value;
    DROP_AMOUNT(thread, 2);
    return true;
}

/** 
 * Assigns a key-value pair to a map.
 * 
 * If the key already exists, just change the value, otherwise add a new entry completely.
 * Pop the callee and the subscript off of the stack afterwards.
 */
static bool map_assign_subscr(ThreadObj *thread, MapObj *callee, Obj *subscript, Obj *value) {
    if (!is_hashable(subscript)) {
        RUNTIME_ERROR(thread, "%s key is not hashable.", obj_type_str(subscript->type));
        return false;
    }
    table_set(&callee->table, subscript, value);
    DROP_AMOUNT(thread, 2);
    return true;
}

/** 
 * Performs a subscript get on an array using an integer.
 * 
 * Deletes the subscript and the callee of the subscript, and pushes the element on the callee
 * at the index of the integer subscript.
 */
static bool array_get_item(ThreadObj *thread, Obj *callee, Obj *subscript, const ZmxInt length) {
    const ZmxInt index = AS_PTR(IntObj, subscript)->number;
    if (!IS_WITHIN_LENGTH(length, index)) {
        RUNTIME_ERROR(thread, "Index " ZMX_INT_FMT " is out of range.", index);
        return false;
    }
    Obj *result;
    switch (callee->type) {
    case OBJ_LIST:
        result = AS_PTR(ListObj, callee)->items.data[index];
        break;
    case OBJ_STRING:
        result = AS_OBJ(
            new_string_obj(&thread->vulnObjs, AS_PTR(StringObj, callee)->string + index, 1)
        );
        break;
    case OBJ_ENUM:
        result = AS_PTR(EnumObj, callee)->members.data[index];
        break;
    default: UNREACHABLE_ERROR(); // Assumes this was called with a subscriptable.
    }
    DROP_PUSH_DEPTH(thread, result, 2);
    return true;
}

/** 
 * Covers one iteration of creating an array slice.
 * 
 * Errors if the slice is out of bounds, and otherwise appends the element iterated.
 */
static bool slice_iteration(
    ThreadObj *thread, Obj *callee, const ZmxInt index, const ZmxInt length, ObjArray *slice
) {
    if (!IS_WITHIN_LENGTH(length, index)) {
        FREE_DA(slice);
        RUNTIME_ERROR(thread, "Slice goes out of range.");
        return false;
    }
    Obj *slicedItem;
    switch (callee->type) {
    case OBJ_LIST:
        slicedItem = AS_PTR(ListObj, callee)->items.data[index];
        break;
    case OBJ_STRING:
        slicedItem = AS_OBJ(new_string_obj(
            &thread->vulnObjs, AS_PTR(StringObj, callee)->string + index, 1)
        );
        break;
    case OBJ_ENUM: {
        // Use NULL as the enum member's enum struct. Will correct it when sliced enum's created.
        EnumMemberObj *member = AS_PTR(EnumMemberObj, AS_PTR(EnumObj, callee)->members.data[index]);
        slicedItem = AS_OBJ(new_enum_member_obj(
            &thread->vulnObjs, NULL, member->name, slice->length)
        );
        break;
    }
    default:
        UNREACHABLE_ERROR();
    }
    PUSH_DA(slice, slicedItem);
    return true;
}

/** Returns a created object from the slice that was made on the callee depending on its type. */
static Obj *obj_from_slice(VulnerableObjs *vulnObjs, Obj *callee, const ObjArray slice) {
    Obj *sliceResult;
    switch (callee->type) {
    case OBJ_LIST: {
        ObjArray list = CREATE_DA();
        for (u32 i = 0; i < slice.length; i++) {
            PUSH_DA(&list, slice.data[i]);
        }
        sliceResult = AS_OBJ(new_list_obj(vulnObjs, list));
        break;
    }
    case OBJ_STRING: {
        CharBuffer sliceBuffer = create_char_buffer();
        for (u32 i = 0; i < slice.length; i++) {
            buffer_append_string(&sliceBuffer, AS_PTR(StringObj, slice.data[i])->string);
        }
        sliceResult = AS_OBJ(new_string_obj(vulnObjs, sliceBuffer.text, sliceBuffer.length));
        free_char_buffer(&sliceBuffer);
        break;
    }
    case OBJ_ENUM: {
        EnumObj *enumObj = new_enum_obj(vulnObjs, AS_PTR(EnumObj, callee)->name);
        for (u32 i = 0; i < slice.length; i++) {
            EnumMemberObj *member = AS_PTR(EnumMemberObj, slice.data[i]);
            member->enumObj = enumObj; // Set the enum holding them to the sliced one.
            PUSH_DA(&enumObj->members, AS_OBJ(member));
            table_set(
                &enumObj->lookupTable,
                AS_OBJ(member->name), AS_OBJ(new_int_obj(vulnObjs, (ZmxInt)i))
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
static bool slice(ThreadObj *thread, Obj *callee, Obj *subscript, const ZmxInt length) {
    ObjArray slice = CREATE_DA();

    IteratorObj *iterator = new_iterator_obj(&thread->vulnObjs, subscript);
    bool hasAllocated = false;
    Obj *current;
    while ((current = iterate(&thread->vulnObjs, iterator, &hasAllocated))) {
        const ZmxInt index = AS_PTR(IntObj, current)->number;
        if (hasAllocated) {
            OPT_DROP_UNROOTED(&thread->vulnObjs);
        }
        if (!slice_iteration(thread, callee, index, length, &slice)) {
            return false;
        }
    }
    Obj *result = obj_from_slice(&thread->vulnObjs, callee, slice);
    DROP_PUSH_DEPTH(thread, result, 2);

    FREE_DA(&slice);
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
static bool array_get_subscr(ThreadObj *thread, Obj *callee, Obj *subscript) {
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
        return array_get_item(thread, callee, subscript, length);
    } else if (subscript->type == OBJ_RANGE) {
        return slice(thread, callee, subscript, length);
    }
    RUNTIME_ERROR(
        thread, "Can't subscript %s with %s.",
        obj_type_str(callee->type), obj_type_str(subscript->type)
    );
    return false;
}

/** 
 * Performs a subscript get on a map, which only accepts hashable subscripts for keys.
 * 
 * Deletes the subscript and the callee of the subscript, and pushes the corresponding value
 * of the key subscript.
 */
static bool map_get_subscr(ThreadObj *thread, MapObj *callee, Obj *subscript) {
    if (!is_hashable(subscript)) {
        RUNTIME_ERROR(thread, "%s key is not hashable.", obj_type_str(subscript->type));
        return false;
    }
    Obj *result = table_get(&callee->table, subscript);
    if (result == NULL) {
        RUNTIME_ERROR(
            thread, "Key '%s' doesn't exist in map.",
            as_string(&thread->vulnObjs, subscript)->string
        );
        return false;
    }
    DROP_PUSH_DEPTH(thread, result, 2);
    return true;
}

/** Try to set (assign or create if one doesn't exist) a value on some property inside an object. */
static bool set_property(ThreadObj *thread, Obj *originalObj, StringObj *name, Obj *value) {
    if (originalObj->type == OBJ_INSTANCE) {
        table_set(&AS_PTR(InstanceObj, originalObj)->fields, AS_OBJ(name), value);
        DROP(thread); // Pop the object being accessed, leaving only the value on top of the stack.
        return true;
    }

    RUNTIME_ERROR(
        thread, "Can't set property on object of type %s.", obj_type_str(originalObj->type)
    );
    return false;
}

/** 
 * Handles a class which inherits an abstract class.
 * 
 * If the inheriting class is also abstract, then it just inherits the abstract methods
 * of the superclass.
 * However, if the inheriting class isn't abstract, then it checks the methods added on that class
 * (assuming that methods get added before inheriting), and errors out if any abstract method
 * wasn't made/overridden in the inheriting class.
 */
static bool inherit_abstract(ThreadObj *thread, ClassObj *cls, ClassObj *superclass) {
    if (cls->isAbstract) {
        // Abstract inheriting abstract adds the super's abstract names to the subclass's abstracts.
        for (u32 i = 0; i < superclass->abstractMethods.length; i++) {
            PUSH_DA(&cls->abstractMethods, superclass->abstractMethods.data[i]);
        }
        return true;
    }

    // Non-abstract inherits abstract, must have already overridden all abstracts with methods.
    for (u32 i = 0; i < superclass->abstractMethods.length; i++) {
        StringObj *abstractName = AS_PTR(StringObj, superclass->abstractMethods.data[i]);
        if (!table_get(&cls->methods, AS_OBJ(abstractName))) {
            RUNTIME_ERROR(thread, "Abstract method '%s' not implemented.", abstractName->string);
            return false;
        }
    }
    return true;
}

/** 
 * Tries to return a method if the passed class or its superclasses have it. Returns NULL if not.
 * 
 * First looks to see if the class has that method directly implemented, if not it recursively
 * calls to see if the superclass has it, which will check its own methods and its superclass
 * if it also has own. This goes on until a method is found or NULL is returned.
 */
static Obj *find_method(ClassObj *cls, StringObj *name) {
    Obj *method = table_get(&cls->methods, AS_OBJ(name));
    if (method) {
        return method;
    } else if (cls->superclass) {
        return find_method(cls->superclass, name);
    } else {
        return NULL; // Couldn't find the method and don't have a superclass to look into.
    }
}

/** 
 * Tries to access a property in a passed instance.
 * 
 * If the property is a method, then it either binds it to a user method or a native method
 * depending on whether the instance is that of a user class or a built-in object.
 * 
 * Returns whether or not it successfully put the property at the top of the stack, completing
 * the get operation.
 * 
 * This function is safe to call with a NULL for either of instance fields and cls.
 * The NULL will be treated as an empty table with nothing to search on it.
 */
static bool get_instance_property(
    ThreadObj *thread, Table *fields, Obj *instance, ClassObj *cls, StringObj *name,
    const bool isNative
) {
    Obj *field = fields ? table_get(fields, AS_OBJ(name)) : NULL; // NULL if no fields passed.
    if (field) {
        PEEK(thread) = field;
        return true;
    }

    Obj *method = cls ? find_method(cls, name) : NULL; // NULL if no class passed.
    if (!method) {
        RUNTIME_ERROR(thread, "No property called '%s'.", name->string);
        return false;
    }
    
    VulnerableObjs *vulnObjs = &thread->vulnObjs;
    Obj *bound = isNative ?
        AS_OBJ(new_native_method_obj(vulnObjs, instance, cls, AS_PTR(NativeFuncObj, method)))
        : AS_OBJ(new_method_obj(vulnObjs, AS_PTR(InstanceObj, instance), AS_PTR(FuncObj, method)));
    PEEK(thread) = bound;
    return true;
}

/** Access a property inside some object that might have multiple properties inside it. */
static bool get_property(ThreadObj *thread, Obj *originalObj, StringObj *name) {
    switch (originalObj->type) {
    case OBJ_INSTANCE: {
        InstanceObj *instance = AS_PTR(InstanceObj, originalObj);
        return get_instance_property(
            thread, &instance->fields, AS_OBJ(instance), instance->cls, name, false
        );
    }
    case OBJ_ENUM: {
        EnumObj *enumObj = AS_PTR(EnumObj, originalObj);
        Obj *propertyIdx = table_get(&enumObj->lookupTable, AS_OBJ(name));
        if (propertyIdx == NULL) {
            RUNTIME_ERROR(thread, "No enum member called '%s'.", name->string);
            return false;
        }
        ASSERT(propertyIdx->type == OBJ_INT, "Enum lookup table value wasn't an integer.");
        PEEK(thread) = enumObj->members.data[AS_PTR(IntObj, propertyIdx)->number];
        return true;
    }
    case OBJ_MODULE: {
        ModuleObj *module = AS_PTR(ModuleObj, originalObj);
        Obj *global = get_global(module, AS_OBJ(name));
        if (global == NULL) {
            RUNTIME_ERROR(thread, "Module doesn't have '%s'.", name->string);
            return false;
        }
        PEEK(thread) = global;
        return true;
    }
    case OBJ_FILE: {
        FileObj *instance = AS_PTR(FileObj, originalObj);
        ClassObj *cls = thread->vm->program->builtIn.fileClass;
        return get_instance_property(thread, &instance->fields, AS_OBJ(instance), cls, name, true);
    }
    case OBJ_THREAD: {
        ThreadObj *instance = AS_PTR(ThreadObj, originalObj);
        ClassObj *cls = thread->vm->program->builtIn.threadClass;
        return get_instance_property(thread, &instance->fields, AS_OBJ(instance), cls, name, true);
    }
    case OBJ_LOCK: {
        LockObj *instance = AS_PTR(LockObj, originalObj);
        ClassObj *cls = thread->vm->program->builtIn.lockClass;
        return get_instance_property(thread, NULL, AS_OBJ(instance), cls, name, true);
    }
    default:
        RUNTIME_ERROR(
            thread, "Object of type %s doesn't have properties.", obj_type_str(originalObj->type)
        );
        return false;
    }
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
static bool destructure(ThreadObj *thread, const u32 amount) {
    if (!is_iterable(PEEK(thread))) {
        RUNTIME_ERROR(
            thread, "Expected iterable for destructuring, got %s instead.",
            obj_type_str(PEEK(thread)->type)
        );
        return false;
    }
    IteratorObj *iterator = new_iterator_obj(&thread->vulnObjs, PEEK(thread));
    DROP(thread); // Now safe to pop iterable, as the protected iterator (in unrooted) holds it.

    bool hasAllocated = false;
    Obj *current;
    u32 i = 0;
    while ((current = iterate(&thread->vulnObjs, iterator, &hasAllocated)) && i < amount) {
        PUSH(thread, current);
        i++;
        if (hasAllocated) {
            OPT_DROP_UNROOTED(&thread->vulnObjs);
        }
    }
    if (i < amount || current != NULL) {
        // One of the finishing conditions hasn't been met, so either too few or too many elements.
        RUNTIME_ERROR(thread, "Expected %"PRIu32" elements in iterable.", amount);
        return false;
    }
    return true;
}

/** 
 * Attempts to iterate an element of a for loop instruction.
 * 
 * Returns the iterated object, or NULL if exhausted (jumps forward if exhausted).
 */
static Obj *for_iter_or_jump(ThreadObj *thread) {
    ASSERT(PEEK(thread)->type == OBJ_ITERATOR, "Must iterate with an iterator.");
    IteratorObj *iterator = AS_PTR(IteratorObj, PEEK(thread));
    const u32 jump = READ_NUMBER(thread);
    
    Obj *element = iterate(&thread->vulnObjs, iterator, NULL);
    if (element == NULL) {
        thread->frame->ip += jump;
    }
    return element;
}

/** 
 * Checks that the pass passed arity is valid with the argument amount + keyword arguments.
 * 
 * Errors and returns false if the arity check fails, otherwise returns true.
 */
static bool check_arity(
    ThreadObj *thread, char *name, const u32 minArity, const u32 maxArity,
    const u32 positionalAmount
) {
    MapObj *kwargs = AS_PTR(MapObj, PEEK(thread));
    char *argString = minArity == 1 ? "argument" : "arguments";
    const u32 allAmount = positionalAmount + kwargs->table.count;

    const bool noOptionals = minArity == maxArity;
    const bool tooFewArgs = allAmount < minArity;
    const bool tooManyArgs = allAmount > maxArity;
    if (tooFewArgs && noOptionals) {
        RUNTIME_ERROR(
            thread, "%s() expected %"PRIu32" %s, got %"PRIu32" instead.",
            name, minArity, argString, allAmount
        );
        return false;
    } else if (tooManyArgs && noOptionals) {
        RUNTIME_ERROR(
            thread, "%s() expected %"PRIu32" %s, got %"PRIu32" instead.",
            name, minArity, argString, allAmount
        );
        return false;
    } else if (tooFewArgs) {
        RUNTIME_ERROR(
            thread, "%s() expected at least %"PRIu32" %s, but only got %"PRIu32".",
            name, minArity, argString, allAmount
        );
        return false;
    } else if (tooManyArgs) {
        RUNTIME_ERROR(
            thread, "%s() expected %"PRIu32" to %"PRIu32" arguments, got %"PRIu32" instead.",
            name, minArity, maxArity, allAmount
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
 * 
 * then iterates through the ones which weren't positionally
 * provided and tries to push an element from the kwargs map then pop it.
 * Otherwise uses the default value of that parameter.
 * Also performs a check that all kwargs have been used at the end to see if some kwarg names were
 * invalid.
 * 
 * Returns whether or not an error occurred while flattening the keyword arguments.
 */
static bool flatten_kwargs(
    ThreadObj *thread, char *name, const FuncParams params, const u32 positionalsAmount
) {
    MapObj *kwargs = AS_PTR(MapObj, safe_pop(thread));

    for (u32 i = 0; i < positionalsAmount; i++) {
        if (table_get(&kwargs->table, params.names.data[i])) {
            // Argument was already provided positionally, but exists again in keyword args.
            RUNTIME_ERROR(
                thread, "for %s(), keyword argument '%s' was already provided positionally.",
                name, AS_PTR(StringObj, params.names.data[i])->string
            );
            return false;
        }
    }
    for (u32 i = positionalsAmount; i < params.names.length; i++) {
        Obj *argValue = table_get(&kwargs->table, params.names.data[i]);
        if (argValue) {
            PUSH(thread, argValue);
            table_delete(&kwargs->table, params.names.data[i]);
        } else if (params.values.data[i] != NULL) {
            PUSH(thread, params.values.data[i]);
        } else {
            RUNTIME_ERROR(
                thread, "%s() expected value for parameter '%s'.",
                name, AS_PTR(StringObj, params.names.data[i])->string
            );
            return false;
        }
    }
    if (kwargs->table.count > 0) {
        // Not exhausted, which means some keyword arguments had names not in the parameters.
        RUNTIME_ERROR(thread, "for %s(), keyword arguments must only have parameter names.", name);
        return false;
    }
    return true;
}

/** Given a function object, returns the dynamic/runtime version of the params struct in it. */
FuncParams derive_runtime_params(FuncObj *func) {
    ObjArray runtimeVals = FLAG_IS_SET(func->flags, FUNC_OPTIONALS_BIT) ?
        AS_PTR(RuntimeFuncObj, func)->paramVals : func->staticParams.values;
    const FuncParams runtimeParams = {
        .minArity = func->staticParams.minArity, .maxArity = func->staticParams.maxArity,
        .names = func->staticParams.names, .values = runtimeVals
    };
    return runtimeParams;
} 

/** 
 * Returns a boolean of whether or not the call is valid. Errors and returns false if not.
 * 
 * Does so using the map to verify if it can be flattened + the amount of params to verify
 * if they're of the right amount.
 */
bool call_is_valid(
    ThreadObj *thread, char *name, const FuncParams params, const u32 positionalAmount,
    MapObj *kwargs
) {
    const u32 kwargsAmount = kwargs->table.count;
    MapObj *kwargsCopy = new_map_obj(&thread->vulnObjs, create_table());
    copy_entries(&kwargs->table, &kwargsCopy->table);
    PUSH(thread, kwargsCopy); // This will have its elements changed instead of the real map.

    if (!check_arity(thread, name, params.minArity, params.maxArity, positionalAmount)) {
        return false;
    }
    if (!(flatten_kwargs(thread, name, params, positionalAmount))) {
        return false;
    }
    DROP_AMOUNT(thread, kwargsAmount); // Drops all flattened kwargs.
    return true;
}

/** Attempts to call the passed object. Returns whether or not it managed to call it. */
static bool call(ThreadObj *thread, Obj *callee, const u32 argsIdx, const u32 argAmount) {
    switch (callee->type) {
    case OBJ_FUNC: {
        FuncObj *func = AS_PTR(FuncObj, callee);
        const FuncParams params = derive_runtime_params(func);
        if (
            !check_arity(thread, func->name->string, params.minArity, params.maxArity, argAmount)
            || !flatten_kwargs(thread, func->name->string, params, argAmount)
        ) {
            return false;
        }
        push_stack_frame(thread, func, thread->stack.objects + argsIdx - 1, thread->frame->sp);
        return true;
    }
    case OBJ_CLASS: {
        ClassObj *cls = AS_PTR(ClassObj, callee);
        thread->stack.objects[argsIdx - 1] = AS_OBJ(new_instance_obj(&thread->vulnObjs, cls));
        if (cls->init) {
            return call(thread, AS_OBJ(cls->init), argsIdx, argAmount);
        } else if (argAmount == 0) {
            DROP(thread); // Pop the kwargs map manually since we didn't actually call anything.
        } else {
            RUNTIME_ERROR(thread, "Expected 0 arguments, got %"PRIu32" instead.", argAmount);
            return false;
        }
        return true;
    }
    case OBJ_METHOD: {
        MethodObj *method = AS_PTR(MethodObj, callee);
        thread->stack.objects[argsIdx - 1] = AS_OBJ(method->instance); 
        return call(thread, AS_OBJ(method->func), argsIdx, argAmount);
    }
    case OBJ_NATIVE_FUNC: {
        NativeFuncObj *native = AS_PTR(NativeFuncObj, callee);
        const FuncParams params = native->params;
        if (
            !check_arity(thread, native->name->string, params.minArity, params.maxArity, argAmount)
            || !flatten_kwargs(thread, native->name->string, params, argAmount)
        ) {
            return false;
        }

        Obj *nativeReturn = native->func(
            thread, thread->stack.objects[argsIdx - 1], thread->stack.objects + argsIdx
        );
        if (nativeReturn == NULL) {
            return false;
        }
        DROP_PUSH_DEPTH(thread, nativeReturn, params.maxArity + 1); // +1 for the callee.
        return true;
    }
    case OBJ_NATIVE_METHOD: {
        NativeMethodObj *method = AS_PTR(NativeMethodObj, callee);
        thread->stack.objects[argsIdx - 1] = AS_OBJ(method->instance);
        return call(thread, AS_OBJ(method->func), argsIdx, argAmount);
    }
    default:
        RUNTIME_ERROR(thread, "Can't call object of type %s.", obj_type_str(callee->type));
        return false;
    }
}

/** 
 * Returns the function object inside an object runnable.
 * 
 * Assumes that the passed runnable is valid and must have an executable function object inside it.
 */
FuncObj *get_runnable_func(Obj *runnable) {
    ASSERT(
        !(runnable->type == OBJ_CLASS && AS_PTR(ClassObj, runnable)->init == NULL),
        "Expected runnable class to have an init method."
    );
    switch (runnable->type) {
    case OBJ_FUNC: return AS_PTR(FuncObj, runnable);
    case OBJ_METHOD: return AS_PTR(MethodObj, runnable)->func;
    case OBJ_CLASS: return AS_PTR(ClassObj, runnable)->init;
    default: UNREACHABLE_ERROR();
    }
}

/** Prepares the thread's stack and state, then executes the interpreter loop on the thread. */
static void execute_thread_entry(ThreadObj *thread, ListObj *args, MapObj *kwargs) {
    FuncObj *runnableFunc = get_runnable_func(thread->runnable);
    push_stack_frame(thread, runnableFunc, thread->stack.objects, thread->stack.objects);
    
    // Lays the function, args, and the kwargs map in the stack to prepare for the call.
    PUSH(thread, runnableFunc);
    for (u32 i = 0; i < args->items.length; i++) {
        PUSH(thread, args->items.data[i]);
    }
    PUSH(thread, kwargs);
    DROP_AMOUNT_PROTECTED(&thread->vulnObjs, 2); // Now args and kwargs are safe in the frame root.

    // Turns the func and args into a call into the function for the thread.
    call(thread, thread->runnable, 1, args->items.length);
    interpreter_loop(thread);
}

/** 
 * The internal thread start function, which executes with a thread that has protected arguments.
 * 
 * The arguments are expected to be found in the thread's protected vulnerable objects.
 * The first one is expected to be a list of positional arguments, while the second protected
 * object is expected to be the map of keyword arguments.
 */
THREAD_FUNC(thread_entry, passedArg) {
    ThreadObj *thread = AS_PTR(ThreadObj, passedArg);
    ObjArray *protected = &thread->vulnObjs.protected;
    ASSERT(
        protected->length == 2,
        "Expected thread to hold 2 arguments (args and kwargs), got %u instead.", protected->length
    );
    ASSERT(
        protected->data[0]->type == OBJ_LIST,
        "Expected first protected to be arg list, got %s instead.",
        obj_type_str(protected->data[0]->type)
    );
    ASSERT(
        protected->data[0]->type == OBJ_LIST,
        "Expected second protected to be kwarg map, got %s instead.",
        obj_type_str(protected->data[1]->type)
    );

    ListObj *args = AS_PTR(ListObj, protected->data[0]);
    MapObj *kwargs = AS_PTR(MapObj, protected->data[1]);
    execute_thread_entry(thread, args, kwargs);
    set_thread_state(thread, THREAD_FINISHED);
    return NULL;
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
static void close_captures(ThreadObj *thread, const u32 pops) {
    for (i64 i = (i64)thread->openCaptures.length - 1; i >= 0; i--) {
        CapturedObj *toClose = AS_PTR(CapturedObj, thread->openCaptures.data[i]);
        if (toClose->stackIdx >= thread->stack.length - pops) {
            toClose->isOpen = false;
            toClose->captured = thread->stack.objects[toClose->stackIdx];
            DROP_DA(&thread->openCaptures);
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
static void call_return(ThreadObj *thread) {
    const u32 arity = thread->frame->func->staticParams.maxArity;
    Obj *returned = safe_pop(thread);
    if (thread->openCaptures.length > 0) {
        close_captures(thread, thread->frame->sp - thread->frame->bp);
    }
    
    pop_stack_frame(thread);
    DROP_PUSH_DEPTH(thread, returned, arity + 1); // +1 to also pop the callee.
}

/**
 * Returns true if the thread is both a spawned thread and has finished its main func execution.
 *  
 * This is dependant on the fact that every spawned thread makes a manual, initial stack frame,
 * and then makes a call on the main function it executes. So if the call stack's size is 1,
 * then the only frame that remains is the first one inserted which was behind the thread's
 * runnable.
 */
static bool is_spawned_thread_done(ThreadObj *thread) {
    return !thread->isMain && thread->callStack.length == 1;
}

/** Captures a variable by creating a capture object and placing it in the appropriate places. */
static void capture_variable(ThreadObj *thread, Obj *capturedObj, const u32 stackLocation) {
    RuntimeFuncObj *closure = AS_PTR(RuntimeFuncObj, thread->frame->func);
    Obj *capture = AS_OBJ(new_captured_obj(&thread->vulnObjs, capturedObj, stackLocation));
    PUSH_DA(&closure->captures, capture);
    PUSH_DA(&thread->openCaptures, capture);
}

/** 
 * Returns whether or not importing the passed absolute path would make a circular import.
 * 
 * Does so by simply checking if it's already in the module contexts array, which means its
 * already in the import chain.
 */
static bool is_circular_import(ModuleObj *importer, StringObj *path) {
    ModuleObj *current = importer;
    while (current != NULL) {
        if (equal_obj(AS_OBJ(current->path), AS_OBJ(path))) {
            return true;
        }
        current = current->importedBy;
    }
    return false;
}

/** 
 * Tries to compile the passed path and sets its context as the one currently executing.
 * 
 * It can fail if the passed path doesn't exist, or causes a circular import. When that happens,
 * it simply returns false, otherwise returns true.
 */
static bool import_module(ThreadObj *thread, StringObj *path) {
    if (!file_exists(path->string)) {
        RUNTIME_ERROR(thread, "File '%s' doesn't exist.", path->string);
        return false;
    }
    // Convert to ensure the path is absolute (important for storing and erroring circular imports).
    char *absolutePath = alloc_absolute_path(path->string);
    path = new_string_obj(&thread->vulnObjs, absolutePath, strlen(absolutePath));
    free(absolutePath);
    if (is_circular_import(thread->module, path)) {
        RUNTIME_ERROR(thread, "Circular import of file '%s' not allowed.", path->string);
        return false;
    }

    char *source = alloc_file_source(path->string);
    ZmxProgram *program = thread->vm->program;
    program->currentFile = path;
    FuncObj *func = compile_file_source(&thread->vulnObjs, source, false);
    free(source);
    if (func == NULL) {
        return false; // Failed to compile.
    }

    PUSH_PROTECTED(&thread->vulnObjs, AS_OBJ(func));
    push_stack_frame(thread, func, thread->frame->sp, thread->frame->sp);
    push_module(thread, path, thread->module);
    DROP_PROTECTED(&thread->vulnObjs);
    return true;
}

/** 
 * Tries to push the values of all names that existed in the passed module from the names list.
 * 
 * Returns whether or not it found and successfully pushed all the values
 * of the corresponding names.
 */
static bool import_names(ThreadObj *thread, ModuleObj *importedModule, ListObj *names) {
    for (u32 i = 0; i < names->items.length; i++) {
        Obj *value = get_global(importedModule, names->items.data[i]);
        if (value) {
            PUSH(thread, value);
        } else {
            RUNTIME_ERROR(
                thread, "Name '%s' not found in module '%s'.",
                AS_PTR(StringObj, names->items.data[i])->string, importedModule->path->string
            );
            return false;            
        }
    }
    return true;
}

/** Executes all the bytecode in the passed VM's function object. */
static void interpreter_loop(ThreadObj *thread) {
    ZmxProgram *program = thread->vm->program;
    VulnerableObjs *vulnObjs = &thread->vulnObjs;
    while (true) {
#if DEBUG_RUNTIME
        print_runtime_state(
            thread->frame->func, thread->stack.objects, thread->stack.length,
            thread->frame->ip - thread->frame->func->bytecode.data, thread->instrSize
        );
#endif
        DROP_ALL_UNROOTED(vulnObjs); // Default drops all vulnerables after ever instruction.
        const OpCode opcode = READ_INSTR(thread);
        switch (opcode) {
        case OP_LOAD_CONST:
            PUSH(thread, READ_CONST(thread));
            break;
        case OP_ARG_16: thread->instrSize = INSTR_TWO_BYTES; break;
        case OP_ARG_32: thread->instrSize = INSTR_FOUR_BYTES; break;
        case OP_TRUE: PUSH(thread, program->internedTrue); break;
        case OP_FALSE: PUSH(thread, program->internedFalse); break;
        case OP_NULL: PUSH(thread, program->internedNull); break;
        case OP_ADD:
            if (BIN_LEFT(thread)->type == OBJ_STRING && BIN_RIGHT(thread)->type == OBJ_STRING) {
                Obj *result = AS_OBJ(concatenate(
                    vulnObjs,
                    AS_PTR(StringObj, BIN_LEFT(thread)), AS_PTR(StringObj, BIN_RIGHT(thread))
                ));
                DROP_PUSH_DEPTH(thread, result, 2);
                break;
            }
            MATH_BIN_OP(thread, "+", NUM_VAL(BIN_LEFT(thread)) + NUM_VAL(BIN_RIGHT(thread)));
            break;
        case OP_SUBTRACT:
            MATH_BIN_OP(thread, "-", NUM_VAL(BIN_LEFT(thread)) - NUM_VAL(BIN_RIGHT(thread)));
            break;
        case OP_MULTIPLY:
            MATH_BIN_OP(thread, "*", NUM_VAL(BIN_LEFT(thread)) * NUM_VAL(BIN_RIGHT(thread)));
            break;
        case OP_DIVIDE:
            if (BIN_LEFT(thread)->type == OBJ_STRING && BIN_RIGHT(thread)->type == OBJ_STRING) {
                CharBuffer fullString = create_char_buffer();
                buffer_append_format(
                    &fullString, "%s%c%s", AS_PTR(StringObj, BIN_LEFT(thread))->string,
                    PATH_SEPARATOR, AS_PTR(StringObj, BIN_RIGHT(thread))->string
                );
                StringObj *result = new_string_obj(vulnObjs, fullString.text, fullString.length);
                free_char_buffer(&fullString);
                DROP_PUSH_DEPTH(thread, result, 2);
                break;
            }
            if (
                IS_NUM(BIN_LEFT(thread)) && IS_NUM(BIN_RIGHT(thread))
                && NUM_VAL(BIN_RIGHT(thread)) == 0
            ) {
                INTERP_LOOP_RUNTIME_ERROR(thread, "Can't divide by 0.");
            }
            MATH_BIN_OP(thread, "/", NUM_VAL(BIN_LEFT(thread)) / NUM_VAL(BIN_RIGHT(thread)));
            break;
        case OP_MODULO:
            if (
                IS_NUM(BIN_LEFT(thread)) && IS_NUM(BIN_RIGHT(thread))
                && NUM_VAL(BIN_RIGHT(thread)) == 0
            ) {
                INTERP_LOOP_RUNTIME_ERROR(thread, "Can't modulo by 0.");
            }
            // Modulo doesn't handle floats in C, so use fmod() if there's a float in the operation.
            MATH_BIN_OP(
                thread, "%",
                BIN_HAS_FLOAT(thread) ?
                    fmod(NUM_VAL(BIN_LEFT(thread)), NUM_VAL(BIN_RIGHT(thread)))
                    : (AS_PTR(IntObj, BIN_LEFT(thread))->number
                        % AS_PTR(IntObj, BIN_RIGHT(thread))->number)
            );
            break;
        case OP_EXPONENT:
            MATH_BIN_OP(thread, "**", pow(NUM_VAL(BIN_LEFT(thread)), NUM_VAL(BIN_RIGHT(thread))));
            break;
        case OP_LSHIFT: 
            BITWISE_BIN_OP(thread, "<<", BIT_VAL(BIN_LEFT(thread)) << BIT_VAL(BIN_RIGHT(thread)));
            break;
        case OP_RSHIFT:
            BITWISE_BIN_OP(thread, ">>", BIT_VAL(BIN_LEFT(thread)) >> BIT_VAL(BIN_RIGHT(thread)));
            break;
        case OP_BITWISE_OR:
            BITWISE_BIN_OP(thread, "|", BIT_VAL(BIN_LEFT(thread)) | BIT_VAL(BIN_RIGHT(thread)));
            break;
        case OP_BITWISE_AND:
            BITWISE_BIN_OP(thread, "&", BIT_VAL(BIN_LEFT(thread)) & BIT_VAL(BIN_RIGHT(thread)));
            break;
        case OP_XOR:
            BITWISE_BIN_OP(thread, "^", BIT_VAL(BIN_LEFT(thread)) ^ BIT_VAL(BIN_RIGHT(thread)));
            break;
        case OP_EQUAL:
            BOOL_BIN_OP(thread, equal_obj(BIN_LEFT(thread), BIN_RIGHT(thread)));
            break;
        case OP_NOT_EQUAL:
            BOOL_BIN_OP(thread, !equal_obj(BIN_LEFT(thread), BIN_RIGHT(thread)));
            break;
        case OP_GREATER:
            NUM_BOOL_BIN_OP(thread, ">", NUM_VAL(BIN_LEFT(thread)) > NUM_VAL(BIN_RIGHT(thread)));
            break;
        case OP_GREATER_EQ:
            NUM_BOOL_BIN_OP(thread, ">=", NUM_VAL(BIN_LEFT(thread)) >= NUM_VAL(BIN_RIGHT(thread)));
            break;
        case OP_LESS:
            NUM_BOOL_BIN_OP(thread, "<", NUM_VAL(BIN_LEFT(thread)) < NUM_VAL(BIN_RIGHT(thread)));
            break;
        case OP_LESS_EQ:
            NUM_BOOL_BIN_OP(thread, "<=", NUM_VAL(BIN_LEFT(thread)) <= NUM_VAL(BIN_RIGHT(thread)));
            break;
        case OP_MINUS:
            if (PEEK(thread)->type == OBJ_INT) {
                const ZmxInt negated = -AS_PTR(IntObj, PEEK(thread))->number;
                PEEK(thread) = AS_OBJ(new_int_obj(vulnObjs, negated));
            } else if (PEEK(thread)->type == OBJ_FLOAT) {
                const ZmxFloat negated = -AS_PTR(FloatObj, PEEK(thread))->number;
                PEEK(thread) = AS_OBJ(new_float_obj(vulnObjs, negated));
            } else {
                INTERP_LOOP_RUNTIME_ERROR(
                    thread, "Can't negate object of type %s.", obj_type_str(PEEK(thread)->type)
                );
            }
            break;
        case OP_NOT: {
            if (as_bool(vulnObjs, PEEK(thread))->boolean) {
                PEEK(thread) = AS_OBJ(program->internedFalse);
            } else {
                PEEK(thread) = AS_OBJ(program->internedTrue);
            }
            break;
        }
        case OP_TILDE: {
            ZmxInt flipped;
            if (PEEK(thread)->type == OBJ_INT) {
                flipped = ~AS_PTR(IntObj, PEEK(thread))->number;
            } else if (PEEK(thread)->type == OBJ_BOOL) {
                flipped = ~((ZmxInt)AS_PTR(BoolObj, PEEK(thread))->boolean);
            } else {
                INTERP_LOOP_RUNTIME_ERROR(
                    thread, "Can't apply '~' to %s.", obj_type_str(PEEK(thread)->type)
                );
            }
            PEEK(thread) = AS_OBJ(new_int_obj(vulnObjs, flipped));
            break;
        }
        case OP_IS: {
            ObjType checkedType;
            switch ((DataType)READ_NUMBER(thread)) {
            case TYPE_INT: checkedType = OBJ_INT; break;
            case TYPE_FLOAT: checkedType = OBJ_FLOAT; break;
            case TYPE_BOOL: checkedType = OBJ_BOOL; break;
            case TYPE_STRING: checkedType = OBJ_STRING; break;
            TOGGLEABLE_DEFAULT_UNREACHABLE();
            }
            PEEK(thread) = AS_OBJ(new_bool_obj(vulnObjs, PEEK(thread)->type == checkedType));
            break;
        }
        case OP_AS: {
            Obj *converted;
            switch ((DataType)READ_NUMBER(thread)) {
            case TYPE_INT: converted = AS_OBJ(as_int(vulnObjs, PEEK(thread))); break;
            case TYPE_FLOAT: converted = AS_OBJ(as_float(vulnObjs, PEEK(thread))); break;
            case TYPE_BOOL: converted = AS_OBJ(as_bool(vulnObjs, PEEK(thread))); break;
            case TYPE_STRING: converted = AS_OBJ(as_string(vulnObjs, PEEK(thread))); break;
            TOGGLEABLE_DEFAULT_UNREACHABLE();
            }
            if (converted == NULL) {
                if (PEEK(thread)->type == OBJ_STRING) {
                    INTERP_LOOP_RUNTIME_ERROR(
                        thread, "Can't convert string '%s' to number.",
                        AS_PTR(StringObj, PEEK(thread))->string
                    );
                }
                INTERP_LOOP_RUNTIME_ERROR(
                    thread, "Can't convert %s to a number.", obj_type_str(PEEK(thread)->type)
                );
            }
            PEEK(thread) = AS_OBJ(converted);
            break;
        }
        case OP_FINISH_STRING: {
            const u32 amount = READ_NUMBER(thread);

            // Build a string from the deepest/oldest till the outermost/newest one in the stack.
            CharBuffer finishedString = create_char_buffer();
            for (u32 i = 1; i <= amount; i++) {
                buffer_append_string(
                    &finishedString, AS_PTR(StringObj, PEEK_DEPTH(thread, amount - i))->string
                );
            }
            DROP_PUSH_DEPTH(
                thread, new_string_obj(vulnObjs, finishedString.text, finishedString.length), amount
            );
            free_char_buffer(&finishedString);
            break;
        }
        case OP_RANGE: {
            Obj *step = PEEK(thread);
            Obj *end = PEEK_DEPTH(thread, 1);
            Obj *start = PEEK_DEPTH(thread, 2);
            if (start->type != OBJ_INT || end->type != OBJ_INT || step->type != OBJ_INT) {
                INTERP_LOOP_RUNTIME_ERROR(
                    thread, "Range takes 3 integers, got %s, %s, and %s instead.",
                    obj_type_str(start->type), obj_type_str(end->type), obj_type_str(step->type)
                );
            }
            if (AS_PTR(IntObj, step)->number < 0) {
                INTERP_LOOP_RUNTIME_ERROR(thread, "Range's step can't be a negative number.");
            }
            DROP_PUSH_DEPTH(
                thread, new_range_obj(vulnObjs, NUM_VAL(start), NUM_VAL(end), NUM_VAL(step)), 3
            );
            break;
        }
        case OP_TERNARY: {
            Obj *falseExpr = PEEK(thread);
            Obj *trueExpr = PEEK_DEPTH(thread, 1);
            Obj *condition = PEEK_DEPTH(thread, 2);
            bool boolean = as_bool(vulnObjs, condition)->boolean; 
            DROP_PUSH_DEPTH(thread, boolean ? trueExpr : falseExpr, 3);
            break;
        }
        case OP_LIST: {
            const u32 length = READ_NUMBER(thread);
            ObjArray items = CREATE_DA();
            for (u32 i = 1; i <= length; i++) {
                PUSH_DA(&items, PEEK_DEPTH(thread, length - i));
            }
            Obj *list = AS_OBJ(new_list_obj(vulnObjs, items));
            DROP_PUSH_DEPTH(thread, list, length);
            break;
        }
        case OP_MAP: {
            // x2 the amount of entries to account for each entry being 2: a key and value.
            const u32 length = READ_NUMBER(thread) * 2;
            Table entries = create_table();
            for (u32 i = 0; i < length; i += 2) {
                Obj *value = PEEK_DEPTH(thread, i);
                Obj *key = PEEK_DEPTH(thread, i + 1);
                if (!is_hashable(key)) {
                    INTERP_LOOP_RUNTIME_ERROR(
                        thread, "Can't hash %s key.", obj_type_str(key->type)
                    );
                }
                table_set(&entries, key, value);
            }
            DROP_PUSH_DEPTH(thread, new_map_obj(vulnObjs, entries), length);
            break;
        }
        case OP_GET_BUILT_IN: {
            Obj *value = table_get(&program->builtIn.funcs, READ_CONST(thread));
            PUSH(thread, value);
            break;
        }
        case OP_DECLARE_GLOBAL:
            set_global(thread->module, READ_CONST(thread), PEEK(thread));
            DROP(thread);
            break;
        case OP_ASSIGN_GLOBAL:
            set_global(thread->module, READ_CONST(thread), PEEK(thread));
            break;
        case OP_GET_GLOBAL: {
            Obj *value = get_global(thread->module, READ_CONST(thread));
            ASSERT(value, "Interpreter global get operation returned NULL.");
            PUSH(thread, value);
            break;
        }
        case OP_ASSIGN_LOCAL:
            thread->frame->bp[READ_NUMBER(thread)] = PEEK(thread);
            break;
        case OP_GET_LOCAL: {
            Obj *value = thread->frame->bp[READ_NUMBER(thread)];
            PUSH(thread, value);
            break;
        }
        case OP_CAPTURE_DEPTH: {
            ASSERT(
                FLAG_IS_SET(thread->frame->func->flags, FUNC_CLOSURE_BIT),
                "Tried to capture inside non-closure."
            );
            const u32 depth = READ_NUMBER(thread);
            capture_variable(thread, PEEK_DEPTH(thread, depth), thread->stack.length - depth - 1);
            break;
        }
        case OP_CAPTURE_AT: {
            ASSERT(
                FLAG_IS_SET(thread->frame->func->flags, FUNC_CLOSURE_BIT),
                "Tried to capture inside non-closure."
            );
            const u32 at = READ_NUMBER(thread);
            capture_variable(
                thread, thread->frame->bp[at], thread->frame->bp + at - thread->stack.objects
            );
            break;
        }
        case OP_ASSIGN_CAPTURED: {
            ASSERT(
                FLAG_IS_SET(thread->frame->func->flags, FUNC_CLOSURE_BIT),
                "Tried to assign captured inside non-closure."
            );
            RuntimeFuncObj *closure = AS_PTR(RuntimeFuncObj, thread->frame->func);
            CapturedObj *capture = AS_PTR(CapturedObj, closure->captures.data[READ_NUMBER(thread)]);

            if (capture->isOpen) {
                thread->stack.objects[capture->stackIdx] = PEEK(thread);
            } else {
                capture->captured = PEEK(thread);
            }
            break;
        }
        case OP_GET_CAPTURED: {
            ASSERT(
                FLAG_IS_SET(thread->frame->func->flags, FUNC_CLOSURE_BIT),
                "Tried to get captured inside non-closure."
            );
            RuntimeFuncObj *closure = AS_PTR(RuntimeFuncObj, thread->frame->func);
            CapturedObj *capture = AS_PTR(CapturedObj, closure->captures.data[READ_NUMBER(thread)]);

            if (capture->isOpen) {
                PUSH(thread, thread->stack.objects[capture->stackIdx]);
            } else {
                PUSH(thread, capture->captured);
            }
            break;
        }
        case OP_SET_PROPERTY: {
            Obj *originalObj = PEEK(thread);
            StringObj *name = AS_PTR(StringObj, READ_CONST(thread));
            Obj *value = PEEK_DEPTH(thread, 1);
            if (!set_property(thread, originalObj, name, value)) {
                INTERP_LOOP_FINISH_ERROR(thread);
            }
            break;
        }
        case OP_GET_PROPERTY: {
            Obj *originalObj = PEEK(thread);
            StringObj *name = AS_PTR(StringObj, READ_CONST(thread));
            if (!get_property(thread, originalObj, name)) {
                INTERP_LOOP_FINISH_ERROR(thread);
            }
            break;
        }
        case OP_GET_SUPER: {
            ASSERT(
                PEEK(thread)->type == OBJ_INSTANCE, "Expected stack top to be instance for super."
            );
            ASSERT(thread->frame->func->cls != NULL, "Super outside class not caught by resolver.");
            ASSERT(
                thread->frame->func->cls->superclass != NULL,
                "Super in non-inheriting class not caught by resolver."
            );
            InstanceObj *instance = AS_PTR(InstanceObj, PEEK(thread));
            StringObj *name = AS_PTR(StringObj, READ_CONST(thread));
            ClassObj *superclass = thread->frame->func->cls->superclass;

            Obj *method = find_method(superclass, name);
            if (method) {
                PEEK(thread) = AS_OBJ(new_method_obj(vulnObjs, instance, AS_PTR(FuncObj, method)));
            } else {
                INTERP_LOOP_RUNTIME_ERROR(
                    thread, "Couldn't find method '%s' in superclass.", name->string
                );
            }
            break;
        }
        case OP_DESTRUCTURE: {
            const u32 amount = READ_NUMBER(thread);
            if (!destructure(thread, amount)) {
                INTERP_LOOP_FINISH_ERROR(thread);
            }
            break;
        }
        case OP_FOR_ASSIGN_VARS: {
            const u32 amount = READ_NUMBER(thread);
            if (!destructure(thread, amount)) {
                INTERP_LOOP_FINISH_ERROR(thread);
            }
            for (u32 i = 0; i < amount; i++) {
                // +1 to go over the iterator.
                PEEK_DEPTH(thread, amount + i + 1) = PEEK_DEPTH(thread, i);
            }
            DROP_AMOUNT(thread, amount);
            break;
        }
        case OP_MAKE_ITER: {
            if (!is_iterable(PEEK(thread))) {
                INTERP_LOOP_RUNTIME_ERROR(
                    thread, "Can't iterate over object of type %s.",
                    obj_type_str(PEEK(thread)->type)
                );
            }
            PEEK(thread) = AS_OBJ(new_iterator_obj(vulnObjs, PEEK(thread)));
            break;
        }
        case OP_FOR_ITER_ASSIGN: {
            Obj *iterated = for_iter_or_jump(thread);
            if (iterated) {
                PEEK_DEPTH(thread, 1) = iterated;
            }
            break;
        }
        case OP_FOR_ITER_LOAD: {
            Obj *iterated = for_iter_or_jump(thread);
            if (iterated) {
                PUSH(thread, iterated);
            }
            break;
        }
        case OP_CALL: {
            const u32 argAmount = READ_NUMBER(thread) + 1; // +1 to account for keyword args.
            const u32 argsIdx = thread->frame->sp - argAmount - thread->stack.objects;

            if (!call(thread, PEEK_DEPTH(thread, argAmount), argsIdx, argAmount - 1)) {
                INTERP_LOOP_FINISH_ERROR(thread);
            }
            break;
        }
        case OP_ASSIGN_SUBSCR: {
            Obj *subscript = PEEK(thread);
            Obj *callee = PEEK_DEPTH(thread, 1);
            Obj *value = PEEK_DEPTH(thread, 2);
            if (callee->type == OBJ_LIST) {
                if (!list_assign_subscr(thread, AS_PTR(ListObj, callee), subscript, value)) {
                    INTERP_LOOP_FINISH_ERROR(thread);
                }
            } else if (callee->type == OBJ_MAP) {
                if (!map_assign_subscr(thread, AS_PTR(MapObj, callee), subscript, value)) {
                    INTERP_LOOP_FINISH_ERROR(thread);
                }
            } else {
                INTERP_LOOP_RUNTIME_ERROR(
                    thread, "Can't subscript assign to %s.", obj_type_str(callee->type)
                );
            }
            break;
        }
        case OP_GET_SUBSCR: {
            Obj *subscript = PEEK(thread);
            Obj *callee = PEEK_DEPTH(thread, 1);
            if (
                callee->type == OBJ_LIST || callee->type == OBJ_STRING || callee->type == OBJ_ENUM
            ) {
                if (!array_get_subscr(thread, callee, subscript)) {
                    INTERP_LOOP_FINISH_ERROR(thread);
                }
            } else if (callee->type == OBJ_MAP) {
                if (!map_get_subscr(thread, AS_PTR(MapObj, callee), subscript)) {
                    INTERP_LOOP_FINISH_ERROR(thread);
                }
            } else {
                INTERP_LOOP_RUNTIME_ERROR(
                    thread, "Can't subscript object of type %s.", obj_type_str(callee->type)
                );
            }
            break;
        }
        case OP_OPTIONALS_FUNC: {
            ObjArray values = AS_PTR(ListObj, PEEK(thread))->items;
            FuncObj *func = AS_PTR(FuncObj, PEEK_DEPTH(thread, 1));
            ASSERT(
                FLAG_IS_SET(func->flags, FUNC_RUNTIME_BIT),
                "Function on the stack is not a runtime one."
            );
            RuntimeFuncObj *withOptionals = AS_PTR(RuntimeFuncObj, func);

            FLAG_ENABLE(withOptionals->func.flags, FUNC_OPTIONALS_BIT);
            for (u32 i = 0; i < withOptionals->func.staticParams.minArity; i++) {
                PUSH_DA(&withOptionals->paramVals, NULL); // C-NULL values for mandatories.
            }
            for (u32 i = 0; i < values.length; i++) {
                PUSH_DA(&withOptionals->paramVals, values.data[i]);
            }
            DROP(thread); // The values list.
            break;
        }
        case OP_FUNC: {
            FuncObj *func = AS_PTR(FuncObj, READ_CONST(thread));
            PUSH(thread, new_runtime_func_obj(vulnObjs, func, thread->module));
            break;
        }
        case OP_MAKE_CLOSURE: {
            FuncObj *func = AS_PTR(FuncObj, PEEK(thread));
            ASSERT(
                FLAG_IS_SET(func->flags, FUNC_RUNTIME_BIT),
                "Function on the stack is not a runtime one."
            );
            RuntimeFuncObj *closure = AS_PTR(RuntimeFuncObj, func);
            FLAG_ENABLE(closure->func.flags, FUNC_CLOSURE_BIT);

            // Copy the current function's closure context just in case its got one too.
            RuntimeFuncObj *frameClosure = AS_PTR(RuntimeFuncObj, thread->frame->func);
            for (u32 i = 0; i < frameClosure->captures.length; i++) {
                PUSH_DA(&closure->captures, frameClosure->captures.data[i]);
            }
            break;
        }
        case OP_CLASS: {
            const bool isAbstract = AS_PTR(BoolObj, PEEK(thread))->boolean;
            PEEK(thread) = AS_OBJ(
                new_class_obj(vulnObjs, AS_PTR(StringObj, READ_CONST(thread)), isAbstract)
            );
            break;
        }
        case OP_INHERIT: {
            if (PEEK(thread)->type != OBJ_CLASS) {
                INTERP_LOOP_RUNTIME_ERROR(
                    thread, "Inherited name must be class, but got %s.",
                    obj_type_str(PEEK(thread)->type)
                );
            }
            ClassObj *superclass = AS_PTR(ClassObj, PEEK(thread));
            ClassObj *cls = AS_PTR(ClassObj, PEEK_DEPTH(thread, 1));
            if (superclass->isAbstract && !inherit_abstract(thread, cls, superclass)) {
                INTERP_LOOP_FINISH_ERROR(thread);
            }
            cls->superclass = superclass;
            DROP(thread);
            break;
        }
        case OP_ADD_INIT: {
            ClassObj *cls = AS_PTR(ClassObj, PEEK_DEPTH(thread, 1));
            cls->init = AS_PTR(FuncObj, PEEK(thread));
            DROP(thread);
            break;
        }
        case OP_METHODS: {
            const u32 amount = READ_NUMBER(thread);
            ClassObj *cls = AS_PTR(ClassObj, PEEK_DEPTH(thread, amount));
            for (u32 i = 0; i < amount; i++) {
                // Order of appending doesn't matter since we're using a hash table.
                FuncObj *method = AS_PTR(FuncObj, PEEK_DEPTH(thread, i));
                method->cls = cls; // Sets the class of the function that is now a method.
                table_set(&cls->methods, AS_OBJ(method->name), AS_OBJ(method));
            }
            DROP_AMOUNT(thread, amount);
            break;
        }
        case OP_ABSTRACT_METHODS: {
            const u32 amount = READ_NUMBER(thread);
            ClassObj *cls = AS_PTR(ClassObj, PEEK_DEPTH(thread, amount));
            ASSERT(cls->isAbstract, "Tried to add abstract methods to non-abstract class.");

            for (u32 i = 0; i < amount; i++) {
                PUSH_DA(&cls->abstractMethods, PEEK_DEPTH(thread, i));
            }
            DROP_AMOUNT(thread, amount);
            break;
        }
        case OP_JUMP: {
            const u32 jump = READ_NUMBER(thread);
            thread->frame->ip += jump;
            break;
        }
        case OP_JUMP_BACK: {
            const u32 jump = READ_NUMBER(thread);
            thread->frame->ip -= jump;
            break;
        }
        case OP_JUMP_IF: {
            const u32 jump = READ_NUMBER(thread);
            if (as_bool(vulnObjs, PEEK(thread))->boolean) {
                thread->frame->ip += jump;
            }
            break;
        }
        case OP_JUMP_IF_NOT: {
            const u32 jump = READ_NUMBER(thread);
            if (!(as_bool(vulnObjs, PEEK(thread))->boolean)) {
                thread->frame->ip += jump;
            }
            break;
        }
        case OP_POP_JUMP_IF_NOT: {
            const u32 jump = READ_NUMBER(thread);
            if (!(as_bool(vulnObjs, PEEK(thread))->boolean)) {
                thread->frame->ip += jump;
            }
            DROP(thread);
            break;
        }
        case OP_POP_JUMP_IF: {
            const u32 jump = READ_NUMBER(thread);
            if (as_bool(vulnObjs, PEEK(thread))->boolean) {
                thread->frame->ip += jump;
            }
            DROP(thread);
            break;
        }
        case OP_POP_JUMP_BACK_IF: {
            const u32 jump = READ_NUMBER(thread);
            if (as_bool(vulnObjs, PEEK(thread))->boolean) {
                thread->frame->ip -= jump;
            }
            DROP(thread);
            break;
        }
        case OP_POP_LOCAL:
            if (thread->openCaptures.length > 0) {
                close_captures(thread, 1);
            }
            DROP(thread);
            break;
        case OP_POP_LOCALS: {
            const u32 pops = READ_NUMBER(thread);
            if (thread->openCaptures.length > 0) {
                close_captures(thread, pops);
            }
            DROP_AMOUNT(thread, pops);
            break;
        }
        case OP_POP_CAPTURES:
            ASSERT(
                FLAG_IS_SET(thread->frame->func->flags, FUNC_CLOSURE_BIT),
                "Tried to pop captured inside non-closure."
            );
            DROP_AMOUNT_DA(
                &AS_PTR(RuntimeFuncObj, thread->frame->func)->captures, READ_NUMBER(thread)
            );
            break;
        case OP_RETURN:
            ASSERT(
                thread->callStack.length > 1, "Tried to return top-level or a nonexistent level."
            );
            call_return(thread);
            if (is_spawned_thread_done(thread)) {
                return;
            }
            break;
        case OP_CLOSURE_RETURN:
            ASSERT(
                FLAG_IS_SET(thread->frame->func->flags, FUNC_CLOSURE_BIT),
                "Tried to closure return from non-closure."
            );
            RuntimeFuncObj *closure = AS_PTR(RuntimeFuncObj, thread->frame->func);
            DROP_AMOUNT_DA(&closure->captures, READ_NUMBER(thread));
            call_return(thread);
            if (is_spawned_thread_done(thread)) {
                return;
            }
            break;
        case OP_COPY_TOP: {
            Obj *top = PEEK(thread);
            PUSH(thread, top);
            break;
        }
        case OP_IMPORT: {
            StringObj *path = AS_PTR(StringObj, READ_CONST(thread));
            if (!import_module(thread, path)) {
                INTERP_LOOP_FINISH_ERROR(thread);
            }
            break;
        }
        case OP_IMPORT_NAMES: {
            ASSERT(PEEK(thread)->type == OBJ_MODULE, "Expected module for imported names.");

            ListObj *names = AS_PTR(ListObj, READ_CONST(thread));
            ModuleObj *importedModule = AS_PTR(ModuleObj, safe_pop(thread));
            if (!import_names(thread, importedModule, names)) {
                INTERP_LOOP_FINISH_ERROR(thread);
            }
            break;
        }
        case OP_START_TRY: {
            const bool saveErrorMessage = AS_PTR(BoolObj, PEEK(thread))->boolean;
            DROP(thread); // Already stored as a C-boolean, safe to drop.

            const u32 relativeCatchSpot = READ_NUMBER(thread);
            u8 *catchIp = thread->frame->ip + relativeCatchSpot;
            PUSH_DA(&thread->catches, create_catch_state(thread, catchIp, saveErrorMessage));
            break;
        }
        case OP_FINISH_TRY:
            DROP_DA(&thread->catches);
            break;
        case OP_RAISE: {
            StringObj *message = AS_PTR(StringObj, PEEK(thread));
            INTERP_LOOP_RUNTIME_ERROR(thread, message->string);
            DROP(thread);
            break;
        }
        case OP_END_MODULE: {
            ModuleObj *importedModule = thread->module;
            pop_stack_frame(thread); // The stack frame associated with the module's top level code.
            PUSH(thread, importedModule);
            thread->module = importedModule->importedBy;
            break;
        }
        case OP_EXIT: {
            Obj *exitCode = PEEK(thread);
            if (exitCode->type != OBJ_INT) {
                INTERP_LOOP_RUNTIME_ERROR(thread, "Expected exit code to be integer.");
            }
            set_exit_code(thread, true, AS_PTR(IntObj, exitCode)->number);
            DROP(thread);
            return;
        }
        case OP_EOF:
            set_exit_code(thread, false, 0);
            return;
        TOGGLEABLE_DEFAULT_UNREACHABLE();
        }
    }
}

/** 
 * Interprets the main thread in the passed VM, then frees/empties all threads except the main one.
 * 
 * Wrapper around interpreter_loop() for things that are done before/after the whole program
 * (like debugging, finishing threads, etc.).
 * 
 * Just returns and doesn't execute when CLI debugging modes are on.
 */
void interpret(Vm *vm) {
    vm->program->isRuntime = true;
    CliHandler *cli = vm->program->cli;
    if (cli->debugTokens || cli->debugAst || cli->debugBytecode) {
        return;
    }

#if DEBUG_RUNTIME
    printf("-------------------- VM START --------------------\n");
#endif
    interpreter_loop(get_main_thread(vm));
#if DEBUG_RUNTIME
    printf("-------------------- VM END --------------------\n");
#endif
    finish_vm_threads(vm);
    vm->program->isRuntime = false;
}

/** 
 * Simply executes the passed source string and frees all used memory except the program's.
 * 
 * This is meant to be the starting point of execution, not a function called by any runtime
 * threads.
 */
void interpret_file_source(ZmxProgram *program, char *source, const bool isMain) {
    VulnerableObjs *vulnObjs = &program->gc.startupVulnObjs;
    FuncObj *func = compile_file_source(vulnObjs, source, isMain);
    if (func == NULL) {
        return; // Failed to compile.
    }
    Vm vm;
    init_vm(&vm, program, AS_PTR(RuntimeFuncObj, func));
    DROP_PROTECTED(vulnObjs); // Compiled function goes inside protected, but it's in VM now. Pop.
    interpret(&vm);

    program->gc.vm = NULL;
    free_vm(&vm);
}
