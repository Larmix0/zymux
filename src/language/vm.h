#ifndef VM_H
#define VM_H

#include "emitter.h"
#include "hash_table.h"
#include "object.h"

/** 
 * The macro for a normal VM runtime error call.
 * 
 * Appends the formatted message to a temporary buffer, then creates a string object
 * which is GC protected and passes it to the base runtime error call.
 * 
 * Unprotects the string object and frees the buffer after the call has finished.
 */
#define RUNTIME_ERROR(thread, ...) \
    do { \
        CharBuffer buffer_ = create_char_buffer(); \
        buffer_append_format(&buffer_, __VA_ARGS__); \
        base_runtime_error( \
            thread, new_string_obj(&(thread)->vulnObjs, buffer_.text, strlen(buffer_.text)) \
        ); \
        free_char_buffer(&buffer_); \
    } while (0)

/** Zymux's virtual machine for interpreting the bytecode. */
typedef struct Vm {
    ZmxProgram *program; /** The Zymux program for general information and allocations. */

    /** 
     * All threads executed in the VM that have IDs equal to their index in the array.
     * 
     * This includes every thread ever ran, so even if a thread finished it doesn't get popped
     * off of this array, only its state is marked as finished for when we iterate/access it.
     * 
     * Not thread-safe to access without a lock.
     */
    ObjArray threadsUnsafe;
    Mutex threadsLock; /** The mutex lock for modifying/accessing the VM's threads. */

    /** 
     * This holds all modules ever imported (accessed by ID keys as indices).
     * 
     * This includes every module ever created, so even if its execution is done it should
     * still remain here.
     * 
     * It is not thread-safe to access without a lock.
     */
    ObjArray modulesUnsafe;
    Mutex modulesLock; /** The mutex lock for modifying/accessing the VM's modules. */
} Vm;

/** 
 * Starts running the VM with the current thread as the main one using the passed function.
 * 
 * Expects the passed function to have runtime context allocated to it.
 * Also, sets the VM currently being GCed to it (the VM being initialized).
 */
void init_vm(Vm *vm, ZmxProgram *program, RuntimeFuncObj *func);

/** Frees all memory that the passed VM allocated. */
void free_vm(Vm *vm);

/** Returns the main thread inside the VM (the VM must at least have one thread). */
ThreadObj *get_main_thread(Vm *vm);

/**
 * Executes the main thread in the passed VM, then frees/empties all threads except the main one.
 * 
 * Doesn't execute when CLI debugging modes are on.
 */
void interpret(Vm *vm);

/** 
 * Simply executes the passed source string and frees all used memory except the program's.
 * 
 * This is meant to be the starting point of execution, not a function called by any runtime
 * threads.
 */
void interpret_file_source(ZmxProgram *program, char *source, const bool isMain);

/** 
 * Base function that tries to raise a runtime error with an object error message.
 * 
 * Sets a catch state and doesn't error if there's one. Meant only as a base error function,
 * so prefer to use macros that wrap this instead of directly calling it.
 */
void base_runtime_error(ThreadObj *thread, StringObj *errorMessage);


/** Safely retrieves the length of the threads array in the passed VM. */
u32 vm_threads_length(Vm *vm);

/** Safely retrieves a thread at a specific index in the VM's array of threads. */
ThreadObj *vm_thread_at(Vm *vm, const u32 index);

/** 
 * Creates a new thread object, pushing it into the VM's threads and returning it.
 * 
 * This is for adding a thread to the VM in a thread-safe manner.
 */
ThreadObj *vm_push_thread(
    VulnerableObjs *vulnObjs, Vm *vm, Obj *runnable, ModuleObj *module,
    const bool isMain, const bool isDaemon
);

/** 
 * Returns the function object inside a runnable.
 * 
 * Assumes that the passed runnable must have an executable function object inside it.
 */
FuncObj *get_runnable_func(Obj *runnable);

/** 
 * The internal thread start function, which executes with a thread that has protected arguments.
 * 
 * The arguments are expected to be found in the thread's protected vulnerable objects.
 * The first one is expected to be a list of positional arguments, while the second protected
 * object is expected to be the map of keyword arguments.
 */
THREAD_FUNC(thread_entry, passedArg);

#endif
