#include <stdio.h>
#include <stdlib.h>

#include "object.h"
#include "thread.h"
#include "vm.h"

/** Starts executing the passed thread object with the passed function with thread signature. */
void run_thread(ThreadObj *thread, ThreadFunc func) {
    if (pthread_create(&thread->native, NULL, func, thread) != 0) {
        THREAD_ERROR("Failed to create internal native thread with ID: %u", thread->id);
    }
    if (thread->isDaemon && pthread_detach(thread->native) != 0) {
        THREAD_ERROR("Failed to make thread daemon with ID: %u", thread->id);
    }

    set_thread_state(thread, THREAD_RUNNING);
}

/** Returns the currently executing native thread. */
ThreadHandle get_current_thread_handle() {
    return pthread_self();
}

/** Joins the passed thread's native handle. Does it whether or not it is valid to do so. */
void join_thread(ThreadObj *thread) {
    pthread_join(thread->native, NULL);

    set_thread_joined(thread, true);
}

/** Initializes the passed mutex. */
bool init_mutex(Mutex *mutex) {
    return pthread_mutex_init(mutex, NULL) == 0;
}

/** Performs a lock on the mutex, so it can't be used from other threads until it's unlocked. */
void mutex_lock(Mutex *mutex) {
    pthread_mutex_lock(mutex);
}

/** Unlocks the passed mutex so other threads can now use it. */
void mutex_unlock(Mutex *mutex) {
    pthread_mutex_unlock(mutex);
}

/** Destroys the resources allocated for the passed mutex. */
void destroy_mutex(Mutex *mutex) {
    pthread_mutex_destroy(mutex);
}

/** Safely retrieves the state of the passed thread object. */
ThreadState get_thread_state(ThreadObj *thread) {
    mutex_lock(&thread->lock);
    const ThreadState state = thread->stateUnsafe;
    mutex_unlock(&thread->lock);
    return state;
}

/** Safely set a state for the passed thread object. */
void set_thread_state(ThreadObj *thread, const ThreadState newState) {
    mutex_lock(&thread->lock);
    thread->stateUnsafe = newState;
    mutex_unlock(&thread->lock);
}

/** Safely returns whether or not the thread has been joined yet. */
bool is_thread_joined(ThreadObj *thread) {
    mutex_lock(&thread->lock);
    const bool isJoined = thread->isJoinedUnsafe;
    mutex_unlock(&thread->lock);
    return isJoined;
}

/** Safely changes whether or not the thread has been joined to the passed bool. */
void set_thread_joined(ThreadObj *thread, const bool hasJoined) {
    mutex_lock(&thread->lock);
    thread->isJoinedUnsafe = hasJoined;
    mutex_unlock(&thread->lock);   
}

/** 
 * Ends all native threads in the VM.
 * 
 * Repeatedly iterates through the array and checks if any new threads that weren't joined are
 * added. This is to mitigate against a thread creating another thread while being joined.
 */
void finish_vm_threads(Vm *vm) {
    bool joinedAny;
    do {
        joinedAny = false;
        for (u32 i = 0; i < vm_threads_length(vm); i++) {
            ThreadObj *thread = vm_thread_at(vm, i);

            if (!is_thread_joined(thread) && !thread->isDaemon && !thread->isMain) {
                join_thread(thread);
                joinedAny = true;
            }
        }
    } while (joinedAny);
    
    // Drop all but main thread (kept for REPL).
    mutex_lock(&vm->threadsLock);
    DROP_AMOUNT_DA(&vm->threadsUnsafe, vm->threadsUnsafe.length - 1);
    mutex_unlock(&vm->threadsLock);
}
