#include <stdio.h>
#include <stdlib.h>

#include "object.h"
#include "program.h"
#include "thread.h"
#include "vm.h"

/** 
 * Attempts to create a native thread (which runs immediately in C).
 * 
 * Returns a boolean of whether or not it managed to create the thread.
 */
static bool run_native_thread(ThreadObj *thread, ThreadFunc func) {
#if OS == WINDOWS_OS
    thread->native = CreateThread(NULL, 0, func, thread, 0, NULL);
    return thread->native != NULL;
#elif OS == UNIX_OS
    return pthread_create(&thread->native, NULL, func, thread) == 0;
#endif
}

/** Starts executing the passed thread object with the passed function with thread signature. */
void run_thread(ThreadObj *thread, ThreadFunc func) {
    if (!run_native_thread(thread, func)) {
        THREAD_ERROR("Failed to create internal native thread with ID: %" PRIu32, thread->id);
    }

#if OS == WINDOWS_OS
    // Doesn't kill it, but releases control of the handle (including joining).
    if (thread->isDaemon) {
        CloseHandle(thread->native);
    }
#elif OS == UNIX_OS
    // Detach, meaning the thread can't be joined anymore.
    if (thread->isDaemon && pthread_detach(thread->native) != 0) {
        THREAD_ERROR("Failed to turn thread daemon with ID: %" PRIu32, thread->id);
    }
#endif

    set_thread_state(thread, THREAD_RUNNING);
}

/** Returns the currently executing native thread. */
ThreadHandle get_current_thread_handle() {
#if OS == WINDOWS_OS
    return OpenThread(THREAD_ALL_ACCESS, FALSE, GetCurrentThreadId());
#elif OS == UNIX_OS
    return pthread_self();
#endif
}

/** Joins the passed thread's native handle. Does it whether or not it is valid to do so. */
void join_thread(ThreadObj *thread) {
    set_thread_joined(thread, true);
#if OS == WINDOWS_OS
    WaitForSingleObject(thread->native, INFINITE);
    CloseHandle(thread->native);
#elif OS == UNIX_OS
    pthread_join(thread->native, NULL);
#endif
}

/** Safely retrieves the state of the passed thread object. */
ThreadState get_thread_state(ThreadObj *thread) {
    MUTEX_LOCK(&thread->threadLock);
    const ThreadState state = thread->stateUnsafe;
    MUTEX_UNLOCK(&thread->threadLock);
    return state;
}

/** Safely set a state for the passed thread object. */
void set_thread_state(ThreadObj *thread, const ThreadState newState) {
    MUTEX_LOCK(&thread->threadLock);
    thread->stateUnsafe = newState;
    MUTEX_UNLOCK(&thread->threadLock);
}

/** Safely returns whether or not the thread has been joined yet. */
bool is_joined_thread(ThreadObj *thread) {
    MUTEX_LOCK(&thread->threadLock);
    const bool isJoined = thread->isJoinedUnsafe;
    MUTEX_UNLOCK(&thread->threadLock);
    return isJoined;
}

/** Safely changes whether or not the thread has been joined to the passed bool. */
void set_thread_joined(ThreadObj *thread, const bool hasJoined) {
    MUTEX_LOCK(&thread->threadLock);
    thread->isJoinedUnsafe = hasJoined;
    MUTEX_UNLOCK(&thread->threadLock);   
}

/** Returns whether or not the thread can be joined. */
static bool is_joinable_thread(ThreadObj *thread) {
    return !is_joined_thread(thread) && get_thread_state(thread) != THREAD_NOT_STARTED
        && !thread->isDaemon && !thread->isMain;
}

/** 
 * Finishes the passed thread's the execution (for when the VM is shutting down).
 * 
 * Returns whether or not it was joined or false if it was an already finished thread.
 */
static bool finish_thread(ThreadObj *thread) {
    if (is_joinable_thread(thread)) {
        join_thread(thread);
        return true;
    } else {
        return false;
    }
}

/** 
 * Ends all native threads in the VM.
 * 
 * Repeatedly iterates through the array and checks if any new threads that weren't joined are
 * added. This is to mitigate against a thread creating another thread while being joined.
 */
void finish_vm_threads(Vm *vm) {
#if OS == WINDOWS_OS
    // Windows uses handles to threads, so the main thread handle must be closed (but not joined).
    CloseHandle(get_main_thread(vm)->native);
#endif

    bool joinedAny;
    do {
        joinedAny = false;
        for (u32 i = 0; i < vm_threads_length(vm); i++) {
            joinedAny = finish_thread(vm_thread_at(vm, i));
        }
    } while (joinedAny);
    
    // Drop all but main thread (kept for REPL).
    MUTEX_LOCK(&vm->threadsLock);
    DROP_AMOUNT_DA(&vm->threadsUnsafe, vm->threadsUnsafe.length - 1);
    MUTEX_UNLOCK(&vm->threadsLock);
}

/** A thread-safe way to print a formatted output to a specific file stream. */
void synchronized_print(ZmxProgram *program, FILE *stream, const char *format, ...) {
    va_list args;
    va_start(args, format);
    MUTEX_LOCK(&program->printLock);
    vfprintf(stream, format, args);
    fflush(stream);
    MUTEX_UNLOCK(&program->printLock);
    va_end(args);
}
