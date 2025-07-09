#ifndef THREAD_H
#define THREAD_H

#include <pthread.h>

typedef struct Vm Vm;
typedef struct ThreadObj ThreadObj;

/** Represents the state of a thread object when it's initially created, ran, and finished. */
typedef enum {
    THREAD_NOT_STARTED, /** The OS-thread hasn't started executing yet. */
    THREAD_RUNNING, /** the OS-thread is currently executing. */
    THREAD_FINISHED /** The OS-thread has fully finished executing. */
} ThreadState;

/** The C-level thread handle internally in the program. */
typedef pthread_t ThreadHandle;

/** The C-level mutex in the internal program. */
typedef pthread_mutex_t Mutex;

/** The signature of a function that a thread can execute. */
typedef void *(*ThreadFunc)(void *);

/** Defines/declares a function to be internally executed by a C thread. Not static by default. */
#define THREAD_FUNC(funcName, argName) void *funcName(void *argName)

/** Starts executing the passed thread object with the passed function with thread signature. */
void run_thread(ThreadObj *thread, ThreadFunc func);

/** Returns the currently executing native thread. */
ThreadHandle get_current_thread_handle();

/** Joins the passed thread's native handle. Does it whether or not it is valid to do so. */
void join_thread(ThreadObj *thread);

/** Initializes the passed mutex. */
bool init_mutex(Mutex *mutex);

/** Performs a lock on the mutex, so it can't be used from other threads until it's unlocked. */
void mutex_lock(Mutex *mutex);

/** Unlocks the passed mutex so other threads can now use it. */
void mutex_unlock(Mutex *mutex);

/** Destroys the resources allocated for the passed mutex. */
void destroy_mutex(Mutex *mutex);

/** Safely retrieves the state of the passed thread object. */
ThreadState get_thread_state(ThreadObj *thread);

/** Safely set a state for the passed thread object. */
void set_thread_state(ThreadObj *thread, const ThreadState newState);

/** Safely returns whether or not the thread has been joined yet. */
bool is_thread_joined(ThreadObj *thread);

/** Safely changes whether or not the thread has been joined to the passed bool. */
void set_thread_joined(ThreadObj *thread, const bool hasJoined);

/** Ends all native threads in the VM. */
void finish_vm_threads(Vm *vm);

#endif
