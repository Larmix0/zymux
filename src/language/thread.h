#ifndef THREAD_H
#define THREAD_H

typedef struct ZmxProgram ZmxProgram;
typedef struct Vm Vm;
typedef struct ThreadObj ThreadObj;

/** Represents the state of a thread object when it's initially created, ran, and finished. */
typedef enum {
    THREAD_PENDING, /** The OS-thread hasn't started executing yet. */
    THREAD_RUNNING, /** the OS-thread is currently executing. */
    THREAD_FINISHED /** The OS-thread has fully finished executing. */
} ThreadState;

#if OS == WINDOWS_OS
#include "windows.h"

/** Defines/declares a function to be internally executed by a C thread. Not static by default. */
#define THREAD_FUNC(funcName, argName) DWORD WINAPI funcName(LPVOID argName)

/** The value that should be returned by every thread func. */
#define THREAD_FUNC_RETURN_VAL 0

#define MUTEX_INIT(mutex) (InitializeCriticalSection(mutex))
#define MUTEX_LOCK(mutex) (EnterCriticalSection(mutex))
#define MUTEX_UNLOCK(mutex) (LeaveCriticalSection(mutex))
#define MUTEX_DESTROY(mutex) (DeleteCriticalSection(mutex))

typedef HANDLE ThreadHandle;
typedef CRITICAL_SECTION Mutex;
typedef DWORD WINAPI (*ThreadFunc)(LPVOID);

#elif OS == UNIX_OS
#include "pthread.h"

/** Defines/declares a function to be internally executed by a C thread. Not static by default. */
#define THREAD_FUNC(funcName, argName) void *funcName(void *argName)

/** The value that should be returned by every thread func. */
#define THREAD_FUNC_RETURN_VAL NULL

#define MUTEX_INIT(mutex) (pthread_mutex_init(mutex, NULL))
#define MUTEX_LOCK(mutex) (pthread_mutex_lock(mutex))
#define MUTEX_UNLOCK(mutex) (pthread_mutex_unlock(mutex))
#define MUTEX_DESTROY(mutex) (pthread_mutex_destroy(mutex))

typedef pthread_t ThreadHandle;
typedef pthread_mutex_t Mutex;
typedef void *(*ThreadFunc)(void *);

#endif

/** Starts executing the passed thread object with the passed function with thread signature. */
void run_thread(ThreadObj *thread, ThreadFunc func);

/** Returns the currently executing native thread. */
ThreadHandle get_current_thread_handle();

/** Joins the passed thread's native handle. Does it whether or not it is valid to do so. */
void join_thread(ThreadObj *thread);

/** Safely retrieves the state of the passed thread object. */
ThreadState get_thread_state(ThreadObj *thread);

/** Safely set a state for the passed thread object. */
void set_thread_state(ThreadObj *thread, const ThreadState newState);

/** Safely returns whether or not the thread has been joined yet. */
bool is_joined_thread(ThreadObj *thread);

/** Safely changes whether or not the thread has been joined to the passed bool. */
void set_thread_joined(ThreadObj *thread, const bool hasJoined);

/** Ends all native threads in the VM. */
void finish_vm_threads(Vm *vm);

/** 
 * A thread-safe way to print a formatted output to a specific file stream.
 * 
 * This acts like a thread-safe fprintf using the print lock inside the program.
 */
void synchronized_print(ZmxProgram *program, FILE *stream, const char *format, ...);

#endif
