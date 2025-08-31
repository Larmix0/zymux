# Built-in multithread module documentation

<br>

# Classes

```
Thread(runnable, daemon=false):
    Represents a thread that executes a runnable user function independently.
    The function executes in parallel (if possible, otherwise concurrently).

    bool 'daemon':
        Whether or not the thread is a daemon thread, meaning that it runs in the background
        and does not get joined like other normal threads at the end of the program.

    null run(args=[], kwargs={}):
        Executes the thread object with an optional list of arguments and map of kwargs if supplied.
        A thread cannot run twice.

    null join(bypass=false):
        Joins the thread to the main executing thread.
        If the thread being joined is daemon, it errors out if bypass is false, otherwise ignores
        the entire function call if bypass is set to true.
```

```
Lock():
    A mutex lock for synchronizing execution when running threads.

    null obtain():
        Grabs control of the lock, and waits for its turn
        if it's already in current use from another thread.

    null release():
        Relinquishes the obtained lock so that another thread can begin using it.
        Raises a runtime error if the lock wasn't obtained to begin with.

    bool held():
        Returns whether or not the lock is currently in use (obtained) by any threads.
```
