# Examples
This directory has examples of simple programs written in Zymux.
<br>
These examples are designed to be simple, small, and showcase certain features of the language.

## Files
### threadedTasks.zmx
A multithreaded program which simulates performing tasks with sleeps and compares the amount of time taken to complete those tasks sequentially in contrast to the time it takes to complete them with multiple threads.
<br>
The program also allows for customizing the amount of "tasks" performed and seconds taken per task in the CLI interface.
<br>
It highlights the language's ability to multithread, handle CLI options, as well as some functions for timing and sleeping.

### contacts.zmx
A program for keeping track of a contacts book using file operations, writing, reading and saving the information of the contacts.
<br>
It highlights the language's ability to do object oriented programming, file operations, and other syntax/built-in features.

## Running examples
Run (Unix): ```./bin/zymux examples/<file>.zmx```
<br>
Run (Windows): ```.\bin\zymux.exe examples\<file>.zmx```

#### Example (Unix)
```sh
./bin/zymux examples/threadedTasks.zmx
```

#### Example (Windows)
```sh
.\bin\zymux examples\contacts.zmx
```
