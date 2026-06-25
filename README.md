# Zymux

A flexible multi-paradigm language that combines the simplicity of Python with C-style syntax familiarity.

[![Zymux test flow](https://github.com/Larmix0/zymux/actions/workflows/test_flow.yml/badge.svg)](https://github.com/Larmix0/zymux/actions/workflows/test_flow.yml)

## Overview

**Zymux** is a **high-level, dynamically typed** programming language that supports various features including **multithreading, exception handling, reading/writing files, running REPL** and others.
<br>
It is a **multi-paradigm** programming language which allows object oriented programming, functional programming, and procedural programming.

The project is documented and tested with integration tests and unit tests written in Lukip, a custom microframework for unit testing.

## Extension
Zymux source files use ```.zmx``` as the file extension.

## Programming Styles
* **Object-Oriented Programming**
    * Classes
    * Objects
    * Constructors
    * Methods and fields
    * Inheritance ("inherits" keyword for subclasses)
    * Polymorphism (via method overriding and duck typing)
    * Abstraction ("abstract" keyword for classes and methods)
* **Functional Programming**
    * First-class functions
    * Higher-order functions
    * Anonymous functions
    * Closures
* **Procedural Programming**
    * Variables
    * Functions
    * Loops (while, for, do-while)
    * Conditionals (if-else and match)

## Other Features
* "entry" keyword to denote main functions that execution begins on after global scope finishes
* Multithreading support with mutexes (`Lock` class), thread joining, and daemon threads 
* Enums for grouping related constants
* Exception handling (with "try", "catch" and "raise")
* Multi-variable declaration and assignment
* Importing other Zymux files
* Opening, reading, and writing to files
* Built-in functions and modules
* Interpolated strings (`$"..."`) and raw strings (`#"..."`)

## Examples

### Basic Example
```
entry {
    print("Hello, world!");
}
```

### Swapping Variables Example
```
entry {
    let swap = func(a, b) { return [b, a]; };

    let [x, y] = [2, 1];
    [x, y] = swap(x, y);

    print($"x value: {x}");
    print($"y value: {y}");
}
```

## Other Examples
Inside the /examples directory are a few more practical programs.

## Documentation
The language's grammar (BNF), built-in classes, modules, functions and runtime opcodes are documented inside the /docs directory.

## Building
Clone in the directory with submodules, then make:

```
git clone --recurse-submodules https://github.com/Larmix0/zymux.git
make
```

### Running
* **Windows:** `.\bin\zymux.exe <file>.zmx`
* **Linux/macOS:** `./bin/zymux <file>.zmx`
* *Omitting the file will run REPL*
* *Using the "--help" option will print a help menu to guide through the other options*

### Testing
Run `make test` to perform all tests

* For running unit tests only, use `make unit-test`
* For running integration (language) tests only, use `make integration-test`. *Keep in mind that it'll only print when it starts and finishes if no failures occurred, and would print error messages on failures.*

### Cleaning
Simply run `make clean`
