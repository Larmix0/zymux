#ifndef OBJECT_H
#define OBJECT_H

#include "constants.h"
#include "data_structures.h"

#define AS_OBJ(object) ((Obj *)object)

typedef struct ZmxProgram ZmxProgram;

/** Represents all the different types of objects in Zymux in an enum. */
typedef enum {
    OBJ_INT,
    OBJ_FLOAT,
    OBJ_BOOL,
    OBJ_STRING,
    OBJ_FUNC
} ObjType;

/** Base object struct for all objects in Zymux (for type punning). */
typedef struct Obj {
    Obj *next;
    ObjType type;
} Obj;

/** Holds an array of objects, which is an array of pointers to them. */
DECLARE_DA_STRUCT(ObjArray, Obj *);

/** An array of unsigned bytes, typically used for bytecode. */
DECLARE_DA_STRUCT(ByteArray, u8);

/** An array of source positions for storing debugging information. */
DECLARE_DA_STRUCT(SourcePositionArray, SourcePosition);

/** An integer object. */
typedef struct {
    Obj obj;
    ZmxInt number;
} IntObj;

/** A floating number object. */
typedef struct {
    Obj obj;
    ZmxFloat number;
} FloatObj;

/** Represents a boolean (true or false) object. */
typedef struct {
    Obj obj;
    bool boolean;
} BoolObj;

/** Represents a string object. */
typedef struct {
    Obj obj;
    char *string;
    int length;
} StringObj;

/**
 * Represents an object that holds some bytecode, its positions and a constant pool for them.
 * 
 * It's a function object because it's a function/method most of the time.
 * However, this is also used for the top-level bytecode and constant pool
 * to achieve uniformity across the VM.
 */
typedef struct {
    Obj obj;

    /** Name of the function. It gets a special name if it's the top-level code. */
    StringObj *name;

    /** Stores the emitted bytecode of the function. */
    ByteArray bytecode;

    /** Stores a position for each bytecode instruction. Useful when we error later at runtime. */
    SourcePositionArray positions;

    /** The pool which we store constant objects inside (ones created at compile time). */
    ObjArray constPool;
} FuncObj;

/** Returns a new allocated integer object. */
IntObj *new_int_obj(ZmxProgram *program, ZmxInt number);

/** Returns a new allocated float object. */
FloatObj *new_float_obj(ZmxProgram *program, ZmxFloat number);

/** Returns a new allocated boolean object. */
BoolObj *new_bool_obj(ZmxProgram *program, bool boolean);

/** Returns a new allocated string object. */
StringObj *new_string_obj(ZmxProgram *program, char *string);

/** Returns a new allocated function object. */
FuncObj *new_func_obj(ZmxProgram *program, StringObj *name);

/** Returns whether or not 2 objects are considered equal. */
bool equal_obj(const Obj *left, const Obj *right);

/** Returns a new string object that is formed from concatenating left with right. */
StringObj *concatenate(ZmxProgram *program, const StringObj *left, const StringObj *right);

/** Returns the passed object as a string value. */
StringObj *as_string(ZmxProgram *program, Obj *object);

/** Returns the passed object's boolean (whether it's considered "truthy" or "falsy"). */
BoolObj *as_bool(ZmxProgram *program, Obj *object);

/** 
 * Prints the passed object to the console.
 * 
 * If debugPrint is true, then it prints the object in the way it's supposed to show when debugging
 * things like bytecode, otherwise it prints it like the user of Zymux will see it when
 * using Zymux's printing.
 */
void print_obj(const Obj *object, const bool debugPrint);

/** Frees all allocated objects stored in the passed program. */
void free_all_objs(ZmxProgram *program);

#endif
