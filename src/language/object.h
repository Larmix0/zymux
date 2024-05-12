#ifndef OBJECT_H
#define OBJECT_H

#include "constants.h"
#include "data_structures.h"

typedef struct ZmxProgram ZmxProgram;
typedef struct Obj Obj; // So Obj can have other Objs inside it.

/** Represents all the different types of objects in Zymux in an enum. */
typedef enum {
    OBJ_INT,
    OBJ_FLOAT,
    OBJ_BOOL,
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

/**
 * TODO: this is a primitive version of the function object for the purposes of storing bytecode,
 * later on it'll have more stuff inside it, like its name.
 * Also, add documentation that mentions Zymux uses FuncObj to store top-level/global bytecode too.
 */
typedef struct {
    Obj obj;
    
    /** Stores the emitted bytecode of the function. */
    ByteArray bytecode;

    /** Stores a position for each bytecode instruction. Useful when we error later at runtime. */
    SourcePositionArray positions;

    /** The pool which we store constant objects inside (ones created at compile time). */
    ObjArray constPool;
} FuncObj;

/** Returns a new allocated int object from the passed integer. */
IntObj *new_int_obj(ZmxProgram *program, ZmxInt number);

/** Returns a new allocated float object from the passed floating number. */
FloatObj *new_float_obj(ZmxProgram *program, ZmxFloat number);

/** Returns a new allocated boolean object from the passed bool. */
BoolObj *new_bool_obj(ZmxProgram *program, bool boolean);

/** TODO: will need to change this as FuncObj gets more parameters that'll need to be passed. */
FuncObj *new_func_obj(ZmxProgram *program);

/** Returns whether or not 2 objects are considered equal. */
bool equal_obj(const Obj *left, const Obj *right);

/** Returns whether the value of the passed object is "truthy" or "falsy" in a C boolean. */
bool obj_as_bool(Obj *object);

/** Prints the passed object to the console. */
void print_obj(const Obj *object);

/** Frees all allocated objects stored in the passed program. */
void free_all_objs(ZmxProgram *program);

#endif
