#ifndef OBJECT_H
#define OBJECT_H

#include <stdbool.h>

#include "built_in.h"
#include "constants.h"
#include "dynamic_array.h"
#include "hash_table.h"

/** Converts anything that "inherits" from object to its type punning base form. */
#define AS_OBJ(object) ((Obj *)object)

/** Resolves to whether or not the passed object is considered a number (integer or float). */
#define IS_NUM(obj) ((obj)->type == OBJ_INT || (obj)->type == OBJ_FLOAT)

/** Resolves to whether or not the passed object can have a bitwise operation applied into it. */
#define IS_BITWISABLE(obj) ((obj)->type == OBJ_INT || (obj)->type == OBJ_BOOL)

/** Returns the number the passed object holds, assuming it's either an integer or float object. */
#define NUM_VAL(obj) \
    ((obj)->type == OBJ_INT ? AS_PTR(IntObj, obj)->number : AS_PTR(FloatObj, obj)->number)

/** Returns the C-value of an object that is presumed to be one that can use bitwise operations. */
#define BIT_VAL(obj) \
    ((obj)->type == OBJ_INT ? AS_PTR(IntObj, obj)->number : AS_PTR(BoolObj, obj)->boolean)

typedef struct ZmxProgram ZmxProgram;
typedef struct Obj Obj;

/** Represents all the different types of objects in Zymux in an enum. */
typedef enum {
    OBJ_INT,
    OBJ_FLOAT,
    OBJ_BOOL,
    OBJ_NULL,
    OBJ_STRING,
    OBJ_RANGE,
    OBJ_LIST,
    OBJ_MAP,
    OBJ_ENUM_MEMBER,
    OBJ_ENUM,
    OBJ_FUNC,
    OBJ_CAPTURED,
    OBJ_CLASS,
    OBJ_INSTANCE,
    OBJ_METHOD,
    OBJ_NATIVE_FUNC,
    OBJ_ITERATOR
} ObjType;

/** Base object struct for all objects in Zymux (for type punning). */
typedef struct Obj {
    Obj *next; /** Linked list of all objects to make sure we can always access all of them. */
    ObjType type; /** The type of the object. */
    bool isReachable; /** Whether or not the object is reachable or could be collected. */
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
typedef struct BoolObj {
    Obj obj;
    bool boolean;
} BoolObj;

/** Represents just a bare null value. */
typedef struct NullObj {
    Obj obj;
} NullObj;

/** Represents a string object. */
typedef struct StringObj {
    Obj obj;
    u32 length;
    u32 hash;
    char *string;
} StringObj;

/** Holds a range of integers to mostly iterate over from start to end with each iteration step. */
typedef struct {
    Obj obj;
    ZmxInt start;
    ZmxInt end;
    ZmxInt step;
} RangeObj;

/** An object which holds multiple other objects in order. */
typedef struct {
    Obj obj;
    ObjArray items;
} ListObj;

/** A map, which holds a hash table of key-value pairs. */
typedef struct {
    Obj obj;
    Table table;
} MapObj;

/** 
 * An enum which has its name, and holds different names inside it in some ordering.
 * 
 * It's implemented by having the enums own name, and a mapping of strings that represent the text
 * of each enum, and a corresponding value in the map which has the index where the enum member
 * object itself is located in the members array.
 * 
 * This formation is important as it allows us the speed of table lookups but also ensures members
 * are ordered in an array for optimized iteration in loops.
 */
typedef struct {
    Obj obj;
    StringObj *name; /** Name of the created enum itself. */
    ObjArray members; /** All the member objects in an ordered array of their indices. */
    Table lookupTable; /** Multiple strings of name keys and integer values to index members. */
} EnumObj;

/** 
 * Represents one enum value in an enum. */
typedef struct {
    Obj obj;
    EnumObj *enumObj; /** The enum which holds this member. */
    StringObj *name; /** The textual string of the member used to access it. */
    ZmxInt index; /** The index on the array of members it's on. */
} EnumMemberObj;

/**
 * Represents an object that holds some bytecode, its positions and a constant pool for them.
 * 
 * It's a function object because it's a function/method most of the time.
 * However, this is also used for the top-level bytecode and constant pool
 * to achieve uniformity across the VM.
 */
typedef struct {
    Obj obj;

    /** Name of the function. It gets a special name if it's the top-level code's function. */
    StringObj *name;

    /** Stores the emitted bytecode of the function. */
    ByteArray bytecode;

    /** Stores a position for each bytecode instruction. Useful when we error later at runtime. */
    SourcePositionArray positions;

    /** The pool which we store constant objects inside of (objects created at compile time). */
    ObjArray constPool;

    /** How many arguments the function takes when being called. */
    u32 arity;

    /**
     * The index in the const pool which the function is located at. Used for runtime errors,
     * since we don't track bytecode by default unless we error, and then recompile to track it.
     */
    int constIdx;

    /** Whether this function can have closure objects inheriting it with a closure context. */
    bool isClosure;
} FuncObj;

/** An object that simply stores a reference to another object. */
typedef struct {
    Obj obj;
    bool isOpen; /** Whether or not the original variable is still alive on the stack. */
    u32 stackLocation; /** Index of the original object getting captured on the VM's stack. */
    Obj *captured; /** The final object of the local variable which was captured. */
} CapturedObj;

/** An array of captured objects. */
DECLARE_DA_STRUCT(CapturedObjArray, CapturedObj *);

/** 
 * A closure which wraps over a function with a context of some captured variables. Not an object
 * 
 * Each function that has captured variables will need to create its own "closure" context
 * each time its creation is encountered in the VM,
 * whereas at compilation we just compile things like its params and bytecode once.
 * 
 * This allows us to create different closure each time we re-visit the code which creates
 * the closure function. This essentially simulates creating a new function at runtime
 * every time we encounter, instead of the one function created at compilation time.
 * 
 * This is not a real object itself, but merely an extension of the function object which is made
 * when necessary. We just call it object for uniformity and treat it similiarly in some places,
 * but it doesn't have its own type in the object type enum.
 */
typedef struct {
    FuncObj func;

    /** Whether this closure represents the top-level code, or any other closure. */
    bool isToplevel;

    /** An array of captured objects that store indirect references to variables closed over. */
    CapturedObjArray captures;
} ClosureObj;

/** Represents a class, which has a name, initializer, and methods. */
typedef struct {
    Obj obj;
    StringObj *name;
    FuncObj *init;
    Table methods;
} ClassObj;

/** Holds the original class, and its own fields which can change at runtime. */
typedef struct {
    Obj obj;
    ClassObj *cls;
    Table fields;
} InstanceObj;

/** A method with the instance tied to it (in order to access "this" inside the method). */
typedef struct {
    Obj obj;
    InstanceObj *instance;
    FuncObj *func;
} MethodObj;

/** A built-in Zymux function whose implementation is written in C. */
typedef struct {
    Obj obj;
    NativeFunc func;
    StringObj *name;
} NativeFuncObj;

/** An iterator, which holds an iterable object which it iterates over its elements 1 by 1. */
typedef struct {
    Obj obj;
    Obj *iterable; /** Object being iterated over. Must be an object that can be iterated on.*/
    u32 iteration; /** The n-th iteration of its elements we're on. */
} IteratorObj;

/** Returns a new allocated integer object. */
IntObj *new_int_obj(ZmxProgram *program, const ZmxInt number);

/** Returns a new allocated float object. */
FloatObj *new_float_obj(ZmxProgram *program, const ZmxFloat number);

/** Returns an interned boolean object depending on the passed bool. */
BoolObj *new_bool_obj(ZmxProgram *program, const bool boolean);

/** Returns an interned null object. */
NullObj *new_null_obj(ZmxProgram *program);

/** Returns a new allocated string object. */
StringObj *new_string_obj(ZmxProgram *program, const char *string, const u32 length);

/** Returns a range object created from the passed numbers. */
RangeObj *new_range_obj(
    ZmxProgram *program, const ZmxInt start, const ZmxInt end, const ZmxInt step
);

/** Returns a list, which encapsulates an array of objects that is ordered. */
ListObj *new_list_obj(ZmxProgram *program, const ObjArray items);

/** Returns an object which holds a hash table of key-value pairs. */
MapObj *new_map_obj(ZmxProgram *program, const Table table);

/**
 * Returns one member of an enum.
 * 
 * Holds the original enum it's a member of, its own member textual representation as a name,
 * and its index inside the enum.
 */
EnumMemberObj *new_enum_member_obj(
    ZmxProgram *program, EnumObj *enumObj, StringObj *name, const ZmxInt index
);

/** Returns an enum, which holds a bunch of names in order. */
EnumObj *new_enum_obj(ZmxProgram *program, StringObj *name);

/** Returns a new allocated function object. */
FuncObj *new_func_obj(ZmxProgram *program, StringObj *name, const u32 arity, const int constIdx);

/** Returns a newely created indirect reference to the passed object (capturing the object). */
CapturedObj *new_captured_obj(ZmxProgram *program, Obj *captured, const u32 stackLocation);

/** 
 * Returns a new closure, which is merely an extended function that has a closure context in it.
 * 
 * This is because closures aren't "objects" but instead a function which has a closure context
 * extension allocated, and that extension can be revealed by converting to the closure type.
 */
ClosureObj *new_closure_obj(ZmxProgram *program, FuncObj *closedFunc, const bool isToplevel);

/** Returns a bare-bones class with only a name. Other information is added later. */
ClassObj *new_class_obj(ZmxProgram *program, StringObj *name);

/** Returns a separate instance of the class, which has its own fields. */
InstanceObj *new_instance_obj(ZmxProgram *program, ClassObj *cls);

/** Returns a method of a class, which is tied to an instance of it (to access its properties). */
MethodObj *new_method_obj(ZmxProgram *program, InstanceObj *instance, FuncObj *func);

/** Returns a new allocated native function object. */
NativeFuncObj *new_native_func_obj(ZmxProgram *program, NativeFunc func, StringObj *name);

/** Returns an iterator which wraps around an iterable object being iterated on. */
IteratorObj *new_iterator_obj(ZmxProgram *program, Obj *iterable);

/** Allocates some objects for interning and puts them in the passed program. */
void intern_objs(ZmxProgram *program);

/** Returns whether or not the passed object is something that can be iterated over. */
bool is_iterable(const Obj *object);

/** 
 * Iterates over an object that is assumed to be iterable.
 * Returns the iterated element of the iterable, or NULL if the iterator's exhausted.
 * 
 * Passing a non-iterable is considered unreachable.
 */
Obj *iterate(ZmxProgram *program, IteratorObj *iterator);

/** Returns whether or not 2 objects are considered equal. */
bool equal_obj(const Obj *left, const Obj *right);

/** Returns a new string object that is formed from concatenating left with right. */
StringObj *concatenate(ZmxProgram *program, const StringObj *left, const StringObj *right);

/** Returns a copy of the passed object as an integer. Returns NULL if conversion failed. */
IntObj *as_int(ZmxProgram *program, const Obj *object);

/** Returns a copy of the passed object as a float. Returns NULL if conversion failed. */
FloatObj *as_float(ZmxProgram *program, const Obj *object);

/** Returns a copy of the passed object's boolean (whether the object is "truthy" or "falsy"). */
BoolObj *as_bool(ZmxProgram *program, const Obj *object);

/** Returns a copy of the passed object as a string. */
StringObj *as_string(ZmxProgram *program, const Obj *object);

/** 
 * Prints the passed object to the console.
 * If debugPrint is on, it prints some extra things to make the output clearer for debugging.
 */
void print_obj(const Obj *object, const bool debugPrint);

/** Returns a readable C string from the passed object type enum. */
char *obj_type_str(ObjType type);

/** Frees the contents of the passed object. */
void free_obj(Obj *object);

/** Frees all allocated objects stored in the passed program. */
void free_all_objs(ZmxProgram *program);

#endif
