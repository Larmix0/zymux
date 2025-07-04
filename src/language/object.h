#ifndef OBJECT_H
#define OBJECT_H

#include <stdbool.h>
#include <stdio.h>

#include "built_in.h"
#include "constants.h"
#include "dynamic_array.h"
#include "file.h"
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
typedef struct ClassObj ClassObj;

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
    OBJ_ITERATOR,
    OBJ_MODULE,
    OBJ_FILE,
    OBJ_FUNC,
    OBJ_CAPTURED,
    OBJ_CLASS,
    OBJ_INSTANCE,
    OBJ_METHOD,
    OBJ_NATIVE_FUNC,
    OBJ_NATIVE_METHOD
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

/** An iterator, which holds an iterable object which it iterates over its elements 1 by 1. */
typedef struct {
    Obj obj;
    Obj *iterable; /** Object being iterated over. Must be an object that can be iterated on.*/
    u32 iteration; /** The n-th iteration of its elements we're on. */
} IteratorObj;

/** Represents an imported module's visible information. */
typedef struct {
    Obj obj;
    StringObj *path; /** The file path of this module. */
    Table globals; /** Globals which can be accessed by other modules (doesn't include privates). */
} ModuleObj;

/** Represents an opened file at runtime. */
typedef struct {
    Obj obj;
    FILE *stream; /** The internally opened file pointer in C, which is a stream. */
    FileMode mode; /** The mode the file is open in (such as read, or write+, etc.). */
    StringObj *path; /** The string path of this file. */
    bool isOpen; /** Stays true until the stream is closed. */
    Table fields;
} FileObj;

/** Represents one enum value in an enum. */
typedef struct {
    Obj obj;
    EnumObj *enumObj; /** The enum which holds this member. */
    StringObj *name; /** The textual string of the member used to access it. */
    ZmxInt index; /** The index on the array of members it's on. */
} EnumMemberObj;

/** Parameters for any kind of function object (user, native, etc.). Not an object. */
typedef struct {
    /** The minimum amount of arguments to provide this function (excludes optional arguments). */
    u32 minArity;

    /** The maximum amount of arguments that this function takes (includes optional arguments). */
    u32 maxArity;

    /** An ordered array of the string names of the function's optional parameters. */
    ObjArray names;

    /** An ordered array of the default value for each one of the function's optional parameters. */
    ObjArray values;
} FuncParams;

/**
 * Represents an object that holds some bytecode, its positions and a constant pool for them.
 * 
 * It's a function object because it's a function/method most of the time.
 * However, this is also used for the top-level's bytecode to achieve uniformity across the VM.
 * 
 * This only stored static information, whereas runtime-only information that is specific to each
 * function creation (like optional values and captured variables) should be handled in a different,
 * runtime function object struct.
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

    /** The class this function is tied to if it's a method. NULL if it's not a method. */
    ClassObj *cls;

    /** The name of the module which this function was declared inside of. */
    StringObj *module;

    /** Whether this represents top-level code of a module, or any other func inside a module. */
    bool isToplevel;

    /** 
     * Static information about the parameters of the function.
     * 
     * All values in these parameters are C-NULLs, even the optional values,
     * as those should be created separately in a runtime func object, since each object
     * keeps track of its own runtime optional values whenever one is created.
     */
    FuncParams params;

    /** Whether or not this has runtime optional values. */
    bool hasOptionals;

    /** Whether this function can have closure objects inheriting it with a closure context. */
    bool isClosure;
} FuncObj;

/** An object that simply stores a reference to another object. */
typedef struct {
    Obj obj;
    bool isOpen; /** Whether or not the original variable is still alive on the stack. */
    u32 stackIdx; /** Index of the original object getting captured on the VM's stack. */
    Obj *captured; /** The final object of the local variable which was captured. */
} CapturedObj;

/** 
 * A runtime-created function which wraps over a function with static info. Not an object by itself.
 * 
 * Each function that has captured variables or optional values will need to create
 * its own context of closed variables or optional values each time its creation is encountered
 * in the VM, whereas at compilation we just compile things like its bytecode and const pool once.
 * 
 * This allows us to create a different set of closed variables or default param values each time
 * we re-visit the code which creates the function.
 * 
 * This is not a real object by itself, but merely an extension of the function object which is made
 * when necessary. We just call it object for uniformity,
 * but it doesn't have its own type in the object type enum, and doesn't store its
 * own base Obj struct, but uses the static func's instead, which allows us to convert and use this
 * and normal/static functions in the same places.
 */
typedef struct {
    FuncObj func;

    /** The runtime value of each parameter of the function. NULL for each mandatory param first. */
    ObjArray paramVals;

    /** An array of captured objects that store indirect references to variables closed over. */
    ObjArray captures;
} RuntimeFuncObj;

/** Represents a class, which has a name, initializer, and methods. */
typedef struct ClassObj {
    Obj obj;
    StringObj *name;
    ClassObj *superclass;
    FuncObj *init;
    Table methods;

    bool isAbstract; /** Affects whether or not this is treated as a normal or abstract class. */
    ObjArray abstractMethods; /** An array of abstract method names (string objects). */
} ClassObj;

/** Holds the original class, and its own fields which can change at runtime. */
typedef struct {
    Obj obj;
    ClassObj *cls;
    Table fields;
} InstanceObj;

/** 
 * A method with the instance tied to it (in order to access "this" inside the method).
 * 
 * This is created on the runtime when the "method" (which is a function inside the class)
 * is called, which creates this object that actually ties the function inside the class
 * to the instance.
 */
typedef struct {
    Obj obj;
    InstanceObj *instance;
    FuncObj *func;
} MethodObj;

/** A built-in function whose implementation is written in C. */
typedef struct {
    Obj obj;
    StringObj *name; /** Its name in the Zymux program. */
    NativeFunc func; /** The C-callable function pointer with the native function signature. */
    FuncParams params; /** Parameters of the native function. */
} NativeFuncObj;

/** 
 * A built-in method, which is a built-in func bound to a built-in object as an instance.
 * 
 * Just like normal methods this only gets created at runtime to bind the instance to
 * the built-in function inside the built-in class
 */
typedef struct {
    Obj obj;
    NativeFuncObj *func; /** The base native function object without any method binding. */
    ClassObj *cls; /** The built-in class this method belongs to. */
    Obj *instance; /** The built-in object itself acts as the instance where fields are accessed. */
} NativeMethodObj;

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

/** Returns an iterator which wraps around an iterable object being iterated on. */
IteratorObj *new_iterator_obj(ZmxProgram *program, Obj *iterable);

/** 
 * Returns an imported module as an object.
 * 
 * Doesn't take responsibility of freeing the passed globals table, as it only copies them.
 */
ModuleObj *new_module_obj(ZmxProgram *program, StringObj *path, const Table globals);

/** 
 * Returns a new file object representing an opened file.
 * 
 * This function expects an already validated file stream and mode.
 */
FileObj *new_file_obj(ZmxProgram *program, FILE *stream, StringObj *path, StringObj *modeStr);

/** 
 * Returns a new allocated function object.
 * 
 * The class a function should be added later while turning the function into a method in the VM,
 * meanwhile every function must be declared inside some module, so it's mandatory to provide it.
 */
FuncObj *new_func_obj(
    ZmxProgram *program, StringObj *name, const u32 minArity, const u32 maxArity,
    StringObj *module, const bool isTopLevel
);

/** Returns a newly created indirect reference to the passed object (capturing the object). */
CapturedObj *new_captured_obj(ZmxProgram *program, Obj *captured, const u32 stackIdx);

/** Returns a func which has some runtime values that might be independent from the static func. */
RuntimeFuncObj *new_runtime_func_obj(
    ZmxProgram *program, FuncObj *func, const bool hasOptionals, const bool isClosure
);

/** Returns a bare-bones class with a name. Other information is added later. */
ClassObj *new_class_obj(ZmxProgram *program, StringObj *name, const bool isAbstract);

/** Returns a separate instance of the class, which has its own fields. */
InstanceObj *new_instance_obj(ZmxProgram *program, ClassObj *cls);

/** Returns a method of a class, which is tied to an instance of it (to access its properties). */
MethodObj *new_method_obj(ZmxProgram *program, InstanceObj *instance, FuncObj *func);

/** Returns a new allocated native function object. */
NativeFuncObj *new_native_func_obj(
    ZmxProgram *program, StringObj *name, NativeFunc func, const FuncParams params
);

/** Returns a method of a built-in class, which is tied to a built-in object as an instance. */
NativeMethodObj *new_native_method_obj(
    ZmxProgram *program, Obj *instance, ClassObj *cls, NativeFuncObj *func
);

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
