#ifndef OBJECT_H
#define OBJECT_H

#include <stdbool.h>
#include <stdio.h>

#include "built_in.h"
#include "constants.h"
#include "dynamic_array.h"
#include "emitter.h"
#include "file.h"
#include "hash_table.h"
#include "thread.h"

/** Converts anything that "inherits" from object to its type punning base form. */
#define AS_OBJ(object) ((Obj *)object)

/** Appends an element onto the vulnerable's unrooted objects array. */
#define PUSH_UNROOTED(vulnObjs, obj) PUSH_DA(&(vulnObjs)->unrooted, AS_OBJ(obj))

/** Pops an element off of a vulnerable's unrooted objects array. */
#define DROP_UNROOTED(vulnObjs) DROP_DA(&(vulnObjs)->unrooted)

/** 
 * Same as the normal drop function, but the name self-documents that it's an optimization.
 * 
 * This is optionally to be used in places where dropping an unrooted object is not actually
 * mandatory, but is an optimization, like when creating lots of temporary objects at runtime,
 * but not wanting to wait till the next instruction before the thread's unrooted array
 * is emptied.
 * 
 * In other words, this is a self-documenting name for object pops in unrooted arrays
 * that indicates it's only a memory optimization call, not that
 * it would've otherwise never been popped.
 */
#define OPT_DROP_UNROOTED(vulnObjs) DROP_UNROOTED(vulnObjs)

/** Drops all the vulnerable's unrooted. Used when we know vulnerables should be empty. */
#define DROP_ALL_UNROOTED(vulnObjs) ((vulnObjs)->unrooted.length = 0)

/** Puts the passed vulnerable object in the protection pool. */
#define PUSH_PROTECTED(vulnObjs, obj) PUSH_DA(&(vulnObjs)->protected, AS_OBJ(obj))

/** Drops the topmost object in the protection pool. */
#define DROP_PROTECTED(vulnObjs) DROP_DA(&(vulnObjs)->protected)

/** Drops multiple objects in the protection pool. */
#define DROP_AMOUNT_PROTECTED(vulnObjs, amount) DROP_AMOUNT_DA(&(vulnObjs)->protected, amount)

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

#define OBJ_REACHABLE_BIT (0x01) /** First bit. Means object is marked as reachable in GC. */
#define OBJ_INITIALIZED_BIT (0x02) /** Second bit. Means object has all its field initialized. */

#define FUNC_RUNTIME_BIT (0x01) /** First bit. Represents any func with runtimem extension. */
#define FUNC_TOP_LEVEL_BIT (0x02) /** Second bit. Reserved for top-level code "functions". */
#define FUNC_CLOSURE_BIT (0x04) /** Third bit. Whether or not this stores a closure context. */
#define FUNC_OPTIONALS_BIT (0x08) /** Fourth bit. Whether or not the func has optional params. */

#define FLAG_IS_SET(flags, bit) (((flags) & (bit)) != 0) /** Checks if a passed flag is on. */
#define FLAG_ENABLE(flags, bit) ((flags) |= (bit)) /** Sets a passed flag as true. */
#define FLAG_DISABLE(flags, bit) ((flags) &= ~(bit)) /** Sets a passed flag as false. */

typedef struct ZmxProgram ZmxProgram;
typedef struct Obj Obj;
typedef struct ClassObj ClassObj;
typedef struct ModuleObj ModuleObj;
typedef struct FuncObj FuncObj;

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
    OBJ_THREAD,
    OBJ_LOCK,
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
    u8 flags; /** The boolean flags, currently an "is reachable" and "is initialized" booleans. */
    Obj *next; /** Linked list of all objects to make sure we can always access all of them. */
    ObjType type; /** The type of the object. */
} Obj;

/** Holds an array of objects, which is an array of pointers to them. */
DECLARE_DA_STRUCT(ObjArray, Obj *);

/** An array of source positions for storing debugging information. */
DECLARE_DA_STRUCT(SourcePositionArray, SourcePosition);

/** 
 * Holds all kind of objects considered "vulnerable" (might not be rooted) in a program.
 * 
 * Protects them from GC till they're in a root or are unneeded. This is assumed to be inside
 * a root itself (such as inside a VM thread).
 */
typedef struct VulnerableObjs {
    ZmxProgram *program; /** The program for convenience during allocation. */
    ObjArray unrooted; /** The array of objects as a temporary root. */
    ObjArray protected; /** An array of manually appended and popped vulnerable objects. */
} VulnerableObjs;

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

/** Represents one enum value in an enum. */
typedef struct {
    Obj obj;
    EnumObj *enumObj; /** The enum which holds this member. */
    StringObj *name; /** The textual string of the member used to access it. */
    ZmxInt index; /** The index on the array of members it's on. */
} EnumMemberObj;

/** An iterator, which holds an iterable object which it iterates over its elements 1 by 1. */
typedef struct {
    Obj obj;
    Obj *iterable; /** Object being iterated over. Must be an object that can be iterated on.*/
    u32 iteration; /** The n-th iteration of its elements we're on. */
} IteratorObj;

/** Represents an imported module's visible information. */
typedef struct ModuleObj {
    Obj obj;
    StringObj *path; /** The file path of this module. */
    ModuleObj *importedBy; /** The module which imported this. NULL if it's the first module. */
    Table globalsUnsafe; /** Globals which can be accessed by other modules. Not thread-safe. */
    Mutex globalsLock; /** The mutex key used for threads to synchronize accessing globals. */

    /** 
     * The ID of this specific module object so it can be identified in the VM module table.
     * 
     * Each module has its own ID because the same file may be imported multiple times,
     * but each time needs its own module object, therefore the ID is a way for us to know
     * the exact module imported.
     */
    u32 id;
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

/** 
 * Represents some information of the VM that needs to be reverted to when an error is caught.
 * 
 * Only stores the amount of locals/captures and not their arrays themselves, as any variables
 * modified inside the try-catch should stay modified, the only thing we revert is removing
 * new variables created inside the try-catch.
 */
typedef struct {
    FuncObj *func; /** Function that has the try-catch block. */
    u8 *ip; /** Points to the first instruction in the catch block of func. */

    u32 localsAmount; /** Amount of locals in the VM's stack when this was created. */
    u32 capturesAmount; /** 0 if func isn't a closure to begin with. */
    u32 openCapturesAmount; /** The amount of captures not closed at the time of the try-catch. */
    Table openIndicesSet; /** The original integer hash set of open captures in the stack. */

    u32 frameAmount; /** The amount of frames the VM had when this was created. */
    ModuleObj *module; /** The module which this try-catch started on. */
    bool saveError; /** Whether or not the error message should be stored as a local variable. */
} CatchState;

/** An array of catch states for being inside try-catch statements. */
DECLARE_DA_STRUCT(CatchStateArray, CatchState);

/** Holds a frame around of information around a currently executing function object. */
typedef struct {
    FuncObj *func; /** Current executing function. */
    u8 *ip; /** Instruction pointer. Points to the byte that should be read next. */
    Obj **bp; /** Base pointer. Points to the first object in the stack frame. */
    Obj **sp; /** Stack pointer. Points to the topmost object in the stack + 1. */
} StackFrame;

/** An array of frames that hold each function call's information at runtime. */
DECLARE_DA_STRUCT(CallStack, StackFrame);

/** 
 * Represents a thread as an object.
 * 
 * Any execution of bytecode in the language is done under a thread, and a thread has both
 * its own information alongside some shared information in the VM.
 * Local context, such as the stack, catch statement points, the call stack are all given
 * to the thread. Each thread also has its own lock so it can have its state check or modified
 * synchronously.
 */
typedef struct ThreadObj {
    Obj obj;
    Vm *vm; /** The virtual machine this is created on. */
    VulnerableObjs vulnObjs; /** Where objects are placed until they reach a proper root. */

    ThreadHandle native; /** The native OS thread in C which is actually running. */
    Mutex threadLock; /** The mutex to access the native thread handle, state, etc. */
    u32 id; /** The ID of this specific thread so it can be identified in the VM thread table. */
    ModuleObj *module; /** The module context this thread is on (like the globals and name). */
    Obj *runnable; /** The first function the thread enters with executing. */
    ThreadState stateUnsafe; /** The current state of execution for the thread. Not thread-safe. */

    bool isMain; /** Whether or not it is the main executing thread the program ran with. */
    bool isDaemon; /** Whether this is a background thread (is daemon), or should be joined. */
    bool isJoinedUnsafe; /** Whether or not this thread has been joined. Not thread-safe. */

    InstrSize instrSize; /** The size of the number instruction to be read after the opcode. */
    ObjArray openCaptures; /** Captures whose locals are still alive on the stack. */
    Table openIndicesSet; /** Stack indices set for open capture objects for O(1) access. */
    CatchStateArray catches; /** Saved states for caught errors where last item is closest catch. */
    CallStack callStack; /** Holds the entire call stack of stack frames during runtime. */
    StackFrame *frame; /** The current frame which holds the bytecode we're executing. */
    Table fields; /** All the fields of the thread instance. */

    /** 
     * The stack that keeps track of objects while executing.
     * 
     * This doesn't use the default dynamic array, but a special array with the same fields.
     * The reason is because we have to change everything that points somewhere in the stack if an
     * append's reallocation causes the stack's address to move.
     * So the default reallocation used in normal dynamic arrays won't cut it.
     */
    struct {
        Obj **objects;
        u32 capacity;
        u32 length;
    } stack;

    /** Locks the stack to access captures because they're potentially shared between threads. */
    Mutex capturesLock;
} ThreadObj;

/** Represents a lock object for synchronizing a thread object. */
typedef struct {
    Obj obj;
    Mutex lock; /** The actual native mutex lock of the lock object. */
    bool isHeld; /** Whether or not the lock is currently in use by a thread. */
} LockObj;

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
typedef struct FuncObj {
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

    /** 
     * Static information about the parameters of the function.
     * 
     * All values in these parameters are C-NULLs, even the optional values,
     * as those should be created separately in a runtime func object, since each object
     * keeps track of its own runtime optional values whenever one is created.
     */
    FuncParams staticParams;

    /** 
     * All the boolean flags of this function.
     * 
     * This includes (in order) "is runtime", "is top level", "is closure", and "has optionals".
     */
    u8 flags;
} FuncObj;

/** An object that simply stores a reference to another object. */
typedef struct {
    Obj obj;
    bool isOpen; /** Whether or not the original variable is still alive on the stack. */
    ThreadObj *threadUnsafe; /** Thread that created this object. Must be accessed with mutex. */
    u32 stackIdx; /** Index of the original object getting captured on the origin thread's stack. */
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
    /** The copy of the statically compiled function. */
    FuncObj func;

    /** The module object this executed under at runtime. */
    ModuleObj *module;

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

/** 
 * Returns an empty vulnerable to place objects that should be manually tracked by the GC.
 * 
 * This must be placed inside some actual root in order to have all its objects tracked by the GC.
 */
VulnerableObjs create_vulnerables(ZmxProgram *program);

/** Frees all memory owned by the vulnerable objects struct. */
void free_vulnerables(VulnerableObjs *vulnObjs);

/** Returns a new allocated integer object. */
IntObj *new_int_obj(VulnerableObjs *vulnObjs, const ZmxInt number);

/** Returns a new allocated float object. */
FloatObj *new_float_obj(VulnerableObjs *vulnObjs, const ZmxFloat number);

/** Returns an interned boolean object depending on the passed bool. */
BoolObj *new_bool_obj(VulnerableObjs *vulnObjs, const bool boolean);

/** Returns an interned null object. */
NullObj *new_null_obj(VulnerableObjs *vulnObjs);

/** Returns a new allocated string object. */
StringObj *new_string_obj(VulnerableObjs *vulnObjs, const char *string, const u32 length);

/** Returns a range object created from the passed numbers. */
RangeObj *new_range_obj(
    VulnerableObjs *vulnObjs, const ZmxInt start, const ZmxInt end, const ZmxInt step
);

/** Returns a list, which encapsulates an array of objects that is ordered. */
ListObj *new_list_obj(VulnerableObjs *vulnObjs, const ObjArray items);

/** Returns an object which holds a hash table of key-value pairs. */
MapObj *new_map_obj(VulnerableObjs *vulnObjs, const Table table);

/**
 * Returns one member of an enum.
 * 
 * Holds the original enum it's a member of, its own member textual representation as a name,
 * and its index inside the enum.
 */
EnumMemberObj *new_enum_member_obj(
    VulnerableObjs *vulnObjs, EnumObj *enumObj, StringObj *name, const ZmxInt index
);

/** Returns an enum, which holds a bunch of names in order. */
EnumObj *new_enum_obj(VulnerableObjs *vulnObjs, StringObj *name);

/** Returns an iterator which wraps around an iterable object being iterated on. */
IteratorObj *new_iterator_obj(VulnerableObjs *vulnObjs, Obj *iterable);

/** Returns an imported module as an object (without setting any names inside it). */
ModuleObj *new_module_obj(
    VulnerableObjs *vulnObjs, StringObj *path, ModuleObj *importedBy, const u32 id
);

/** 
 * Returns a new file object representing an opened file.
 * 
 * This function expects an already validated file stream and mode.
 */
FileObj *new_file_obj(VulnerableObjs *vulnObjs, FILE *stream, StringObj *path, StringObj *modeStr);

/** Returns an initialized thread object (but doesn't actually execute it). */
ThreadObj *new_thread_obj(
    VulnerableObjs *vulnObjs, Vm *vm, Obj *runnable, ModuleObj *module,
    const u32 id, const bool isMain, const bool isDaemon
);

/** Returns an initialized mutex lock object for threads. */
LockObj *new_lock_obj(VulnerableObjs *vulnObjs);

/** 
 * Returns a new allocated function object.
 * 
 * The class a function should be added later while turning the function into a method in the VM.
 */
FuncObj *new_func_obj(
    VulnerableObjs *vulnObjs, StringObj *name, const u32 minArity, const u32 maxArity,
    const bool isTopLevel
);

/** Returns a newly created indirect reference to the passed object (capturing the object). */
CapturedObj *new_captured_obj(
    VulnerableObjs *vulnObjs, Obj *captured, ThreadObj *threadUnsafe, const u32 stackIdx
);

/** Returns a func which has some runtime values that might be independent from the static func. */
RuntimeFuncObj *new_runtime_func_obj(VulnerableObjs *vulnObjs, FuncObj *func, ModuleObj *module);

/** Returns a bare-bones class with a name. Other information is added later. */
ClassObj *new_class_obj(VulnerableObjs *vulnObjs, StringObj *name, const bool isAbstract);

/** Returns a separate instance of the class, which has its own fields. */
InstanceObj *new_instance_obj(VulnerableObjs *vulnObjs, ClassObj *cls);

/** Returns a method of a class, which is tied to an instance of it (to access its properties). */
MethodObj *new_method_obj(VulnerableObjs *vulnObjs, InstanceObj *instance, FuncObj *func);

/** Returns a new allocated native function object. */
NativeFuncObj *new_native_func_obj(
    VulnerableObjs *vulnObjs, StringObj *name, NativeFunc func, const FuncParams params
);

/** Returns a method of a built-in class, which is tied to a built-in object as an instance. */
NativeMethodObj *new_native_method_obj(
    VulnerableObjs *vulnObjs, Obj *instance, ClassObj *cls, NativeFuncObj *func
);

/** Allocates some objects for interning and puts them in the passed program. */
void intern_objs(VulnerableObjs *vulnObjs);

/** Returns whether or not the passed object is something that can be iterated over. */
bool is_iterable(const Obj *object);

/** 
 * Iterates over an object that is assumed to be iterable.
 * 
 * Returns the iterated element of the iterable, or NULL if the iterator's exhausted.
 * Passing a non-iterable is considered unreachable.
 * 
 * The boolean address is an optional boolean to be set which indicates whether this function
 * has allocated a new object, or reused one already in the iterator.
 * Passing NULL is perfectly valid for cases where that boolean is not needed.
 */
Obj *iterate(VulnerableObjs *vulnObjs, IteratorObj *iterator, bool *hasAllocated);

/** Returns a new string object that is formed from concatenating left with right. */
StringObj *concatenate(VulnerableObjs *vulnObjs, const StringObj *left, const StringObj *right);

/** Safely retrieves the state of the passed thread object. */
ThreadState get_thread_state(ThreadObj *thread);

/** Safely set a state for the passed thread object. */
void set_thread_state(ThreadObj *thread, const ThreadState newState);

/** Returns whether or not 2 objects are considered equal. */
bool equal_obj(const Obj *left, const Obj *right);

/** Returns a copy of the passed object as an integer. Returns NULL if conversion failed. */
IntObj *as_int(VulnerableObjs *vulnObjs, const Obj *object);

/** Returns a copy of the passed object as a float. Returns NULL if conversion failed. */
FloatObj *as_float(VulnerableObjs *vulnObjs, const Obj *object);

/** Returns a copy of the passed object's boolean (whether the object is "truthy" or "falsy"). */
BoolObj *as_bool(VulnerableObjs *vulnObjs, const Obj *object);

/** Returns a copy of the passed object as a string. */
StringObj *as_string(VulnerableObjs *vulnObjs, const Obj *object);

/** 
 * Prints the passed object to the console.
 * 
 * If debug print is on, it prints some extra things to make the output clearer for debugging.
 * This printing function doesn't use the printing lock for synchronization.
 */
void print_obj(const Obj *object, const bool debugPrint);

/** Returns a readable C string from the passed object type enum. */
char *obj_type_str(ObjType type);

/** Frees the contents of the passed object. */
void free_obj(Obj *object);

/** Frees all allocated objects stored in the passed program. */
void free_all_objs(ZmxProgram *program);

#endif
