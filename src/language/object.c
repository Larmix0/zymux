#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.h"
#include "char_buffer.h"
#include "emitter.h"
#include "lexer.h"
#include "object.h"
#include "program.h"

/** Frees a thread object's stack. */
#define FREE_STACK(thread) (free((thread)->stack.objects))

/** Allocates and returns an object. objType is the enum type, cType is the C struct itself. */
#define NEW_OBJ(vulnObjs, objType, cType) \
    ((cType *)new_obj(vulnObjs, objType, sizeof(cType)))

/** The macro for properly returning out of an object allocator of a specific type. */
#define OBJ_TYPE_ALLOCATOR_RETURN(object) \
    do { \
        FLAG_ENABLE(AS_OBJ(object)->flags, OBJ_INITIALIZED_BIT); \
        return (object); \
    } while (0)


static CharBuffer object_cstring(const Obj *object, const bool debugCString);
void set_thread_state(ThreadObj *thread, const ThreadState newState);
static void create_fields(VulnerableObjs *vulnObjs, Table *fields, const u32 amount, ...);
void print_obj(const Obj *object, const bool debugPrint);
char *obj_type_str(ObjType type);

/** 
 * Returns an empty vulnerable to place objects that should be manually tracked by the GC.
 * 
 * This must be placed inside some actual root in order to have all its objects tracked by the GC.
 */
VulnerableObjs create_vulnerables(ZmxProgram *program) {
    VulnerableObjs vulnObjs = {
        .program = program, .unrooted = CREATE_DA(), .protected = CREATE_DA()
    };
    return vulnObjs;
}

/** Frees all memory owned by the vulnerable objects struct. */
void free_vulnerables(VulnerableObjs *vulnObjs) {
    FREE_DA(&vulnObjs->unrooted);
    FREE_DA(&vulnObjs->protected);
}

/** Allocates a new object of the passed type and size. */
static Obj *new_obj(VulnerableObjs *vulnObjs, const ObjType type, const size_t size) {
    ZmxProgram *program = vulnObjs->program;
    Obj *object = malloc(size);
    if (object == NULL) {
        // GC then retry once more with the macro that automatically errors on allocation failure.
        gc_collect(vulnObjs->program);
        object = ALLOC(size);
    }
#if DEBUG_LOG_GC
    printf("Allocate %p | %s (%zu bytes)\n", (void*)object, obj_type_str(type), size);
#endif

    PUSH_UNROOTED(vulnObjs, object); // Now safe from collection.
    object->flags = 0; // Sets all flags to false by default.
    object->type = type;
    
    mutex_lock(program->gc.lock);
    program->gc.allocated += size;
    bool shouldCollect = program->gc.allocated > program->gc.nextCollection;
    if (shouldCollect) {
        program->gc.nextCollection *= COLLECTION_GROWTH;
    }
    // Insert in the list of all objects so it can start to be collected.
    object->next = program->allObjs;
    program->allObjs = object;
    mutex_unlock(program->gc.lock);

    if (shouldCollect) {
        gc_collect(program);
    }
#if DEBUG_ALWAYS_GC
    gc_collect(program);
#endif
    return object;
}

/** Allocates an int object from the passed integer. */
IntObj *new_int_obj(VulnerableObjs *vulnObjs, const ZmxInt number) {
    IntObj *object = NEW_OBJ(vulnObjs, OBJ_INT, IntObj);
    object->number = number;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns a new allocated float object from the passed floating number. */
FloatObj *new_float_obj(VulnerableObjs *vulnObjs, const ZmxFloat number) {
    FloatObj *object = NEW_OBJ(vulnObjs, OBJ_FLOAT, FloatObj);
    object->number = number;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns an interned boolean object depending on the passed bool. */
BoolObj *new_bool_obj(VulnerableObjs *vulnObjs, const bool boolean) {
    BoolObj *object = NEW_OBJ(vulnObjs, OBJ_BOOL, BoolObj);
    object->boolean = boolean;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns an interned null object. */
NullObj *new_null_obj(VulnerableObjs *vulnObjs) {
    NullObj *object = NEW_OBJ(vulnObjs, OBJ_NULL, NullObj);
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** 
 * Returns a new allocated string object.
 * 
 * This function manually allocates a copy of the passed string, so the responsibility
 * of potentially having to free the passed string is not passed to the string object.
 * 
 * We also use string interning to save memory on identical strings. In the case of a match
 * with an existing, interned string we just return it without manually allocating another one.
 * If that does occur while the GC is in protection mode, we manually protect the interned object
 * as if we called the real allocator and it frozen the created object itself.
 */
StringObj *new_string_obj(VulnerableObjs *vulnObjs, const char *string, const u32 length) {
    ZmxProgram *program = vulnObjs->program;
    const u32 hash = hash_string(string, length);
    Obj *interned = table_get_string_key(&program->internedStrings, string, length, hash);
    if (interned) {
        return AS_PTR(StringObj, interned);
    }

    StringObj *object = NEW_OBJ(vulnObjs, OBJ_STRING, StringObj);
    object->length = length;
    object->hash = hash;
    object->string = ARRAY_ALLOC(object->length + 1, char);
    strncpy(object->string, string, object->length);
    object->string[object->length] = '\0';

    table_set(&program->internedStrings, AS_OBJ(object), AS_OBJ(program->internedNull));
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns a range object created from the passed numbers. */
RangeObj *new_range_obj(
    VulnerableObjs *vulnObjs, const ZmxInt start, const ZmxInt end, const ZmxInt step
) {
    RangeObj *object = NEW_OBJ(vulnObjs, OBJ_RANGE, RangeObj);
    object->start = start;
    object->end = end;
    object->step = step;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns a list, which encapsulates an array of objects that is ordered. */
ListObj *new_list_obj(VulnerableObjs *vulnObjs, const ObjArray items) {
    ListObj *object = NEW_OBJ(vulnObjs, OBJ_LIST, ListObj);
    object->items = items;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns a map, which holds a hash table of key-value pairs. */
MapObj *new_map_obj(VulnerableObjs *vulnObjs, const Table table) {
    MapObj *object = NEW_OBJ(vulnObjs, OBJ_MAP, MapObj);
    object->table = table;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/**
 * Returns one member of an enum.
 * 
 * Holds the original enum it's a member of, its own member textual representation as a name,
 * and its index inside the enum.
 */
EnumMemberObj *new_enum_member_obj(
    VulnerableObjs *vulnObjs, EnumObj *enumObj, StringObj *name, const ZmxInt index
) {
    EnumMemberObj *object = NEW_OBJ(vulnObjs, OBJ_ENUM_MEMBER, EnumMemberObj);
    object->enumObj = enumObj;
    object->name = name;
    object->index = index;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns an enum, which holds a bunch of names in order. */
EnumObj *new_enum_obj(VulnerableObjs *vulnObjs, StringObj *name) {
    EnumObj *object = NEW_OBJ(vulnObjs, OBJ_ENUM, EnumObj);
    object->name = name;

    object->lookupTable = create_table();
    INIT_DA(&object->members);
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns an iterator which wraps around an iterable object being iterated on. */
IteratorObj *new_iterator_obj(VulnerableObjs *vulnObjs, Obj *iterable) {
    IteratorObj *object = NEW_OBJ(vulnObjs, OBJ_ITERATOR, IteratorObj);
    object->iterable = iterable;
    object->iteration = 0;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** 
 * Returns an imported module as an object.
 * 
 * Doesn't take responsibility of freeing the passed globals table, as it only copies them.
 */
ModuleObj *new_module_obj(
    VulnerableObjs *vulnObjs, StringObj *path, ModuleObj *importedBy, const u32 id
) {
    ModuleObj *object = NEW_OBJ(vulnObjs, OBJ_MODULE, ModuleObj);
    object->path = path;
    object->importedBy = importedBy;
    object->globalsUnsafe = create_table();
    init_mutex(&object->globalsLock);

    object->id = id;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** 
 * Returns a new file object representing an opened file.
 * 
 * This function expects an already validated file stream and mode.
 */
FileObj *new_file_obj(VulnerableObjs *vulnObjs, FILE *stream, StringObj *path, StringObj *modeStr) {
    FileObj *object = NEW_OBJ(vulnObjs, OBJ_FILE, FileObj);
    object->stream = stream;
    object->mode = parse_file_mode(modeStr->string, modeStr->length);
    object->path = path;
    object->isOpen = true;

    create_fields(vulnObjs, &object->fields, 2, "path", AS_OBJ(path), "mode", AS_OBJ(modeStr));
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns an initialized thread object (but doesn't actually execute it). */
ThreadObj *new_thread_obj(
    VulnerableObjs *vulnObjs, Vm *vm, Obj *runnable, ModuleObj *module,
    const u32 id, const bool isMain, const bool isDaemon
) {
    ThreadObj *object = NEW_OBJ(vulnObjs, OBJ_THREAD, ThreadObj);
    
    // Mark as not started and leave the native thread handle uninitialized.
    init_mutex(&object->lock);
    set_thread_state(object, THREAD_NOT_STARTED);
    object->runnable = runnable;
    object->vm = vm;
    object->module = module;
    object->id = id;
    object->isMain = isMain;
    object->isDaemon = isDaemon;
    object->isJoinedUnsafe = false;
    create_fields(vulnObjs, &object->fields, 1, "daemon", new_bool_obj(vulnObjs, isDaemon));

    object->vulnObjs = create_vulnerables(vulnObjs->program);
    INIT_DA(&object->catches);
    INIT_DA(&object->openCaptures);
    INIT_DA(&object->callStack);
    object->instrSize = INSTR_ONE_BYTE;
    object->frame = NULL;
    object->stack.objects = NULL;
    object->stack.capacity = 0;
    object->stack.length = 0;

    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns an initialized mutex lock object for threads. */
LockObj *new_lock_obj(VulnerableObjs *vulnObjs) {
    LockObj *object = NEW_OBJ(vulnObjs, OBJ_LOCK, LockObj);
    
    init_mutex(&object->lock);
    object->isHeld = false;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** 
 * Returns a new allocated function object.
 * 
 * The class a function should be added later while turning the function into a method in the VM.
 */
FuncObj *new_func_obj(
    VulnerableObjs *vulnObjs, StringObj *name, const u32 minArity, const u32 maxArity,
    const bool isTopLevel
) {
    FuncObj *object = NEW_OBJ(vulnObjs, OBJ_FUNC, FuncObj);
    object->name = name;
    object->flags = 0;
    if (isTopLevel) {
        FLAG_ENABLE(object->flags, FUNC_TOP_LEVEL_BIT);
    }

    object->cls = NULL;
    object->staticParams.minArity = minArity;
    object->staticParams.maxArity = maxArity;
    INIT_DA(&object->staticParams.names);
    INIT_DA(&object->staticParams.values);

    INIT_DA(&object->bytecode);
    INIT_DA(&object->positions);
    INIT_DA(&object->constPool);
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns a newly created indirect reference to the passed object (capturing the object). */
CapturedObj *new_captured_obj(VulnerableObjs *vulnObjs, Obj *captured, const u32 stackIdx) {
    CapturedObj *object = NEW_OBJ(vulnObjs, OBJ_CAPTURED, CapturedObj);
    object->isOpen = true;
    object->stackIdx = stackIdx;
    object->captured = captured;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** 
 * Returns a func which has some runtime values that might be independent from the static func.
 * 
 * This is because runtime functions aren't "objects", but instead a function
 * which has an extra runtime context extension allocated,
 * and that extension can be revealed by converting to the runtime func type.
 * 
 * We make the runtime function by setting its static func to a copy of the passed static func.
 * This indirect copy (because we use func obj directly instead func obj pointer) means that
 * booleans (like is closure and has optionals) will actually be independent from the original
 * func that still has them set as false, although arrays and pointers will still be shared.
 */
RuntimeFuncObj *new_runtime_func_obj(VulnerableObjs *vulnObjs, FuncObj *func, ModuleObj *module) {
    RuntimeFuncObj *object = NEW_OBJ(vulnObjs, OBJ_FUNC, RuntimeFuncObj);
    Obj base = object->func.obj; // Runtime function's base obj.
    object->func = *func; // Copy.
    object->func.obj = base; // Change the base obj back to the runtime func's base after copying.

    FLAG_ENABLE(object->func.flags, FUNC_RUNTIME_BIT);
    object->module = module;
    INIT_DA(&AS_PTR(RuntimeFuncObj, object)->paramVals);
    INIT_DA(&AS_PTR(RuntimeFuncObj, object)->captures);

    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Creates a bare-bones class with a name. Other information is added later. */
ClassObj *new_class_obj(VulnerableObjs *vulnObjs, StringObj *name, const bool isAbstract) {
    ClassObj *object = NEW_OBJ(vulnObjs, OBJ_CLASS, ClassObj);
    object->name = name;
    object->isAbstract = isAbstract;

    object->superclass = NULL;
    object->init = NULL;
    object->methods = create_table();
    INIT_DA(&object->abstractMethods);
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns a separate instance of the class, which has its own fields. */
InstanceObj *new_instance_obj(VulnerableObjs *vulnObjs, ClassObj *cls) {
    InstanceObj *object = NEW_OBJ(vulnObjs, OBJ_INSTANCE, InstanceObj);
    object->cls = cls;
    object->fields = create_table();
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns a method of a class, which is tied to an instance of it (to access its properties). */
MethodObj *new_method_obj(VulnerableObjs *vulnObjs, InstanceObj *instance, FuncObj *func) {
    MethodObj *object = NEW_OBJ(vulnObjs, OBJ_METHOD, MethodObj);
    object->instance = instance;
    object->func = func;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns a new allocated native function object. */
NativeFuncObj *new_native_func_obj(
    VulnerableObjs *vulnObjs, StringObj *name, NativeFunc func, const FuncParams params
) {
    NativeFuncObj *object = NEW_OBJ(vulnObjs, OBJ_NATIVE_FUNC, NativeFuncObj);
    object->name = name;
    object->func = func;
    object->params = params;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Returns a method of a built-in class, which is tied to a built-in object as an instance. */
NativeMethodObj *new_native_method_obj(
    VulnerableObjs *vulnObjs, Obj *instance, ClassObj *cls, NativeFuncObj *func
) {
    NativeMethodObj *object = NEW_OBJ(vulnObjs, OBJ_NATIVE_METHOD, NativeMethodObj);
    object->instance = instance;
    object->func = func;
    object->cls = cls;
    OBJ_TYPE_ALLOCATOR_RETURN(object);
}

/** Allocates some objects for interning and puts them in the passed program. */
void intern_objs(VulnerableObjs *vulnObjs) {
    ZmxProgram *program = vulnObjs->program;
    program->internedTrue = new_bool_obj(vulnObjs, true);
    program->internedFalse = new_bool_obj(vulnObjs, false);
    program->internedNull = new_null_obj(vulnObjs);
}

/** Appends a C-string representation of the passed list. */
static void list_cstring(const ListObj *list, CharBuffer *string) {
    buffer_append_char(string, '[');
    for (u32 i = 0; i < list->items.length; i++) {
        if (i != 0) {
            buffer_append_string(string, ", ");
        }
        CharBuffer itemBuf = object_cstring(list->items.data[i], true);
        buffer_append_format(string, "%s", itemBuf.text);
        free_char_buffer(&itemBuf);
    }
    buffer_append_char(string, ']');
}

/** Appends a C-string representation of the passed map. */
static void map_cstring(const MapObj *map, CharBuffer *string) {
    buffer_append_char(string, '{');
    bool firstFilled = true;
    for (u32 i = 0; i < map->table.capacity; i++) {
        if (EMPTY_ENTRY(&map->table.entries[i])) {
            continue;
        }
        if (!firstFilled) {
            buffer_append_string(string, ", ");
        } else {
            firstFilled = false;
        }
        CharBuffer keyBuf = object_cstring(map->table.entries[i].key, true);
        CharBuffer valueBuf = object_cstring(map->table.entries[i].value, true);
        buffer_append_format(string, "%s: %s", keyBuf.text, valueBuf.text);
        free_char_buffer(&keyBuf);
        free_char_buffer(&valueBuf);
    }
    buffer_append_char(string, '}');

}

/** 
 * Returns a C-string representation of the object in an allocated char buffer.
 * 
 * The debug string parameter indicates whether this should be returned as a debug string
 * with extra information such as quotes in strings.
 */
static CharBuffer object_cstring(const Obj *object, const bool debugCString) {
    CharBuffer string = create_char_buffer();
    switch (object->type) {
    case OBJ_INT:
        buffer_append_format(&string, ZMX_INT_FMT, AS_PTR(IntObj, object)->number);
        break;
    case OBJ_FLOAT:
        buffer_append_format(&string, ZMX_FLOAT_FMT, AS_PTR(FloatObj, object)->number);
        break;
    case OBJ_BOOL:
        buffer_append_string(&string, AS_PTR(BoolObj, object)->boolean ? "true" : "false");
        break;
    case OBJ_NULL:
        buffer_append_string(&string, "null");
        break;
    case OBJ_STRING:
        if (debugCString) {
            buffer_append_format(&string, "'%s'", AS_PTR(StringObj, object)->string);
        } else {
            buffer_append_string(&string, AS_PTR(StringObj, object)->string);
        }
        break;
    case OBJ_RANGE: {
        RangeObj *range = AS_PTR(RangeObj, object);
        buffer_append_format(
            &string, ZMX_INT_FMT ".." ZMX_INT_FMT ".." ZMX_INT_FMT,
            range->start, range->end, range->step
        );
        break;
    }
    case OBJ_LIST:
        list_cstring(AS_PTR(ListObj, object), &string);
        break;
    case OBJ_MAP:
        map_cstring(AS_PTR(MapObj, object), &string);
        break;
    case OBJ_ENUM:
        buffer_append_format(&string, "<enum %s>", AS_PTR(EnumObj, object)->name->string);
        break;
    case OBJ_ENUM_MEMBER: {
        EnumMemberObj *member = AS_PTR(EnumMemberObj, object);
        buffer_append_format(&string, "%s.%s", member->enumObj->name->string, member->name->string);
        break;
    }
    case OBJ_ITERATOR:
        buffer_append_string(&string, "<iterator>");
        break;
    case OBJ_MODULE:
        buffer_append_format(&string, "<module %s>", AS_PTR(ModuleObj, object)->path->string);
        break;
    case OBJ_FILE:
        buffer_append_format(&string, "<file %s>", AS_PTR(FileObj, object)->path->string);
        break;
    case OBJ_THREAD:
        buffer_append_string(&string, "<thread>");
        break;
    case OBJ_LOCK:
        buffer_append_format(
            &string, "<lock (held=%s)>", AS_PTR(LockObj, object)->isHeld ? "true" : "false"
        );
        break;
    case OBJ_FUNC:
        buffer_append_format(&string, "<function %s>", AS_PTR(FuncObj, object)->name->string);
        break;
    case OBJ_CAPTURED:
        buffer_append_format(&string, "<captured>");
        break;
    case OBJ_CLASS:
        buffer_append_format(&string, "<class %s>", AS_PTR(ClassObj, object)->name->string);
        break;
    case OBJ_INSTANCE:
        buffer_append_format(
            &string, "<instance of %s>", AS_PTR(InstanceObj, object)->cls->name->string
        );
        break;
    case OBJ_METHOD: {
        MethodObj *method = AS_PTR(MethodObj, object);
        buffer_append_format(
            &string, "<instance method %s.%s>",
            method->instance->cls->name->string, method->func->name->string
        );
        break;
    }
    case OBJ_NATIVE_FUNC:
        buffer_append_format(
            &string, "<native function %s>", AS_PTR(NativeFuncObj, object)->name->string
        );
        break;
    case OBJ_NATIVE_METHOD: {
        NativeMethodObj *method = AS_PTR(NativeMethodObj, object);
        buffer_append_format(
            &string, "<instance method %s.%s>",
            method->cls->name->string, method->func->name->string
        );
        break;
    }
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    return string;
}

/** Returns whether or not the passed object is something that can be iterated over. */
bool is_iterable(const Obj *object) {
    switch (object->type) {
    case OBJ_STRING:
    case OBJ_RANGE:
    case OBJ_LIST:
    case OBJ_MAP:
    case OBJ_ENUM:
        return true;

    case OBJ_INT:
    case OBJ_FLOAT:
    case OBJ_BOOL:
    case OBJ_NULL:
    case OBJ_ENUM_MEMBER:
    case OBJ_ITERATOR:
    case OBJ_MODULE:
    case OBJ_FILE:
    case OBJ_THREAD:
    case OBJ_LOCK:
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_NATIVE_METHOD:
        return false;
    }
    UNREACHABLE_ERROR();
}

/** 
 * Iterates over an object that is assumed to be iterable.
 * 
 * Returns the iterated element of the iterable, or NULL if the iterator's exhausted.
 * Passing a non-iterable is considered unreachable.
 * 
 * The boolean address is an optional boolean to be set which indicates whether this function
 * has allocated a new object, or reused one already in the iterator.
 * Passing NULL is valid for cases where that boolean is not needed.
 */
Obj *iterate(VulnerableObjs *vulnObjs, IteratorObj *iterator, bool *hasAllocated) {
#define SET_HAS_ALLOCATED_TO(hasAllocated, toBool) if (hasAllocated) {*(hasAllocated) = (toBool);}

    ASSERT(is_iterable(iterator->iterable), "Attempted to iterate over non-iterable object.");
    SET_HAS_ALLOCATED_TO(hasAllocated, false); // No alloc default (especially for early returns).
    switch (iterator->iterable->type) {
    case OBJ_STRING: {
        StringObj *string = AS_PTR(StringObj, iterator->iterable);
        if (iterator->iteration >= string->length) {
            return NULL;
        }
        SET_HAS_ALLOCATED_TO(hasAllocated, true);
        return AS_OBJ(new_string_obj(vulnObjs, string->string + iterator->iteration++, 1));
    }
    case OBJ_RANGE: {
        RangeObj *range = AS_PTR(RangeObj, iterator->iterable);
        const bool isIncreasing = range->start < range->end;
        const ZmxInt result = isIncreasing ? range->start + (iterator->iteration * range->step)
            : range->start - (iterator->iteration * range->step);
            
        iterator->iteration++;
        if ((isIncreasing && result > range->end) || (!isIncreasing && result < range->end)) {
            return NULL;
        }
        SET_HAS_ALLOCATED_TO(hasAllocated, true);
        return AS_OBJ(new_int_obj(vulnObjs, result));
    }
    case OBJ_LIST: {
        ListObj *list = AS_PTR(ListObj, iterator->iterable);
        if (iterator->iteration >= list->items.length) {
            return NULL;
        }
        return list->items.data[iterator->iteration++];
    }
    case OBJ_MAP: {
        MapObj *map = AS_PTR(MapObj, iterator->iterable);
        if (iterator->iteration >= map->table.count) {
            return NULL;
        }
        iterator->iteration++;
        u32 filledCounter = 0; // How many non-empty entries we have encountered.
        for (u32 i = 0; i < map->table.capacity; i++) {
            if (!EMPTY_ENTRY(&map->table.entries[i])) {
                filledCounter++;
            }
            if (!EMPTY_ENTRY(&map->table.entries[i]) && filledCounter == iterator->iteration) {
                ObjArray keyValue = CREATE_DA();
                PUSH_DA(&keyValue, map->table.entries[i].key);
                PUSH_DA(&keyValue, map->table.entries[i].value);
                SET_HAS_ALLOCATED_TO(hasAllocated, true);
                return AS_OBJ(new_list_obj(vulnObjs, keyValue));
            }
        }
        UNREACHABLE_ERROR();
    }
    case OBJ_ENUM: {
        EnumObj *enumObj = AS_PTR(EnumObj, iterator->iterable);
        if (iterator->iteration >= enumObj->members.length) {
            return NULL;
        }
        return enumObj->members.data[iterator->iteration++];
    }
    case OBJ_INT:
    case OBJ_FLOAT:
    case OBJ_BOOL:
    case OBJ_NULL:
    case OBJ_ENUM_MEMBER:
    case OBJ_ITERATOR:
    case OBJ_MODULE:
    case OBJ_FILE:
    case OBJ_THREAD:
    case OBJ_LOCK:
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_NATIVE_METHOD:
        UNREACHABLE_ERROR();
    }
    UNREACHABLE_ERROR();
#undef SET_HAS_ALLOCATED_TO
}

/** Returns a new string object that is formed from concatenating left with right. */
StringObj *concatenate(VulnerableObjs *vulnObjs, const StringObj *left, const StringObj *right) {
    char *concatenated = ARRAY_ALLOC(left->length + right->length + 1, char);
    strncpy(concatenated, left->string, left->length);
    strncpy(concatenated + left->length, right->string, right->length);
    concatenated[left->length + right->length] = '\0';

    StringObj *result = new_string_obj(vulnObjs, concatenated, left->length + right->length);
    free(concatenated);
    return result;
}

/** Initializes and sets an amount of string and object pairs as fields in the passed fields. */
static void create_fields(VulnerableObjs *vulnObjs, Table *fields, const u32 amount, ...) {
    *fields = create_table();

    va_list args;
    va_start(args, amount);
    for (u32 i = 0; i < amount; i++) {
        char *name = va_arg(args, char *);
        Obj *value = va_arg(args, Obj *);
        table_string_set(vulnObjs, fields, name, value);
    }
    va_end(args);
}

/** Returns whether or not the 2 lists passed are considered equal. */
static bool equal_lists(ListObj *left, ListObj *right) {
    if (left->items.length != right->items.length) {
        return false;
    }
    for (u32 i = 0; i < left->items.length; i++) {
        if (!equal_obj(left->items.data[i], right->items.data[i])) {
            return false;
        }
    }
    return true;
}

/** Returns whether or not the 2 maps passed are considered equal. */
static bool equal_maps(MapObj *left, MapObj *right) {
    if (left->table.count != right->table.count) {
        return false;
    }
    // Must manually check like this because some entries might move during inserts/deletions.
    for (u32 i = 0; i < left->table.capacity; i++) {
        if (EMPTY_ENTRY(&left->table.entries[i])) {
            continue;
        }
        Entry *leftEntry = &left->table.entries[i];
        Entry *rightEntry = table_key_entry(&right->table, left->table.entries[i].key);
        if (
            EMPTY_ENTRY(rightEntry)
            || !equal_obj(leftEntry->key, rightEntry->key)
            || !equal_obj(leftEntry->value, rightEntry->value)
        ) {
            return false;
        }
    }
    return true;
}

/** 
 * Returns whether or not 2 objects are considered equal.
 * 
 * The types must be equal, unless they're numbers (floats or integers), in that case they could
 * be of differing types but still equal (like 2 == 2.0 being true).
 * 
 * This is also why the switch doesn't have to handle integers or floats,
 * as they would be unreachable.
 */
bool equal_obj(const Obj *left, const Obj *right) {
    if (IS_NUM(left) && IS_NUM(right)) {
        return NUM_VAL(left) == NUM_VAL(right);
    }
    if (left->type != right->type) {
        return false;
    }

    switch (left->type) {
    case OBJ_STRING:
    case OBJ_ENUM:
    case OBJ_ITERATOR:
    case OBJ_MODULE:
    case OBJ_FILE:
    case OBJ_THREAD:
    case OBJ_LOCK:
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_NATIVE_METHOD:
        return left == right; // Compare addresses directly.

    case OBJ_BOOL:
        return AS_PTR(BoolObj, left)->boolean == AS_PTR(BoolObj, right)->boolean;
    case OBJ_NULL:
        return true;
    case OBJ_RANGE: {
        RangeObj *leftRange = AS_PTR(RangeObj, left);
        RangeObj *rightRange = AS_PTR(RangeObj, right);
        return leftRange->start == rightRange->start && leftRange->end == rightRange->end
            && leftRange->step == rightRange->step;
    }
    case OBJ_LIST:
        return equal_lists(AS_PTR(ListObj, left), AS_PTR(ListObj, right));
    case OBJ_MAP:
        return equal_maps(AS_PTR(MapObj, left), AS_PTR(MapObj, right));
    case OBJ_ENUM_MEMBER: {
        EnumMemberObj *leftMember = AS_PTR(EnumMemberObj, left);
        EnumMemberObj *rightMember = AS_PTR(EnumMemberObj, right);
        return equal_obj(AS_OBJ(leftMember->enumObj), AS_OBJ(rightMember->enumObj))
            && leftMember->index == rightMember->index;
    }
    case OBJ_INT:
    case OBJ_FLOAT:
        UNREACHABLE_ERROR(); // Ints and floats were handled at the top.
    }
    UNREACHABLE_ERROR();
}

/** Verifies that a string can be converted to an integer or float. */
static bool string_is_num(char *string) {
    if (string[0] == '\0') {
        return false; // Empty string, not a number.
    }
    char current;
    while ((current = *string++)) {
        if (current == '.') {
            if (string[0] == '\0') {
                return false; // Nothing after the float dot, invalid.
            }
            while ((current = *string++)) {
                if (!IS_DIGIT(current)) {
                    return false;   
                }
            }
            return true; // Finished float.
        } else if (!IS_DIGIT(current)) {
            return false;   
        }
    }
    return true; // Finished integer.
}

/** Returns a copy of the passed object as an integer. Returns NULL if conversion failed. */
IntObj *as_int(VulnerableObjs *vulnObjs, const Obj *object) {
    ZmxInt result;
    switch (object->type) {
    case OBJ_INT: result = AS_PTR(IntObj, object)->number; break;
    case OBJ_FLOAT: result = (ZmxInt)(AS_PTR(FloatObj, object)->number); break;
    case OBJ_BOOL: result = (ZmxInt)(AS_PTR(BoolObj, object)->boolean); break;
    case OBJ_STRING: {
        StringObj *str = AS_PTR(StringObj, object);
        if (!string_is_num(str->string)) {
            return NULL;
        }
        result = strtoll(str->string, NULL, 10);
        break;
    }
    case OBJ_ENUM_MEMBER: result = AS_PTR(EnumMemberObj, object)->index; break;

    case OBJ_NULL:
    case OBJ_RANGE:
    case OBJ_LIST:
    case OBJ_MAP:
    case OBJ_ENUM:
    case OBJ_ITERATOR:
    case OBJ_MODULE:
    case OBJ_FILE:
    case OBJ_LOCK:
    case OBJ_THREAD:
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_NATIVE_METHOD:
        // Can't ever convert to integer.
        return NULL;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    return new_int_obj(vulnObjs, result);   
}

/** Returns a copy of the passed object as a float. Returns NULL if conversion failed. */
FloatObj *as_float(VulnerableObjs *vulnObjs, const Obj *object) {
    ZmxFloat result;
    switch (object->type) {
    case OBJ_INT: result = (ZmxFloat)(AS_PTR(IntObj, object)->number); break;
    case OBJ_FLOAT: result = AS_PTR(FloatObj, object)->number; break;
    case OBJ_BOOL: result = (ZmxFloat)(AS_PTR(BoolObj, object)->boolean); break;
    case OBJ_STRING: {
        StringObj *str = AS_PTR(StringObj, object);
        if (!string_is_num(str->string)) {
            return NULL;
        }
        result = strtod(str->string, NULL);
        break;
    }
    case OBJ_ENUM_MEMBER: result = (ZmxFloat)(AS_PTR(EnumMemberObj, object)->index); break;

    case OBJ_NULL:
    case OBJ_RANGE:
    case OBJ_LIST:
    case OBJ_MAP:
    case OBJ_ENUM:
    case OBJ_ITERATOR:
    case OBJ_MODULE:
    case OBJ_FILE:
    case OBJ_THREAD:
    case OBJ_LOCK:
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_NATIVE_METHOD:
        // Can't ever convert to float.
        return NULL;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    return new_float_obj(vulnObjs, result);      
}

/** Returns a copy of the passed object's boolean (whether the object is "truthy" or "falsy"). */
BoolObj *as_bool(VulnerableObjs *vulnObjs, const Obj *object) {
    bool result;
    switch (object->type) {
    case OBJ_RANGE:
    case OBJ_ENUM:
    case OBJ_ITERATOR:
    case OBJ_MODULE:
    case OBJ_FILE:
    case OBJ_THREAD:
    case OBJ_LOCK:
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_NATIVE_METHOD:
        // Always considered "truthy".
        result = true;
        break;

    case OBJ_INT: result = AS_PTR(IntObj, object)->number != 0; break;
    case OBJ_FLOAT: result = AS_PTR(FloatObj, object)->number != 0.0; break;
    case OBJ_BOOL: result = AS_PTR(BoolObj, object)->boolean; break;
    case OBJ_NULL: result = false; break;
    case OBJ_STRING: result = AS_PTR(StringObj, object)->length != 0; break;
    case OBJ_LIST: result = AS_PTR(ListObj, object)->items.length != 0; break;
    case OBJ_MAP: result = AS_PTR(MapObj, object)->table.count != 0; break;
    case OBJ_ENUM_MEMBER: result = AS_PTR(EnumMemberObj, object)->index != 0; break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    return new_bool_obj(vulnObjs, result);
}

/** Returns a copy of the passed object as a string. */
StringObj *as_string(VulnerableObjs *vulnObjs, const Obj *object) {
    CharBuffer cstring = object_cstring(object, false);
    StringObj *result = new_string_obj(vulnObjs, cstring.text, cstring.length);
    free_char_buffer(&cstring);
    return result;
}

/** 
 * Prints the passed object to the console.
 * 
 * Simply wraps around getting an object's c-string and prints it out.
 * If debugPrint is on, it prints some extra things to make the output clearer during debugging.
 */
void print_obj(const Obj *object, const bool debugPrint) {
    CharBuffer string = object_cstring(object, debugPrint);
    printf("%s", string.text);
    free_char_buffer(&string);
}

/** Returns a readable C string from the passed object type enum. */
char *obj_type_str(ObjType type) {
    switch (type) {
    case OBJ_INT: return "integer";
    case OBJ_FLOAT: return "float";
    case OBJ_BOOL: return "bool";
    case OBJ_NULL: return "null";
    case OBJ_STRING: return "string";
    case OBJ_RANGE: return "range";
    case OBJ_LIST: return "list";
    case OBJ_MAP: return "map";
    case OBJ_ENUM: return "enum";
    case OBJ_ENUM_MEMBER: return "enum member";
    case OBJ_ITERATOR: return "iterator";
    case OBJ_MODULE: return "module";
    case OBJ_FILE: return "file";
    case OBJ_THREAD: return "thread";
    case OBJ_LOCK: return "lock";
    case OBJ_FUNC: return "function";
    case OBJ_CAPTURED: return "captured";
    case OBJ_CLASS: return "class";
    case OBJ_INSTANCE: return "instance";
    case OBJ_METHOD: return "instance method";
    case OBJ_NATIVE_FUNC: return "native function";
    case OBJ_NATIVE_METHOD: return "native method";
    }
    UNREACHABLE_ERROR();
}

/** Frees memory allocated by a function parameters struct. */
static void free_func_params(FuncParams *params) {
    FREE_DA(&params->names);
    FREE_DA(&params->values);
}

/** Frees the allocated content/arrays of a function. */
static void free_func_obj(FuncObj *func) {
    free_func_params(&func->staticParams);
    FREE_DA(&func->bytecode);
    FREE_DA(&func->constPool);
    FREE_DA(&func->positions);
}

/** 
 * Frees the contents of the passed object.
 * 
 * Note that any objects which have other objects inside them (like a function with a string name)
 * shouldn't free that object, as it was already stored in the Zymux program struct,
 * which will free it later anyways.
 */
void free_obj(Obj *object) {
#if DEBUG_LOG_GC
    printf("Free %p | %s (", (void*)object, obj_type_str(object->type));
    print_obj(object, true);
    printf(")\n");
#endif

    switch (object->type) {
    case OBJ_STRING:
        free(AS_PTR(StringObj, object)->string);
        break;
    case OBJ_LIST:
        FREE_DA(&AS_PTR(ListObj, object)->items);
        break;
    case OBJ_MAP:
        free_table(&AS_PTR(MapObj, object)->table);
        break;
    case OBJ_ENUM:
        free_table(&AS_PTR(EnumObj, object)->lookupTable);
        FREE_DA(&AS_PTR(EnumObj, object)->members);
        break;
    case OBJ_MODULE:
        free_table(&AS_PTR(ModuleObj, object)->globalsUnsafe);
        destroy_mutex(&AS_PTR(ModuleObj, object)->globalsLock);
        break;
    case OBJ_FILE:
        if (AS_PTR(FileObj, object)->isOpen) {
            fclose(AS_PTR(FileObj, object)->stream); // Only close if it's not already closed.
        }
        free_table(&AS_PTR(FileObj, object)->fields);
        break;
    case OBJ_THREAD: {
        ThreadObj *thread = AS_PTR(ThreadObj, object);
        free_table(&AS_PTR(ThreadObj, object)->fields);
        free_vulnerables(&thread->vulnObjs);
        FREE_DA(&thread->openCaptures);
        FREE_DA(&thread->callStack);
        FREE_DA(&thread->catches);
        FREE_STACK(thread);
        destroy_mutex(&thread->lock);
        break;
    }
    case OBJ_LOCK:
        destroy_mutex(&AS_PTR(LockObj, object)->lock);
        break;
    case OBJ_FUNC: {
        FuncObj *func = AS_PTR(FuncObj, object);
        if (FLAG_IS_SET(func->flags, FUNC_RUNTIME_BIT)) {
            RuntimeFuncObj *runtimeFunc = AS_PTR(RuntimeFuncObj, func);
            FREE_DA(&runtimeFunc->paramVals);
            FREE_DA(&runtimeFunc->captures);
            if (FLAG_IS_SET(func->flags, FUNC_TOP_LEVEL_BIT)) {
                free_func_obj(func); // Top-level gets one func that is also a closure, so free it.
            }
        } else {
            free_func_obj(func);
        }
        break;
    }
    case OBJ_CLASS:
        free_table(&AS_PTR(ClassObj, object)->methods);
        FREE_DA(&AS_PTR(ClassObj, object)->abstractMethods);
        break;
    case OBJ_INSTANCE:
        free_table(&AS_PTR(InstanceObj, object)->fields);
        break;
    case OBJ_NATIVE_FUNC:
        free_func_params(&AS_PTR(NativeFuncObj, object)->params);
        break;
    case OBJ_INT:
    case OBJ_FLOAT:
    case OBJ_BOOL:
    case OBJ_NULL:
    case OBJ_RANGE:
    case OBJ_ENUM_MEMBER:
    case OBJ_ITERATOR:
    case OBJ_CAPTURED:
    case OBJ_METHOD:
    case OBJ_NATIVE_METHOD:
        break; // The object itself doesn't own any memory.
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    free(object);
}

/** Frees all objects that were allocated and placed inside the passed program. */
void free_all_objs(ZmxProgram *program) {
    if (program->allObjs == NULL) {
        return;
    }

    Obj *current = program->allObjs;
    Obj *next = current->next;
    while (current) {
        free_obj(current);
        current = next;
        if (next) {
            next = next->next;
        }
    }
    program->allObjs = NULL; // Just in case we start allocating objects again.
}
