#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.h"
#include "char_buffer.h"
#include "lexer.h"
#include "object.h"
#include "program.h"

/** Allocates and returns an object. objType is the enum type, cType is the C struct itself. */
#define NEW_OBJ(program, objType, cType) \
    ((cType *)new_obj(program, objType, sizeof(cType)))

static CharBuffer object_cstring(const Obj *object);
void print_obj(const Obj *object, const bool debugPrint);
char *obj_type_str(ObjType type);

/** Allocates a new object of the passed type and size. */
static Obj *new_obj(ZmxProgram *program, const ObjType type, const size_t size) {
    Obj *object = malloc(size);
    if (object == NULL) {
        // GC then retry once more with the macro that automatically errors on allocation failure.
        gc_collect(program);
        object = ALLOC(size);
    }
#if DEBUG_LOG_GC
    printf("Allocate %p | %s (%zu bytes)\n", (void*)object, obj_type_str(type), size);
#endif

    program->gc.allocated += size;
    if (program->gc.allocated > program->gc.nextCollection ) {
        program->gc.nextCollection *= COLLECTION_GROWTH;
        gc_collect(program);
    }
#if DEBUG_ALWAYS_GC
    gc_collect(program);
#endif

    object->isReachable = false;
    object->type = type;
    object->next = program->allObjs;
    program->allObjs = object;
    if (program->gc.protectionLayers > 0) {
        APPEND_DA(&program->gc.protected, object);
    }
    return object;
}

/** Allocates an int object from the passed integer. */
IntObj *new_int_obj(ZmxProgram *program, const ZmxInt number) {
    IntObj *object = NEW_OBJ(program, OBJ_INT, IntObj);
    object->number = number;
    return object;
}

/** Returns a new allocated float object from the passed floating number. */
FloatObj *new_float_obj(ZmxProgram *program, const ZmxFloat number) {
    FloatObj *object = NEW_OBJ(program, OBJ_FLOAT, FloatObj);
    object->number = number;
    return object;
}

/** Returns an interned boolean object depending on the passed bool. */
BoolObj *new_bool_obj(ZmxProgram *program, const bool boolean) {
    return boolean ? program->internedTrue : program->internedFalse;
}

/** Returns an interned null object. */
NullObj *new_null_obj(ZmxProgram *program) {
    return program->internedNull;
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
 * as if we called the real allocator and it protected the created object itself.
 */
StringObj *new_string_obj(ZmxProgram *program, const char *string, const u32 length) {
    const u32 hash = hash_string(string, length);
    Obj *interned = table_get_string(&program->internedStrings, string, length, hash);
    if (interned) {
        if (program->gc.protectionLayers > 0) {
            GC_PROTECT_OBJ(&program->gc, interned); // Manually protect interned string from GC.
        }
        return AS_PTR(StringObj, interned);
    }

    StringObj *object = NEW_OBJ(program, OBJ_STRING, StringObj);
    object->length = length;
    object->hash = hash;
    object->string = ARRAY_ALLOC(object->length + 1, char);
    strncpy(object->string, string, object->length);
    object->string[object->length] = '\0';

    table_set(&program->internedStrings, AS_OBJ(object), AS_OBJ(program->internedNull));
    return object;
}

/** Returns a range object created from the passed numbers. */
RangeObj *new_range_obj(
    ZmxProgram *program, const ZmxInt start, const ZmxInt end, const ZmxInt step
) {
    RangeObj *object = NEW_OBJ(program, OBJ_RANGE, RangeObj);
    object->start = start;
    object->end = end;
    object->step = step;
    return object;
}

/** Returns a list, which encapsulates an array of objects that is ordered. */
ListObj *new_list_obj(ZmxProgram *program, const ObjArray items) {
    ListObj *object = NEW_OBJ(program, OBJ_LIST, ListObj);
    object->items = items;
    return object;
}

/** Returns a map, which holds a hash table of key-value pairs. */
MapObj *new_map_obj(ZmxProgram *program, const Table table) {
    MapObj *object = NEW_OBJ(program, OBJ_MAP, MapObj);
    object->table = table;
    return object;
}

/**
 * Returns one member of an enum.
 * 
 * Holds the original enum it's a member of, its own member textual representation as a name,
 * and its index inside the enum.
 */
EnumMemberObj *new_enum_member_obj(
    ZmxProgram *program, EnumObj *enumObj, StringObj *name, const ZmxInt index
) {
    EnumMemberObj *object = NEW_OBJ(program, OBJ_ENUM_MEMBER, EnumMemberObj);
    object->enumObj = enumObj;
    object->name = name;
    object->index = index;
    return object;
}

/** Returns an enum, which holds a bunch of names in order. */
EnumObj *new_enum_obj(ZmxProgram *program, StringObj *name) {
    EnumObj *object = NEW_OBJ(program, OBJ_ENUM, EnumObj);
    object->name = name;

    object->lookupTable = create_table();
    INIT_DA(&object->members);
    return object;
}

/** 
 * Returns a new allocated function object with a name to it and its members initialized.
 * 
 * If isClosure is set to true, then it'll allocate a larger function that allows it to be
 * converted to a closure type that reveals an extra closure context that not every function may
 * have.
 */
FuncObj *new_func_obj(
    ZmxProgram *program, StringObj *name, const u32 minArity, const u32 maxArity
) {
    FuncObj *object = NEW_OBJ(program, OBJ_FUNC, FuncObj);
    object->name = name;
    object->hasOptionals = false;
    object->isClosure = false;
    object->cls = NULL;

    object->params.minArity = minArity;
    object->params.maxArity = maxArity;
    INIT_DA(&object->params.names);
    INIT_DA(&object->params.values);

    INIT_DA(&object->bytecode);
    INIT_DA(&object->positions);
    INIT_DA(&object->constPool);
    return object;
}

/** Returns a newely created indirect reference to the passed object (capturing the object). */
CapturedObj *new_captured_obj(ZmxProgram *program, Obj *captured, const u32 stackLocation) {
    CapturedObj *object = NEW_OBJ(program, OBJ_CAPTURED, CapturedObj);
    object->isOpen = true;
    object->stackLocation = stackLocation;
    object->captured = captured;
    return object;
}

/** 
 * Returns a func which has some runtime values that might be independant from the static func.
 * 
 * This is because runtime functions aren't "objects", but instead a function
 * which has an extra runtime context extension allocated,
 * and that extension can be revealed by converting to the runtime func type.
 * 
 * We make the runtime function by setting its static func to a copy of the passed static func.
 * This indirect copy (because we use func obj directly instead func obj pointer) means that
 * booleans (like is closure and has optionals) will actually be independant from the original
 * func that still has them set as false, although arrays and pointers will still be shared.
 * 
 * Worth mentioning that if the function we're wrapping is also a runtime function itself,
 * then we need to manually copy its runtime contexts to the new returned function, as copying
 * the static function alone truncates that extra information off.
 */
RuntimeFuncObj *new_runtime_func_obj(
    ZmxProgram *program, FuncObj *func, const bool isToplevel, const bool hasOptionals,
    const bool isClosure
) {
    RuntimeFuncObj *object = NEW_OBJ(program, OBJ_FUNC, RuntimeFuncObj);
    Obj base = object->func.obj; // Runtime function's base obj.
    object->func = *func; // Copy.
    object->func.obj = base; // Change the base obj back to the runtime func's base after copying.

    object->isToplevel = isToplevel;
    object->func.hasOptionals = hasOptionals;
    object->func.isClosure = isClosure;
    INIT_DA(&AS_PTR(RuntimeFuncObj, object)->paramVals);
    INIT_DA(&AS_PTR(RuntimeFuncObj, object)->captures);

    if (func->isClosure) {
        // Copy its closure context if we're wrapping a closure. No need to copy optionals though.
        RuntimeFuncObj *wrappedClosure = AS_PTR(RuntimeFuncObj, func);
        for (u32 i = 0; i < wrappedClosure->captures.length; i++) {
            APPEND_DA(&object->captures, wrappedClosure->captures.data[i]);
        }
    }
    return object;
}

/** Creates a bare-bones class with a name. Other information is added later. */
ClassObj *new_class_obj(ZmxProgram *program, StringObj *name, const bool isAbstract) {
    ClassObj *object = NEW_OBJ(program, OBJ_CLASS, ClassObj);
    object->name = name;
    object->isAbstract = isAbstract;

    object->superclass = NULL;
    object->init = NULL;
    object->methods = create_table();
    INIT_DA(&object->abstractMethods);
    return object;
}

/** Returns a separate instance of the class, which has its own fields. */
InstanceObj *new_instance_obj(ZmxProgram *program, ClassObj *cls) {
    InstanceObj *object = NEW_OBJ(program, OBJ_INSTANCE, InstanceObj);
    object->cls = cls;
    object->fields = create_table();
    return object;
}

/** Returns a method of a class, which is tied to an instance of it (to access its properties). */
MethodObj *new_method_obj(ZmxProgram *program, InstanceObj *instance, FuncObj *func) {
    MethodObj *object = NEW_OBJ(program, OBJ_METHOD, MethodObj);
    object->instance = instance;
    object->func = func;
    return object;
}

/** Returns a new allocated native function object. */
NativeFuncObj *new_native_func_obj(
    ZmxProgram *program, StringObj *name, NativeFunc func, const FuncParams params
) {
    NativeFuncObj *object = NEW_OBJ(program, OBJ_NATIVE_FUNC, NativeFuncObj);
    object->name = name;
    object->func = func;
    object->params = params;
    return object;
}

/** Returns an iterator which wraps around an iterable object being iterated on. */
IteratorObj *new_iterator_obj(ZmxProgram *program, Obj *iterable) {
    IteratorObj *object = NEW_OBJ(program, OBJ_ITERATOR, IteratorObj);
    object->iterable = iterable;
    object->iteration = 0;
    return object;
}

/** Allocates some objects for interning and puts them in the passed program. */
void intern_objs(ZmxProgram *program) {
    program->internedTrue = NEW_OBJ(program, OBJ_BOOL, BoolObj);
    program->internedTrue->boolean = true;

    program->internedFalse = NEW_OBJ(program, OBJ_BOOL, BoolObj);
    program->internedFalse->boolean = false;

    program->internedNull = NEW_OBJ(program, OBJ_NULL, NullObj);
}

/** Appends a C-string representation of the passed list. */
static void list_cstring(const ListObj *list, CharBuffer *string) {
    buffer_append_char(string, '[');
    for (u32 i = 0; i < list->items.length; i++) {
        if (i != 0) {
            buffer_append_string(string, ", ");
        }
        CharBuffer itemBuf = object_cstring(list->items.data[i]);
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
        CharBuffer keyBuf = object_cstring(map->table.entries[i].key);
        CharBuffer valueBuf = object_cstring(map->table.entries[i].value);
        buffer_append_format(string, "%s: %s", keyBuf.text, valueBuf.text);
        free_char_buffer(&keyBuf);
        free_char_buffer(&valueBuf);
    }
    buffer_append_char(string, '}');

}

/** Returns a C-string representation of the object in an allocated char buffer. */
static CharBuffer object_cstring(const Obj *object) {
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
        buffer_append_string(&string, AS_PTR(StringObj, object)->string);
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
    case OBJ_FUNC:
        buffer_append_format(&string, "<function %s>", AS_PTR(FuncObj, object)->name->string);
        break;
    case OBJ_CAPTURED: {
        buffer_append_format(&string, "<captured>");
        break;
    }
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
    case OBJ_ITERATOR: {
        buffer_append_format(&string, "<iterator>");
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
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_ITERATOR:
        return false;
    }
    UNREACHABLE_ERROR();
}

/** 
 * Iterates over an object that is assumed to be iterable.
 * Returns the iterated element of the iterable, or NULL if the iterator's exhausted.
 * 
 * Passing a non-iterable is considered unreachable.
 */
Obj *iterate(ZmxProgram *program, IteratorObj *iterator) {
    ASSERT(is_iterable(iterator->iterable), "Attempted to iterate over non-iterable object.");

    switch (iterator->iterable->type) {
    case OBJ_STRING: {
        StringObj *string = AS_PTR(StringObj, iterator->iterable);
        if (iterator->iteration >= string->length) {
            return NULL;
        }
        return AS_OBJ(new_string_obj(program, string->string + iterator->iteration++, 1));
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
        return AS_OBJ(new_int_obj(program, result));
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
                APPEND_DA(&keyValue, map->table.entries[i].key);
                APPEND_DA(&keyValue, map->table.entries[i].value);
                return AS_OBJ(new_list_obj(program, keyValue));
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
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_ITERATOR:
        UNREACHABLE_ERROR();
    }
    UNREACHABLE_ERROR();
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
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_ITERATOR:
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

/** Returns a new string object that is formed from concatenating left with right. */
StringObj *concatenate(ZmxProgram *program, const StringObj *left, const StringObj *right) {
    char *concatenated = ARRAY_ALLOC(left->length + right->length + 1, char);
    strncpy(concatenated, left->string, left->length);
    strncpy(concatenated + left->length, right->string, right->length);
    concatenated[left->length + right->length] = '\0';

    StringObj *result = new_string_obj(program, concatenated, left->length + right->length);
    free(concatenated);
    return result;
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
IntObj *as_int(ZmxProgram *program, const Obj *object) {
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
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_ITERATOR:
        // Can't ever convert to integer.
        return NULL;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    return new_int_obj(program, result);   
}

/** Returns a copy of the passed object as a float. Returns NULL if conversion failed. */
FloatObj *as_float(ZmxProgram *program, const Obj *object) {
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
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_ITERATOR:
        // Can't ever convert to float.
        return NULL;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    return new_float_obj(program, result);      
}

/** Returns a copy of the passed object's boolean (whether the object is "truthy" or "falsy"). */
BoolObj *as_bool(ZmxProgram *program, const Obj *object) {
    bool result;
    switch (object->type) {
    case OBJ_RANGE:
    case OBJ_ENUM:
    case OBJ_FUNC:
    case OBJ_CAPTURED:
    case OBJ_CLASS:
    case OBJ_INSTANCE:
    case OBJ_METHOD:
    case OBJ_NATIVE_FUNC:
    case OBJ_ITERATOR:
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
    return new_bool_obj(program, result);
}

/** Returns a copy of the passed object as a string. */
StringObj *as_string(ZmxProgram *program, const Obj *object) {
    CharBuffer cstring = object_cstring(object);
    StringObj *result = new_string_obj(program, cstring.text, cstring.length - 1); // -1 for NUL.
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
    if (debugPrint && object->type == OBJ_STRING) {
        putchar('\'');
    }
    CharBuffer string = object_cstring(object);
    printf("%s", string.text);
    free_char_buffer(&string);
    if (debugPrint && object->type == OBJ_STRING) {
        putchar('\'');
    }
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
    case OBJ_FUNC: return "function";
    case OBJ_CAPTURED: return "captured";
    case OBJ_CLASS: return "class";
    case OBJ_INSTANCE: return "instance";
    case OBJ_METHOD: return "instance method";
    case OBJ_NATIVE_FUNC: return "native function";
    case OBJ_ITERATOR: return "iterator";
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
    free_func_params(&func->params);
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
    case OBJ_FUNC: {
        FuncObj *func = AS_PTR(FuncObj, object);
        if (func->hasOptionals || func->isClosure) {
            RuntimeFuncObj *runtimeFunc = AS_PTR(RuntimeFuncObj, func);
            FREE_DA(&runtimeFunc->paramVals);
            FREE_DA(&runtimeFunc->captures);
            if (runtimeFunc->isToplevel) {
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
    case OBJ_CAPTURED:
    case OBJ_METHOD:
    case OBJ_ITERATOR:
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
}
