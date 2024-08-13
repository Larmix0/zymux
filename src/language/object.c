#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.h"
#include "char_buffer.h"
#include "object.h"
#include "program.h"

/** Allocates and returns an object. objType is the enum type, actualType is the C struct itself. */
#define NEW_OBJ(program, objType, actualType) \
    ((actualType *)new_obj(program, objType, sizeof(actualType)))

void print_obj(const Obj *object, const bool debugPrint);
char *obj_type_string(ObjType type);

/** Allocates a new object of the passed type and size. */
static Obj *new_obj(ZmxProgram *program, const ObjType type, const size_t size) {
    Obj *object = malloc(size);
    if (object == NULL) {
        // GC then retry once more with the macro that automatically errors on allocation failure.
        gc_collect(program);
        object = ALLOC(size);
    }
#if DEBUG_GC_PRINT
    printf("Allocate %p | %s (%zu bytes)\n", (void*)object, obj_type_string(type), size);
#endif

    program->gc.allocated += size;
    if (program->gc.allocated > program->gc.nextCollection ) {
        program->gc.nextCollection *= COLLECTION_GROWTH;
        gc_collect(program);
    }
#if DEBUG_GC_ALWAYS
    gc_collect(program);
#endif

    object->isReachable = false;
    object->type = type;
    object->next = program->allObjs;
    program->allObjs = object;
    if (program->gc.protectNewObjs) {
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
 */
StringObj *new_string_obj(ZmxProgram *program, const char *string) {
    u32 hash = hash_string(string);
    Obj *interned = table_get_string(&program->internedStrings, string, hash);
    if (interned != NULL) {
        return AS_PTR(StringObj, interned);
    }

    StringObj *object = NEW_OBJ(program, OBJ_STRING, StringObj);
    object->length = strlen(string);
    object->hash = hash;

    object->string = ARRAY_ALLOC(object->length + 1, char);
    strncpy(object->string, string, object->length);
    object->string[object->length] = '\0';
    table_set(&program->internedStrings, AS_OBJ(object), AS_OBJ(program->internedNull));
    return object;
}

/** Returns an allocated string object from a length based string (may not be NUL terminated). */
StringObj *string_obj_from_len(ZmxProgram *program, const char *string, const u32 length) {
    CharBuffer terminated = create_char_buffer();
    buffer_append_string_len(&terminated, string, length);
    StringObj *asObj = new_string_obj(program, terminated.text);
    free_char_buffer(&terminated);
    return asObj;
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

/** Returns a new allocated function object with a name to it and its members initialized. */
FuncObj *new_func_obj(ZmxProgram *program, StringObj *name, const int constIdx) {
    FuncObj *object = NEW_OBJ(program, OBJ_FUNC, FuncObj);
    object->name = name;
    object->constIdx = constIdx;
    INIT_DA(&object->bytecode);
    INIT_DA(&object->positions);
    INIT_DA(&object->constPool);
    return object;
}

/** Returns a new allocated native function object. */
NativeFuncObj *new_native_func_obj(ZmxProgram *program, NativeFunc func, StringObj *name) {
    NativeFuncObj *object = NEW_OBJ(program, OBJ_NATIVE_FUNC, NativeFuncObj);
    object->func = func;
    object->name = name;
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

/** Returns whether or not the passed object is something that can be iterated over. */
bool is_iterable(const Obj *object) {
    switch (object->type) {
    case OBJ_STRING:
    case OBJ_RANGE:
        return true;

    case OBJ_INT:
    case OBJ_FLOAT:
    case OBJ_BOOL:
    case OBJ_NULL:
    case OBJ_FUNC:
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
        return AS_OBJ(string_obj_from_len(program, string->string + iterator->iteration++, 1));
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
    case OBJ_INT:
    case OBJ_FLOAT:
    case OBJ_BOOL:
    case OBJ_NULL:
    case OBJ_FUNC:
    case OBJ_NATIVE_FUNC:
    case OBJ_ITERATOR:
        UNREACHABLE_ERROR();
    }
    UNREACHABLE_ERROR();
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
    case OBJ_RANGE:
    case OBJ_FUNC:
    case OBJ_NATIVE_FUNC:
    case OBJ_ITERATOR:
        return left == right; // Compare addresses directly.
    case OBJ_BOOL:
        return AS_PTR(BoolObj, left)->boolean == AS_PTR(BoolObj, right)->boolean;
    case OBJ_NULL:
        return true;
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

    StringObj *result = new_string_obj(program, concatenated);
    free(concatenated);
    return result;
}

// TODO: objects which can error during conversion can simply change the program's hasErrored
// and/or return NULL?

/** Returns the passed object as a string object. */
StringObj *as_string(ZmxProgram *program, const Obj *object) {
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
    case OBJ_FUNC:
        buffer_append_format(&string, "<function %s>", AS_PTR(FuncObj, object)->name->string);
        break;
    case OBJ_NATIVE_FUNC:
        buffer_append_format(
            &string, "<native function %s>", AS_PTR(NativeFuncObj, object)->name->string
        );
        break;
    case OBJ_ITERATOR:
        buffer_append_format(
            &string, "<iterator %s>", as_string(program, AS_PTR(IteratorObj, object)->iterable)
        );
        break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    StringObj *result = new_string_obj(program, string.text);
    free_char_buffer(&string);
    return result;
}

/** Returns the passed object as a boolean (whether it's considered "truthy" or "falsy"). */
BoolObj *as_bool(ZmxProgram *program, const Obj *object) {
    bool result;
    switch (object->type) {
    case OBJ_RANGE:
    case OBJ_FUNC:
    case OBJ_NATIVE_FUNC:
    case OBJ_ITERATOR:
        // Always considered "truthy".
        result = true;
        break;

    case OBJ_INT: result = AS_PTR(IntObj, object)->number != 0; break;
    case OBJ_FLOAT: result = AS_PTR(FloatObj, object)->number != 0.0; break;
    case OBJ_STRING: result = AS_PTR(StringObj, object)->length != 0; break;
    case OBJ_BOOL: result = AS_PTR(BoolObj, object)->boolean; break;
    case OBJ_NULL: result = false; break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
    return new_bool_obj(program, result);
}

/** Prints the passed object to the console. The output depends on whether to debugPrint or not. */
void print_obj(const Obj *object, const bool debugPrint) {
    // TODO: potentially just make this a printing wrapper over as_string()?
    switch (object->type) {
    case OBJ_INT:
        printf(ZMX_INT_FMT, AS_PTR(IntObj, object)->number);
        break;
    case OBJ_FLOAT:
        printf(ZMX_FLOAT_FMT, AS_PTR(FloatObj, object)->number);
        break;
    case OBJ_BOOL:
        printf("%s", AS_PTR(BoolObj, object)->boolean ? "true" : "false");
        break;
    case OBJ_NULL:
        printf("null");
        break;
    case OBJ_STRING:
        if (debugPrint) {
            printf("'%s'", AS_PTR(StringObj, object)->string);
        } else {
            printf("%s", AS_PTR(StringObj, object)->string);
        }
        break;
    case OBJ_RANGE: {
        RangeObj *range = AS_PTR(RangeObj, object);
        printf(
            ZMX_INT_FMT ".." ZMX_INT_FMT ".." ZMX_INT_FMT, range->start, range->end, range->step
        );
        break;
    }
    case OBJ_FUNC:
        printf("<function %s>", AS_PTR(FuncObj, object)->name->string);
        break;
    case OBJ_NATIVE_FUNC:
        printf("<native function %s>", AS_PTR(NativeFuncObj, object)->name->string);
        break;
    case OBJ_ITERATOR:
        printf("<iterator ");
        print_obj(AS_PTR(IteratorObj, object)->iterable, debugPrint);
        putchar('>');
        break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
}

/** Returns a readable C string from the passed object type enum. */
char *obj_type_string(ObjType type) {
    switch (type) {
    case OBJ_INT: return "integer";
    case OBJ_FLOAT: return "float";
    case OBJ_BOOL: return "bool";
    case OBJ_NULL: return "null";
    case OBJ_STRING: return "string";
    case OBJ_RANGE: return "range";
    case OBJ_FUNC: return "function";
    case OBJ_NATIVE_FUNC: return "native function";
    case OBJ_ITERATOR: return "iterator";
    }
    UNREACHABLE_ERROR();
}

/** 
 * Frees the contents of the passed object.
 * 
 * Note that any objects which have other objects inside them (like a function with a string name)
 * shouldn't free that object, as it was already stored in a Zymux program struct,
 * which will free it later anyways.
 */
void free_obj(Obj *object) {
#if DEBUG_GC_PRINT
    printf("Free %p | %s (", (void*)object, obj_type_string(object->type));
    print_obj(object, true);
    printf(")\n");
#endif

    switch (object->type) {
    case OBJ_STRING:
        free(AS_PTR(StringObj, object)->string);
        break;
    case OBJ_FUNC: {
        FuncObj *func = AS_PTR(FuncObj, object);
        FREE_DA(&func->bytecode);
        FREE_DA(&func->constPool);
        FREE_DA(&func->positions);
        break;
    }
    case OBJ_INT:
    case OBJ_FLOAT:
    case OBJ_BOOL:
    case OBJ_NULL:
    case OBJ_RANGE:
    case OBJ_NATIVE_FUNC:
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
    while (current != NULL) {
        free_obj(current);
        current = next;
        if (next != NULL) {
            next = next->next;
        }
    }
}
