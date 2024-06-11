#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "object.h"
#include "program.h"

/** Allocates and returns an object. objType is the enum type, actualType is the C struct itself. */
#define NEW_OBJ(program, objType, actualType) \
    ((actualType *)new_obj(program, objType, sizeof(actualType)))

/** Allocates a new object of the passed type and size. */
static Obj *new_obj(ZmxProgram *program, const ObjType type, const size_t size) {
    Obj *object = ZMX_ALLOC(size);
    object->type = type;

    object->next = program->allObjs;
    program->allObjs = object;
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

/** Returns a new allocated boolean object from the passed bool. TODO: intern booleans. */
BoolObj *new_bool_obj(ZmxProgram *program, const bool boolean) {
    BoolObj *object = NEW_OBJ(program, OBJ_BOOL, BoolObj);
    object->boolean = boolean;
    return object;
}

/** 
 * Returns a new allocated string object.
 * 
 * This function manually allocates a copy of the passed string, so the responsibility
 * of potentially having to free the passed string is not passed to the string object.
 */
StringObj *new_string_obj(ZmxProgram *program, const char *string) {
    StringObj *object = NEW_OBJ(program, OBJ_STRING, StringObj);
    object->length = strlen(string);

    object->string = ZMX_ARRAY_ALLOC(object->length + 1, char);
    strncpy(object->string, string, object->length);
    object->string[object->length] = '\0';
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

/** Returns whether or not 2 objects are considered equal. */
bool equal_obj(const Obj *left, const Obj *right) {
    if (left->type != right->type) {
        return false;
    }

    switch (left->type) {
    case OBJ_FUNC:
        return left == right; // Compare addresses directly.
    
    case OBJ_INT: return AS_PTR(IntObj, left)->number == AS_PTR(IntObj, right)->number;
    case OBJ_FLOAT: return AS_PTR(FloatObj, left)->number == AS_PTR(FloatObj, right)->number;
    case OBJ_BOOL: return AS_PTR(BoolObj, left)->boolean == AS_PTR(BoolObj, right)->boolean;
    case OBJ_STRING:
        // TODO: this is temporary. Once interning is added, do an address equality check.
        if (AS_PTR(StringObj, left)->length != AS_PTR(StringObj, right)->length) {
            return false;
        }
        return strncmp(
            AS_PTR(StringObj, left)->string,
            AS_PTR(StringObj, right)->string,
            AS_PTR(StringObj, left)->length
        );
    default: UNREACHABLE_ERROR();
    }
}

/** Returns a new string object that is formed from concatenating left with right. */
StringObj *concatenate(ZmxProgram *program, const StringObj *left, const StringObj *right) {
    char *concatenated = ZMX_ARRAY_ALLOC(left->length + right->length + 1, char);
    strncpy(concatenated, left->string, left->length);
    strncpy(concatenated + left->length, right->string, right->length);
    concatenated[left->length + right->length] = '\0';

    StringObj *result = new_string_obj(program, concatenated);
    free(concatenated);
    return result;
}

// TODO: objects which can error during conversion can simply change the program's hasErrored
// and/or return NULL?

/** Returns the passed object as a string value. */
StringObj *as_string(ZmxProgram *program, const Obj *object) {
    char *string;
    switch (object->type) {
    case OBJ_INT: {
        ZmxInt value = AS_PTR(IntObj, object)->number;
        const int length = snprintf(NULL, 0, ZMX_INT_FMT, value);
        string = ZMX_ARRAY_ALLOC(length + 1, char);
        snprintf(string, length + 1, ZMX_INT_FMT, value);
        break;
    }
    case OBJ_FLOAT: {
        ZmxFloat value = AS_PTR(FloatObj, object)->number;
        const int length = snprintf(NULL, 0, ZMX_FLOAT_FMT, value);
        string = ZMX_ARRAY_ALLOC(length + 1, char);
        snprintf(string, length + 1, ZMX_FLOAT_FMT, value);
        break;
    }
    case OBJ_FUNC:
        string = ZMX_ARRAY_ALLOC(AS_PTR(FuncObj, object)->name->length, char);
        break;
    case OBJ_BOOL:
        // No need to allocate since it's only true or false.
        return new_string_obj(program, AS_PTR(BoolObj, object)->boolean ? "true" : "false");
    case OBJ_STRING:
        return AS_PTR(StringObj, object);
    default:
        UNREACHABLE_ERROR();
    }
    StringObj *result = new_string_obj(program, string);
    free(string);
    return result;
}

/** Returns the passed object as a boolean (whether it's considered "truthy" or "falsy"). */
BoolObj *as_bool(ZmxProgram *program, const Obj *object) {
    bool result;
    switch (object->type) {
    case OBJ_FUNC:
        // Always considered "truthy".
        result = true;
        break;

    case OBJ_INT: result = AS_PTR(IntObj, object)->number != 0; break;
    case OBJ_FLOAT: result = AS_PTR(FloatObj, object)->number != 0.0; break;
    case OBJ_STRING: result = AS_PTR(StringObj, object)->length != 0; break;
    case OBJ_BOOL: return AS_PTR(BoolObj, object);
    default: UNREACHABLE_ERROR();
    }
    return new_bool_obj(program, result);
}

/** 
 * Prints the passed object to the console. The output depends on whether to debugPrint or not. */
void print_obj(const Obj *object, const bool debugPrint) {
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
    case OBJ_STRING:
        if (debugPrint) {
            printf("\"%s\"", AS_PTR(StringObj, object)->string);
        } else {
            printf("%s", AS_PTR(StringObj, object)->string);
        }
        break;
    case OBJ_FUNC:
        printf("<func \"%s\">", AS_PTR(FuncObj, object)->name->string);
        break;
    default:
        UNREACHABLE_ERROR();
    }
}

/** 
 * Frees the contents of any given object.
 * 
 * Note that any objects which have other objects inside them (like a function with a string name)
 * shouldn't free that object, as it was already stored in a Zymux program struct,
 * which will free it later anyways.
 */
static void free_obj_contents(Obj *object) {
    switch (object->type) {
        case OBJ_INT:
        case OBJ_FLOAT:
        case OBJ_BOOL:
            return;

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
        default:
            UNREACHABLE_ERROR();
    }
}

/** Frees all objects that were allocated and placed inside the passed program. */
void free_all_objs(ZmxProgram *program) {
    if (program->allObjs == NULL) {
        return;
    }

    Obj *current = program->allObjs;
    Obj *next = current->next;
    while (current != NULL) {
        free_obj_contents(current);
        free(current);
        current = next;
        if (next != NULL) {
            next = next->next;
        }
    }
}
