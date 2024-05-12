#include "stdio.h"
#include "stdlib.h"

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
IntObj *new_int_obj(ZmxProgram *program, ZmxInt number) {
    IntObj *object = NEW_OBJ(program, OBJ_INT, IntObj);
    object->number = number;
    return object;
}

/** Returns a new allocated float object from the passed floating number. */
FloatObj *new_float_obj(ZmxProgram *program, ZmxFloat number) {
    FloatObj *object = NEW_OBJ(program, OBJ_FLOAT, FloatObj);
    object->number = number;
    return object;
}

/** TODO: change the implementation here too once function objects get more members. */
FuncObj *new_func_obj(ZmxProgram *program) {
    FuncObj *object = NEW_OBJ(program, OBJ_FUNC, FuncObj);
    INIT_DA(&object->bytecode);
    INIT_DA(&object->positions);
    INIT_DA(&object->constPool);
    return object;
}

/** Prints the passed object to the console. */
void print_obj(Obj *object) {
    switch (object->type) {
        case OBJ_INT: printf(ZMX_INT_FMT, AS_PTR(object, IntObj)->number); break;
        case OBJ_FLOAT: printf(ZMX_FLOAT_FMT, AS_PTR(object, FloatObj)->number); break;
        case OBJ_FUNC: // TODO: Implement function print when we add strings. 
        default: UNREACHABLE_ERROR();
    }
}

/** 
 * Frees the contents of any given object.
 * 
 * Note that any objects which have other objects inside them (like a function with a string name)
 * shouldn't free that object, as it was already stored in a Zymux program struct,
 * which will free it later anyways.
 */
static void free_obj_contents(Obj *obj) {
    switch (obj->type) {
        case OBJ_INT:
        case OBJ_FLOAT:
            return;

        case OBJ_FUNC: {
            FuncObj *func = AS_PTR(obj, FuncObj);
            FREE_DA(&func->bytecode);
            FREE_DA(&func->constPool);
            FREE_DA(&func->positions);
            break;
        }

        default: UNREACHABLE_ERROR();
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
