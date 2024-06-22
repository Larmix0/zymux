#include <stdbool.h>

#include "hash_table.h"
#include "report_error.h"

/** Returns whether or not a given object is hashable. */
bool is_hashable(Obj *object) {
    switch (object->type) {
    case OBJ_INT:
    case OBJ_FLOAT:
    case OBJ_BOOL:
    case OBJ_STRING:
        return true;
    case OBJ_FUNC:
        return false;
    default:
        UNREACHABLE_ERROR();
    }
}

/** Hashes the 64 bit Zymux integer into an unsigned 32 bit hash number and returns it. */
static u32 hash_int(ZmxInt number) {
    return (u32)(number ^ (number >> 32));
}

/**
 * Hashes a float object.
 * 
 * This is done by inserting its bits into a union that can reinterpret its bits as an integer,
 * then hashing it as if it was an integer.
 */
static u32 hash_float(ZmxFloat floatNumber) {
    typedef struct {
        ZmxFloat asFloat;
        ZmxInt asInt;
    } NumberBits;

    NumberBits bits = {.asFloat = floatNumber};
    return hash_int(bits.asInt);
}

/** Hashes a string using the DJB2 hashing function. */
static u32 hash_string(char *string) {
    u32 hash = 5381;
    char current;
    while ((current = *string++)) {
        hash = ((hash << 5) + hash) + current;
    }
    return hash;
}

/** 
 * Returns the hash value of an object assuming it is hashable.
 * 
 * If the object passed is not hashable it'll become an unreachable error.
 */
u32 hash_obj(Obj *object) {
    switch (object->type) {
    case OBJ_INT: return hash_int(AS_PTR(IntObj, object)->number);
    case OBJ_FLOAT: return hash_float(AS_PTR(FloatObj, object)->number);
    case OBJ_STRING: return hash_string(AS_PTR(StringObj, object)->string);
    case OBJ_BOOL: return AS_PTR(BoolObj, object)->boolean ? 1 : 0;

    default:
        // Anything else shouldn't have been called to begin with.
        UNREACHABLE_ERROR();
    }
}
