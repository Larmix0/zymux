#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include "object.h"

/** Returns whether or not a given object is hashable. */
bool is_hashable(Obj *object);

/** 
 * Returns the hash value of an object assuming it is hashable.
 * 
 * If the object passed is not hashable it'll become an unreachable error.
 */
u32 hash_obj(Obj *object);

#endif
