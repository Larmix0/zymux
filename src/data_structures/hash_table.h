#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include <stdlib.h>

#include "object.h"

/** How much should a hash table grow per allocation (used with multiplication). */
#define TABLE_GROWTH_FACTOR 2

/** Minimum capacity of a hash table. */
#define TABLE_MIN_CAP 16

/** Macro for increasing the capacity of a hash table. */
#define TABLE_INCREASE_CAP(capacity) \
    ((capacity) = (capacity) < TABLE_MIN_CAP ? TABLE_MIN_CAP : (capacity) * TABLE_GROWTH_FACTOR)

/** Evalutes a bool of whether or not the length exceeds the allowed amount relative to capacity. */
#define TABLE_OVER_MAX_LOAD(table) ((table)->count + 1 > (table)->capacity * 0.75)

/** Whether the passed entry is considered empty. */
#define EMPTY_ENTRY(entry) ((entry)->key == NULL && (entry)->value == NULL)

/** 
 * An entry in the hash table consisting of key-value pairs and the PSL for robin hood hashing.
 * 
 * PSL (Probe Sequence Length) is how many places away the entry is from its desired spot.
 */
typedef struct {
    Obj *key;
    Obj *value;
    u32 psl;
} Entry;

/** A hash table of object key value pair entries. */
typedef struct {
    u32 count; /** How many elements are set in the table (excludes empty ones). */
    u32 capacity; /** How many elements including filled and empty ones are in the table. */
    Entry *entries; /** The array of key-value pair entries. */
} Table;

/** Returns whether or not a given object is hashable. */
bool is_hashable(Obj *object);

/** 
 * Returns the hash value of an object assuming it is hashable.
 * 
 * If the object passed is not hashable it'll become an unreachable error.
 */
u32 hash_obj(Obj *object);

/** Creates an empty hash table. */
Table create_table();

/** 
 * Gets the corresponding value object from the passed key.
 * 
 * Returns NULL if the key doesn't exist in the table.
 */
Obj *table_get(Table *table, Obj *key);

/** Sets a key value pair in the passed hash table. */
void table_set(Table *table, Obj *key, Obj *value);

/** 
 * Deletes an entry from the table and returns whether or not the deletion was successful.
 * 
 * The deletion can fail in the case of the key being deleted not existing for example.
 */
bool table_delete(Table *table, Obj *key);

/** Frees the memory allocated by the passed hash table. */
void free_table(Table *table);

#endif
