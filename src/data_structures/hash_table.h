#ifndef HASH_TABLE_H
#define HASH_TABLE_H

#include <stdbool.h>
#include <stdlib.h>

#include "constants.h"

typedef struct Obj Obj;
typedef struct VulnerableObjs VulnerableObjs;

/** A bool of whether or not the length exceeds the allowed amount relative to capacity. */
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
bool is_hashable(const Obj *object);

/** Hashes a string using the DJB2 hashing function. */
u32 hash_string(const char *string, const u32 length);

/** 
 * Returns the hash value of an object assuming it is hashable and immutable.
 * 
 * If the object passed is not hashable it'll become an unreachable error.
 */
u32 get_hash(const Obj *object);

/** Creates an empty hash table. */
Table create_table();

/** Sets each entry in the "from" table on the "to" table, essentially copying all its entries. */
void copy_entries(const Table *from, Table *to);

/** Gets the passed key's entire respective entry in the hash table, empty or not. */
Entry *table_key_entry(Table *table, Obj *key);

/** 
 * Gets the corresponding value object from the passed key.
 * 
 * Returns NULL if the key doesn't exist in the table.
 */
Obj *table_get(Table *table, Obj *key);

/** 
 * Sets a key value pair in the passed hash table.
 * 
 * If the key already exists, then it modified the value, otherwise creates a new key-value entry.
 */
void table_set(Table *table, Obj *key, Obj *value);

/** 
 * Deletes an entry from the table and returns whether or not the deletion was successful.
 * 
 * The deletion can fail in the case of the key being deleted not existing for example.
 */
bool table_delete(Table *table, Obj *key);

/** 
 * Performs a get operation on a table using a C-string (converted to object in function).
 * 
 * Assumes the passed string is terminated.
 */
Obj *table_string_get(VulnerableObjs *vulnObjs, Table *table, const char *keyString);

/** Performs a get operation on a table using a number directly. */
Obj *table_int_get(VulnerableObjs *vulnObjs, Table *table, const ZmxInt keyNumber);

/** 
 * Sets a key value pair in a hash table of key string to value objects.
 * 
 * This is for convenience when inserting a key-value pair into a table where all keys are
 * strings anyway. The passed string is assumed to be terminated.
 */
void table_string_set(VulnerableObjs *vulnObjs, Table *table, const char *keyString, Obj *value);

/** Sets a key value pair on a table of key numbers. Convenience for passing the number directly. */
void table_int_set(VulnerableObjs *vulnObjs, Table *table, const ZmxInt keyNumber, Obj *value);

/**
 * Deletes a string object off of the table using a passed C-string (assumed to be terminated).
 * 
 * Returns whether or not it succeeded.
 */
bool table_string_delete(VulnerableObjs *vulnObjs, Table *table, const char *keyString);

/** 
 * Deletes an integer object off of the table using a passed C integer.
 * 
 * Returns whether or not it succeeded
 */
bool table_int_delete(VulnerableObjs *vulnObjs, Table *table, const ZmxInt keyNumber);

/** Returns the string key in a hash table if it exists, otherwise returns NULL. */
Obj *table_get_string_key(Table *table, const char *string, const u32 length, const u32 hash);

/** Frees the memory allocated by the passed hash table. */
void free_table(Table *table);

#endif
