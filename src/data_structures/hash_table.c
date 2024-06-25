#include <stdbool.h>

#include "hash_table.h"
#include "report_error.h"

/** A number index in the table wrapped around like modulo (but more efficiently). */
#define GET_ENTRY_IDX(index, table) ((index) & ((table)->capacity - 1))

void table_set(Table *table, Obj *key, Obj *value);

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

/** Returns the hash of the passed object. */
u32 get_hash(Obj *object) {
    return hash_obj(object); // TODO: change this later when some hashes are stored on objects.
}

/** Creates an empty hash table. */
Table create_table() {
    Table table = {.count = 0, .capacity = 0, .entries = NULL};
    return table;
}

/** Swaps all fields of the passed entries. */
static void swap_entries(Entry *left, Entry *right) {
    Entry oldLeft = {.key = left->key, .value = left->value, .psl = left->psl};
    left->key = right->key;
    left->value = right->value;
    left->psl = right->psl;

    right->key = oldLeft.key;
    right->value = oldLeft.value;
    right->psl = oldLeft.psl;
}

/** Turns the passed entry into an empty one. */
static void make_entry_empty(Entry *entry) {
    entry->key = NULL;
    entry->value = NULL;
    entry->psl = 0;
}

/** 
 * Remakes the current table with higher capacity.
 * 
 * This means the key value pairs are re-positioned due to the size increase of the hash table,
 * which causes the hashed keys to end up in different places instead of wrapping around earlier.
 */
static void adjust_capacity(Table *oldTable) {
    Table newTable = create_table();
    newTable.capacity = oldTable->capacity;
    INCREASE_CAPACITY(newTable.capacity);

    newTable.entries = ARRAY_ALLOC(newTable.capacity, Entry);
    for (u32 i = 0; i < newTable.capacity; i++) {
        make_entry_empty(&newTable.entries[i]);
    }
    // Copy the key value pairs from the old to the new table.
    for (u32 i = 0; i < oldTable->capacity; i++) {
        Entry *entry = &oldTable->entries[i];
        if (entry->key != NULL) {
            table_set(&newTable, entry->key, entry->value);
        }
    }
    free_table(oldTable);
    *oldTable = newTable;
}

/** Gets the passed key's respective entry in the hash table, empty or not. */
static Entry *get_entry_of_key(Table *table, Obj *key) {
    if (table->entries == NULL) {
        adjust_capacity(table); // The whole table was a NULL, so we set it to return a NULL entry.
    }
    
    u32 index = GET_ENTRY_IDX(get_hash(key), table);
    while (true) {
        Entry *entry = &table->entries[index];
        if (EMPTY_ENTRY(entry)) {
            return entry;
        } else if (equal_obj(entry->key, key)) {
            return entry;
        }
        index = GET_ENTRY_IDX(index + 1, table);
    }
}

/** 
 * Gets the corresponding value object from the passed key.
 * 
 * Returns NULL if the key doesn't exist in the table.
 */
Obj *table_get(Table *table, Obj *key) {
    Entry *entry = get_entry_of_key(table, key);
    if (EMPTY_ENTRY(entry)) {
        return NULL;
    }

    return entry->value;
}

/** 
 * Sets a key value pair in the passed hash table.
 * 
 * When using robin hood hashing, we increase the current PSL as we're moving through the
 * non-empty elements.
 * Then, if we have a PSL of the current key value pair to be inserted being bigger than the
 * element's place where it wants to go, then we swap between the key-value pairs,
 * and look for a new place for that element that had a low PSL (low PSL is good).
 * 
 * At the end, whatever key value pair is at the end when we find an empty entry or an entry
 * with the same key, we'll set that to the last key value pair we were looking to insert.
 */
void table_set(Table *table, Obj *key, Obj *value) {
    if (TABLE_OVER_MAX_LOAD(table)) {
        adjust_capacity(table);
    }

    u32 index = GET_ENTRY_IDX(get_hash(key), table);
    Entry toInsert = {.key = key, .value = value, .psl = 0};
    Entry *current;
    while (true) {
        current = &table->entries[index];
        if (EMPTY_ENTRY(current)) {
            table->count++; // It was a new empty entry, so add to count.
            break;
        } else if (equal_obj(current->key, toInsert.key)) {
            break;
        }
        
        if (toInsert.psl > current->psl) {
            swap_entries(current, &toInsert); // Swap the poor toInsert with the rich current.
        }
        toInsert.psl++;
        index = GET_ENTRY_IDX(index + 1, table);
    }
    *current = toInsert;
}

/** 
 * Deletes an entry from the table and returns whether or not the deletion was successful.
 * 
 * The deletion can fail in the case of the key being deleted not existing for example.
 * 
 * It's done with backwards shifting instead of the usual tombstone approach. Backwards shifting
 * is moving each element in front of the removed element one spot back since it's empty if they'd
 * like to go back (the removed element was forcing them to go an extra place forward).
 */
bool table_delete(Table *table, Obj *key) {
    Entry *entry = get_entry_of_key(table, key);
    if (EMPTY_ENTRY(entry)) {
        return false; // Empty.
    }

    make_entry_empty(entry);
    table->count--;
    u32 index = GET_ENTRY_IDX(entry - table->entries, table); // Index of element we removed.
    Entry *current, *next;
    // Backwards shift until 0 PSL (well-placed or empty element). 
    while (true) {
        next = &table->entries[GET_ENTRY_IDX(index + 1, table)];
        if (next->psl == 0) {
            return true;
        }

        current = &table->entries[index];
        *current = *next;
        current->psl--;
        index = GET_ENTRY_IDX(index + 1, table);
        make_entry_empty(next);
    }
    return true;
}

/** Frees the memory allocated by the passed hash table. */
void free_table(Table *table) {
    free(table->entries);
}
