#include "lukip.h"

#include "cli_handler.h"
#include "program.h"

#include "hash_table.c"

#define NEW_TEST_STRING(program, string) (new_string_obj(program, string, strlen(string)))

static VulnerableObjs *defaultVulnObjs; /** Default vulnerables for objects in the hash tables. */
static Table defaultTable; /** Default table for testing. */

/** Setup for initializing the global hash table. */
DECLARE_SETUP(table_setup) {
    defaultTable = create_table();

    CliHandler cli = create_cli_handler(0, NULL);
    ZmxProgram *program = new_zmx_program("Table test.", &cli, false);
    defaultVulnObjs = &program->gc.startupVulnObjs;
}

/** Setup for freeing the global hash table. */
DECLARE_TEARDOWN(table_teardown) {
    free_table(&defaultTable);
    free_zmx_program(defaultVulnObjs->program);
}

/** Tests that the function for getting a key string object from a C string works. */
PRIVATE_TEST_CASE(test_table_get_string) {
    expand_table(&defaultTable);
    char *something = "Something.";
    char *name = "Name";
    Obj *somethingAsObj = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, something));
    Obj *nameAsObj = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, name));
    ASSERT_NULL(get_string_table_key(
        &defaultTable, something, strlen(something), hash_string(something, strlen(something))
    ));
    ASSERT_NULL(get_string_table_key(
        &defaultTable, name, strlen(name), hash_string(name, strlen(name))
    ));

    table_set(&defaultTable, somethingAsObj, AS_OBJ(new_null_obj(defaultVulnObjs)));
    Obj *foundSomething = get_string_table_key(
        &defaultTable, something, strlen(something), hash_string(something, strlen(something))
    );
    ASSERT_NOT_NULL(foundSomething);
    ASSERT_STRING_EQUAL(AS_PTR(StringObj, foundSomething)->string, something);

    table_set(&defaultTable, nameAsObj, AS_OBJ(new_null_obj(defaultVulnObjs)));
    Obj *foundName = get_string_table_key(
        &defaultTable, name, strlen(name), hash_string(name, strlen(name))
    );
    ASSERT_NOT_NULL(foundName);
    ASSERT_STRING_EQUAL(AS_PTR(StringObj, foundName)->string, name);

    table_delete(&defaultTable, somethingAsObj);
    ASSERT_NULL(get_string_table_key(
        &defaultTable, something, strlen(something), hash_string(something, strlen(something))
    ));
    foundName = get_string_table_key(
        &defaultTable, name, strlen(name), hash_string(name, strlen(name))
    );
    ASSERT_NOT_NULL(foundName);
    ASSERT_STRING_EQUAL(AS_PTR(StringObj, foundName)->string, name);
}

/** Tests that the table's deletion works properly and back shifts elements if optimal. */
PRIVATE_TEST_CASE(test_table_delete) {
    expand_table(&defaultTable);
    Obj *firstKey = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "First."));
    Obj *firstValue = AS_OBJ(new_int_obj(defaultVulnObjs, 2));
    table_set(&defaultTable, firstKey, firstValue);
    u32 firstIndex = table_key_entry(&defaultTable, firstKey) - defaultTable.entries;

    Entry *manuallySet = &defaultTable.entries[GET_ENTRY_IDX(firstIndex + 1, &defaultTable)];
    manuallySet->key = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Second."));
    manuallySet->value = AS_OBJ(new_int_obj(defaultVulnObjs, 2));
    manuallySet->psl = 1;

    manuallySet = &defaultTable.entries[GET_ENTRY_IDX(firstIndex + 2, &defaultTable)];
    manuallySet->key = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Third."));
    manuallySet->value = AS_OBJ(new_int_obj(defaultVulnObjs, 3));
    manuallySet->psl = 0;

    ASSERT_NOT_NULL(table_get(&defaultTable, firstKey));
    ASSERT_TRUE(table_delete(&defaultTable, firstKey));
    ASSERT_NULL(table_get(&defaultTable, firstKey));

    Entry *movedSecond = &defaultTable.entries[GET_ENTRY_IDX(firstIndex, &defaultTable)];
    ASSERT_TRUE(equal_obj(movedSecond->key, AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Second."))));
    ASSERT_INT_EQUAL(movedSecond->psl, 0);

    Entry *sameThird = &defaultTable.entries[GET_ENTRY_IDX(firstIndex + 2, &defaultTable)];
    ASSERT_TRUE(
        sameThird->key != NULL
        && equal_obj(sameThird->key, AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Third.")))
    );
}

/** Tests that the table's set adds the key value pair or changes it if it was already set. */
PRIVATE_TEST_CASE(test_table_set) {
    expand_table(&defaultTable);
    Obj *key = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "To set."));
    Obj *originalValue = AS_OBJ(new_bool_obj(defaultVulnObjs, true));
    Obj *modifiedValue = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Changed."));

    Obj *beforeAdding = table_get(&defaultTable, key);
    ASSERT_NULL(beforeAdding);

    table_set(&defaultTable, key, originalValue);
    Obj *afterAdding = table_get(&defaultTable, key);
    ASSERT_TRUE(afterAdding != NULL && equal_obj(afterAdding, originalValue));

    table_set(&defaultTable, key, modifiedValue);
    Obj *afterModifying = table_get(&defaultTable, key);
    ASSERT_TRUE(afterModifying != NULL && equal_obj(afterModifying, modifiedValue));
}

/** Tests that the table's get performs the operation correctly by manually inserting elements. */
PRIVATE_TEST_CASE(test_table_get) {
    expand_table(&defaultTable);
    Obj *key = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "A key."));
    Obj *value = AS_OBJ(new_int_obj(defaultVulnObjs, 234));

    Obj *before = table_get(&defaultTable, key);
    ASSERT_NULL(before);

    Entry *manuallySet = &defaultTable.entries[GET_ENTRY_IDX(get_hash(key), &defaultTable)];
    manuallySet->key = key;
    manuallySet->value = value;
    manuallySet->psl = 1;

    Obj *after = table_get(&defaultTable, key);
    ASSERT_TRUE(after != NULL && equal_obj(after, value));
}

/** Tests that we correctly get the corresponding entry from a key, empty or not. */
PRIVATE_TEST_CASE(test_get_entry_of_key) {
    expand_table(&defaultTable);
    Obj *key = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Key."));
    Obj *value = AS_OBJ(new_float_obj(defaultVulnObjs, 11.2));

    Entry *beforeAdding = table_key_entry(&defaultTable, key);
    ASSERT_NULL(beforeAdding->key);
    ASSERT_NULL(beforeAdding->value);
    ASSERT_INT_EQUAL(beforeAdding->psl, 0);

    Entry *manuallySet = &defaultTable.entries[GET_ENTRY_IDX(get_hash(key), &defaultTable)];
    manuallySet->key = key;
    manuallySet->value = value;
    manuallySet->psl = 1;

    Entry *afterAdding = table_key_entry(&defaultTable, key);
    ASSERT_TRUE(afterAdding->key != NULL && equal_obj(afterAdding->key, key));
    ASSERT_TRUE(afterAdding->value != NULL && equal_obj(afterAdding->value, value));
    ASSERT_INT_EQUAL(beforeAdding->psl, 1);
}

/** 
 * Tests that expanding the table works.
 * 
 * This includes both expanding an empty table into a sized table, and expanding that sized table
 * into a bigger one while not losing the elements of the original table, as resizing the table
 * requires re-inserting elements into it due to using (hash % size), where size has changed.
 */
PRIVATE_TEST_CASE(test_expand_table) {
    expand_table(&defaultTable);
    ASSERT_INT_EQUAL(defaultTable.capacity, MINIMUM_CAPACITY);
    ASSERT_INT_EQUAL(defaultTable.count, 0);
    for (u32 i = 0; i < defaultTable.capacity; i++) {
        Entry *entry = &defaultTable.entries[i];
        ASSERT_NULL(entry->key);
        ASSERT_NULL(entry->value);
        ASSERT_INT_EQUAL(entry->psl, 0);
    }

    Entry *manuallySet = &defaultTable.entries[defaultTable.capacity / 2];
    manuallySet->key = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Manual."));
    manuallySet->value = AS_OBJ(new_bool_obj(defaultVulnObjs, true));
    manuallySet->psl = 3;

    expand_table(&defaultTable);
    bool foundManuallySet = false;
    for (u32 i = 0; i < defaultTable.capacity; i++) {
        Entry *entry = &defaultTable.entries[i];
        if (entry->key == NULL) {
            ASSERT_NULL(defaultTable.entries[i].key);
            ASSERT_NULL(defaultTable.entries[i].value);
            ASSERT_INT_EQUAL(defaultTable.entries[i].psl, 0);
            continue;
        }
        ASSERT_FALSE(foundManuallySet);
        foundManuallySet = true;
        ASSERT_TRUE(equal_obj(entry->key, AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Manual."))));
        ASSERT_TRUE(equal_obj(entry->value, AS_OBJ(new_bool_obj(defaultVulnObjs, true))));
        // Don't assert PSL as it might change while expanding and moving to new locations.
    }
}

/** Tests that the functions for swapping the fields of 2 entries works properly. */
PRIVATE_TEST_CASE(test_swap_entries) {
    Entry left = {
        .key = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Left.")),
        .value = AS_OBJ(new_int_obj(defaultVulnObjs, 111)),
        .psl = 232
    };
    Entry right = {
        .key = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Right.")),
        .value = AS_OBJ(new_bool_obj(defaultVulnObjs, false)),
        .psl = 909
    };
    swap_entries(&left, &right);
    ASSERT_TRUE(equal_obj(left.key, AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Right."))));
    ASSERT_TRUE(equal_obj(left.value, AS_OBJ(new_bool_obj(defaultVulnObjs, false))));
    ASSERT_INT_EQUAL(left.psl, 909);
    ASSERT_TRUE(equal_obj(right.key, AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Left."))));
    ASSERT_TRUE(equal_obj(right.value, AS_OBJ(new_int_obj(defaultVulnObjs, 111))));
    ASSERT_INT_EQUAL(right.psl, 232);
}

/** Tests that the function for emptying entries works. */
PRIVATE_TEST_CASE(test_make_entry_empty) {
    Entry entry = {
        .key = AS_OBJ(NEW_TEST_STRING(defaultVulnObjs, "Test.")),
        .value = AS_OBJ(new_int_obj(defaultVulnObjs, 2)),
        .psl = 32
    };
    make_entry_empty(&entry);
    ASSERT_NULL(entry.key);
    ASSERT_NULL(entry.value);
    ASSERT_INT_EQUAL(entry.psl, 0);
}

/** Tests dynamic_array.c/.h. */
void test_hash_table() {
    MAKE_FIXTURE(table_setup, table_teardown);
    TEST(test_make_entry_empty);
    TEST(test_swap_entries);
    TEST(test_expand_table);
    TEST(test_get_entry_of_key);
    TEST(test_table_get);
    TEST(test_table_set);
    TEST(test_table_delete);
    TEST(test_table_get_string);
    RESET_FIXTURE();
}
