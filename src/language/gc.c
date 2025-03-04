#include <stdio.h>

#include "compiler.h"
#include "gc.h"
#include "object.h"
#include "program.h"
#include "vm.h"

/** The default size of how many bytes before the first garbage collection is performed. */
#define FIRST_COLLECTION_SIZE 128 * 1024

static void mark_obj_array(ObjArray objects);
static void mark_table(Table *table);

/** Creates a garbage collector. The parameters can be NULL if they don't exist. */
Gc create_gc(Compiler *compiler, Vm *vm) {
    Gc gc = {
        .compiler = compiler, .vm = vm, .allocated = 0, .nextCollection = FIRST_COLLECTION_SIZE,
        .protected = CREATE_DA(), .protectionLayers = 0
    };
    return gc;
}

/** Creates a garbage collector whose values are immediately set to the default NULLs/0. */
Gc create_empty_gc() {
    Gc gc = {
        .compiler = NULL, .vm = NULL, .allocated = 0, .nextCollection = FIRST_COLLECTION_SIZE,
        .protected = CREATE_DA(), .protectionLayers = 0
    };
    return gc;
}

/** Frees the memory the garbage collector has allocated. */
void free_gc(Gc *gc) {
    FREE_DA(&gc->protected);
}

/** Marks the passed object and the objects it has inside it recursively. */
static void mark_obj(Obj *object) {
    if (object == NULL || object->isReachable) {
        return; // Already marked or doesn't exist.
    }

#if DEBUG_GC_PRINT
    printf("Mark %p | %s (", (void*)object, obj_type_str(object->type));
    print_obj(object, true);
    printf(")\n");
#endif
    object->isReachable = true;
    switch (object->type) {
    case OBJ_CAPTURED:
        mark_obj(AS_PTR(CapturedObj, object)->captured);
        break;
    case OBJ_FUNC: {
        FuncObj *func = AS_PTR(FuncObj, object);
        mark_obj(AS_OBJ(func->name));
        mark_obj_array(func->constPool);
        if (func->isClosure) {
            // Mark the closure context of a function with a closure extension.
            ClosureObj *closure = AS_PTR(ClosureObj, object);
            for (u32 i = 0; i < closure->captures.length; i++) {
                mark_obj(AS_OBJ(closure->captures.data[i]));
            }
        }
        break;
    }
    case OBJ_NATIVE_FUNC:
        mark_obj(AS_OBJ(AS_PTR(NativeFuncObj, object)->name));
        break;
    case OBJ_ITERATOR:
        mark_obj(AS_OBJ(AS_PTR(IteratorObj, object)->iterable));
        break;
    case OBJ_LIST:
        mark_obj_array(AS_PTR(ListObj, object)->items);
        break;
    case OBJ_MAP:
        mark_table(&AS_PTR(MapObj, object)->table);
        break;
    case OBJ_INT:
    case OBJ_FLOAT:
    case OBJ_BOOL:
    case OBJ_NULL:
    case OBJ_STRING:
    case OBJ_RANGE:
        break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
}

/** Marks a full array of objects. */
static void mark_obj_array(ObjArray objects) {
    for (u32 i = 0; i < objects.length; i++) {
        mark_obj(objects.data[i]);
    }
}

/** Marks all objects inside the passed hash table. */
static void mark_table(Table *table) {
    for (u32 i = 0; i < table->capacity; i++) {
        mark_obj(table->entries[i].key);
        mark_obj(table->entries[i].value);
    }
}

/** Mark all object roots in the passed compiler. */
static void mark_compiler(Compiler *compiler) {
    mark_obj(AS_OBJ(compiler->func));
}

/** Mark all object roots in the passed VM. */
static void mark_vm(Vm *vm) {
    for (u32 i = 0; i < STACK_LENGTH(vm); i++) {
        mark_obj(vm->stack.objects[i]);
    }
    for (u32 i = 0; i < vm->callStack.length; i++) {
        mark_obj(AS_OBJ(vm->callStack.data[i].func));
    }
    mark_table(&vm->globals);
}

/** Marks all reachable objects in the program. */
static void gc_mark(ZmxProgram *program) {
    Gc *gc = &program->gc;
    mark_obj_array(gc->protected);
    
    if (gc->compiler != NULL) {
        mark_compiler(gc->compiler);
    }
    if (gc->vm != NULL) {
        mark_vm(gc->vm);
    }
    // Mark the program's stored objects.
    mark_obj(AS_OBJ(program->currentFile));
    mark_obj(AS_OBJ(program->mainFile));
    mark_obj(AS_OBJ(program->internedNull));
    mark_obj(AS_OBJ(program->internedTrue));
    mark_obj(AS_OBJ(program->internedFalse));
    mark_table(&program->builtIn);
}

/**
 * Deletes all key value pairs of weak references inside the passed table.
 * 
 * This is made for things such as interning, where the table itself doesn't count as a root
 * (since the user can't directly make use of it), therefore its objects are getting deleted
 * despite being in the table.
 * 
 * The problem with this is that simply freeing an object doesn't remove it from the table,
 * so we end up with dangling pointers if we don't manually delete the elements from the table
 * before their memory is freed.
 * 
 * Also, we can't delete keys as we go from the table, because their deletion might shift the next
 * element (which may also need to be deleted) back 1 spot, making us never iterating over that key.
 * For example, if key 2 and 3 are both to be deleted, and key 3 has a psl > 0,
 * then directly deleting key 2 will shift key 3 to its spot, while the loop changes the i to 3,
 * meaning that the loop skips over key 3 entirely.
 */
static void table_delete_weak_references(Table *table, ZmxProgram *program) {
    ObjArray deletedKeys = CREATE_DA();
    for (u32 i = 0; i < table->capacity; i++) {
        Entry *entry = &table->entries[i];
        if (EMPTY_ENTRY(entry)) {
            continue;
        }

        if (!entry->key->isReachable) {
            APPEND_DA(&deletedKeys, entry->key);
        } else if (!entry->value->isReachable) {
            // Set the value to null if the key isn't getting sweeped but the value is.
            entry->value = AS_OBJ(new_null_obj(program));
        }
    }
    for (u32 i = 0; i < deletedKeys.length; i++) {
        table_delete(table, deletedKeys.data[i]);
    }
    FREE_DA(&deletedKeys);
}

/** Deletes all non-reachable objects, and resets reachable ones to prepare for the next GC. */
static void gc_sweep(ZmxProgram *program) {
#if DEBUG_GC_PRINT
    printf("========================== GC sweep. ==========================\n");
#endif
    if (program->allObjs == NULL) {
        return; // Nothing to sweep.
    }

    Obj *previous = NULL;
    Obj *current = program->allObjs;
    while (current != NULL) {
        if (current->isReachable) {
            current->isReachable = false;
            previous = current;
            current = current->next;
        } else if (previous == NULL) {
            program->allObjs = current->next;
            free_obj(current);
            current = program->allObjs;
        } else {
            previous->next = current->next;
            free_obj(current);
            current = previous->next;
        }
    }
}

/** Goes through the garbage collection phase by seeing reachable objects in the program's GC. */
void gc_collect(ZmxProgram *program) {
#if DEBUG_GC_PRINT
    printf("========================== GC start. ==========================\n");
#endif

    gc_mark(program);
    table_delete_weak_references(&program->internedStrings, program);
    gc_sweep(program);

#if DEBUG_GC_PRINT
    printf("========================== GC end. ==========================\n");
#endif
}
