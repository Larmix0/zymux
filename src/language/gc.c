#include <stdio.h>

#include "compiler.h"
#include "gc.h"
#include "object.h"
#include "program.h"
#include "vm.h"

/** The default size of how many bytes before the first garbage collection is performed. */
#define FIRST_COLLECTION_SIZE 128 * 1024

static void mark_obj_array(ObjArray objects);
static void mark_table(Table table);

/** Creates an empty garbage collector. */
Gc create_gc() {
    Gc gc = {
        .compiler = NULL, .vm = NULL,
        .allocated = 0, .nextCollection = FIRST_COLLECTION_SIZE,
        .frozenLayers = 0, .frozen = CREATE_DA(), .protected = CREATE_DA()
    };
    return gc;
}

/** Frees the memory the garbage collector has allocated. */
void free_gc(Gc *gc) {
    FREE_DA(&gc->frozen);
    FREE_DA(&gc->protected);
}

/** Marks all objects inside a function params struct. */
static void mark_func_params(FuncParams params) {
    mark_obj_array(params.names);
    mark_obj_array(params.values);
}

/** Marks the passed object and the objects it has inside it recursively. */
static void mark_obj(Obj *object) {
    if (object == NULL || object->isReachable) {
        return; // Already marked or doesn't exist.
    }

#if DEBUG_LOG_GC
    printf("Mark %p | %s (", (void*)object, obj_type_str(object->type));
    print_obj(object, true);
    printf(")\n");
#endif
    object->isReachable = true;
    switch (object->type) {
    case OBJ_LIST:
        mark_obj_array(AS_PTR(ListObj, object)->items);
        break;
    case OBJ_MAP:
        mark_table(AS_PTR(MapObj, object)->table);
        break;
    case OBJ_ENUM:
        mark_obj_array(AS_PTR(EnumObj, object)->members);
        mark_table(AS_PTR(EnumObj, object)->lookupTable);
        mark_obj(AS_OBJ(AS_PTR(EnumObj, object)->name));
        break;
    case OBJ_ENUM_MEMBER:
        mark_obj(AS_OBJ(AS_PTR(EnumMemberObj, object)->enumObj));
        mark_obj(AS_OBJ(AS_PTR(EnumMemberObj, object)->name));
        break;
    case OBJ_ITERATOR:
        mark_obj(AS_OBJ(AS_PTR(IteratorObj, object)->iterable));
        break;
    case OBJ_MODULE:
        mark_obj(AS_OBJ(AS_PTR(ModuleObj, object)->path));
        mark_table(AS_PTR(ModuleObj, object)->globals);
        break;
    case OBJ_FILE:
        mark_obj(AS_OBJ(AS_PTR(FileObj, object)->path));
        mark_table(AS_PTR(FileObj, object)->fields);
        break;
    case OBJ_FUNC: {
        FuncObj *func = AS_PTR(FuncObj, object);
        mark_obj(AS_OBJ(func->name));
        mark_obj(AS_OBJ(func->cls));
        mark_obj_array(func->constPool);
        mark_func_params(func->params);
        if (func->hasOptionals || func->isClosure) {
            // Mark the closure context of a function with a closure extension.
            RuntimeFuncObj *runtimeFunc = AS_PTR(RuntimeFuncObj, object);
            mark_obj_array(runtimeFunc->paramVals);
            mark_obj_array(runtimeFunc->captures);
        }
        break;
    }
    case OBJ_CAPTURED:
        mark_obj(AS_PTR(CapturedObj, object)->captured);
        break;
    case OBJ_CLASS:
        mark_obj(AS_OBJ(AS_PTR(ClassObj, object)->name));
        mark_obj(AS_OBJ(AS_PTR(ClassObj, object)->superclass));
        mark_obj(AS_OBJ(AS_PTR(ClassObj, object)->init));
        mark_table(AS_PTR(ClassObj, object)->methods);
        mark_obj_array(AS_PTR(ClassObj, object)->abstractMethods);
        break;
    case OBJ_INSTANCE: 
        mark_obj(AS_OBJ(AS_PTR(InstanceObj, object)->cls));
        mark_table(AS_PTR(InstanceObj, object)->fields);
        break;
    case OBJ_METHOD:
        mark_obj(AS_OBJ(AS_PTR(MethodObj, object)->instance));
        mark_obj(AS_OBJ(AS_PTR(MethodObj, object)->func));
        break;
    case OBJ_NATIVE_FUNC:
        mark_obj(AS_OBJ(AS_PTR(NativeFuncObj, object)->name));
        mark_func_params(AS_PTR(NativeFuncObj, object)->params);
        break;
    case OBJ_NATIVE_METHOD:
        mark_obj(AS_OBJ(AS_PTR(NativeMethodObj, object)->instance));
        mark_obj(AS_OBJ(AS_PTR(NativeMethodObj, object)->cls));
        mark_obj(AS_OBJ(AS_PTR(NativeMethodObj, object)->func));
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
static void mark_table(Table table) {
    for (u32 i = 0; i < table.capacity; i++) {
        mark_obj(table.entries[i].key);
        mark_obj(table.entries[i].value);
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

    for (u32 i = 0; i < vm->catches.length; i++) {
        mark_obj(AS_OBJ(vm->catches.data[i].func));
    }
    for (u32 i = 0; i < vm->modules.length; i++) {
        mark_obj_array(vm->modules.data[i].openCaptures);
        mark_table(vm->modules.data[i].globals);
    }
}

/** Marks all the built-in objects in the program. */
static void mark_built_ins(BuiltIns *builtIn) {
    mark_table(builtIn->funcs);
    
    mark_obj(AS_OBJ(builtIn->fileClass));
}

/** Marks all the objects stored in a program. */
static void mark_program(ZmxProgram *program) {
    mark_obj(AS_OBJ(program->currentFile));
    mark_obj(AS_OBJ(program->internedNull));
    mark_obj(AS_OBJ(program->internedTrue));
    mark_obj(AS_OBJ(program->internedFalse));
    mark_built_ins(&program->builtIn);
}

/** Marks all reachable objects in the program. */
static void gc_mark(ZmxProgram *program) {
    Gc *gc = &program->gc;
    mark_obj_array(gc->frozen);
    mark_obj_array(gc->protected);
    
    if (gc->compiler) {
        mark_compiler(gc->compiler);
    }
    if (gc->vm) {
        mark_vm(program->gc.vm);
    }
    mark_program(program);
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
            // Set the value to null if the key isn't getting swept but the value is.
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
#if DEBUG_LOG_GC
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
#if DEBUG_LOG_GC
    printf("========================== GC start. ==========================\n");
#endif

    gc_mark(program);
    table_delete_weak_references(&program->internedStrings, program);
    gc_sweep(program);

#if DEBUG_LOG_GC
    printf("========================== GC end. ==========================\n");
#endif
}
