#include <stdio.h>

#include "compiler.h"
#include "gc.h"
#include "object.h"
#include "program.h"
#include "vm.h"

/** The default size of how many bytes before the first garbage collection is performed. */
#define FIRST_COLLECTION_SIZE 128 * 1024

static void mark_obj(Obj *object);

/** Creates an empty garbage collector. */
Gc create_gc(ZmxProgram *program) {
    Gc gc = {
        .startupVulnObjs = create_vulnerables(program), .vm = NULL,
        .allocated = 0, .nextCollection = FIRST_COLLECTION_SIZE, .lock = TYPE_ALLOC(Mutex)
    };
    MUTEX_INIT(gc.lock);
    return gc;
}

/** Frees the memory the garbage collector has allocated. */
void free_gc(Gc *gc) {
    MUTEX_DESTROY(gc->lock);
    free(gc->lock);
    free_vulnerables(&gc->startupVulnObjs);
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

/** Marks all vulnerable objects in a vulnerables struct.  */
static void mark_vulnerables(VulnerableObjs *vulnObjs) {
    mark_obj_array(vulnObjs->unrooted);
    mark_obj_array(vulnObjs->protected);
}

/** Marks all objects inside the thread and its structures, including its vulnerables. */
static void mark_thread(ThreadObj *thread) {
    mark_vulnerables(&thread->vulnObjs);
    mark_obj(AS_OBJ(thread->runnable));
    mark_obj(AS_OBJ(thread->module));
    mark_obj_array(thread->openCaptures);
    mark_table(thread->openIndicesSet);

    for (u32 i = 0; i < thread->stack.length; i++) {
        mark_obj(thread->stack.objects[i]);
    }
    for (u32 i = 0; i < thread->callStack.length; i++) {
        mark_obj(AS_OBJ(thread->callStack.data[i].func));
    }
    for (u32 i = 0; i < thread->catches.length; i++) {
        mark_obj(AS_OBJ(thread->catches.data[i].func));
    }   
}

/** Marks all objects inside a function params struct. */
static void mark_func_params(FuncParams params) {
    mark_obj_array(params.names);
    mark_obj_array(params.values);
}

/** Marks a full function (including its runtime context if there's one.) */
static void mark_func(FuncObj *func) {
    mark_obj(AS_OBJ(func->name));
    mark_obj(AS_OBJ(func->cls));
    mark_obj_array(func->constPool);
    mark_func_params(func->staticParams);

    if (FLAG_IS_SET(func->flags, FUNC_RUNTIME_BIT)) {
        // Runtime extension. Also marks the original static func's base so it won't get deleted.
        RuntimeFuncObj *runtimeFunc = AS_PTR(RuntimeFuncObj, func);
        FLAG_ENABLE(runtimeFunc->originalBase->flags, OBJ_REACHABLE_BIT);
        mark_obj_array(runtimeFunc->paramVals);
        mark_obj_array(runtimeFunc->captures);
    }
}

/** Mark all object roots in the passed VM (including threads). */
static void mark_vm(Vm *vm) {
    MUTEX_LOCK(&vm->threadsLock);
    mark_obj_array(vm->threadsUnsafe);
    MUTEX_UNLOCK(&vm->threadsLock);

    MUTEX_LOCK(&vm->modulesLock);
    mark_obj_array(vm->modulesUnsafe);
    MUTEX_UNLOCK(&vm->modulesLock);
}

/** Marks all the built-in objects in the program. */
static void mark_built_ins(BuiltIns *builtIn) {
    mark_table(builtIn->funcs);
    mark_table(builtIn->modules);
    
    mark_obj(AS_OBJ(builtIn->intClass));
    mark_obj(AS_OBJ(builtIn->floatClass));
    mark_obj(AS_OBJ(builtIn->stringClass));
    mark_obj(AS_OBJ(builtIn->listClass));
    mark_obj(AS_OBJ(builtIn->mapClass));
    mark_obj(AS_OBJ(builtIn->fileClass));
    mark_obj(AS_OBJ(builtIn->threadClass));
    mark_obj(AS_OBJ(builtIn->lockClass));
}

/** Marks all the objects stored in a program. */
static void mark_program(ZmxProgram *program) {
    mark_obj(AS_OBJ(program->currentFile));
    mark_obj(AS_OBJ(program->internedNull));
    mark_obj(AS_OBJ(program->internedTrue));
    mark_obj(AS_OBJ(program->internedFalse));
    mark_built_ins(&program->builtIn);
}

/** Marks the passed object and the objects it has inside it recursively. */
static void mark_obj(Obj *object) {
    if (
        object == NULL
        || FLAG_IS_SET(object->flags, OBJ_REACHABLE_BIT)
        || !FLAG_IS_SET(object->flags, OBJ_INITIALIZED_BIT)
    ) {
        return; // doesn't exist, is already marked reachable, or is not even intialized yet.
    }
    FLAG_ENABLE(object->flags, OBJ_REACHABLE_BIT);

#if DEBUG_LOG_GC
    printf("Mark %p | %s (", (void*)object, obj_type_str(object->type));
    print_obj(object, true);
    printf(")\n");
#endif
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
        mark_obj(AS_OBJ(AS_PTR(ModuleObj, object)->importedBy));

        MUTEX_LOCK(&(AS_PTR(ModuleObj, object)->globalsLock));
        mark_table(AS_PTR(ModuleObj, object)->globalsUnsafe);
        MUTEX_UNLOCK(&(AS_PTR(ModuleObj, object)->globalsLock));
        break;
    case OBJ_FILE:
        mark_obj(AS_OBJ(AS_PTR(FileObj, object)->path));
        mark_table(AS_PTR(FileObj, object)->fields);
        break;
    case OBJ_THREAD:
        mark_thread(AS_PTR(ThreadObj, object));
        break;
    case OBJ_FUNC:
        mark_func(AS_PTR(FuncObj, object));
        break;
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
    case OBJ_LOCK:
        break;
    TOGGLEABLE_DEFAULT_UNREACHABLE();
    }
}

/** Marks all reachable objects in the program. */
static void gc_mark(ZmxProgram *program) {
    Gc *gc = &program->gc;

    mark_vulnerables(&gc->startupVulnObjs);
    mark_program(program);
    if (gc->vm) {
        mark_vm(program->gc.vm);
    }
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
 * element (which may also need to be deleted) back 1 spot, making us never iterate over that key.
 * For example, if key 2 and 3 are both to be deleted, and key 3 has a PSL > 0,
 * then directly deleting key 2 will shift key 3 to its spot, while the loop changes the i to 3,
 * meaning that the loop skips over key 3 entirely.
 */
static void table_delete_weak_references(Table *table) {
    ObjArray deletedKeys = CREATE_DA();
    for (u32 i = 0; i < table->capacity; i++) {
        Entry *entry = &table->entries[i];
        if (!EMPTY_ENTRY(entry) && !FLAG_IS_SET(entry->key->flags, OBJ_REACHABLE_BIT)) {
            PUSH_DA(&deletedKeys, entry->key);
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
        // Don't free if it's been marked as reachable, or isn't even initialized.
        if (
            FLAG_IS_SET(current->flags, OBJ_REACHABLE_BIT)
            || !FLAG_IS_SET(current->flags, OBJ_INITIALIZED_BIT)
        ) {
            FLAG_DISABLE(current->flags, OBJ_REACHABLE_BIT); // Prepare for next GC.
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

    MUTEX_LOCK(program->gc.lock);
    gc_mark(program);

    table_delete_weak_references(&program->internedStrings);
    gc_sweep(program);
    MUTEX_UNLOCK(program->gc.lock);

#if DEBUG_LOG_GC
    printf("========================== GC end. ==========================\n");
#endif
}
