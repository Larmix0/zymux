#include "lukip.h"

#include "vm.c"

/** Tests that the runtime stack's functionality works properly. */
PRIVATE_TEST_CASE(test_runtime_stack) {
    Vm createVm = {.stack = CREATE_STACK()};
    ASSERT_INT_EQUAL(createVm.stack.capacity, 0);
    ASSERT_NULL(createVm.stack.objects);

    ZmxProgram program = create_zmx_program("test", false);
    GC_PUSH_PROTECTION(&program.gc);
    Vm vm = create_vm(&program, new_func_obj(&program, program.currentFile, 0, 0));

    PUSH(&vm, AS_OBJ(new_int_obj(&program, 32)));
    PUSH(&vm, AS_OBJ(new_float_obj(&program, 53.1)));
    ASSERT_INT_EQUAL(STACK_LENGTH(&vm), 2);
    ASSERT_TRUE(equal_obj(vm.stack.objects[0], AS_OBJ(new_int_obj(&program, 32))));
    ASSERT_TRUE(equal_obj(vm.stack.objects[1], AS_OBJ(new_float_obj(&program, 53.1))));

    DROP(&vm);
    ASSERT_INT_EQUAL(STACK_LENGTH(&vm), 1);
    Obj *popped = POP(&vm);
    ASSERT_TRUE(equal_obj(popped, AS_OBJ(new_int_obj(&program, 32))));
    ASSERT_INT_EQUAL(STACK_LENGTH(&vm), 0);

    PUSH(&vm, AS_OBJ(new_bool_obj(&program, false)));
    PUSH(&vm, AS_OBJ(new_int_obj(&program, 32)));
    PUSH(&vm, AS_OBJ(new_float_obj(&program, 53.1)));
    PUSH(&vm, AS_OBJ(new_int_obj(&program, -23)));
    ASSERT_TRUE(equal_obj(PEEK(&vm), AS_OBJ(new_int_obj(&program, -23))));
    ASSERT_TRUE(equal_obj(PEEK_DEPTH(&vm, 3), AS_OBJ(new_bool_obj(&program, false))));
    DROP_AMOUNT(&vm, 3);
    ASSERT_TRUE(equal_obj(PEEK(&vm), AS_OBJ(new_bool_obj(&program, false))));

    free_vm(&vm);
    free_zmx_program(&program);
}

/** Tests vm.c. */
void test_vm() {
    TEST(test_runtime_stack);
}
