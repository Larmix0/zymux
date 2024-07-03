#include "lukip.h"

#include "vm.c"

/** Tests that the runtime stack's functionality works properly. */
PRIVATE_TEST_CASE(test_runtime_stack) {
    Vm createVm = {.stack = CREATE_STACK()};
    ASSERT_INT_EQUAL(createVm.stack.capacity, 0);
    ASSERT_NULL(createVm.stack.objects);

    ZmxProgram program = create_zmx_program("test", false);
    program.gc.protectNewObjs = true;
    Vm vm = create_vm(&program, new_func_obj(&program, new_string_obj(&program, "funcName"), -1));

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

    PUSH(&vm, AS_OBJ(new_string_obj(&program, "String.")));
    PUSH(&vm, AS_OBJ(new_int_obj(&program, 32)));
    PUSH(&vm, AS_OBJ(new_float_obj(&program, 53.1)));
    PUSH(&vm, AS_OBJ(new_int_obj(&program, -23)));
    ASSERT_TRUE(equal_obj(PEEK(&vm), AS_OBJ(new_int_obj(&program, -23))));
    ASSERT_TRUE(equal_obj(PEEK_DEPTH(&vm, 3), AS_OBJ(new_string_obj(&program, "String."))));
    DROP_AMOUNT(&vm, 3);
    ASSERT_TRUE(equal_obj(PEEK(&vm), AS_OBJ(new_string_obj(&program, "String."))));

    free_vm(&vm);
    free_zmx_program(&program);
}

/** Tests that copy_const_indices copies them properly. */
PRIVATE_TEST_CASE(test_copy_const_indices) {
    ZmxProgram program = create_zmx_program("test", false);
    program.gc.protectNewObjs = true;
    
    CallStack callStack = CREATE_DA();
    APPEND_DA(
        &callStack,
        create_stack_frame(new_func_obj(&program, new_string_obj(&program, "First."), -1), NULL)
    );
    APPEND_DA(
        &callStack,
        create_stack_frame(new_func_obj(&program, new_string_obj(&program, "Second."), 4), NULL)
    );
    APPEND_DA(
        &callStack,
        create_stack_frame(new_func_obj(&program, new_string_obj(&program, "Third."), 27), NULL)
    );
    APPEND_DA(
        &callStack,
        create_stack_frame(new_func_obj(&program, new_string_obj(&program, "Fourth."), 0), NULL)
    );

    IntArray constants = copy_const_indices(callStack);
    ASSERT_INT_EQUAL(constants.length, 3);
    ASSERT_INT_EQUAL(constants.data[0], 4);
    ASSERT_INT_EQUAL(constants.data[1], 27);
    ASSERT_INT_EQUAL(constants.data[2], 0);

    FREE_DA(&callStack);
    FREE_DA(&constants);
    free_zmx_program(&program);
}

/** Tests vm.c. */
void test_vm() {
    TEST(test_runtime_stack);
    TEST(test_copy_const_indices);
}
