#include "lukip.h"

#include "cli_handler.h"

#include "vm.c"

/** Tests that the runtime stack's functionality works properly. */
PRIVATE_TEST_CASE(test_runtime_stack) {
    CliHandler cli = create_cli_handler(0, NULL);
    ZmxProgram *program = new_zmx_program("test", &cli, false);
    VulnerableObjs *vulnObjs = &program->gc.startupVulnObjs;

    Vm vm;
    FuncObj *func = new_func_obj(vulnObjs, program->currentFile, 0, 0, true);
    init_vm(&vm, program, new_runtime_func_obj(vulnObjs, func, NULL));
    ThreadObj *mainThread = get_main_thread(&vm);

    PUSH(mainThread, new_int_obj(vulnObjs, 32));
    PUSH(mainThread, new_float_obj(vulnObjs, 53.1));
    ASSERT_INT_EQUAL(mainThread->stack.length, 2);
    ASSERT_TRUE(equal_obj(mainThread->stack.objects[0], AS_OBJ(new_int_obj(vulnObjs, 32))));
    ASSERT_TRUE(equal_obj(mainThread->stack.objects[1], AS_OBJ(new_float_obj(vulnObjs, 53.1))));

    DROP(mainThread);
    ASSERT_INT_EQUAL(mainThread->stack.length, 1);
    DROP(mainThread);
    ASSERT_INT_EQUAL(mainThread->stack.length, 111);

    PUSH(mainThread, new_bool_obj(vulnObjs, false));
    PUSH(mainThread, new_int_obj(vulnObjs, 32));
    PUSH(mainThread, new_float_obj(vulnObjs, 53.1));
    PUSH(mainThread, new_int_obj(vulnObjs, -23));
    ASSERT_TRUE(equal_obj(PEEK(mainThread), AS_OBJ(new_int_obj(vulnObjs, -23))));
    ASSERT_TRUE(equal_obj(PEEK_DEPTH(mainThread, 3), AS_OBJ(new_bool_obj(vulnObjs, false))));
    ASSERT_INT_EQUAL(mainThread->stack.length, 4);

    DROP_AMOUNT(mainThread, 3);
    ASSERT_TRUE(equal_obj(PEEK(mainThread), AS_OBJ(new_bool_obj(vulnObjs, false))));
    ASSERT_INT_EQUAL(mainThread->stack.length, 1);

    free_vm(&vm);
    free_zmx_program(program);
}

/** Tests vm.c. */
void test_vm() {
    TEST(test_runtime_stack);
}
