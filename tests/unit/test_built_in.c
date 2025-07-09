#include "lukip.h"

#include "program.h"

#include "built_in.c"

/** Tests that the built-in names are loaded correctly. */
PRIVATE_TEST_CASE(test_load_built_ins) {
    CliHandler cli = create_cli_handler(0, NULL);
    ZmxProgram *program = new_zmx_program("Anything", &cli, false);
    
    const char *printString = "print";
    Obj *printName = AS_OBJ(new_string_obj(
        &program->gc.startupVulnObjs, printString, strlen(printString)
    ));
    Obj *nativePrint = table_get(&program->builtIn.funcs, printName);

    ASSERT_TRUE(nativePrint->type == OBJ_NATIVE_FUNC);
    ASSERT_TRUE(equal_obj(AS_OBJ(AS_PTR(NativeFuncObj, nativePrint)->name), printName));
    ASSERT_ADDRESS_EQUAL(AS_PTR(NativeFuncObj, nativePrint)->func, native_print);

    free_zmx_program(program);
}

/** Tests built_in.c/.h. */
void test_built_in() {
    TEST(test_load_built_ins);
}
