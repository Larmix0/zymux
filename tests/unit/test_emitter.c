#include "lukip.h"

#include "compiler.h"
#include "data_structures.h"

#include "emitter.c"

// TODO: test number emit and read when they're fully implemented.

Compiler *defaultCompiler; /** Default compiler for testing the bytecode emitters. */

/** A setup to initialize the default compiler. */
PRIVATE_DECLARE_SETUP(setup_default_compiler) {
    ZmxProgram *program = ZMX_TYPE_ALLOC(ZmxProgram);
    *program = create_zmx_program("default", false);
    NodeArray defaultAst = CREATE_DA();


    defaultCompiler = ZMX_TYPE_ALLOC(Compiler);
    *defaultCompiler = create_compiler(program, defaultAst, true);
}

/** A teardown for the default compiler and other things it uses. */
PRIVATE_DECLARE_TEARDOWN(teardown_default_compiler) {
    free_all_nodes(defaultCompiler->program);
    FREE_DA(&defaultCompiler->ast);

    free_zmx_program(defaultCompiler->program);
    free(defaultCompiler->program);

    free_compiler(defaultCompiler);
    free(defaultCompiler);
}

/** Tests the main byte emit function. */
PRIVATE_TEST_CASE(test_emit) {
    emit_instr(defaultCompiler, OP_LOAD_CONST, create_src_pos(1, 12, 6));
    emit_instr(defaultCompiler, OP_ADD, create_src_pos(1, 1, 1));
    emit_instr(defaultCompiler, OP_END, create_src_pos(99, 100, 100));

    u8 *bytecode = defaultCompiler->func->bytecode.data;
    ASSERT_UINT8_EQUAL(bytecode[0], OP_LOAD_CONST);
    ASSERT_UINT8_EQUAL(bytecode[1], OP_ADD);
    ASSERT_UINT8_EQUAL(bytecode[2], OP_END);

    SourcePosition *positions = defaultCompiler->func->positions.data;
    ASSERT_TRUE(equal_position(positions[0], create_src_pos(1, 12, 6)));
    ASSERT_TRUE(equal_position(positions[1], create_src_pos(1, 1, 1)));
    ASSERT_TRUE(equal_position(positions[2], create_src_pos(99, 100, 100)));
}

/** Tests the function which appends objects onto the constant pull and emits their index. */
PRIVATE_TEST_CASE(test_emit_const) {
    emit_const(
        defaultCompiler, OP_LOAD_CONST, AS_PTR(new_int_obj(defaultCompiler->program, 72), Obj),
        create_src_pos(1, 2, 3)
    );
    // TODO: when more const-based instructions and more objects get added,
    // change the const loading instruction here with something else, and the object to StringObj.
    emit_const(
        defaultCompiler, OP_LOAD_CONST, AS_PTR(new_int_obj(defaultCompiler->program, 14), Obj),
        create_src_pos(54, 336, 101)
    );

    u8 *bytecode = defaultCompiler->func->bytecode.data;
    ASSERT_UINT8_EQUAL(bytecode[0], OP_LOAD_CONST);
    ASSERT_UINT8_EQUAL(bytecode[1], 0);
    ASSERT_UINT8_EQUAL(bytecode[2], OP_LOAD_CONST);
    ASSERT_UINT8_EQUAL(bytecode[3], 1);

    SourcePosition *positions = defaultCompiler->func->positions.data;
    ASSERT_TRUE(equal_position(positions[0], create_src_pos(1, 2, 3)));
    ASSERT_TRUE(equal_position(positions[1], create_src_pos(1, 2, 3)));
    ASSERT_TRUE(equal_position(positions[2], create_src_pos(54, 336, 101)));
    ASSERT_TRUE(equal_position(positions[3], create_src_pos(54, 336, 101)));

    Obj **constPool = defaultCompiler->func->constPool.data;
    ASSERT_INT_EQUAL(constPool[bytecode[1]]->type, OBJ_INT);
    ASSERT_INT_EQUAL(constPool[bytecode[3]]->type, OBJ_INT);

    ASSERT_INT_EQUAL(AS_PTR(constPool[bytecode[1]], IntObj)->number, 72);
    ASSERT_INT_EQUAL(AS_PTR(constPool[bytecode[3]], IntObj)->number, 14);
}

/** Tests emitter.c. */
void test_emitter() {
    MAKE_FIXTURE(setup_default_compiler, teardown_default_compiler);
    TEST(test_emit);
    TEST(test_emit_const);
    RESET_FIXTURE();
}
