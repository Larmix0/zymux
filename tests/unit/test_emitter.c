#include "lukip.h"

#include "cli_handler.h"
#include "compiler.h"
#include "dynamic_array.h"

#include "emitter.c"

Compiler defaultCompiler; /** Default compiler for testing the bytecode emitters. */

/** A setup to initialize the default compiler. */
PRIVATE_DECLARE_SETUP(setup_default_compiler) {
    CliHandler cli = create_cli_handler(0, NULL);
    ZmxProgram *program = new_zmx_program("default", &cli, false);
    NodeArray defaultAst = CREATE_DA();

    defaultCompiler = create_compiler(&program->gc.startupVulnObjs, defaultAst, true);
}

/** A teardown for the default compiler and other things it uses. */
PRIVATE_DECLARE_TEARDOWN(teardown_default_compiler) {
    free_zmx_program(defaultCompiler.program);
    free_compiler(&defaultCompiler);
}

/** Tests the main byte emit function. */
PRIVATE_TEST_CASE(test_emit_instr) {
    emit_instr(&defaultCompiler, OP_LOAD_CONST, create_src_pos(1, 12, 6));
    emit_instr(&defaultCompiler, OP_ADD, create_src_pos(1, 1, 1));
    emit_instr(&defaultCompiler, OP_EOF, create_src_pos(99, 100, 100));

    ByteArray *bytecode = &defaultCompiler.func->bytecode;
    ASSERT_UINT8_EQUAL(bytecode->data[0], OP_LOAD_CONST);
    ASSERT_UINT8_EQUAL(bytecode->data[1], OP_ADD);
    ASSERT_UINT8_EQUAL(bytecode->data[2], OP_EOF);

    SourcePositionArray *positions = &defaultCompiler.func->positions;
    ASSERT_TRUE(equal_position(positions->data[0], create_src_pos(1, 12, 6)));
    ASSERT_TRUE(equal_position(positions->data[1], create_src_pos(1, 1, 1)));
    ASSERT_TRUE(equal_position(positions->data[2], create_src_pos(99, 100, 100)));
}

/** Tests that insertion and removal work properly during emission of bytecode. */
PRIVATE_TEST_CASE(test_insert_and_remove) {
    ByteArray *bytecode = &defaultCompiler.func->bytecode;
    emit_instr(&defaultCompiler, OP_LOAD_CONST, create_src_pos(1, 12, 6));
    emit_instr(&defaultCompiler, OP_ADD, create_src_pos(1, 1, 1));
    emit_instr(&defaultCompiler, OP_EOF, create_src_pos(99, 100, 100));

    insert_byte(&defaultCompiler, OP_ARG_16, 1);
    insert_byte(&defaultCompiler, OP_ARG_32, 3);

    ASSERT_UINT8_EQUAL(bytecode->data[0], OP_LOAD_CONST);
    ASSERT_UINT8_EQUAL(bytecode->data[1], OP_ARG_16);
    ASSERT_UINT8_EQUAL(bytecode->data[2], OP_ADD);
    ASSERT_UINT8_EQUAL(bytecode->data[3], OP_ARG_32);
    ASSERT_UINT8_EQUAL(bytecode->data[4], OP_EOF);

    remove_byte(&defaultCompiler, 2);
    remove_byte(&defaultCompiler, 3);

    ASSERT_UINT8_EQUAL(bytecode->data[0], OP_LOAD_CONST);
    ASSERT_UINT8_EQUAL(bytecode->data[1], OP_ARG_16);
    ASSERT_UINT8_EQUAL(bytecode->data[2], OP_ARG_32);
}

/** Tests the function which appends objects onto the constant pull and emits their index. */
PRIVATE_TEST_CASE(test_emit_const) {
    VulnerableObjs *vulnObjs = &defaultCompiler.program->gc.startupVulnObjs;
    emit_const(
        &defaultCompiler, OP_LOAD_CONST,
        AS_OBJ(new_int_obj(vulnObjs, 72)),
        create_src_pos(1, 2, 3)
    );
    const char *globalGet = "name";
    emit_const(
        &defaultCompiler, OP_GET_GLOBAL,
        AS_OBJ(new_string_obj(vulnObjs, globalGet, strlen(globalGet))),
        create_src_pos(54, 336, 101)
    );

    ByteArray *bytecode = &defaultCompiler.func->bytecode;
    ASSERT_UINT8_EQUAL(bytecode->data[0], OP_LOAD_CONST);
    ASSERT_UINT8_EQUAL(bytecode->data[1], 0);
    ASSERT_UINT8_EQUAL(bytecode->data[2], OP_GET_GLOBAL);
    ASSERT_UINT8_EQUAL(bytecode->data[3], 1);

    SourcePositionArray *positions = &defaultCompiler.func->positions;
    ASSERT_TRUE(equal_position(positions->data[0], create_src_pos(1, 2, 3)));
    ASSERT_TRUE(equal_position(positions->data[1], create_src_pos(1, 2, 3)));
    ASSERT_TRUE(equal_position(positions->data[2], create_src_pos(54, 336, 101)));
    ASSERT_TRUE(equal_position(positions->data[3], create_src_pos(54, 336, 101)));

    Obj **constPool = defaultCompiler.func->constPool.data;
    ASSERT_INT_EQUAL(constPool[bytecode->data[1]]->type, OBJ_INT);
    ASSERT_INT_EQUAL(constPool[bytecode->data[3]]->type, OBJ_STRING);

    ASSERT_INT_EQUAL(AS_PTR(IntObj, constPool[bytecode->data[1]])->number, 72);
    ASSERT_STRING_EQUAL(AS_PTR(StringObj, constPool[bytecode->data[3]])->string, "name");
}

/** Tests that the number reader and writer functions read and write to the bytecode properly. */
PRIVATE_TEST_CASE(test_number_read_and_write) {
    emit_number(&defaultCompiler, OP_AS, TYPE_FLOAT, create_src_pos(3, 4, 5));
    emit_number(&defaultCompiler, OP_FINISH_STRING, U8_MAX + 100, create_src_pos(333, 44, 58));
    emit_number(&defaultCompiler, OP_FINISH_STRING, U16_MAX + 100, create_src_pos(1, 0, 1));
    
    InstrSize size = INSTR_ONE_BYTE;
    u32 actual = bytecode_number(&defaultCompiler.func->bytecode, 1, &size);
    ASSERT_UINT32_EQUAL(actual, TYPE_FLOAT);
    size = INSTR_TWO_BYTES;
    actual = bytecode_number(&defaultCompiler.func->bytecode, 4, &size);
    ASSERT_UINT32_EQUAL(actual, U8_MAX + 100);
    size = INSTR_FOUR_BYTES;
    actual = bytecode_number(&defaultCompiler.func->bytecode, 8, &size);
    ASSERT_UINT32_EQUAL(actual, U16_MAX + 100);

    SourcePositionArray *positions = &defaultCompiler.func->positions;
    ASSERT_TRUE(equal_position(positions->data[0], create_src_pos(3, 4, 5)));
    ASSERT_TRUE(equal_position(positions->data[1], create_src_pos(3, 4, 5)));
    ASSERT_TRUE(equal_position(positions->data[2], create_src_pos(333, 44, 58)));
    ASSERT_TRUE(equal_position(positions->data[3], create_src_pos(333, 44, 58)));
    ASSERT_TRUE(equal_position(positions->data[4], create_src_pos(333, 44, 58)));
    ASSERT_TRUE(equal_position(positions->data[5], create_src_pos(333, 44, 58)));
    ASSERT_TRUE(equal_position(positions->data[6], create_src_pos(1, 0, 1)));
    ASSERT_TRUE(equal_position(positions->data[7], create_src_pos(1, 0, 1)));
    ASSERT_TRUE(equal_position(positions->data[8], create_src_pos(1, 0, 1)));
    ASSERT_TRUE(equal_position(positions->data[9], create_src_pos(1, 0, 1)));
    ASSERT_TRUE(equal_position(positions->data[10], create_src_pos(1, 0, 1)));
    ASSERT_TRUE(equal_position(positions->data[11], create_src_pos(1, 0, 1)));
}

/** Tests that 1 byte sized jumps work properly. */
PRIVATE_TEST_CASE(test_small_jumps) {
    ByteArray *bytecode = &defaultCompiler.func->bytecode;
    const SourcePosition pos = create_src_pos(0, 0, 0);
    const u32 firstJump = emit_unpatched_jump(&defaultCompiler, OP_JUMP, pos);
    for (int i = 0; i < 5; i++) {
        emit_instr(&defaultCompiler, OP_TRUE, pos);
    }
    patch_jump(&defaultCompiler, firstJump, bytecode->length - 1, true);
    emit_jump(&defaultCompiler, OP_JUMP, 0, false, pos);
    emit_instr(&defaultCompiler, OP_EOF, pos);
    write_jumps(&defaultCompiler);

    const u8 expected[] = {
        OP_JUMP, 4, OP_TRUE, OP_TRUE, OP_TRUE, OP_TRUE, OP_TRUE, OP_JUMP, 9, OP_EOF
    };
    ASSERT_SIZE_T_EQUAL(bytecode->length, sizeof(expected));
    ASSERT_BYTES_EQUAL(bytecode->data, expected, bytecode->length);
}

/** Makes assertions for a specific jump with the passed information. */
static void assert_large_jump(
    const u8 jumpInstr, const u8 argSize, const u32 argSizeIdx, const u32 expectedSize
) {
    ByteArray *bytecode = &defaultCompiler.func->bytecode;
    ASSERT_UINT8_EQUAL(bytecode->data[argSizeIdx], argSize);
    ASSERT_UINT8_EQUAL(bytecode->data[argSizeIdx + 1], jumpInstr);

    InstrSize readSize = argSize == OP_ARG_16 ? INSTR_TWO_BYTES : INSTR_FOUR_BYTES;
    ASSERT_TRUE(readSize == get_number_size(expectedSize));
    const u32 inserted = bytecode_number(&defaultCompiler.func->bytecode, argSizeIdx + 2, &readSize);
    ASSERT_UINT32_EQUAL(inserted, expectedSize);
}

/** Tests that jumps over 1 byte in size work and get resolved correctly. */
PRIVATE_TEST_CASE(test_large_jumps) {
    ByteArray *bytecode = &defaultCompiler.func->bytecode;
    const SourcePosition pos = create_src_pos(0, 0, 0);

    emit_instr(&defaultCompiler, OP_TRUE, pos);
    const u32 forwardTwoBytes = emit_unpatched_jump(&defaultCompiler, OP_JUMP, pos);
    const u32 forwardToBackJump = emit_unpatched_jump(&defaultCompiler, OP_JUMP, pos);
    const u32 forwardTwoToFourBytes = emit_unpatched_jump(&defaultCompiler, OP_JUMP, pos);
    const u32 forwardToEnd = emit_unpatched_jump(&defaultCompiler, OP_JUMP, pos);

    for (int i = 0; i < U8_MAX - 4; i++) {
        emit_instr(&defaultCompiler, OP_TRUE, pos);
    }
    bytecode->data[bytecode->length - 1] = OP_FALSE;
    patch_jump(&defaultCompiler, forwardTwoBytes, bytecode->length - 1, true);

    for (int i = 0; i < U16_MAX + 10; i++) {
        emit_instr(&defaultCompiler, OP_TRUE, pos);
    }
    bytecode->data[bytecode->length - U8_MAX - 9] = OP_FALSE;
    patch_jump(&defaultCompiler, forwardTwoToFourBytes, bytecode->length - U8_MAX - 9, true);
    emit_jump(&defaultCompiler, OP_JUMP, 3, false, pos);
    patch_jump(&defaultCompiler, forwardToBackJump, bytecode->length - 2, true);
    emit_jump(&defaultCompiler, OP_JUMP, bytecode->length - 2, false, pos);

    bytecode->data[bytecode->length - (U16_MAX - 5)] = OP_FALSE;
    emit_jump(&defaultCompiler, OP_JUMP, bytecode->length - (U16_MAX - 5), false, pos);
    emit_jump(&defaultCompiler, OP_JUMP, 1, false, pos);
    emit_instr(&defaultCompiler, OP_EOF, pos);
    patch_jump(&defaultCompiler, forwardToEnd, bytecode->length - 1, true);

    write_jumps(&defaultCompiler);

    assert_large_jump(OP_JUMP, OP_ARG_16, 1, 268);
    assert_large_jump(OP_JUMP, OP_ARG_32, 5, 65808);
    assert_large_jump(OP_JUMP, OP_ARG_32, 11, 65538);
    assert_large_jump(OP_JUMP, OP_ARG_32, 17, 65816);

    assert_large_jump(OP_JUMP, OP_ARG_32, 65819, 65820);
    ASSERT_UINT8_EQUAL(bytecode->data[65826], 8);
    assert_large_jump(OP_JUMP, OP_ARG_32, 65827, 65540);
    assert_large_jump(OP_JUMP, OP_ARG_32, 65833, 65838);
}

/** Tests emitter.c. */
void test_emitter() {
    MAKE_FIXTURE(setup_default_compiler, teardown_default_compiler);
    TEST(test_emit_instr);
    TEST(test_insert_and_remove);
    TEST(test_emit_const);
    TEST(test_number_read_and_write);
    TEST(test_small_jumps);
    TEST(test_large_jumps);
    RESET_FIXTURE();
}
