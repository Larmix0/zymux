#include "compiler.h"
#include "emitter.h"

static void adjust_jump(Jump *jump, const i64 adjustment);
static void fix_jumps(
    JumpArray *jumps, const u32 jumpIdx, const int addedBefore, const int addedAfter
);

/** Returns the instruction size of the passed number. */
static InstrSize get_number_size(const u32 number) {
    if (number <= U8_MAX) {
        return INSTR_ONE_BYTE;
    } else if (number > U8_MAX && number <= U16_MAX) {
        return INSTR_TWO_BYTES;
    }
    return INSTR_FOUR_BYTES;
}

/** Creates a jump which may change later due to patching. */
static Jump create_jump(const u32 index, const u32 size, const bool isForward) {
    Jump jump = {
        .index = index, .size = size, .isForward = isForward, .isResolved = false,
        .instrSize = INSTR_ONE_BYTE, .indexOffset = 0, .sizeOffset = 0
    };
    // Default offset after reading, relative to the jump instruction, and depending on forwardness.
    jump.size += isForward ? -2 : 2;
    jump.instrSize = get_number_size(jump.size);
    if (jump.instrSize != INSTR_ONE_BYTE && !isForward) {
        // -1 for already existing byte. +1 to go over arg size, so 0.
        adjust_jump(&jump, jump.instrSize);
    }
    return jump;
}

/** Changes the size of the jump and adjusts instruction size accordingly. */
static void adjust_jump(Jump *jump, const i64 adjustment) {
    jump->sizeOffset += adjustment;
    InstrSize before = jump->instrSize;
    jump->instrSize = get_number_size(jump->size + jump->sizeOffset);

    if (jump->instrSize != before && !jump->isForward) {
        adjust_jump(jump, jump->instrSize - before);
        if (before == INSTR_ONE_BYTE) {
            jump->sizeOffset++; // Go over the newely added arg size byte.
        }
    }
}

/** 
 * General function for inserting a byte of bytecode and its position.
 * 
 * The position is automatically made to be the previous byte's own position.
 * Also, it assumes that the byte is not inserted at index 0 when grabbing the position.
 */
static void insert_byte(Compiler *compiler, u8 byte, const u32 index) {
    ByteArray *bytecode = &compiler->func->bytecode;
    ASSERT(index <= bytecode->length, "Inserting byte outside bytecode boundary.");
    ASSERT(index != 0, "Can't insert byte at the very start because position is unknown.");

    for (u32 i = index; i < bytecode->length; i++) {
        u8 originalByte = bytecode->data[i];
        bytecode->data[i] = byte;
        byte = originalByte;
    }
    APPEND_DA(bytecode, byte);
    SourcePositionArray *positions = &compiler->func->positions;
    SourcePosition pos = positions->data[index - 1];
    for (u32 i = index; i < positions->length; i++) {
        SourcePosition originalPos = positions->data[i];
        positions->data[i] = pos;
        pos = originalPos;
    }
    APPEND_DA(positions, pos);
}

/** Removes a byte at the passed index, and removes its corresponding position. */
static void remove_byte(Compiler *compiler, const u32 index) {
    ByteArray *bytecode = &compiler->func->bytecode;
    ASSERT(index < bytecode->length, "Removing byte outside bytecode boundary.");

    for (u32 i = index; i < bytecode->length - 1; i++) {
        bytecode->data[i] = bytecode->data[i + 1];
    }
    DROP_DA(bytecode);

    SourcePositionArray *positions = &compiler->func->positions;
    for (u32 i = index; i < positions->length - 1; i++) {
        positions->data[i] = positions->data[i + 1];
    }
    DROP_DA(positions);
}

/** Inserts a number in the bytecode array at startIdx. */
static void insert_number(Compiler *compiler, const u32 startIdx, const u32 number) {
    InstrSize numberSize = get_number_size(number);
    if (numberSize == INSTR_ONE_BYTE) {
        insert_byte(compiler, number, startIdx);
    } else if (numberSize == INSTR_TWO_BYTES) {
        insert_byte(compiler, number >> 8, startIdx);
        insert_byte(compiler, number & 0xFF, startIdx + 1);
    } else {
        for (int byte = INSTR_FOUR_BYTES - 1, idx = 0; byte >= 0; byte--, idx++) {
            insert_byte(compiler, (number >> 8 * byte) & 0xFF, startIdx + idx);
        }
    }
}

/** 
 * Emits/appends a single byte instruction to the passed compiler's current function.
 * Also appends bytePos as debugging info to the positions.
 */
void emit_instr(Compiler *compiler, const u8 instr, const SourcePosition pos) {
    APPEND_DA(&compiler->func->bytecode, instr);
    APPEND_DA(&compiler->func->positions, pos);
}

/** 
 * Emits an instruction followed by a number that's at most the size of an integer.
 * 
 * It starts emitting the number in order of the largest (leftmost) byte first,
 * then goes 1 byte to the right till the rightmost byte.
 * 
 * The index of the number to be inserted starts after the instruction itself. It may be incremented
 * if there's more than one byte for the instruction (an arg size byte is to be added).
 */
void emit_number(Compiler *compiler, const u8 instr, const u32 number, const SourcePosition pos) {
    u32 numIdx = compiler->func->bytecode.length + 1;
    InstrSize numSize = get_number_size(number);

    if (numSize == INSTR_ONE_BYTE) {
        emit_instr(compiler, instr, pos);
    } else if (numSize == INSTR_ONE_BYTE) {
        emit_instr(compiler, OP_ARG_16, pos);
        emit_instr(compiler, instr, pos);
        numIdx++;
    } else {
        emit_instr(compiler, OP_ARG_32, pos);
        emit_instr(compiler, instr, pos);
        numIdx++;
    }
    insert_number(compiler, numIdx, number);
}

/** 
 * Reads a number that's a max size of a U32 which begins from numStart as an index in bytecode.
 * 
 * The amount of bytes read depends on the passed instruction size. Worth mentioning that the 
 * instruction size pointer is automatically set to 1 byte for next reads after being called.
 */
u32 read_number(const ByteArray *bytecode, const u32 numStart, InstrSize *size) {
    ASSERT(numStart + *size - 1 < bytecode->length, "Reading outside bytecode boundary.");
    u32 total = 0;
    for (int i = 0; i < (int)*size; i++) {
        total = (total << 8) | bytecode->data[numStart + i];
    }
    *size = INSTR_ONE_BYTE;
    return total;
}

/** Emits an instruction followed by an idx after to be used for an object in the const pool. */
void emit_const(Compiler *compiler, const u8 instr, Obj *constant, const SourcePosition pos) {
    APPEND_DA(&compiler->func->constPool, constant);
    const u32 constIdx = compiler->func->constPool.length - 1;
    emit_number(compiler, instr, constIdx, pos);
}

/** 
 * Emits a default empty/unfinished jump, which will have the actual jump itself put on later.
 * 
 * Returns the index of the jump instruction
 */
u32 emit_unpatched_jump(Compiler *compiler, const u8 instr, const SourcePosition pos) {
    emit_instr(compiler, instr, pos);
    emit_instr(compiler, 0, pos);
    return compiler->func->bytecode.length - 2;
}

/** Patches a jump in the compiler from the passed jump information. */
void patch_jump(Compiler *compiler, const u32 start, const u32 end, const bool isForward) {
    u32 jumpSize = end > start ? end - start : start - end;
    APPEND_DA(&compiler->jumps, create_jump(start, jumpSize, isForward));
}

/** 
 * Emits a normal jump instruction which is already patched.
 * 
 * The jump starts from the current byte and uses the passed "to" as the jump destination.
 */
void emit_jump(
    Compiler *compiler, const u8 instr, const u32 to, const bool isForward, const SourcePosition pos
) {
    u32 jumpIdx = emit_unpatched_jump(compiler, instr, pos);
    patch_jump(compiler, jumpIdx, to, isForward);
}

/** 
 * Emits some amount of local variable pops in a compiler optimized manner.
 * Doesn't emit anything if poppedAmount is 0.
 */
void emit_local_pops(Compiler *compiler, const u32 poppedAmount, const SourcePosition pos) {
    if (poppedAmount == 1) {
        emit_instr(compiler, OP_POP_LOCAL, pos);
    } else if (poppedAmount > 1) {
        emit_number(compiler, OP_POP_LOCALS, poppedAmount, pos);
    }
}

/** Emits a number of captured pops. Emits none if poppedAmount is 0. */
void emit_captured_pops(Compiler *compiler, const u32 poppedAmount, const SourcePosition pos) {
    if (poppedAmount > 0) {
        emit_number(compiler, OP_POP_CAPTURES, poppedAmount, pos);
    }   
}

/** 
 * Fixes the passed jump depending on the jump and its passed additions.
 * 
 * If the jump is fully within toFix's jump, then we add all of the added bytes as an extra
 * number to jump over (this includes both forward and backward jumps).
 * 
 * If toFix is specifically a backwards jump, and its destination is the passed jump to be fixed
 * from, then we also add the extra bytes to jump over both addedBefore and addedAfter.
 * We add addedAfter to skip over the jump bytes and go to the instruction directly.
 * 
 * If there is addedBefore, then that is the arg size byte, and if there's ever an arg size byte
 * then that byte should become the destination instead of the jump instruction itself.
 * This is because we want to know the size of the jump to be read before reading it, so if there's
 * a size byte, we read it before the jump itself.
 */
static void fix_size(
    Jump *toFix, const Jump *jump, const int addedBefore, const int addedAfter
) {
// +2 offset on the index after reading the default 1 byte jump instruction and 1 byte jump size.
#define FORWARD_JUMP_DEST(jump) ((jump)->index + 2 + (jump)->size)
#define BACKWARD_JUMP_DEST(jump) ((jump)->index + 2 - (jump)->size)

#define FULLY_WITHIN_JUMP(jump, checked) \
    ((!(jump)->isForward \
        && (jump)->index > (checked)->index && BACKWARD_JUMP_DEST(jump) < (checked)->index) \
    || ((jump)->isForward \
        && ((jump)->index < (checked)->index && FORWARD_JUMP_DEST(jump) > (checked)->index)))
#define DEST_OF_BACKWARD_JUMP(jump, checked) \
    (!(jump)->isForward \
        && (jump)->index > (checked)->index && BACKWARD_JUMP_DEST(jump) == (checked)->index)

    if (FULLY_WITHIN_JUMP(toFix, jump) || DEST_OF_BACKWARD_JUMP(toFix, jump)) {
        adjust_jump(toFix, addedBefore + addedAfter);
    }

#undef FORWARD_JUMP_DEST
#undef BACKWARD_JUMP_DEST
#undef FULLY_WITHIN_JUMP
#undef DEST_OF_BACKWARD_JUMP
}

/** 
 * Modifies and fixes jump's size if its instrSize has changed from "before".
 * 
 * if it was 1 byte before but not after, then it must've added 1 arg size byte before,
 * and 1 or 3 extra bytes (on top of the original one) after as the jump size.
 * 
 * However, if it was 2 bytes before and has changed, then we know it must have turned from 2 to 4
 * bytes. But 2 bytes itself may get resolved and modify the bytecode, so depending on a boolean
 * of whether or not it's been resolved, we add an extra 0 bytes before and 2 after
 * (indicating that the arg size just changed from 16 bits to 32 bits, but not adding an extra byte)
 * while the 2 bytes after are the extra added bytes going from 2 to 4.
 * 
 * If it's not been resolved then we add 1 before (the arg 32 bit byte), and 3 bytes after
 * (going from the default 1 byte read to a 4 byte read).
 */
static void ensure_size(JumpArray *jumps, const InstrSize before, Jump *jump, const u32 jumpIdx) {
    if (before == jump->instrSize) {
        return; // Nothing changed.
    }

    if (before == INSTR_ONE_BYTE) {
        fix_jumps(jumps, jumpIdx, 1, jump->instrSize - 1);
    } else if (before == INSTR_TWO_BYTES) {
        if (jump->isResolved) {
            fix_jumps(jumps, jumpIdx, 0, 2);
        } else {
            fix_jumps(jumps, jumpIdx, 1, 3);
        }
    }
}

/** 
 * Fixes the array of jumps depending on the passed jump's location and how many bytes inserted.
 * 
 * It does so by seeing if the jump's changes affect that of any jump in the array of jumps,
 * and after potentially making the appropriate changes to any given jump we ensure whether
 * the jump's size also became bigger and therefore requires more instructions.
 */
static void fix_jumps(
    JumpArray *jumps, const u32 jumpIdx, const int addedBefore, const int addedAfter
) {
    Jump *jump = &jumps->data[jumpIdx];
    for (u32 i = 0; i < jumps->length; i++) {
        if (i == jumpIdx) {
            continue;
        }

        Jump *toFix = &jumps->data[i];
        InstrSize instrSizeBefore = toFix->instrSize;
        fix_size(toFix, jump, addedBefore, addedAfter);
        if (toFix->index > jump->index && i > jumpIdx) {
            toFix->indexOffset += addedBefore + addedAfter; // Jump is and gets resolved beforehand.
        }

        ensure_size(jumps, instrSizeBefore, toFix, i);
    }
    jump->indexOffset += addedBefore;
    jump->isResolved = true;
}

/** Write the passed jump inside the compiler's bytecode. */
static void write_jump(Compiler *compiler, Jump *jump) {
    ByteArray *bytecode = &compiler->func->bytecode;
    u32 index = jump->index + jump->indexOffset;
    u32 size = jump->size + jump->sizeOffset;
    if (jump->instrSize == INSTR_ONE_BYTE) {
        bytecode->data[index + 1] = size;
        return;
    }

    // Insert an argument size byte, then remove the default 1 byte and insert the actual number.
    u8 argSize = jump->instrSize == INSTR_TWO_BYTES ? OP_ARG_16 : OP_ARG_32;
    insert_byte(compiler, argSize, index - 1);
    remove_byte(compiler, index + 1);
    insert_number(compiler, index + 1, size);
}

/** Writes all the jumps of the compiler in the actual bytecode after fixing/resolving them. */
void write_jumps(Compiler *compiler) {
    JumpArray *jumps = &compiler->jumps;
    for (u32 i = 0; i < jumps->length; i++) {
        Jump *jump = &jumps->data[i];
        if (jump->instrSize != INSTR_ONE_BYTE && !jump->isResolved) {
            fix_jumps(&compiler->jumps, i, 1, (int)jump->instrSize - 1);
        }
    }

    for (u32 i = 0; i < jumps->length; i++) {
        write_jump(compiler, &jumps->data[i]);
    }
}
