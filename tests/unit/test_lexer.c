#include <stdarg.h>
#include <stdio.h>

#include "lukip.h"

#include "object.h"
#include "token.h"

#include "lexer.c"

/** The last token which was appended to the lexer. */
#define LAST_TOKEN(lexer) (LAST_ITEM_DA(&(lexer)->tokens))

/** A default source string for testing. */
static char *defaultSource = "float && var (); $'str {2 * 3}' break; 22 44.2"
    "\n// single line\n hey /*muti-\nline*/ end";
static Lexer *defaultLexer; /** The default lexer to hold and lex the default source. */

/** A setup to initialize the default lexer. */
PRIVATE_DECLARE_SETUP(setup_default_lexer) {
    ZmxProgram *program = TYPE_ALLOC(ZmxProgram);
    *program = create_zmx_program("default", false);

    defaultLexer = TYPE_ALLOC(Lexer);
    *defaultLexer = create_lexer(program, defaultSource);
}

/** A teardown for the default lexer and the other things it uses. */
PRIVATE_DECLARE_TEARDOWN(teardown_default_lexer) {
    free_zmx_program(defaultLexer->program);
    free(defaultLexer->program);

    free_lexer(defaultLexer);
    free(defaultLexer);
}

/** 
 * Creates a normal token with no union values, but with a position.
 * 
 * Creates the position from the passed line and column, then figures the length from the lexeme.
 */
static Token create_token_pos(
    char *lexeme, const int line, const int column, const TokenType type
) {
    Token token = {
        .lexeme = lexeme, .pos = create_src_pos(line, column, strlen(lexeme)), .type = type
    };
    return token;
}

/** 
 * Appends multiple tokens and an automatic EOF.
 * The amount is how many tokens are passed to append.
 */
static void append_test_tokens(TokenArray *tokens, const int amount, ...) {
    va_list args;
    va_start(args, amount);
    for (int i = 0; i < amount; i++) {
        APPEND_DA(tokens, va_arg(args, Token));
    }
    APPEND_DA(tokens, create_token("", TOKEN_EOF));
    va_end(args);
}

/** 
 * Compares the tokens in the lexer with the passed expectedArray.
 * If compareSpots is set to true, it'll also compare the the line and column of the tokens.
 */
static void compare_lexed(Lexer *lexer, TokenArray *expectedArray, const bool compareSpots) {
    const int lexerLength = lexer->tokens.length;
    const int expectedLength = expectedArray->length;
    ASSERT_INT_EQUAL(lexer->tokens.length, expectedArray->length);

    const int shortest = lexerLength > expectedLength ? expectedLength : lexerLength;
    for (int tokenIdx = 0; tokenIdx < shortest; tokenIdx++) {
        Token lexed = lexer->tokens.data[tokenIdx];
        Token expected = expectedArray->data[tokenIdx];
        ASSERT_CUSTOM(
            equal_token(lexed, expected),
            "Token %d (lexeme='%.*s' type=%s) != (lexeme='%s', type=%s)",
            tokenIdx + 1, lexed.pos.length, lexed.lexeme,
            token_type_string(lexed.type), expected.lexeme, token_type_string(expected.type)
        );
        if (compareSpots) {
            ASSERT_INT_EQUAL(lexed.pos.line, expected.pos.line);
            ASSERT_INT_EQUAL(lexed.pos.column, expected.pos.column);
        }
    }
}

/** Tests lex() with a bunch of programs that shouldn't error. */
PRIVATE_TEST_CASE(test_lex_successful_programs) {
    char *sources[] = {
        "",
        "(2 + 3) * 30 ** 2 / 1.5 - 0.2;",
        "<<=",
        "\n \t  \n",
        " variable /**/*/ // Should have star and slash lexed.\n endvar",
        "// e\na",
        "/*hello \n\n ignored.*/\n\t",
        "// One line comment source.",
        "hello /* should be \n\n\tignored here */ match; //double?\n int",
        "\n345 /*/**/\n -234\n // Should not lex anything related to the multiline comment",
        "ide // Single line /*multiline*/ comment\n 'Bare string.' /*normal\n multiline\t\n*/ end"
    }; 
    const size_t sourcesAmount = sizeof(sources) / sizeof(char *);
    TokenArray *tokens2DArray = ARRAY_ALLOC(sourcesAmount, TokenArray);
    for (size_t i = 0; i < sourcesAmount; i++) {
        INIT_DA(&tokens2DArray[i]);
    }
    append_test_tokens(&tokens2DArray[0], 0);
    append_test_tokens(
        &tokens2DArray[1], 14,
        create_token("(", TOKEN_LPAR), create_int_token("2", 10), create_token("+", TOKEN_PLUS),
        create_int_token("3", 10), create_token(")", TOKEN_RPAR), create_token("*", TOKEN_STAR), 
        create_int_token("30", 10), create_token("**", TOKEN_EXPO), create_int_token("2", 10),
        create_token("/", TOKEN_SLASH), create_float_token("1.5"),
        create_token("-", TOKEN_MINUS), create_float_token("0.2"),
        create_token(";", TOKEN_SEMICOLON)
    );
    append_test_tokens(&tokens2DArray[2], 1, create_token("<<=", TOKEN_LSHIFT_EQ));
    append_test_tokens(&tokens2DArray[3], 0);

    append_test_tokens(
        &tokens2DArray[4], 4,
        create_token("variable", TOKEN_IDENTIFIER), create_token("*", TOKEN_STAR),
        create_token("/", TOKEN_SLASH), create_token("endvar", TOKEN_IDENTIFIER)
    );
    append_test_tokens(&tokens2DArray[5], 1, create_token("a", TOKEN_IDENTIFIER));
    append_test_tokens(&tokens2DArray[6], 0);
    append_test_tokens(&tokens2DArray[7], 0);
    append_test_tokens(
        &tokens2DArray[8], 4,
        create_token("hello", TOKEN_IDENTIFIER), create_token("match", TOKEN_MATCH_KW),
        create_token(";", TOKEN_SEMICOLON), create_token("int", TOKEN_INT_KW)
    );
    append_test_tokens(
        &tokens2DArray[9], 3,
        create_int_token("345", 10), create_token("-", TOKEN_MINUS), create_int_token("234", 10)
    );
    append_test_tokens(
        &tokens2DArray[10], 4,
        create_token("ide", TOKEN_IDENTIFIER), create_string_token("Bare string."),
        create_token("", TOKEN_STRING_END), create_token("end", TOKEN_IDENTIFIER)
    );

    for (size_t arrayIdx = 0; arrayIdx < sourcesAmount; arrayIdx++) {
        ZmxProgram program = create_zmx_program("testLex", false);
        Lexer lexer = create_lexer(&program, sources[arrayIdx]);
        lex(&lexer);
        ASSERT_FALSE(lexer.program->hasErrored);

        compare_lexed(&lexer, &tokens2DArray[arrayIdx], false);
        free_lexer(&lexer);
        free_zmx_program(&program);
    }
    for (size_t i = 0; i < sourcesAmount; i++) {
        FREE_DA(&tokens2DArray[i]);
    }
    free(tokens2DArray);
}

/** Tests lex() with a bunch of erroneous programs. */
PRIVATE_TEST_CASE(test_lex_errors) {
    char *sources[] = {
        "SomeIdentifier 'Unterminated string.",
        "/* Unterminated multiline comment.",
        "0b34 // Binary is only 1s and 0s",
        "23 + 0x",
        "@\\@@ invalidSyntax.",
        "#$",
        "#$###$#$'Repeated.'",
        "$'Unclosed { in interpolated.'"
    }; 
    const size_t sourcesAmount = sizeof(sources) / sizeof(char *);
    for (size_t i = 0; i < sourcesAmount; i++) {
        ZmxProgram program = create_zmx_program("testError", false);
        Lexer lexer = create_lexer(&program, sources[i]);
        lex(&lexer);
        ASSERT_TRUE(lexer.program->hasErrored);
        
        free_lexer(&lexer);
        free_zmx_program(&program);
    }
}

/** Performs a test on a source code that includes every single token in Zymux. */
PRIVATE_TEST_CASE(test_lex_all_tokens) {
    char *source = "string int float bool class const let private func if else while for "
        "do return break continue true false null as is in super this init abstract inherits "
        "match case default from import && || ! & | ^ << >> ~ += -= *= /= %= **= &= |= <<= >>= "
        "+-*/%** == != > >= < <= ()[]{} ; , . ?: $'str literal {100}' 22 44.2 = variable ..";
    TokenArray allTokens = CREATE_DA();
    append_test_tokens(
        &allTokens, 84,
        create_token("string", TOKEN_STRING_KW), create_token("int", TOKEN_INT_KW),
        create_token("float", TOKEN_FLOAT_KW), create_token("bool", TOKEN_BOOL_KW),
        create_token("class", TOKEN_CLASS_KW), create_token("const", TOKEN_CONST_KW),
        create_token("let", TOKEN_LET_KW), create_token("private", TOKEN_PRIVATE_KW),
        create_token("func", TOKEN_FUNC_KW), create_token("if", TOKEN_IF_KW),
        create_token("else", TOKEN_ELSE_KW), create_token("while", TOKEN_WHILE_KW),
        create_token("for", TOKEN_FOR_KW), create_token("do", TOKEN_DO_KW),
        create_token("return", TOKEN_RETURN_KW), create_token("break", TOKEN_BREAK_KW),
        create_token("continue", TOKEN_CONTINUE_KW), create_token("true", TOKEN_TRUE_KW),
        create_token("false", TOKEN_FALSE_KW), create_token("null", TOKEN_NULL_KW),
        create_token("as", TOKEN_AS_KW), create_token("is", TOKEN_IS_KW),
        create_token("in", TOKEN_IN_KW), create_token("super", TOKEN_SUPER_KW),
        create_token("this", TOKEN_THIS_KW), create_token("init", TOKEN_INIT_KW),
        create_token("abstract", TOKEN_ABSTRACT_KW), create_token("inherits", TOKEN_INHERITS_KW),
        create_token("match", TOKEN_MATCH_KW), create_token("case", TOKEN_CASE_KW),
        create_token("default", TOKEN_DEFAULT_KW), create_token("from", TOKEN_FROM_KW),
        create_token("import", TOKEN_IMPORT_KW), create_token("&&", TOKEN_AMPER_AMPER),
        create_token("||", TOKEN_BAR_BAR), create_token("!", TOKEN_BANG),
        create_token("&", TOKEN_AMPER), create_token("|", TOKEN_BAR),
        create_token("^", TOKEN_CARET), create_token("<<", TOKEN_LSHIFT),
        create_token(">>", TOKEN_RSHIFT), create_token("~", TOKEN_TILDE),
        create_token("+=", TOKEN_PLUS_EQ), create_token("-=", TOKEN_MINUS_EQ),
        create_token("*=", TOKEN_STAR_EQ), create_token("/=", TOKEN_SLASH_EQ),
        create_token("%=", TOKEN_PERCENT_EQ), create_token("**=", TOKEN_EXPO_EQ),
        create_token("&=", TOKEN_AMPER_EQ), create_token("|=", TOKEN_BAR_EQ),
        create_token("<<=", TOKEN_LSHIFT_EQ), create_token(">>=", TOKEN_RSHIFT_EQ),
        create_token("+", TOKEN_PLUS), create_token("-", TOKEN_MINUS),
        create_token("*", TOKEN_STAR), create_token("/", TOKEN_SLASH),
        create_token("%", TOKEN_PERCENT), create_token("**", TOKEN_EXPO),
        create_token("==", TOKEN_EQ_EQ), create_token("!=", TOKEN_BANG_EQ),
        create_token(">", TOKEN_GREATER), create_token(">=", TOKEN_GREATER_EQ),
        create_token("<", TOKEN_LESS), create_token("<=", TOKEN_LESS_EQ),
        create_token("(", TOKEN_LPAR), create_token(")", TOKEN_RPAR),
        create_token("[", TOKEN_LSQUARE), create_token("]", TOKEN_RSQUARE),
        create_token("{", TOKEN_LCURLY), create_token("}", TOKEN_RCURLY),
        create_token(";", TOKEN_SEMICOLON), create_token(",", TOKEN_COMMA),
        create_token(".", TOKEN_DOT), create_token("?", TOKEN_QUESTION_MARK),
        create_token(":", TOKEN_COLON), create_string_token("str literal "),
        create_token("", TOKEN_INTERPOLATE), create_int_token("100", 10),
        create_token("", TOKEN_STRING_END), create_int_token("22", 10),
        create_float_token("44.2"), create_token("=", TOKEN_EQ),
        create_token("variable", TOKEN_IDENTIFIER), create_token("..", TOKEN_DOT_DOT)
    );
    ZmxProgram program = create_zmx_program("testAll", false);
    Lexer lexer = create_lexer(&program, source);
    lex(&lexer);

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zmx_program(&program);
    FREE_DA(&allTokens);
}

/** A test for ensuring that the lexer keeps track of lines and columns correctly. */
PRIVATE_TEST_CASE(test_lex_spots) {
    char *source = "line1\n line2 break\n\n\n \t line5 555\n\n";
    TokenArray allTokens = CREATE_DA();
    append_test_tokens(
        &allTokens, 5,
        create_token_pos("line1", 1, 1, TOKEN_IDENTIFIER),
        create_token_pos("line2", 2, 2, TOKEN_IDENTIFIER),
        create_token_pos("break", 2, 8, TOKEN_BREAK_KW),
        create_token_pos("line5", 5, 4, TOKEN_IDENTIFIER),
        (Token){
            .lexeme = "555", .pos = create_src_pos(5, 10, 3),
            .type = TOKEN_INT_LIT, .intVal = 555
        }
    );
    // Set EOF position manually.
    allTokens.data[allTokens.length - 1].pos = create_src_pos(7, 1, 0);
    ZmxProgram program = create_zmx_program("testLine", false);
    Lexer lexer = create_lexer(&program, source);
    lex(&lexer);

    compare_lexed(&lexer, &allTokens, true);
    free_lexer(&lexer);
    free_zmx_program(&program);
    FREE_DA(&allTokens);
}

/** 
 * Tests that the lexer handles all supported numbers in Zymux correctly.
 * Which includes floats and integers in decimal, as well as some other bases like hexadecimal.
 * It also tests that we handle preceding 0s correctly.  
 */
PRIVATE_TEST_CASE(test_lex_number) {
    char *source = "0x0 0x00ff0 0xea2301 0b0 0b101 0b010 0b00100110111 0o00 0o02707 0o241 "
        "000x000 00o77 0 0.0 0.010 0013 002.3300 931453229 23.3 3.0";

    TokenArray allTokens = CREATE_DA();
    append_test_tokens(
        &allTokens, 20,
        create_int_token("0", 16), create_int_token("ff0", 16),
        create_int_token("ea2301", 16), create_int_token("0", 2),
        create_int_token("101", 2), create_int_token("10", 2),
        create_int_token("100110111", 2), create_int_token("0", 8),
        create_int_token("2707", 8), create_int_token("241", 8),
        create_int_token("0", 16), create_int_token("77", 8),
        create_int_token("0", 10), create_float_token("0.0"),
        create_float_token("0.010"), create_int_token("13", 10),
        create_float_token("2.3300"), create_int_token("931453229", 10),
        create_float_token("23.3"), create_float_token("3.0")
    );
    ZmxProgram program = create_zmx_program("testNumber", false);
    Lexer lexer = create_lexer(&program, source);
    while (!IS_EOF(&lexer)) {
        START_TOKEN(&lexer);
        lex_number(&lexer);
        ignore_whitespace(&lexer);
    }
    APPEND_DA(&lexer.tokens, create_token("", TOKEN_EOF));

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zmx_program(&program);
    FREE_DA(&allTokens);
}

/** Tests that the lexer handles all names (keywords and identifiers) correctly. */
PRIVATE_TEST_CASE(test_lex_name) {
    char *source = "hello returnme break _ notKeyword__ float L2dm3e44 _22_ string _NAME_HERE";
    TokenArray allTokens = CREATE_DA();
    append_test_tokens(
        &allTokens, 10,
        create_token("hello", TOKEN_IDENTIFIER), create_token("returnme", TOKEN_IDENTIFIER),
        create_token("break", TOKEN_BREAK_KW), create_token("_", TOKEN_IDENTIFIER),
        create_token("notKeyword__", TOKEN_IDENTIFIER), create_token("float", TOKEN_FLOAT_KW),
        create_token("L2dm3e44", TOKEN_IDENTIFIER), create_token("_22_", TOKEN_IDENTIFIER),
        create_token("string", TOKEN_STRING_KW), create_token("_NAME_HERE", TOKEN_IDENTIFIER)
    );
    ZmxProgram program = create_zmx_program("testName", false);
    Lexer lexer = create_lexer(&program, source);
    while (!IS_EOF(&lexer)) {
        START_TOKEN(&lexer);
        lex_name(&lexer);
        ignore_whitespace(&lexer);
    }
    APPEND_DA(&lexer.tokens, create_token("", TOKEN_EOF));

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zmx_program(&program);
    FREE_DA(&allTokens);
}

/** 
 * Tests that the lexer handles different kinds of strings correctly
 * This includes normal, interpolated, raw and interpolated + raw as well as some edge cases.
 */
PRIVATE_TEST_CASE(test_lex_string) {
    char *source = "'Normal\\t string\\\\n w/escapes'"
        "#'This is a\\t raw string.\\n\\\\n'"
        "$'Interpolated \\{ <- escaped {3 * $\"And {2 + 3} is nested\" + 2}'"
        "$#'Interpolated raw \\{2 + 3}'"
        "$'{$\"{1 + 2}\"} end'"
        "$' { $\" { 7 ** 23 } \" } '"
        "'unclosed { left curly, but not interpolated.'"
        "$'{\t}Empty {}{\t}brace.{ }'";

    TokenArray allTokens = CREATE_DA();
    append_test_tokens(
        &allTokens, 59,

        create_string_token("Normal\t string\\n w/escapes"),
        create_token("", TOKEN_STRING_END),

        create_string_token("This is a\\t raw string.\\n\\\\n"),
        create_token("", TOKEN_STRING_END),

        create_string_token("Interpolated { <- escaped "),
        create_token("", TOKEN_INTERPOLATE), create_int_token("3", 10),
        create_token("*", TOKEN_STAR), create_string_token("And "),
        create_token("", TOKEN_INTERPOLATE), create_int_token("2", 10),
        create_token("+", TOKEN_PLUS), create_int_token("3", 10),
        create_token("", TOKEN_INTERPOLATE), create_string_token(" is nested"),
        create_token("", TOKEN_STRING_END), create_token("+", TOKEN_PLUS),
        create_int_token("2", 10), create_token("", TOKEN_STRING_END),

        create_string_token("Interpolated raw \\"), create_token("", TOKEN_INTERPOLATE),
        create_int_token("2", 10), create_token("+", TOKEN_PLUS), create_int_token("3", 10),
        create_token("", TOKEN_STRING_END),

        create_string_token(""), create_token("", TOKEN_INTERPOLATE),
        create_string_token(""), create_token("", TOKEN_INTERPOLATE),
        create_int_token("1", 10), create_token("+", TOKEN_PLUS),
        create_int_token("2", 10), create_token("", TOKEN_STRING_END),
        create_token("", TOKEN_INTERPOLATE), create_string_token(" end"),
        create_token("", TOKEN_STRING_END),

        create_string_token(" "), create_token("", TOKEN_INTERPOLATE),
        create_string_token(" "), create_token("", TOKEN_INTERPOLATE),
        create_int_token("7", 10), create_token("**", TOKEN_EXPO),
        create_int_token("23", 10), create_token("", TOKEN_INTERPOLATE),
        create_string_token(" "), create_token("", TOKEN_STRING_END),
        create_token("", TOKEN_INTERPOLATE), create_string_token(" "),
        create_token("", TOKEN_STRING_END),

        create_string_token("unclosed { left curly, but not interpolated."),
        create_token("", TOKEN_STRING_END),

        create_string_token(""), create_token("", TOKEN_INTERPOLATE),
        create_string_token("Empty "), create_token("", TOKEN_INTERPOLATE),
        create_string_token(""), create_token("", TOKEN_INTERPOLATE),
        create_string_token("brace."), create_token("", TOKEN_STRING_END)
    );
    ZmxProgram program = create_zmx_program("testString", false);
    Lexer lexer = create_lexer(&program, source);
    while (!IS_EOF(&lexer)) {
        START_TOKEN(&lexer);
        lex_string(&lexer);
        ignore_whitespace(&lexer);
    }
    APPEND_DA(&lexer.tokens, create_token("", TOKEN_EOF));

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zmx_program(&program);

    FREE_DA(&allTokens);
}

/** 
 * Ensures that we lex tokens that are written with special characters (like ">=").
 * We test every single case in the lexer once:
 *     "<< <= < <" tests all cases in compound_assigns().
 *     "&= && &" tests all cases in two_or_default().
 *     ".. ." tests both cases in one_or_default().
 *     ";" tests that we handle something that can only be a single character correctly.
 */
PRIVATE_TEST_CASE(test_lex_chars_tokens) {
    char *source = "<<= << <= < &= && & .. . ;";
    TokenArray allTokens = CREATE_DA();
    append_test_tokens(
        &allTokens, 10,
        create_token("<<=", TOKEN_LSHIFT_EQ), create_token("<<", TOKEN_LSHIFT),
        create_token("<=", TOKEN_LESS_EQ), create_token("<", TOKEN_LESS),
        create_token("&=", TOKEN_AMPER_EQ), create_token("&&", TOKEN_AMPER_AMPER),
        create_token("&", TOKEN_AMPER), create_token("..", TOKEN_DOT_DOT),
        create_token(".", TOKEN_DOT), create_token(";", TOKEN_SEMICOLON)
    );
    ZmxProgram program = create_zmx_program("testString", false);
    Lexer lexer = create_lexer(&program, source);
    for (int i = 0; i < 4; i++) {
        START_TOKEN(&lexer);
        ADVANCE(&lexer);
        two_compound_assigns(&lexer, '<', TOKEN_LESS, TOKEN_LESS_EQ, TOKEN_LSHIFT, TOKEN_LSHIFT_EQ);
        ignore_whitespace(&lexer);
    }
    for (int i = 0; i < 3; i++) {
        START_TOKEN(&lexer);
        ADVANCE(&lexer);
        two_or_default(&lexer, TOKEN_AMPER, '&', TOKEN_AMPER_AMPER, '=', TOKEN_AMPER_EQ);
        ignore_whitespace(&lexer);
    }
    for (int i = 0; i < 2; i++) {
        START_TOKEN(&lexer);
        ADVANCE(&lexer);
        one_or_default(&lexer, TOKEN_DOT, '.', TOKEN_DOT_DOT);
        ignore_whitespace(&lexer);
    }
    START_TOKEN(&lexer);
    ADVANCE(&lexer);
    append_lexed(&lexer, TOKEN_SEMICOLON);
    APPEND_DA(&lexer.tokens, create_token("", TOKEN_EOF));

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zmx_program(&program);
    FREE_DA(&allTokens);
}


/** Tests the valid syntax checker. */
PRIVATE_TEST_CASE(test_valid_syntax) {
    ASSERT_TRUE(valid_syntax('<'));
    ASSERT_TRUE(valid_syntax('$'));
    ASSERT_FALSE(valid_syntax('@'));
    ASSERT_TRUE(valid_syntax('3'));
    ASSERT_TRUE(valid_syntax('a'));
    ASSERT_TRUE(valid_syntax('_'));
}


/** Tests the macro helpers that help the lexer in things (like peeking and advancing). */
PRIVATE_TEST_CASE(test_lexer_macro_helpers) {
    ASSERT_CHAR_EQUAL(PEEK(defaultLexer), 'f');
    ASSERT_CHAR_EQUAL(PEEK_NEXT(defaultLexer), 'l');

    char advanced = ADVANCE_PEEK(defaultLexer);
    ASSERT_CHAR_EQUAL(advanced, 'f');

    ADVANCE(defaultLexer);
    ASSERT_INT_EQUAL(CURRENT_TOKEN_LENGTH(defaultLexer), 2);
    ASSERT_CHAR_EQUAL(PEEK(defaultLexer), 'o');

    ADVANCE_DOUBLE(defaultLexer);
    ASSERT_CHAR_EQUAL(PEEK(defaultLexer), 't');
    ASSERT_INT_EQUAL(CURRENT_TOKEN_LENGTH(defaultLexer), 4);

    RETREAT(defaultLexer);
    ASSERT_CHAR_EQUAL(PEEK_NEXT(defaultLexer), 't');
    RETREAT(defaultLexer);
    ASSERT_CHAR_EQUAL(PEEK(defaultLexer), 'o');

    ASSERT_FALSE(MATCH(defaultLexer, 'h'));
    ASSERT_TRUE(MATCH(defaultLexer, 'o'));
    ASSERT_TRUE(MATCH(defaultLexer, 'a'));
    
    ASSERT_TRUE(PEEK(defaultLexer) == 't');
    ASSERT_FALSE(MATCH(defaultLexer, 'f'));
    ASSERT_TRUE(PEEK(defaultLexer) == 't');

    ASSERT_FALSE(IS_EOF(defaultLexer));
    defaultLexer->current = defaultLexer->source + defaultLexer->sourceLength;
    ASSERT_TRUE(IS_EOF(defaultLexer));

    ASSERT_CHAR_EQUAL(LOWERED_CHAR('d'), 'd');
    ASSERT_CHAR_EQUAL(LOWERED_CHAR('T'), 't');
    ASSERT_CHAR_EQUAL(LOWERED_CHAR('='), '=');

    ASSERT_TRUE(IS_ALPHA('G'));
    ASSERT_FALSE(IS_ALPHA('$'));
    ASSERT_TRUE(IS_ALPHA('_'));
    ASSERT_TRUE(IS_ALPHA('v'));

    ASSERT_TRUE(IS_DIGIT('4'));
    ASSERT_FALSE(IS_DIGIT('v'));
    ASSERT_TRUE(IS_DIGIT('2'));
}

/** Tests that alloc_current_lexeme() properly allocates and terminates the current lexeme. */
PRIVATE_TEST_CASE(test_alloc_lexeme) {
    START_TOKEN(defaultLexer);
    char *emptyStart = alloc_current_lexeme(defaultLexer);
    ASSERT_STRING_EQUAL(emptyStart, "");

    while (PEEK(defaultLexer) != ' ') {
        ADVANCE(defaultLexer);
    }
    char *floatKeyword = alloc_current_lexeme(defaultLexer);
    ASSERT_STRING_EQUAL(floatKeyword, "float");

    START_TOKEN(defaultLexer);
    char *emptyMiddle = alloc_current_lexeme(defaultLexer);
    ASSERT_STRING_EQUAL(emptyMiddle, "");

    free(emptyStart);
    free(floatKeyword);
    free(emptyMiddle);
}

/** Tests that ignore_whitespace ignores them properly. */
PRIVATE_TEST_CASE(test_handling_whitespace) {
    // There isn't whitespace in the beginning, so peek result shouldn't change.
    const char current = PEEK(defaultLexer);
    ignore_whitespace(defaultLexer);
    ASSERT_CHAR_EQUAL(current, PEEK(defaultLexer)); 

    while (PEEK(defaultLexer) != '\n') {
        ADVANCE(defaultLexer);
    }
    ASSERT_INT_EQUAL(defaultLexer->line, 1);
    ignore_whitespace(defaultLexer);
    ASSERT_INT_EQUAL(defaultLexer->line, 3);

    ignore_whitespace(defaultLexer);
    lex_token(defaultLexer);
    ASSERT_TRUE(
        equal_token(LAST_TOKEN(defaultLexer), create_token("hey", TOKEN_IDENTIFIER))
    );
    ignore_whitespace(defaultLexer);
    lex_token(defaultLexer);
    ASSERT_TRUE(
        equal_token(LAST_TOKEN(defaultLexer), create_token("end", TOKEN_IDENTIFIER))
    );
}

/** Tests the relevant functions related to structs. */
PRIVATE_TEST_CASE(test_lexer_struct_functions) {
    const Token created = create_token_pos("test", 2, 2, TOKEN_IDENTIFIER);
    const Token manual = {
        .lexeme = "test", .pos = create_src_pos(2, 2, 4), .type = TOKEN_IDENTIFIER
    };
    ASSERT_TRUE(equal_token(manual, created));
    ASSERT_INT_EQUAL(manual.pos.line, created.pos.line);
    ASSERT_INT_EQUAL(manual.pos.column, created.pos.column);

    // The setup already uses create_lexer(), so just test it on it.
    ASSERT_STRING_EQUAL(defaultLexer->program->currentFile->string, "default");
    ASSERT_STRING_EQUAL(defaultLexer->source, defaultSource);
    ASSERT_TRUE(
        defaultLexer->source == defaultLexer->current
        && defaultLexer->current == defaultLexer->tokenStart
    );
    ASSERT_INT_EQUAL(defaultLexer->line, 1);
    ASSERT_INT_EQUAL(defaultLexer->sourceLength, strlen(defaultSource));

    while (PEEK(defaultLexer) != ' ') {
        ADVANCE(defaultLexer);
    }
    append_lexed(defaultLexer, TOKEN_FLOAT_KW);
    Token keyword = create_token_pos("float", 1, 2, TOKEN_FLOAT_KW);
    ASSERT_TRUE(equal_token(LAST_TOKEN(defaultLexer), keyword));

    append_implicit(defaultLexer, TOKEN_INTERPOLATE);
    ASSERT_TRUE(equal_token(
        LAST_TOKEN(defaultLexer),
        create_token_pos("", defaultLexer->line, defaultLexer->column, TOKEN_INTERPOLATE)
    ));

    ADVANCE(defaultLexer);
    START_TOKEN(defaultLexer);
    ADVANCE_DOUBLE(defaultLexer);
    ADVANCE(defaultLexer);
    append_implicit(defaultLexer, TOKEN_STRING_END);
    ASSERT_TRUE(equal_token(
        LAST_TOKEN(defaultLexer),
        create_token_pos("", defaultLexer->line, defaultLexer->column, TOKEN_STRING_END)
    ));
}

/** Tests that errors are made and appended correctly. */
PRIVATE_TEST_CASE(test_error_functions) {
    append_error_at(defaultLexer, defaultLexer->source + 2, create_src_pos(1, 1, 4), "at");
    const Token manualAt = {
        .lexeme = defaultLexer->source + 2, .pos = create_src_pos(1, 1, 4), .type = TOKEN_ERROR,
    };
    ASSERT_TRUE(equal_token(LAST_TOKEN(defaultLexer), manualAt));
    
    while (!IS_EOF(defaultLexer) && PEEK(defaultLexer) != ' ') {
        ADVANCE(defaultLexer);
    }
    append_lexed_error(defaultLexer, "lexed");
    const Token manualLexed = {
        .lexeme = defaultLexer->source, .pos = create_src_pos(1, 1, 5), .type = TOKEN_ERROR,
    };
    ASSERT_TRUE(equal_token(LAST_TOKEN(defaultLexer), manualLexed));
}

/** Tests the helper functions that return whether something is within a base or not. */
PRIVATE_TEST_CASE(test_base_number_checkers) {
    ASSERT_TRUE(is_bin_digit('1'));
    ASSERT_TRUE(is_bin_digit('0'));
    ASSERT_FALSE(is_bin_digit('3'));
    ASSERT_FALSE(is_bin_digit('f'));

    ASSERT_TRUE(is_oct_digit('0'));
    ASSERT_TRUE(is_oct_digit('7'));
    ASSERT_TRUE(is_oct_digit('4'));
    ASSERT_FALSE(is_oct_digit('8'));
    ASSERT_FALSE(is_oct_digit('9'));
    ASSERT_FALSE(is_oct_digit('T'));
    
    ASSERT_TRUE(is_hex_digit('0'));
    ASSERT_TRUE(is_hex_digit('5'));
    ASSERT_TRUE(is_hex_digit('9'));
    ASSERT_TRUE(is_hex_digit('a'));
    ASSERT_TRUE(is_hex_digit('e'));
    ASSERT_TRUE(is_hex_digit('F'));
    ASSERT_FALSE(is_hex_digit('T'));
    ASSERT_FALSE(is_hex_digit('u'));
}

/** Tests lexer.c. */
void test_lexer() {
    MAKE_FIXTURE(setup_default_lexer, teardown_default_lexer);
    TEST(test_base_number_checkers);
    TEST(test_valid_syntax);
    TEST(test_handling_whitespace);
    TEST(test_alloc_lexeme);
    TEST(test_lexer_macro_helpers);
    TEST(test_lexer_struct_functions);
    TEST(test_error_functions);
    RESET_FIXTURE();

    TEST(test_lex_number);
    TEST(test_lex_name);
    TEST(test_lex_string);
    TEST(test_lex_chars_tokens);

    TEST(test_lex_successful_programs);
    TEST(test_lex_errors);
    TEST(test_lex_all_tokens);
    TEST(test_lex_spots);
}
