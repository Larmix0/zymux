#include <stdarg.h>
#include <stdio.h>

#include "lukip.h"
#include "debug_tokens.h"

#include "lexer.c"

#define LAST_TOKEN(lexer) ((lexer)->tokens.tokens[(lexer)->tokens.length - 1])

char *defaultSource = "float && var (); $'str {2 * 3}' break; 22 44.2"
    "\n// single line\n hey /*muti-\nline*/ end";
Lexer *defaultLexer;

static void setup_default_lexer() {
    ZymuxProgram *program = ZMX_ALLOC(ZymuxProgram);
    *program = create_zymux_program("default", false);

    defaultLexer = ZMX_ALLOC(Lexer);
    *defaultLexer = create_lexer(program, defaultSource);
}

static void teardown_default_lexer() {
    free_zymux_program(defaultLexer->program);
    free(defaultLexer->program);
    free_lexer(defaultLexer);
    free(defaultLexer);
}

static Token test_token(char *lexeme, TokenType type) {
    Token token = create_token(lexeme, 0, 0, type);
    return token;
}

static Token test_token_string(char *lexeme) {
    Token token = create_token(lexeme, 0, 0, TOKEN_STRING_LIT);
    token.stringVal = create_char_buffer();
    buffer_append_string(&token.stringVal, lexeme);
    return token;
}

static Token test_token_int(char *lexeme, int base) {
    Token token = create_token(lexeme, 0, 0, TOKEN_INT_LIT);
    token.intVal = strtol(lexeme, NULL, base);
    return token;
}

static void append_test_tokens(TokenArray *tokens, const int amount, ...) {
    va_list args;
    va_start(args, amount);
    for (int i = 0; i < amount; i++) {
        append_token(tokens, va_arg(args, Token));
    }
    append_token(tokens, test_token("", TOKEN_EOF));
    va_end(args);
}

static void compare_lexed(Lexer *lexer, TokenArray *expectedArray, bool compareLines) {
    const int lexerLength = lexer->tokens.length;
    const int expectedLength = expectedArray->length;
    LUKIP_INT_EQUAL(lexer->tokens.length, expectedArray->length);

    const int shortest = lexerLength > expectedLength ? expectedLength : lexerLength;
    for (int tokenIdx = 0; tokenIdx < shortest; tokenIdx++) {
        Token lexed = lexer->tokens.tokens[tokenIdx];
        Token expected = expectedArray->tokens[tokenIdx];
        LUKIP_CUSTOM(
            tokens_equal(lexed, expected),
            "Token %d (lexeme=\"%.*s\" type=%s) != (lexeme=\"%s\", type=%s)",
            tokenIdx + 1, lexed.length, lexed.lexeme,
            type_to_string(lexed.type), expected.lexeme, type_to_string(expected.type)
        );
        if (compareLines) {
            LUKIP_INT_EQUAL(lexed.line, expected.line);
        }
    }
}

// TODO: test newlines working.
static void test_lex_successful_programs() {
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
    TokenArray *tokens2DArray = ZMX_ARRAY_ALLOC(sourcesAmount, TokenArray);
    for (size_t i = 0; i < sourcesAmount; i++) {
        tokens2DArray[i] = create_token_array();
    }
    append_test_tokens(&tokens2DArray[0], 0);
    append_test_tokens(
        &tokens2DArray[1], 14,
        test_token("(", TOKEN_LPAR), test_token_int("2", 10),
        test_token("+", TOKEN_PLUS), test_token_int("3", 10),
        test_token(")", TOKEN_RPAR), test_token("*", TOKEN_STAR), 
        test_token_int("30", 10), test_token("**", TOKEN_EXPO),
        test_token_int("2", 10), test_token("/", TOKEN_SLASH),
        test_token("1.5", TOKEN_FLOAT_LIT), test_token("-", TOKEN_MINUS),
        test_token("0.2", TOKEN_FLOAT_LIT), test_token(";", TOKEN_SEMICOLON)
    );
    append_test_tokens(&tokens2DArray[2], 1, test_token("<<=", TOKEN_LSHIFT_EQ));
    append_test_tokens(&tokens2DArray[3], 0);

    append_test_tokens(
        &tokens2DArray[4], 4,
        test_token("variable", TOKEN_IDENTIFIER), test_token("*", TOKEN_STAR),
        test_token("/", TOKEN_SLASH), test_token("endvar", TOKEN_IDENTIFIER)
    );
    append_test_tokens(
        &tokens2DArray[5], 1, test_token("a", TOKEN_IDENTIFIER)
    );
    append_test_tokens(&tokens2DArray[6], 0);
    append_test_tokens(&tokens2DArray[7], 0);
    append_test_tokens(
        &tokens2DArray[8], 4,
        test_token("hello", TOKEN_IDENTIFIER), test_token("match", TOKEN_MATCH_KW),
        test_token(";", TOKEN_SEMICOLON), test_token("int", TOKEN_INT_KW)
    );
    append_test_tokens(
        &tokens2DArray[9], 3,
        test_token_int("345", 10), test_token("-", TOKEN_MINUS),
        test_token_int("234", 10)
    );
    append_test_tokens(
        &tokens2DArray[10], 4,
        test_token("ide", TOKEN_IDENTIFIER), test_token_string("Bare string."),
        test_token("", TOKEN_STRING_END), test_token("end", TOKEN_IDENTIFIER)
    );

    for (size_t arrayIdx = 0; arrayIdx < sourcesAmount; arrayIdx++) {
        ZymuxProgram program = create_zymux_program("testLex", false);
        Lexer lexer = create_lexer(&program, sources[arrayIdx]);
        lex(&lexer);

        compare_lexed(&lexer, &tokens2DArray[arrayIdx], false);
        free_lexer(&lexer);
        free_zymux_program(&program);
    }
    for (size_t i = 0; i < sourcesAmount; i++) {
        free_token_array(&tokens2DArray[i]);
    }
    free(tokens2DArray);
}

static void test_lex_errors() {
    char *sources[] = {
        "SomeIdentifier 'Unterminated string.",
        "/* Unterminated multiline comment.",
        "0b34 // Binary is only 1s and 0s",
        "23 + 0x",
        "@\\@@ invalidSyntax.",
        "#$",
        "#$###$#$'Repeated.'"
    }; 
    const size_t sourcesAmount = sizeof(sources) / sizeof(char *);
    for (size_t i = 0; i < sourcesAmount; i++) {
        ZymuxProgram program = create_zymux_program("testError", false);
        Lexer lexer = create_lexer(&program, sources[i]);
        lex(&lexer);
        LUKIP_IS_TRUE(lexer.program->hasErrored);
        free_lexer(&lexer);
    }
}

static void test_lex_all_tokens() {
    char *source = "string int float bool list class const let private func if else while for do "
        "return break continue true false null as is in super this init abstract inherits "
        "match case default from import && || ! & | ^ << >> ~ += -= *= /= %= **= &= |= <<= >>= ~= "
        "+-*/%** == != > >= < <= ()[]{} ; , . ?: $'str literal {100}' 22 44.2 = variable ..";
    TokenArray allTokens = create_token_array();
    append_test_tokens(
        &allTokens, 86,
        test_token("string", TOKEN_STRING_KW), test_token("int", TOKEN_INT_KW),
        test_token("float", TOKEN_FLOAT_KW), test_token("bool", TOKEN_BOOL_KW),
        test_token("list", TOKEN_LIST_KW), test_token("class", TOKEN_CLASS_KW),
        test_token("const", TOKEN_CONST_KW), test_token("let", TOKEN_LET_KW),
        test_token("private", TOKEN_PRIVATE_KW), test_token("func", TOKEN_FUNC_KW),
        test_token("if", TOKEN_IF_KW), test_token("else", TOKEN_ELSE_KW),
        test_token("while", TOKEN_WHILE_KW), test_token("for", TOKEN_FOR_KW),
        test_token("do", TOKEN_DO_KW), test_token("return", TOKEN_RETURN_KW),
        test_token("break", TOKEN_BREAK_KW), test_token("continue", TOKEN_CONTINUE_KW),
        test_token("true", TOKEN_TRUE_KW), test_token("false", TOKEN_FALSE_KW),
        test_token("null", TOKEN_NULL_KW), test_token("as", TOKEN_AS_KW),
        test_token("is", TOKEN_IS_KW), test_token("in", TOKEN_IN_KW),
        test_token("super", TOKEN_SUPER_KW), test_token("this", TOKEN_THIS_KW),
        test_token("init", TOKEN_INIT_KW), test_token("abstract", TOKEN_ABSTRACT_KW),
        test_token("inherits", TOKEN_INHERITS_KW), test_token("match", TOKEN_MATCH_KW),
        test_token("case", TOKEN_CASE_KW), test_token("default", TOKEN_DEFAULT_KW),
        test_token("from", TOKEN_FROM_KW), test_token("import", TOKEN_IMPORT_KW),
        test_token("&&", TOKEN_AMPER_AMPER), test_token("||", TOKEN_BAR_BAR),
        test_token("!", TOKEN_BANG), test_token("&", TOKEN_AMPER),
        test_token("|", TOKEN_BAR), test_token("^", TOKEN_CARET),
        test_token("<<", TOKEN_LSHIFT), test_token(">>", TOKEN_RSHIFT),
        test_token("~", TOKEN_TILDE), test_token("+=", TOKEN_PLUS_EQ),
        test_token("-=", TOKEN_MINUS_EQ), test_token("*=", TOKEN_STAR_EQ),
        test_token("/=", TOKEN_SLASH_EQ), test_token("%=", TOKEN_MODULO_EQ),
        test_token("**=", TOKEN_EXPO_EQ), test_token("&=", TOKEN_AMPER_EQ),
        test_token("|=", TOKEN_BAR_EQ), test_token("<<=", TOKEN_LSHIFT_EQ),
        test_token(">>=", TOKEN_RSHIFT_EQ), test_token("~=", TOKEN_TILDE_EQ),
        test_token("+", TOKEN_PLUS), test_token("-", TOKEN_MINUS),
        test_token("*", TOKEN_STAR), test_token("/", TOKEN_SLASH),
        test_token("%", TOKEN_MODULO), test_token("**", TOKEN_EXPO),
        test_token("==", TOKEN_EQ_EQ), test_token("!=", TOKEN_BANG_EQ),
        test_token(">", TOKEN_GREATER), test_token(">=", TOKEN_GREATER_EQ),
        test_token("<", TOKEN_LESS), test_token("<=", TOKEN_LESS_EQ),
        test_token("(", TOKEN_LPAR), test_token(")", TOKEN_RPAR),
        test_token("[", TOKEN_LSQUARE), test_token("]", TOKEN_RSQUARE),
        test_token("{", TOKEN_LCURLY), test_token("}", TOKEN_RCURLY),
        test_token(";", TOKEN_SEMICOLON), test_token(",", TOKEN_COMMA),
        test_token(".", TOKEN_DOT), test_token("?", TOKEN_QUESTION_MARK),
        test_token(":", TOKEN_COLON), test_token_string("str literal "),
        test_token("", TOKEN_FORMAT), test_token_int("100", 10),
        test_token("", TOKEN_STRING_END), test_token_int("22", 10),
        test_token("44.2", TOKEN_FLOAT_LIT), test_token("=", TOKEN_EQ),
        test_token("variable", TOKEN_IDENTIFIER), test_token("..", TOKEN_DOT_DOT)
    );
    ZymuxProgram program = create_zymux_program("testAll", false);
    Lexer lexer = create_lexer(&program, source);
    lex(&lexer);

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zymux_program(&program);
    free_token_array(&allTokens);
}

static void test_lex_lines() {
    char *source = "line1\n line2 2.2\n\n\n line5 555\n\n";
    TokenArray allTokens = create_token_array();
    append_test_tokens(
        &allTokens, 5,
        create_token("line1", 1, 1, TOKEN_IDENTIFIER),
        create_token("line2", 2, 2, TOKEN_IDENTIFIER),
        create_token("2.2", 2, 2, TOKEN_FLOAT_LIT),
        create_token("line5", 5, 2, TOKEN_IDENTIFIER),
        (Token){.lexeme = "555", .length = 3, .line = 5, .type = TOKEN_INT_LIT, .intVal = 555}
    );
    allTokens.tokens[allTokens.length - 1].line = 7; // Set EOF line.
    ZymuxProgram program = create_zymux_program("testLine", false);
    Lexer lexer = create_lexer(&program, source);
    lex(&lexer);

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zymux_program(&program);
    free_token_array(&allTokens);
}

static void test_lex_number() {
    char *source = "0x0 0x00ff0 0xea2301 0b0 0b101 0b010 0b00100110111 0o00 0o02707 0o241"
        " 0 0.0 0.010 0013 002.3300 931453229 23.3 3.0";

    TokenArray allTokens = create_token_array();
    append_test_tokens(
        &allTokens, 18,
        test_token_int("0", 16), test_token_int("ff0", 16),
        test_token_int("ea2301", 16), test_token_int("0", 2),
        test_token_int("101", 2), test_token_int("10", 2),
        test_token_int("100110111", 2), test_token_int("0", 8),
        test_token_int("2707", 8), test_token_int("241", 8),
        test_token_int("0", 10), test_token("0.0", TOKEN_FLOAT_LIT),
        test_token("0.010", TOKEN_FLOAT_LIT), test_token_int("13", 10),
        test_token("2.3300", TOKEN_FLOAT_LIT), test_token_int("931453229", 10),
        test_token("23.3", TOKEN_FLOAT_LIT), test_token("3.0", TOKEN_FLOAT_LIT)
    );
    ZymuxProgram program = create_zymux_program("testNumber", false);
    Lexer lexer = create_lexer(&program, source);
    while (!IS_EOF(&lexer)) {
        START_TOKEN(&lexer);
        lex_number(&lexer);
        ignore_whitespace(&lexer);
    }
    append_token(&lexer.tokens, test_token("", TOKEN_EOF));

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zymux_program(&program);
    free_token_array(&allTokens);
}

static void test_lex_name() {
    char *source = "hello returnme break _ notKeyword__ float L2dm3e44 _22_ string _NAME_HERE";
    TokenArray allTokens = create_token_array();
    append_test_tokens(
        &allTokens, 10,
        test_token("hello", TOKEN_IDENTIFIER), test_token("returnme", TOKEN_IDENTIFIER),
        test_token("break", TOKEN_BREAK_KW), test_token("_", TOKEN_IDENTIFIER),
        test_token("notKeyword__", TOKEN_IDENTIFIER), test_token("float", TOKEN_FLOAT_KW),
        test_token("L2dm3e44", TOKEN_IDENTIFIER), test_token("_22_", TOKEN_IDENTIFIER),
        test_token("string", TOKEN_STRING_KW), test_token("_NAME_HERE", TOKEN_IDENTIFIER)
    );
    ZymuxProgram program = create_zymux_program("testName", false);
    Lexer lexer = create_lexer(&program, source);
    while (!IS_EOF(&lexer)) {
        START_TOKEN(&lexer);
        lex_name(&lexer);
        ignore_whitespace(&lexer);
    }
    append_token(&lexer.tokens, test_token("", TOKEN_EOF));

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zymux_program(&program);
    free_token_array(&allTokens);
}

static void test_lex_string() {
    char *source = "'Normal\\t string\\\\n w/escapes'"
        "#'This is a\\t raw string.\\n\\\\n'"
        "$'Interpolated \\{ <- escaped {3 * $\"And {2 + 3} is nested\" + 2}'"
        "$#'Interpolated raw \\{2 + 3}'"
        "$'{$\"{1 + 2}\"} end'"
        "$' { $\" { 7 ** 23 } \" } '"
        "$'unclosed { left curly.'";

    TokenArray allTokens = create_token_array();
        append_test_tokens(
        &allTokens, 51,
        test_token_string("Normal\t string\\n w/escapes"),
        test_token("", TOKEN_STRING_END),
        test_token_string("This is a\\t raw string.\\n\\\\n"),
        test_token("", TOKEN_STRING_END),
        test_token_string("Interpolated { <- escaped "),
        test_token("", TOKEN_FORMAT), test_token_int("3", 10),
        test_token("*", TOKEN_STAR), test_token_string("And "),
        test_token("", TOKEN_FORMAT), test_token_int("2", 10),
        test_token("+", TOKEN_PLUS), test_token_int("3", 10),
        test_token("", TOKEN_FORMAT), test_token_string(" is nested"),
        test_token("", TOKEN_STRING_END), test_token("+", TOKEN_PLUS),
        test_token_int("2", 10), test_token("", TOKEN_STRING_END),
        test_token_string("Interpolated raw \\"), test_token("", TOKEN_FORMAT),
        test_token_int("2", 10), test_token("+", TOKEN_PLUS),
        test_token_int("3", 10), test_token("", TOKEN_STRING_END),
        test_token_string(""), test_token("", TOKEN_FORMAT),
        test_token_string(""), test_token("", TOKEN_FORMAT),
        test_token_int("1", 10), test_token("+", TOKEN_PLUS),
        test_token_int("2", 10), test_token("", TOKEN_STRING_END),
        test_token("", TOKEN_FORMAT), test_token_string(" end"),
        test_token("", TOKEN_STRING_END),
        test_token_string(" "), test_token("", TOKEN_FORMAT),
        test_token_string(" "), test_token("", TOKEN_FORMAT),
        test_token_int("7", 10), test_token("**", TOKEN_EXPO),
        test_token_int("23", 10), test_token("", TOKEN_FORMAT),
        test_token_string(" "), test_token("", TOKEN_STRING_END),
        test_token("", TOKEN_FORMAT), test_token_string(" "),
        test_token("", TOKEN_STRING_END),
        test_token_string("unclosed { left curly."), test_token("", TOKEN_STRING_END)
    );
    ZymuxProgram program = create_zymux_program("testString", false);
    Lexer lexer = create_lexer(&program, source);
    while (!IS_EOF(&lexer)) {
        START_TOKEN(&lexer);
        lex_string(&lexer);
        ignore_whitespace(&lexer);
    }
    append_token(&lexer.tokens, test_token("", TOKEN_EOF));

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zymux_program(&program);
    free_token_array(&allTokens);
}

static void test_lex_chars_tokens() {
    char *source = "<<= << <= < &= && & .. . ;";
    TokenArray allTokens = create_token_array();
    append_test_tokens(
        &allTokens, 10,
        test_token("<<=", TOKEN_LSHIFT_EQ), test_token("<<", TOKEN_LSHIFT),
        test_token("<=", TOKEN_LESS_EQ), test_token("<", TOKEN_LESS),
        test_token("&=", TOKEN_AMPER_EQ), test_token("&&", TOKEN_AMPER_AMPER),
        test_token("&", TOKEN_AMPER), test_token("..", TOKEN_DOT_DOT),
        test_token(".", TOKEN_DOT), test_token(";", TOKEN_SEMICOLON)
    );
    ZymuxProgram program = create_zymux_program("testString", false);
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
    append_token(&lexer.tokens, test_token("", TOKEN_EOF));

    compare_lexed(&lexer, &allTokens, false);
    free_lexer(&lexer);
    free_zymux_program(&program);
    free_token_array(&allTokens);
}

static void test_valid_syntax() {
    LUKIP_IS_TRUE(valid_syntax('<'));
    LUKIP_IS_TRUE(valid_syntax('$'));
    LUKIP_IS_FALSE(valid_syntax('@'));
    LUKIP_IS_TRUE(valid_syntax('3'));
    LUKIP_IS_TRUE(valid_syntax('a'));
    LUKIP_IS_TRUE(valid_syntax('_'));
}


static void test_lexer_macro_helpers() {
    LUKIP_CHAR_EQUAL(PEEK(defaultLexer), 'f');
    LUKIP_CHAR_EQUAL(PEEK_DISTANCE(defaultLexer, 7), '&');

    char advanced = ADVANCE_PEEK(defaultLexer);
    LUKIP_CHAR_EQUAL(advanced, 'f');

    ADVANCE(defaultLexer);
    LUKIP_INT_EQUAL(CURRENT_TOKEN_LENGTH(defaultLexer), 2);
    LUKIP_CHAR_EQUAL(PEEK(defaultLexer), 'o');

    ADVANCE_AMOUNT(defaultLexer, 2);
    LUKIP_CHAR_EQUAL(PEEK(defaultLexer), 't');
    LUKIP_CHAR_EQUAL(PEEK_DISTANCE(defaultLexer, -1), 'a');
    LUKIP_INT_EQUAL(CURRENT_TOKEN_LENGTH(defaultLexer), 4);

    RETREAT(defaultLexer);
    LUKIP_CHAR_EQUAL(PEEK(defaultLexer), 'a');

    LUKIP_IS_FALSE(IS_EOF(defaultLexer));
    defaultLexer->current = defaultLexer->source + defaultLexer->sourceLength;
    LUKIP_IS_TRUE(IS_EOF(defaultLexer));

    LUKIP_CHAR_EQUAL(LOWERED_CHAR('d'), 'd');
    LUKIP_CHAR_EQUAL(LOWERED_CHAR('T'), 't');
    LUKIP_CHAR_EQUAL(LOWERED_CHAR('='), '=');

    LUKIP_IS_TRUE(IS_ALPHA('G'));
    LUKIP_IS_FALSE(IS_ALPHA('$'));
    LUKIP_IS_TRUE(IS_ALPHA('_'));
    LUKIP_IS_TRUE(IS_ALPHA('v'));

    LUKIP_IS_TRUE(IS_DIGIT('4'));
    LUKIP_IS_FALSE(IS_DIGIT('v'));
    LUKIP_IS_TRUE(IS_DIGIT('2'));
}

static void test_handling_whitespace() {
    // There isn't whitespace in the beginning, so peek result shouldn't change.
    char current = PEEK(defaultLexer);
    ignore_whitespace(defaultLexer);
    LUKIP_CHAR_EQUAL(current, PEEK(defaultLexer)); 

    while (PEEK(defaultLexer) != '\n') {
        ADVANCE(defaultLexer);
    }
    LUKIP_INT_EQUAL(defaultLexer->line, 1);
    ignore_whitespace(defaultLexer);
    LUKIP_INT_EQUAL(defaultLexer->line, 3);

    ignore_whitespace(defaultLexer);
    lex_token(defaultLexer);
    LUKIP_IS_TRUE(
        tokens_equal(LAST_TOKEN(defaultLexer), create_token("hey", 3, 2, TOKEN_IDENTIFIER))
    );
    ignore_whitespace(defaultLexer);
    lex_token(defaultLexer);
    LUKIP_IS_TRUE(
        tokens_equal(LAST_TOKEN(defaultLexer), create_token("end", 3, 8, TOKEN_IDENTIFIER))
    );
}

static void test_lexer_struct_functions() {
    Token stackLexeme = create_token("test", 2, 2, TOKEN_IDENTIFIER);
    Token heapLexeme = create_token("test", 2, 2, TOKEN_IDENTIFIER);
    Token manual = {.lexeme = "test", .length = 4, .line = 2, .type = TOKEN_IDENTIFIER};
    LUKIP_IS_TRUE(tokens_equal(manual, stackLexeme));
    LUKIP_INT_EQUAL(manual.line, stackLexeme.line);
    LUKIP_IS_TRUE(tokens_equal(manual, heapLexeme));
    LUKIP_INT_EQUAL(manual.line, heapLexeme.line);

    // The setup already uses create_lexer(), so just test it on it.
    LUKIP_STRING_EQUAL(defaultLexer->program->currentFile, "default");
    LUKIP_STRING_EQUAL(defaultLexer->source, defaultSource);
    LUKIP_IS_TRUE(
        defaultLexer->source == defaultLexer->current
        && defaultLexer->current == defaultLexer->tokenStart
    );
    LUKIP_INT_EQUAL(defaultLexer->line, 1);
    LUKIP_INT_EQUAL(defaultLexer->sourceLength, strlen(defaultSource));

    while (PEEK(defaultLexer) != ' ') {
        ADVANCE(defaultLexer);
    }
    append_lexed(defaultLexer, TOKEN_FLOAT_KW);
    Token keyword = create_token("float", 1, 2, TOKEN_FLOAT_KW);
    LUKIP_IS_TRUE(tokens_equal(LAST_TOKEN(defaultLexer), keyword));

    TokenArray tokens = create_token_array();
    append_token(&tokens, heapLexeme); // Passes freeing responsibility to token array.
    LUKIP_IS_TRUE(tokens_equal(tokens.tokens[0], heapLexeme));

    append_token(&tokens, implicit_token(defaultLexer, TOKEN_FORMAT));
    LUKIP_IS_TRUE(tokens_equal(
        tokens.tokens[1], create_token("", defaultLexer->line, defaultLexer->column, TOKEN_FORMAT)
    ));

    ADVANCE(defaultLexer);
    START_TOKEN(defaultLexer);
    ADVANCE_AMOUNT(defaultLexer, 3);
    append_token(&tokens, implicit_token(defaultLexer, TOKEN_STRING_END));
    LUKIP_IS_TRUE(tokens_equal(
        tokens.tokens[2],
        create_token("", defaultLexer->line, defaultLexer->column, TOKEN_STRING_END)
    ));

    free_token_array(&tokens);
}

static void test_error_functions() {
    append_error_at(defaultLexer, "at", defaultLexer->source + 2, 4, 1, 1);
    Token manualAt = {
        .lexeme = defaultLexer->source + 2, .length = 4,
        .line = 1, .column = 1,
        .errorMessage = "lexed", .type = TOKEN_ERROR,
    };
    LUKIP_IS_TRUE(tokens_equal(LAST_TOKEN(defaultLexer), manualAt));
    
    while (!IS_EOF(defaultLexer) && PEEK(defaultLexer) != ' ') {
        ADVANCE(defaultLexer);
    }
    append_lexed_error(defaultLexer, "lexed");
    Token manualLexed = {
        .lexeme = defaultLexer->source, .length = 5,
        .line = 1, .column = 1,
        .errorMessage = "lexed", .type = TOKEN_ERROR,
    };
    LUKIP_IS_TRUE(tokens_equal(LAST_TOKEN(defaultLexer), manualLexed));
}

static void test_base_number_checkers() {
    LUKIP_IS_TRUE(is_bin_digit('1'));
    LUKIP_IS_TRUE(is_bin_digit('0'));
    LUKIP_IS_FALSE(is_bin_digit('3'));
    LUKIP_IS_FALSE(is_bin_digit('f'));

    LUKIP_IS_TRUE(is_oct_digit('0'));
    LUKIP_IS_TRUE(is_oct_digit('7'));
    LUKIP_IS_TRUE(is_oct_digit('4'));
    LUKIP_IS_FALSE(is_oct_digit('8'));
    LUKIP_IS_FALSE(is_oct_digit('9'));
    LUKIP_IS_FALSE(is_oct_digit('T'));
    
    LUKIP_IS_TRUE(is_hex_digit('0'));
    LUKIP_IS_TRUE(is_hex_digit('5'));
    LUKIP_IS_TRUE(is_hex_digit('9'));
    LUKIP_IS_TRUE(is_hex_digit('a'));
    LUKIP_IS_TRUE(is_hex_digit('e'));
    LUKIP_IS_TRUE(is_hex_digit('F'));
    LUKIP_IS_FALSE(is_hex_digit('T'));
    LUKIP_IS_FALSE(is_hex_digit('u'));
}

static void test_tokens_equal() {
    Token equal1 = {.lexeme = "test", .length = 4, .line = 2, .type = TOKEN_IDENTIFIER};
    Token equal2 = {.lexeme = "test", .length = 4, .line = 2, .type = TOKEN_IDENTIFIER};
    LUKIP_IS_TRUE(tokens_equal(equal1, equal2));

    Token notEqual1 = {.lexeme = "first", .length = 3, .line = 2, .type = TOKEN_IDENTIFIER};
    Token notEqual2 = {.lexeme = "different", .length = 3, .line = 2, .type = TOKEN_IDENTIFIER};
    LUKIP_IS_FALSE(tokens_equal(notEqual1, notEqual2));
}

void test_lexer() {
    TEST(test_tokens_equal);
    TEST(test_base_number_checkers);

    MAKE_TEST_FIXTURE(setup_default_lexer, teardown_default_lexer);
    TEST(test_valid_syntax);
    TEST(test_handling_whitespace);
    TEST(test_lexer_macro_helpers);
    TEST(test_lexer_struct_functions);
    TEST(test_error_functions);
    RESET_TEST_FIXTURE();

    TEST(test_lex_number);
    TEST(test_lex_name);
    TEST(test_lex_string);
    TEST(test_lex_chars_tokens);

    TEST(test_lex_successful_programs);
    TEST(test_lex_errors);
    TEST(test_lex_all_tokens);
    TEST(test_lex_lines);
}
