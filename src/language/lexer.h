#ifndef LEXER_H
#define LEXER_H

#include "zymux_program.h"

#define TOKEN_IS_TYPE(token, expected) (token.type == expected)

typedef enum {
    /** Data type keyword. */
    TOKEN_STRING_KW, TOKEN_INT_KW, TOKEN_FLOAT_KW, TOKEN_BOOL_KW, TOKEN_LIST_KW,

    /** Identifier declaration keyword. */
    TOKEN_CLASS_KW, TOKEN_CONST_KW, TOKEN_LET_KW, TOKEN_PRIVATE_KW, TOKEN_FUNC_KW,

    /** Control flow keyword. */
    TOKEN_IF_KW, TOKEN_ELSE_KW, TOKEN_WHILE_KW, TOKEN_FOR_KW, TOKEN_DO_KW,

    /** Jump keyword. */
    TOKEN_RETURN_KW, TOKEN_BREAK_KW, TOKEN_CONTINUE_KW,

    /** Value keyword. */
    TOKEN_TRUE_KW, TOKEN_FALSE_KW, TOKEN_NULL_KW,

    /** operator keyword. */
    TOKEN_AS_KW, TOKEN_IS_KW, TOKEN_IN_KW,

    /** OOP-related keyword. */
    TOKEN_SUPER_KW, TOKEN_THIS_KW, TOKEN_INIT_KW, TOKEN_ABSTRACT_KW, TOKEN_INHERITS_KW,

    /** Match-related keyword. */
    TOKEN_MATCH_KW, TOKEN_CASE_KW, TOKEN_DEFAULT_KW,

    /** Import-related keyword. */
    TOKEN_FROM_KW, TOKEN_IMPORT_KW,

    /** Logical operator. */
    TOKEN_AMPER_AMPER, TOKEN_BAR_BAR, TOKEN_BANG,

    /** Bitwise operator. */
    TOKEN_AMPER, TOKEN_BAR, TOKEN_CARET, TOKEN_LSHIFT, TOKEN_RSHIFT, TOKEN_TILDE,

    /** Compound arithmetic assignment operator. */
    TOKEN_PLUS_EQ, TOKEN_MINUS_EQ, TOKEN_STAR_EQ, TOKEN_SLASH_EQ, TOKEN_MODULO_EQ, TOKEN_EXPO_EQ,

    /** Compound bitwise assignment operator. */
    TOKEN_AMPER_EQ, TOKEN_BAR_EQ, TOKEN_CARET_EQ, TOKEN_LSHIFT_EQ, TOKEN_RSHIFT_EQ, TOKEN_TILDE_EQ,

    /** Arithmetic operator. */
    TOKEN_PLUS, TOKEN_MINUS, TOKEN_STAR, TOKEN_SLASH, TOKEN_MODULO, TOKEN_EXPO,

    /** Equality operator.*/
    TOKEN_EQ_EQ, TOKEN_BANG_EQ,

    /** Comparison operator. */
    TOKEN_GREATER, TOKEN_GREATER_EQ, TOKEN_LESS, TOKEN_LESS_EQ,

    /** Bracket-type. */
    TOKEN_LPAR, TOKEN_RPAR, TOKEN_LSQUARE, TOKEN_RSQUARE, TOKEN_LCURLY, TOKEN_RCURLY,

    /** Punctuation. */
    TOKEN_SEMICOLON, TOKEN_COMMA, TOKEN_DOT, TOKEN_QUESTION_MARK, TOKEN_COLON,

    /** Related to interpolated strings. */
    TOKEN_FORMAT, TOKEN_STRING_END,

    /** Literal. */
    TOKEN_STRING_LIT, TOKEN_INT_LIT, TOKEN_FLOAT_LIT,

    /** Miscellaneous token. */
    TOKEN_EQ, TOKEN_IDENTIFIER, TOKEN_DOT_DOT, TOKEN_EOF, TOKEN_ERROR, TOKEN_NONE
} TokenType;

typedef struct {
    char *lexeme;
    size_t length;
    int line;
    TokenType type;
    // TODO: Have an enum field for ints and strings, ints are just ZmxInts. Strings are allocated.
    // While instead having it point to source for most other things.
} Token;

typedef struct {
    int length;
    int capacity;
    Token *tokens;
} TokenArray;

typedef struct {
    char *tokenStart;
    char *current;
    char *source;
    size_t sourceLength;
    int line;
    ZymuxProgram *program;
    TokenArray tokens;
} Lexer;

TokenArray create_token_array();
void append_token(TokenArray *tokens, Token token);
Token create_token(char *message, const int line, const TokenType type);
Token token_alloc_lexeme(const char *message, const int line, const TokenType type);
bool tokens_equal(const Token left, const Token right);
void free_token_array(TokenArray *tokens);

Lexer create_lexer(ZymuxProgram *program, char *source);
bool lex(Lexer *lexer);
void free_lexer(Lexer *lexer);

#endif
