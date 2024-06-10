#ifndef TOKEN_H
#define TOKEN_H

#include "constants.h"
#include "data_structures.h"
#include "report_error.h"

/** Every type of token that Zymux supports. */
typedef enum {
    /** Data type keyword. */
    TOKEN_STRING_KW, TOKEN_INT_KW, TOKEN_FLOAT_KW, TOKEN_BOOL_KW, TOKEN_LIST_KW, TOKEN_MAP_KW,

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

    /** Implicitly inserted (without the user writing it themselves). */
    TOKEN_INTERPOLATE, TOKEN_STRING_END, TOKEN_EOF,

    /** Literal. */
    TOKEN_STRING_LIT, TOKEN_INT_LIT, TOKEN_FLOAT_LIT,

    /** Miscellaneous token. */
    TOKEN_ASSIGN, TOKEN_IDENTIFIER, TOKEN_DOT_DOT, TOKEN_ERROR
} TokenType;

/** A series of characters from the source code stored as a single unit. */
typedef struct {
    char *lexeme; /** Points somewhere in the source at the beginning of the token. */
    SourcePosition pos; /** Position in the source code. */
    TokenType type; /** What type of token it is. */

    /** Union for special tokens that require extra info like literals and errors. */
    union {
        /** 
         * Integer literals can be written in ways other than decimals (like binary or hexadecimal).
         * Therefore, in order to know the actual value we have to parse the literal in the source
         * and store its integer number in the token, which is intVal.
         */
        ZmxInt intVal;

        /** 
         * Floats are literals, so their values are also directly parsed during lexing.
         * This is mostly just for uniformity with integers and strings that actually require
         * parsing at lex time.
         */
        ZmxFloat floatVal;

        /** 
         * We lex a given string literal by manually appending characters to a buffer,
         * This is because escape sequences make the resulting literal potentially different
         * from what is in the original source code which is unescaped.
         * 
         * We use a string and length struct instead of CharBuffer to save bytes on capacity.
         */
        struct {
            int length;
            char *text;
        } stringVal;
    };
} Token;

/** An array of tokens. */
DECLARE_DA_STRUCT(TokenArray, Token);

/** Frees any allocated contents in any of the tokens in the passed token array. */
void free_tokens_contents(TokenArray *tokens);

/** Returns whether or not 2 tokens are considered equal. */
bool equal_token(const Token left, const Token right);

/** Returns the passed token type as a string literal in all uppercase. */
char *token_type_as_string(const TokenType type);

#endif
