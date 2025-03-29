#ifndef TOKEN_H
#define TOKEN_H

#include "constants.h"
#include "dynamic_array.h"
#include "report_error.h"

/** Every type of token that Zymux supports. */
typedef enum {
    /** Data type keyword. */
    TOKEN_STRING_KW, TOKEN_INT_KW, TOKEN_FLOAT_KW, TOKEN_BOOL_KW,

    /** Identifier declaration keyword. */
    TOKEN_CLASS_KW, TOKEN_FUNC_KW, TOKEN_ENUM_KW, TOKEN_CONST_KW, TOKEN_LET_KW, TOKEN_PRIVATE_KW,

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

    /** Error handling keyword. */
    TOKEN_TRY_KW, TOKEN_CATCH_KW, TOKEN_RAISE_KW,

    /** Import-related keyword. */
    TOKEN_FROM_KW, TOKEN_IMPORT_KW,

    /** Logical operator. */
    TOKEN_AMPER_AMPER, TOKEN_BAR_BAR, TOKEN_BANG,

    /** Bitwise operator. */
    TOKEN_AMPER, TOKEN_BAR, TOKEN_CARET, TOKEN_LSHIFT, TOKEN_RSHIFT, TOKEN_TILDE,

    /** Compound arithmetic assignment operator. */
    TOKEN_PLUS_EQ, TOKEN_MINUS_EQ, TOKEN_STAR_EQ, TOKEN_SLASH_EQ, TOKEN_PERCENT_EQ, TOKEN_EXPO_EQ,

    /** Compound bitwise assignment operator. */
    TOKEN_LSHIFT_EQ, TOKEN_RSHIFT_EQ, TOKEN_AMPER_EQ, TOKEN_BAR_EQ, TOKEN_CARET_EQ,

    /** Arithmetic operator. */
    TOKEN_PLUS, TOKEN_MINUS, TOKEN_STAR, TOKEN_SLASH, TOKEN_PERCENT, TOKEN_EXPO,

    /** Equality operator.*/
    TOKEN_EQ_EQ, TOKEN_BANG_EQ,

    /** Comparison operator. */
    TOKEN_GREATER, TOKEN_GREATER_EQ, TOKEN_LESS, TOKEN_LESS_EQ,

    /** Bracket-type. */
    TOKEN_LPAR, TOKEN_RPAR, TOKEN_LSQUARE, TOKEN_RSQUARE, TOKEN_LCURLY, TOKEN_RCURLY,

    /** Punctuation. */
    TOKEN_SEMICOLON, TOKEN_COMMA, TOKEN_DOT, TOKEN_DOT_DOT, TOKEN_QUESTION_MARK, TOKEN_COLON,

    /** Implicitly inserted (without the user writing it themselves). */
    TOKEN_INTERPOLATE, TOKEN_STRING_END, TOKEN_EOF,

    /** Literal. */
    TOKEN_STRING_LIT, TOKEN_INT_LIT, TOKEN_FLOAT_LIT,

    /** Miscellaneous. */
    TOKEN_EQ, TOKEN_IDENTIFIER, TOKEN_ERROR
} TokenType;

/** A series of characters from the source code stored as a single unit. */
typedef struct {
    char *lexeme; /** Points somewhere in the source at the beginning of the token. */
    i64 lexedIdx; /** This token's index into a lexed tokens array. -1 if it's not in an array. */
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

/** Returns the passed token type as a string literal in all uppercase. */
char *token_type_string(const TokenType type);

/** Creates a token without union values, but does have a position in some lexing array. */
Token create_token_at(char *lexeme, const TokenType type, const i64 lexedIdx);

/** Creates a "normal" token, which is a token that doesn't have any union values or lexed index. */
Token create_token(char *lexeme, const TokenType type);

/** 
 * Creates a synthetic integer literal token.
 * 
 * The lexeme holds the number we want to convert into intVal. We use base to know
 * what base we'll parse.
 */
Token create_int_token(char *lexeme, const int base, const i64 lexedIdx);

/** 
 * Creates a synthetic string literal token with a string and its length passed.
 * 
 * The lexeme and text of the stringVal are both set to point directly to
 * the passed string's address. This means that if the string passed is heap allocated,
 * it shouldn't be freed until the created token itself is no longer needed.
 */
Token create_string_token(char *string, const u32 length, const i64 lexedIdx);

/** Returns a version of the passed token that converts it into a string literal token. */
Token as_string_token(const Token token);

/** Returns whether or not 2 tokens are considered equal. */
bool equal_token(const Token left, const Token right);

/** Frees any allocated contents in any of the tokens in the passed token array. */
void free_tokens_contents(TokenArray *tokens);

#endif
