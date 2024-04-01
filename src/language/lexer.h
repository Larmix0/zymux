#ifndef LEXER_H
#define LEXER_H

#include "char_buffer.h"
#include "zymux_program.h"

#define TOKEN_IS_TYPE(token, expected) (token.type == expected)

/** Every type of token that Zymux supports. */
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

    /** Implicitly inserted (without the user writing it themselves). */
    TOKEN_FORMAT, TOKEN_STRING_END, TOKEN_EOF,

    /** Literal. */
    TOKEN_STRING_LIT, TOKEN_INT_LIT, TOKEN_FLOAT_LIT,

    /** Miscellaneous token. */
    TOKEN_EQ, TOKEN_IDENTIFIER, TOKEN_DOT_DOT, TOKEN_ERROR, TOKEN_NONE
} TokenType;

/** A series of characters from the source code stored as a single unit. */
typedef struct {
    char *lexeme; /** points somewhere in the source at the beginning of the token */
    int length; /** how long the lexeme is (since lexeme is not terminated). */
    int line; /** Line of where the token is present in the source code. */
    int column; /** The column of the first character in the token. */
    TokenType type; /** What type of token it is. */

    /** Union for special tokens that require extra info like literals and errors. */
    union {
        /** 
         * Integer literals can be written in ways other than decimals in Zymux,
         * which are currently binary (0b), octal (0o) and hexadecimal (0x).
         * Therefore, in order to know the actual value we have to parse the literal in the source
         * and store it's integer number in the token, which is intVal.
         */
        ZmxInt intVal;

        /** 
         * We lex a given string literal by manually appending characters to a buffer,
         * This is because escape sequences make the resulting literal potentially different
         * from what is in the original source code which is unescaped.
         */
        CharBuffer stringVal;

        /**
         * When an error occurs, it still points to a lexeme and length in the source, which is
         * the erroneous token that gets displayed. errorMessage is the message to be printed.
         */
        char *errorMessage;
    };
} Token;

/** An array of tokens. */
typedef struct {
    int length;
    int capacity;
    Token *tokens;
} TokenArray;

/** A lexer for a given piece of source code to produce its array of tokens for parsing later. */
typedef struct {
    char *tokenStart;
    int tokenLine;
    int tokenColumn;

    char *current;
    int line;
    int column;

    char *source;
    size_t sourceLength;
    
    ZymuxProgram *program;
    TokenArray tokens;
} Lexer;

/** Returns an initialized TokenArray. */
TokenArray create_token_array();

/** Appends the passed Token to the TokenArray. */
void append_token(TokenArray *tokens, Token token);

/** Return a general token (with no union values) from the parameters. */
Token create_token(char *message, const int line, const int column, const TokenType type);

/** Frees all the memory the passed TokenArray has used. */
void free_token_array(TokenArray *tokens);

/** Returns an initialized Lexer. */
Lexer create_lexer(ZymuxProgram *program, char *source);
/** Frees all the memory the lexer has used (including the tokens it generated). */
void free_lexer(Lexer *lexer);

/** Lexes the passed lexer's source code into its tokens array and returns whether it errored. */
bool lex(Lexer *lexer);

/** Returns whether or not 2 tokens are considered equal. */
bool tokens_equal(const Token left, const Token right);

#endif
