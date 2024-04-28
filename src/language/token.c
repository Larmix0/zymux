#include <stdlib.h>
#include <string.h>

#include "token.h"

/** Returns the passed type as a string literal in all uppercase. */
char *type_to_string(const TokenType type) {
    switch (type) {
        case TOKEN_STRING_KW: return "STRING";
        case TOKEN_INT_KW: return "INT";
        case TOKEN_FLOAT_KW: return "FLOAT";
        case TOKEN_BOOL_KW: return "BOOL";
        case TOKEN_LIST_KW: return "LIST";
        case TOKEN_MAP_KW: return "MAP";
        case TOKEN_CLASS_KW: return "CLASS";
        case TOKEN_CONST_KW: return "CONST";
        case TOKEN_LET_KW: return "LET";
        case TOKEN_PRIVATE_KW: return "PRIVATE";
        case TOKEN_FUNC_KW: return "FUNC";
        case TOKEN_IF_KW: return "IF";
        case TOKEN_ELSE_KW: return "ELSE";
        case TOKEN_WHILE_KW: return "WHILE";
        case TOKEN_FOR_KW: return "FOR";
        case TOKEN_DO_KW: return "DO";
        case TOKEN_RETURN_KW: return "RETURN";
        case TOKEN_BREAK_KW: return "BREAK";
        case TOKEN_CONTINUE_KW: return "CONTINUE";
        case TOKEN_TRUE_KW: return "TRUE";
        case TOKEN_FALSE_KW: return "FALSE";
        case TOKEN_NULL_KW: return "NULL";
        case TOKEN_AS_KW: return "AS";
        case TOKEN_IS_KW: return "IS";
        case TOKEN_IN_KW: return "IN";
        case TOKEN_SUPER_KW: return "SUPER";
        case TOKEN_THIS_KW: return "THIS";
        case TOKEN_INIT_KW: return "INIT";
        case TOKEN_ABSTRACT_KW: return "ABSTRACT";
        case TOKEN_INHERITS_KW: return "INHERITS";
        case TOKEN_MATCH_KW: return "MATCH";
        case TOKEN_CASE_KW: return "CASE";
        case TOKEN_DEFAULT_KW: return "DEFAULT";
        case TOKEN_FROM_KW: return "FROM";
        case TOKEN_IMPORT_KW: return "IMPORT";
        case TOKEN_AMPER_AMPER: return "AMPERSAND_AMPERSAND";
        case TOKEN_BAR_BAR: return "BAR_BAR";
        case TOKEN_BANG: return "BANG";
        case TOKEN_AMPER: return "AMPERSAND";
        case TOKEN_BAR: return "BAR";
        case TOKEN_CARET: return "CARET";
        case TOKEN_LSHIFT: return "LEFT_SHIFT";
        case TOKEN_RSHIFT: return "RIGHT_SHIFT";
        case TOKEN_TILDE: return "TILDE";
        case TOKEN_PLUS_EQ: return "PLUS_EQUAL";
        case TOKEN_MINUS_EQ: return "MINUS_EQUAL";
        case TOKEN_STAR_EQ: return "STAR_EQUAL";
        case TOKEN_SLASH_EQ: return "SLASH_EQUAL";
        case TOKEN_MODULO_EQ: return "MODULO_EQUAL";
        case TOKEN_EXPO_EQ: return "EXPONENT_EQUAL";
        case TOKEN_AMPER_EQ: return "AMPERSAND_EQUAL";
        case TOKEN_BAR_EQ: return "BAR_EQUAL";
        case TOKEN_CARET_EQ: return "CARET_EQUAL";
        case TOKEN_LSHIFT_EQ: return "LEFT_SHIFT_EQUAL";
        case TOKEN_RSHIFT_EQ: return "RIGHT_SHIFT_EQUAL";
        case TOKEN_TILDE_EQ: return "TILDE_EQUAL";
        case TOKEN_PLUS: return "PLUS";
        case TOKEN_MINUS: return "MINUS";
        case TOKEN_STAR: return "STAR";
        case TOKEN_SLASH: return "SLASH";
        case TOKEN_MODULO: return "MODULO";
        case TOKEN_EXPO: return "EXPONENT";
        case TOKEN_EQ_EQ: return "EQUAL_EQUAL";
        case TOKEN_BANG_EQ: return "BANG_EQUAL";
        case TOKEN_GREATER: return "GREATER";
        case TOKEN_GREATER_EQ: return "GREATER_EQUAL";
        case TOKEN_LESS: return "LESS";
        case TOKEN_LESS_EQ: return "LESS_EQUAL";
        case TOKEN_LPAR: return "LEFT_PARENTHESIS";
        case TOKEN_RPAR: return "RIGHT_PARENTHESIS";
        case TOKEN_LSQUARE: return "LEFT_SQUARE_BRACKET";
        case TOKEN_RSQUARE: return "RIGHT_SQUARE_BRACKET";
        case TOKEN_LCURLY: return "LEFT_CURLY_BRACKET";
        case TOKEN_RCURLY: return "RIGHT_CURLY_BRACKET";
        case TOKEN_SEMICOLON: return "SEMICOLON";
        case TOKEN_COMMA: return "COMMA";
        case TOKEN_DOT: return "DOT";
        case TOKEN_QUESTION_MARK: return "QUESTION_MARK";
        case TOKEN_COLON: return "COLON";
        case TOKEN_FORMAT: return "STRING_FORMAT";
        case TOKEN_STRING_END: return "STRING_END";
        case TOKEN_STRING_LIT: return "STRING_LITERAL";
        case TOKEN_INT_LIT: return "INTEGER_LITERAL";
        case TOKEN_FLOAT_LIT: return "FLOAT_LITERAL";
        case TOKEN_EQ: return "EQUAL";
        case TOKEN_IDENTIFIER: return "IDENTIFIER";
        case TOKEN_DOT_DOT: return "DOT_DOT";
        case TOKEN_EOF: return "EOF";
        case TOKEN_ERROR: return "ERROR";
    }
    UNREACHABLE_ERROR();
    return NULL; // Unreachable error already exits. This is so the compiler doesn't yell.
}

/** Frees any allocated contents in the passed token array, which is usually a union values. */
void free_tokens_contents(TokenArray *tokens) {
    for (int i = 0; i < tokens->length; i++) {
        if (tokens->data[i].type == TOKEN_STRING_LIT) {
            free(tokens->data[i].stringVal.text);
        }
    }
}

/** 
 * Compares if 2 tokens are considered equal.
 * 
 * Tokens must be of equal type and have the same lexeme.
 * Literals are an exception as their literal value within the tokens is
 * compared instead of the lexeme.
 */
bool equal_token(const Token left, const Token right) {
    if (left.type != right.type) {
        return false;
    }
    
    if (left.type == TOKEN_INT_LIT) {
        return left.intVal == right.intVal;
    }
    if (left.type == TOKEN_FLOAT_LIT) {
        return left.floatVal == right.floatVal;
    }
    if (left.type == TOKEN_STRING_LIT) {
        if (left.stringVal.length != right.stringVal.length) {
            return false;
        }
        return strncmp(left.stringVal.text, right.stringVal.text, left.stringVal.length) == 0;
    }

    if (left.pos.length != right.pos.length) {
        return false;
    }
    return strncmp(left.lexeme, right.lexeme, left.pos.length) == 0;
}