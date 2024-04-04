#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"
#include "data_structures.h"
#include "errors.h"

#define CURRENT_TOKEN_LENGTH(lexer) ((lexer)->current - (lexer)->tokenStart)
#define START_TOKEN(lexer) \
    ((lexer)->tokenStart = (lexer)->current, \
    (lexer)->tokenLine = (lexer)->line, \
    (lexer)->tokenColumn = (lexer)->column)

#define PEEK(lexer) (*(lexer)->current)
#define PEEK_DISTANCE(lexer, distance) \
    ((lexer)->current + (distance) >= (lexer)->source + (lexer)->sourceLength \
        ? '\0' : (lexer)->current[(distance)])

#define ADVANCE_PEEK(lexer) ((lexer)->column++, *(lexer)->current++)
#define ADVANCE(lexer) ((lexer)->column++, (lexer)->current++)
#define ADVANCE_AMOUNT(lexer, amount) ((lexer)->column += (amount), (lexer)->current += (amount))

#define MATCH(lexer, expected) (PEEK(lexer) == (expected) ? (ADVANCE(lexer), true) : false)
#define RETREAT(lexer) ((lexer)->column--, (lexer)->current--)

#define LOWERED_CHAR(ch) ((ch) >= 'A' && (ch) <= 'Z' ? (ch) + 32 : (ch))
#define IS_ALPHA(ch) ((LOWERED_CHAR((ch)) >= 'a' && LOWERED_CHAR((ch)) <= 'z') || (ch) == '_')
#define IS_DIGIT(ch) ((ch) >= '0' && (ch) <= '9')

#define IS_EOF(lexer) (*(lexer)->current == '\0')

/** Signature of a function used for checking if a character is within a base. */
typedef bool (*BaseCheckFunc)(const char ch);

/** A struct specifically to store information about a string that's being lexed. */
typedef struct {
    bool isRaw;
    bool isInterpolated;
    char *interpolationStart;
    int interpolationColumn;
    int interpolationDepth;
    int escapes;
    char quote;
} StringLexer;

static void lex_token(Lexer *lexer);

/** Returns an initialized lexer with the source code passed. */
Lexer create_lexer(ZymuxProgram *program, char *source) {
    Lexer lexer = {
        .current = source, .tokenStart = source, .source = source,
        .sourceLength = strlen(source),
        .program = program,
        .line = 1, .column = 1,
        .tokenLine = 1, .tokenColumn = 1,
        .tokens = create_token_array()
    };
    return lexer;
}

/** Frees all the memory the lexing stage of Zymux has used. */
void free_lexer(Lexer *lexer) {
    free_token_array(&lexer->tokens);
}

/** Returns an initialized TokenArray. */
TokenArray create_token_array() {
    TokenArray tokens = {0};
    return tokens;
}

/** Appends a token to a TokenArray. */
void append_token(TokenArray *tokens, Token token) {
    APPEND_DA(tokens->tokens, tokens->length, tokens->capacity, token, Token);
}

/** Frees all memory that a TokenArray has used. */
void free_token_array(TokenArray *tokens) {
    for (int i = 0; i < tokens->length; i++) {
        if (tokens->tokens[i].type == TOKEN_STRING_LIT) {
            free_char_buffer(&tokens->tokens[i].stringVal);
        }
    }
    free(tokens->tokens);
}

/** Returns an initialized StringLexer. */
static StringLexer create_string_lexer() {
    StringLexer string = {0};
    return string;
}

/** Return a token with no union values set. The fields are set from the parameters. */
Token create_token(char *message, const int line, const int column, const TokenType type) {
    Token token = {
        .lexeme = message, .length = strlen(message),
        .line = line, .column = column,
        .type = type
    };
    return token;
}

/** 
 * Appends an implicit token, which is empty.
 * it's a token of length 0 that the user didn't explicitly write,
 * and is only useful internally in Zymux.
 */
static void append_implicit(Lexer *lexer, const TokenType type) {
    append_token(&lexer->tokens, create_token("", lexer->line, lexer->column, type));
}

/** Appends the token we've been lexing as an error with the message. */
static void append_lexed_error(Lexer *lexer, char *message) {
    Token errorToken = {
        .lexeme = lexer->tokenStart, .length = CURRENT_TOKEN_LENGTH(lexer),
        .line = lexer->tokenLine, .column = lexer->tokenColumn,
        .errorMessage = message, .type = TOKEN_ERROR,
    };
    append_token(&lexer->tokens, errorToken);
    SYNTAX_ERROR(lexer->program, errorToken);
}

/** Appends an error token in a specific place in the source code with the message. */
static void append_error_at(
    Lexer *lexer, char *message, char *errorStart, const int length,
    const int line, const int column
) {
    Token errorToken = {
        .lexeme = errorStart, .length = length,
        .line = line, .column = column,
        .errorMessage = message, .type = TOKEN_ERROR,
    };
    append_token(&lexer->tokens, errorToken);
    SYNTAX_ERROR(lexer->program, errorToken);
}

/** Appends a token that was lexed and advanced over of the passed type. */
static void append_lexed(Lexer *lexer, const TokenType type) {
    Token token = {
        .lexeme = lexer->tokenStart, .length = CURRENT_TOKEN_LENGTH(lexer),
        .line = lexer->tokenLine, .column = lexer->tokenColumn,
        .type = type
    };
    append_token(&lexer->tokens, token);
}

/** Appends an integer that was lexed and sets its intVal to the already parsed literal value. */
static void append_lexed_int(Lexer *lexer, ZmxInt integer) {
    Token token = {
        .lexeme = lexer->tokenStart, .length = CURRENT_TOKEN_LENGTH(lexer),
        .line = lexer->tokenLine, lexer->tokenColumn,
        .type = TOKEN_INT_LIT, .intVal = integer
    };
    append_token(&lexer->tokens, token);
}

/** Appends a string that was lexed by creating the stringVal from the passed buffer. */
static void append_lexed_string(Lexer *lexer, StringLexer *string, CharBuffer buffer) {
    Token token = {
        .lexeme = lexer->tokenStart, 
        .length = buffer.length + string->escapes - 1, // -1 because CharBuffer counts NULL.
        .line = lexer->tokenLine, lexer->tokenColumn,
        .type = TOKEN_STRING_LIT, .stringVal = buffer
    };
    append_token(&lexer->tokens, token);
}

/** 
 * Compares if 2 tokens are considered equal
 * Tokens must be of equal type and have the same lexeme.
 * 
 * Integers and strings an exception as their literal value within the tokens is compared instead.
 */
bool tokens_equal(const Token left, const Token right) {
    if (left.type != right.type) {
        return false;
    }
    
    if (left.type == TOKEN_INT_LIT) {
        return left.intVal == right.intVal;
    }
    if (left.type == TOKEN_STRING_LIT) {
        if (left.stringVal.length != right.stringVal.length) {
            return false;
        }
        return strncmp(left.stringVal.text, right.stringVal.text, left.stringVal.length) == 0;
    }

    if (left.length != right.length) {
        return false;
    }
    return strncmp(left.lexeme, right.lexeme, left.length) == 0;
}

/** One line comments, which start with 2 slashes then ignore everything until EOF or newline. */
static void comment(Lexer *lexer) {
    ADVANCE_AMOUNT(lexer, 2);
    while (!IS_EOF(lexer) && PEEK(lexer) != '\n') {
        ADVANCE(lexer);
    }
}

/** Starts with slash-asterisk and skips characters until EOF or a closing asterisk-slash. */
static void multiline_comment(Lexer *lexer) {
    START_TOKEN(lexer);
    ADVANCE_AMOUNT(lexer, 2);

    while (true) {
        switch (ADVANCE_PEEK(lexer)) {
        case '\0':
            append_error_at(
                lexer, "Unterminated multiline comment.", lexer->tokenStart, 2,
                lexer->tokenLine, lexer->tokenColumn
            );
            RETREAT(lexer);
            return;
        case '\n':
            lexer->line++;
            lexer->column = 1;
            break;
        case '*':
            if (MATCH(lexer, '/')) {
                return;
            }
            break;
        default:
            break;
        }
    }
}

/** Skips over all whitespace, including comments. */
static void ignore_whitespace(Lexer *lexer) {
    while (true) {
        switch (PEEK(lexer)) {
        case ' ':
        case '\t':
        case '\r':
            ADVANCE(lexer);
            break;
        case '\n':
            ADVANCE(lexer);
            lexer->line++;
            lexer->column = 1;
            break;
        case '/':
            if (PEEK_DISTANCE(lexer, 1) == '/') {
                comment(lexer);
                break;
            }
            if (PEEK_DISTANCE(lexer, 1) == '*') {
                multiline_comment(lexer);
                break;
            }
            return;
        default:
            return;
        }
    }
}

/** 
 * Appends a token which is either a default one, or a secondary.
 * The secondary one is lexed if its expected character is found.
 */
static void one_or_default(
    Lexer *lexer, const TokenType defaultType, const char expected, const TokenType expectedType
) {
    if (MATCH(lexer, expected)) {
        append_lexed(lexer, expectedType);
    } else {
        append_lexed(lexer, defaultType);
    }
}

/** 
 * Appends a token which is either a default one, or a one of 2 other possibilities.
 * The other 2 are lexed if their expected character is found.
 */
static void two_or_default(
    Lexer *lexer, const TokenType defaultType,
    const char expected1, const TokenType type1, const char expected2, const TokenType type2
) {
    if (MATCH(lexer, expected1)) {
        append_lexed(lexer, type1);
    } else {
        one_or_default(lexer, defaultType, expected2, type2);
    }
}

/** 
 * Appends a token for a character that may denote 2 possible compound assignments.
 * It's for characters which act like one_or_default(),
 * but can be repeated for another one_or_default().
 * 
 * Example:
 *     "*" and "*=" can be "**" and "**=" because the first character is repeated.
 *     "*" is overloadedChar, TOKEN_STAR is original, TOKEN_STAR_EQUAL is originalCompound,
 *     TOKEN_EXPONENT would be extended, and TOKEN_EXPONENT_EQUAL is extendedCompound.
 */
static void two_compound_assigns(
    Lexer *lexer, const char overloadedChar, 
    const TokenType original, const TokenType originalCompound,
    const TokenType extended, const TokenType extendedCompound
) {
    if (PEEK(lexer) == overloadedChar && PEEK_DISTANCE(lexer, 1) == '=') {
        ADVANCE_AMOUNT(lexer, 2);
        append_lexed(lexer, extendedCompound);
    } else {
        two_or_default(lexer, original, overloadedChar, extended, '=', originalCompound);
    }
}

/** Checks if the thing scanned so far matches with the passed keyword. */
static bool is_keyword(Lexer *lexer, const char *keyword) {
    const int length = strlen(keyword);
    if (CURRENT_TOKEN_LENGTH(lexer) != length) {
        return false;
    }
    return strncmp(lexer->tokenStart, keyword, length) == 0;
}

/** Returns a token type of what the currently lexed token is. Either a keyword or identifier. */
static TokenType get_name_type(Lexer *lexer) {
    switch (*lexer->tokenStart) {
    case 'a':
        if (is_keyword(lexer, "as")) return TOKEN_AS_KW;
        if (is_keyword(lexer, "abstract")) return TOKEN_ABSTRACT_KW;
        break;
    case 'b':
        if (is_keyword(lexer, "break")) return TOKEN_BREAK_KW;
        if (is_keyword(lexer, "bool")) return TOKEN_BOOL_KW;
        break;
    case 'c':
        if (is_keyword(lexer, "case")) return TOKEN_CASE_KW;
        if (is_keyword(lexer, "const")) return TOKEN_CONST_KW;
        if (is_keyword(lexer, "continue")) return TOKEN_CONTINUE_KW;
        if (is_keyword(lexer, "class")) return TOKEN_CLASS_KW;
        break;
    case 'd':
        if (is_keyword(lexer, "default")) return TOKEN_DEFAULT_KW;
        if (is_keyword(lexer, "do")) return TOKEN_DO_KW;
        break;
    case 'e':
        if (is_keyword(lexer, "else")) return TOKEN_ELSE_KW;
        break;
    case 'f':
        if (is_keyword(lexer, "float")) return TOKEN_FLOAT_KW;
        if (is_keyword(lexer, "false")) return TOKEN_FALSE_KW;
        if (is_keyword(lexer, "for")) return TOKEN_FOR_KW;
        if (is_keyword(lexer, "func")) return TOKEN_FUNC_KW;
        if (is_keyword(lexer, "from")) return TOKEN_FROM_KW;
        break;
    case 'i':
        if (is_keyword(lexer, "if")) return TOKEN_IF_KW;
        if (is_keyword(lexer, "int")) return TOKEN_INT_KW;
        if (is_keyword(lexer, "is")) return TOKEN_IS_KW;
        if (is_keyword(lexer, "in")) return TOKEN_IN_KW;
        if (is_keyword(lexer, "init")) return TOKEN_INIT_KW;
        if (is_keyword(lexer, "inherits")) return TOKEN_INHERITS_KW;
        if (is_keyword(lexer, "import")) return TOKEN_IMPORT_KW;
        break;
    case 'l':
        if (is_keyword(lexer, "list")) return TOKEN_LIST_KW;
        if (is_keyword(lexer, "let")) return TOKEN_LET_KW;
        break;
    case 'm':
        if (is_keyword(lexer, "match")) return TOKEN_MATCH_KW;
        break;
    case 'n':
        if (is_keyword(lexer, "null")) return TOKEN_NULL_KW;
        break;
    case 'p':
        if (is_keyword(lexer, "private")) return TOKEN_PRIVATE_KW;
        break;
    case 'r':
        if (is_keyword(lexer, "return")) return TOKEN_RETURN_KW;
        break;
    case 's':
        if (is_keyword(lexer, "string")) return TOKEN_STRING_KW;
        if (is_keyword(lexer, "super")) return TOKEN_SUPER_KW;
        break;
    case 't':
        if (is_keyword(lexer, "true")) return TOKEN_TRUE_KW;
        if (is_keyword(lexer, "this")) return TOKEN_THIS_KW;
        break;
    case 'w':
        if (is_keyword(lexer, "while")) return TOKEN_WHILE_KW;
        break;
    default:
        break;
    }
    return TOKEN_IDENTIFIER;
}

/** Handles fully lexing a "name" in Zymux, which is any identifier or keyword. */
static void lex_name(Lexer *lexer) {
    while (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    const TokenType type = get_name_type(lexer);
    append_lexed(lexer, type);
}

/** 
 * Appends the lexed integer token.
 * This function is also responsible for passing the literal's actual value to the token.
 * 
 * We parse the number by iterating through the token right to left,
 * then parse the value of the place we're on by subtracting the ASCII value.
 * If the number is over '9' ASCII and we start going through the alphabet,
 * then we add 10 because alphabetical characters start at 10 (A in hex is 10, not 0).
 * 
 * We multiply the place's value by the base to the power of which place it is.
 * So if it's a hexadecimal number and it's the third place then multiply by 16^2 (i starts at 0),
 * which is 16, so if it was hex (A0) then that would be 160.
 */
static void append_lexed_base(Lexer *lexer, const int base, bool hasPrefix) {
    ZmxInt sum = 0;
    for (int i = 0; i < CURRENT_TOKEN_LENGTH(lexer); i++) {
        char place = PEEK_DISTANCE(lexer, -i - 1);
        ZmxInt placeValue = IS_DIGIT(place) ? place - '0' : LOWERED_CHAR(place) - 'a' + 10;
        sum += zmx_power(base, i) * placeValue;
    }
    if (hasPrefix) {
        lexer->tokenStart -= 2;
        lexer->tokenColumn -= 2;
    }
    append_lexed_int(lexer, sum);
}

/** Errors a number literal covering the number until next isn't alphabetical or numerical. */
static void invalid_literal(Lexer *lexer, char *message, bool hasPrefix) {
    if (hasPrefix) {
        lexer->tokenStart -= 2;
        lexer->tokenColumn -= 2;
    }
    while (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    append_lexed_error(lexer, message);
}

/** 
 * Finishes lexing an arbitrary integer base.
 * It advances through characters as long as they return true on a BaseCheckFunc,
 * then appends the lexed base.
 */
static void lex_base(
    Lexer *lexer, const BaseCheckFunc is_within_base, const int base, char *errorMessage
) {
    ADVANCE_AMOUNT(lexer, 2);
    START_TOKEN(lexer);
    if (!is_within_base(PEEK(lexer))) {
        invalid_literal(lexer, errorMessage, true);
        return;
    }

    while (is_within_base(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    if (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        invalid_literal(lexer, errorMessage, true);
    } else {
        append_lexed_base(lexer, base, true);   
    }
}

/** Finishes the lexing of a float after the whole number and dot. */
static void finish_float(Lexer *lexer) {
    while (IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    if (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        invalid_literal(lexer, "Invalid float literal.", false);
    } else {
        append_lexed(lexer, TOKEN_FLOAT_LIT);
    }
}

/** Lexes and appends a "normal" number, which is a decimal integer or float. */
static void normal_number(Lexer *lexer) {
    START_TOKEN(lexer);
    while (IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    
    if (PEEK(lexer) == '.' && IS_DIGIT(PEEK_DISTANCE(lexer, 1))) {
        ADVANCE_AMOUNT(lexer, 2);
        finish_float(lexer);
        return;
    }

    if (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        invalid_literal(lexer, "Invalid integer literal.", false);
        return;
    }
    append_lexed_base(lexer, 10, false);
}

/** Returns whether the passed ch is a binary number or not. */
static bool is_bin_digit(const char ch) {
    return ch == '0' || ch == '1';
}

/** Returns whether the passed ch is an octal number or not. */
static bool is_oct_digit(const char ch) {
    return ch >= '0' && ch <= '7';
}

/** Returns whether the passed ch is a hexadecimal number or not. */
static bool is_hex_digit(const char ch) {
    const char lowered = LOWERED_CHAR(ch);
    return IS_DIGIT(lowered) || (lowered >= 'a' && lowered <= 'f');
}

/** 
 * Handles lexing an entire number, whether it's float, int, hex or others.
 * 
 * If an alphabetical or integer character is seen at the end of a number,
 * then an error will be reported.
 * All preceding zeroes are ignored.
 */
static void lex_number(Lexer *lexer) {
    if (PEEK(lexer) != '0') {
        normal_number(lexer);
        return;
    }

    // Skip zero padding, except the last one.
    while (PEEK(lexer) == '0' && PEEK_DISTANCE(lexer, 1) == '0') {
        ADVANCE(lexer);
    }
    switch (LOWERED_CHAR(PEEK_DISTANCE(lexer, 1))) {
    case 'b': lex_base(lexer, is_bin_digit, 2, "invalid binary literal."); break;
    case 'o': lex_base(lexer, is_oct_digit, 8, "invalid octal literal."); break;
    case 'x': lex_base(lexer, is_hex_digit, 16, "invalid hexadecimal literal."); break;
    default:
        // Skip last zero if it's not the only thing in the whole number.
        if (PEEK(lexer) == '0' && IS_DIGIT(PEEK_DISTANCE(lexer, 1))) {
            ADVANCE(lexer);
        }
        normal_number(lexer); break;
    }
}

/** Ignores whitespace for interpolated expressions, where newlines and comments aren't allowed. */
static bool ignore_interpolation_whitespace(Lexer *lexer) {
    while (true) {
        switch (PEEK(lexer)) {
        case ' ':
        case '\t':
        case '\r':
            ADVANCE(lexer);
            break;
        case '\n':
            // Should've errored earlier, so unreachable.
            UNREACHABLE_ERROR();
            return false;
        case '/':
            if (PEEK_DISTANCE(lexer, 1) == '/' || PEEK_DISTANCE(lexer, 1) == '*') {
                append_error_at(
                    lexer, "Can't have comment inside string interpolation.",
                    lexer->tokenStart, 2, lexer->tokenLine, lexer->tokenColumn
                );
                return false;
            }
            return true;
        default:
            return true;
        }
    }
}

/** 
 * Resets the state after finishing the interpolation.
 *  
 * Resets the StringLexer's interpolation state,
 * and sets the lexer to start scanning after the closing brace. */
static void finish_interpolation(
    Lexer *lexer, StringLexer *string, char *closingBrace, int closingBraceColumn
) {
    lexer->current = closingBrace + 1;
    lexer->column = closingBraceColumn + 1;
    string->interpolationStart = NULL;
    string->interpolationColumn = 0;
}

/** 
 * Lexes tokens until we reach the closing brace.
 * After lexing them, we restore the lexer's state to after the closing brace.
 */
static void lex_interpolation(Lexer *lexer, StringLexer *string) {
    char *exprEnd = lexer->current;
    int exprEndColumn = lexer->column;
    lexer->current = string->interpolationStart + 1;
    lexer->column = string->interpolationColumn + 1;
    if (!ignore_interpolation_whitespace(lexer) || PEEK(lexer) == '}') {
        // Empty/starts with error, don't interpolate anything.
        finish_interpolation(lexer, string, exprEnd, exprEndColumn);
        return;
    }

    append_implicit(lexer, TOKEN_FORMAT);
    while (ignore_interpolation_whitespace(lexer) && lexer->current < exprEnd) {
        lex_token(lexer);
    }
    finish_interpolation(lexer, string, exprEnd, exprEndColumn);
}

/** 
 * Prepares to start lexing the interpolation's tokens.
 * It does so by appending the current buffer literal then a format beforehand.
 * Returns whether or not we actually lexed the interpolation.
 */
static bool interpolate(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    string->interpolationDepth--;
    if (string->interpolationDepth > 0) {
        // Not the correct closing brace. Simply append the brace, and keep going.
        buffer_append_char(buffer, ADVANCE_PEEK(lexer));
        return false;
    }

    buffer_pop_amount(buffer, lexer->current - string->interpolationStart);
    append_lexed_string(lexer, string, *buffer);
    *buffer = create_char_buffer();
    lex_interpolation(lexer, string);
    return true;
}

/** 
 * Finishes an interpolation.
 * 
 * It tries to interpolate, if it succeeds then it'll append a TOKEN_FORMAT and keep going.
 * There's an edge case if we see the closing token though, indicating the string finished
 * on the interpolation, in which case we append STRING_END ourselves and return false.
 * 
 * Returns whether or not we should keep scanning string characters.
 */
static bool interpolation_end(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    if (!interpolate(lexer, string, buffer)) {
        return true;
    }
    
    START_TOKEN(lexer);
    if (MATCH(lexer, string->quote)) {
        append_implicit(lexer, TOKEN_STRING_END);
        return false;
    } else {
        append_implicit(lexer, TOKEN_FORMAT);
        return true;
    }
}

/** 
 * Starts preparing the StringLexer's state for a closing brace to interpolate.
 * It stores where the open brace is at so when we encounter the closing brace,
 * we can set the lexer to start scanning non-string tokens after the opening brace.
 * 
 * We also have to store how many open braces deep we are
 * so we know how many closing braces to expect before the outermost open brace is finished.
 * Kind of like a stack.
 */
static void interpolation_start(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    if (string->interpolationStart == NULL) {
        string->interpolationStart = lexer->current;
        string->interpolationColumn = lexer->column;
    }
    string->interpolationDepth++;
    buffer_append_char(buffer, ADVANCE_PEEK(lexer));
}

/** 
 * Appends and advances over an escape character made up of a backslash and the escaped character.
 * Appends the escaped character by default if it's not a built in sequence in C (like "\t").
 */
static void escape_character(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    switch (PEEK_DISTANCE(lexer, 1)) {
        case 'n': buffer_append_char(buffer, '\n'); break;
        case 't': buffer_append_char(buffer, '\t'); break;
        case 'r': buffer_append_char(buffer, '\r'); break;
        case 'b': buffer_append_char(buffer, '\b'); break;
        default: buffer_append_char(buffer, PEEK_DISTANCE(lexer, 1)); break;
    }
    string->escapes++;
    ADVANCE_AMOUNT(lexer, 2);
}

/** 
 * Appends one character to the literal char buffer.
 * It may call other scanning helpers if it sees a specific character while some metadata is set.
 * It could also error if it encounters a newline or EOF.
 * 
 * Return whether or not the StringLexer should continue scanning characters.
 */
static bool scan_string_char(
    Lexer *lexer, StringLexer *string, CharBuffer *buffer, char *quote, int quoteColumn
) {
    char current = PEEK(lexer);
    if (current == '\\' && !string->isRaw) {
        escape_character(lexer, string, buffer);
        return true;
    }
    if (current == '{' && string->isInterpolated) {
        interpolation_start(lexer, string, buffer);
        return true;
    }
    if (current == '}' && string->interpolationStart != NULL) {
        return interpolation_end(lexer, string, buffer);
    }
    if (IS_EOF(lexer) || current == '\n') {
        append_error_at(lexer, "Unterminated string.", quote, 1, lexer->line, quoteColumn);
        return false;
    }
    buffer_append_char(buffer, ADVANCE_PEEK(lexer));
    return true;
}

/** 
 * Finishes the lexing of a string after the metadata of the StringLexer is fully set.
 * We scan the string character by character until we see a closing quote.
 * 
 * If it goes right, we append the last literal we had stored in the buffer and a STRING_END token.
 */
static void finish_string(Lexer *lexer, StringLexer *string) {
    char *quote = lexer->current - 1;
    int quoteColumn = lexer->column - 1;
    CharBuffer buffer = create_char_buffer();
    while (!MATCH(lexer, string->quote)) {
        if (!scan_string_char(lexer, string, &buffer, quote, quoteColumn)) {
            free_char_buffer(&buffer);
            return;
        }
    }

    if (string->interpolationDepth != 0) {
        append_error_at(
            lexer, "Unclosed \"{\" in interpolated string.",
            string->interpolationStart, 1, lexer->line, string->interpolationColumn
        );
    }
    append_lexed_string(lexer, string, buffer);
    append_implicit(lexer, TOKEN_STRING_END);
}

/** Read all upcoming metadata characters in a row and report them in one error. */
static void repeated_metadata_error(Lexer *lexer) {
    while (!IS_EOF(lexer) && (PEEK(lexer) == '#' || PEEK(lexer) == '$')) {
        ADVANCE(lexer);
    }
    append_lexed_error(lexer, "Can't repeat \"$\" or \"#\" on string.");
}

/** Represents how setting some metadata went. */
typedef enum {
    METADATA_SUCCESS,
    METADATA_END,
    METADATA_REPEATED_ERROR,
    METADATA_MISSING_STRING_ERROR
} MetadataStatus;

/** 
 * Sets a piece of metadata on the string lexer.
 * The metadata is expected to be the current character in the lexer.
 * It returns a status of how it went.
 */
static MetadataStatus set_string_metadata(Lexer *lexer, StringLexer *string) {
    switch (PEEK(lexer)) {
    case '#':
        if (string->isRaw) {
            repeated_metadata_error(lexer);
            return METADATA_REPEATED_ERROR;
        }
        string->isRaw = true;
        return METADATA_SUCCESS;
    case '$':
        if (string->isInterpolated) {
            repeated_metadata_error(lexer);
            return METADATA_REPEATED_ERROR;
        }
        string->isInterpolated = true;
        return METADATA_SUCCESS;
    case '\'':
    case '"':
        string->quote = PEEK(lexer);
        return METADATA_END;
    default:
        // Error the non-metadata character then go back so it can be lexed normally after.
        START_TOKEN(lexer);
        ADVANCE(lexer);
        append_lexed_error(lexer, "Expected string after \"#\" or \"$\".");
        RETREAT(lexer);
        START_TOKEN(lexer);
        return METADATA_MISSING_STRING_ERROR;
    }
}

/** 
 * Handles lexing an entire string.
 * We handle interpolation by alternating between a string literal 
 * and a series of an expression's tokens with a format token in-between them
 * then a string_end token to denote the end of any given whole string, interpolated or not.
 * Also, strings that start with interpolation begin with an implicit empty literal for uniformity.
 * 
 * This is how a series of tokens would look like for a string with interpolation:
 *     STRING_LITERAL, STRING_FORMAT, INT_LITERAL STRING_FORMAT, STRING_END.
 * 
 * even non-interpolated strings have a string_end token at the end, also for uniformity. Example:
 *     STRING_LITERAL, STRING_END.
 */
static void lex_string(Lexer *lexer) {
    START_TOKEN(lexer);
    StringLexer string = create_string_lexer();
    MetadataStatus result = METADATA_SUCCESS;

    while (result != METADATA_END) {
        result = set_string_metadata(lexer, &string);
        if (result == METADATA_MISSING_STRING_ERROR) {
            return;
        }
        if (result == METADATA_REPEATED_ERROR) {
            continue;
        }
        ADVANCE(lexer);
    }
    START_TOKEN(lexer);
    finish_string(lexer, &string);
}

/** Returns whether or not the passed ch is a valid character in Zymux. */
static bool valid_syntax(const char ch) {
    switch (ch) {
    case '#':
    case '$':
    case '\'':
    case '"':
    case ';':
    case ':':
    case ',':
    case '?':
    case '(':
    case ')':
    case '[':
    case ']':
    case '{':
    case '}':
    case '+':
    case '-':
    case '/':
    case '%':
    case '=':
    case '!':
    case '^':
    case '~':
    case '.':
    case '&': 
    case '|':
    case '*':
    case '>':
    case '<':
    case ' ':
    case '\r':
    case '\t':
    case '\n':
        return true;
    default:
        if (IS_ALPHA(ch) || IS_DIGIT(ch)) {
            return true;
        }
        return false;
    }
}


/** 
 * To be called when an invalid piece of syntax is called.
 * It advances until EOF or encountering valid syntax, errors.
 */
static void unsupported_syntax(Lexer *lexer) {
    while (!IS_EOF(lexer) && !valid_syntax(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    append_lexed_error(lexer, "Unsupported syntax.");
    return;
}

/** 
 * General function to be called when starting to lex a new token.
 * Doesn't take whitespace into account.
 * Always ends in appending at least one token when called, even if it's just an error token.
 */
static void lex_token(Lexer *lexer) {
    START_TOKEN(lexer);
    if (IS_EOF(lexer)) {
        append_implicit(lexer, TOKEN_EOF);
        return;
    }

    char current = ADVANCE_PEEK(lexer);
    switch (current) {
    case ';': append_lexed(lexer, TOKEN_SEMICOLON); break;
    case ':': append_lexed(lexer, TOKEN_COLON); break;
    case ',': append_lexed(lexer, TOKEN_COMMA); break;
    case '?': append_lexed(lexer, TOKEN_QUESTION_MARK); break;
    case '(': append_lexed(lexer, TOKEN_LPAR); break;
    case ')': append_lexed(lexer, TOKEN_RPAR); break;
    case '[': append_lexed(lexer, TOKEN_LSQUARE); break;
    case ']': append_lexed(lexer, TOKEN_RSQUARE); break;
    case '{': append_lexed(lexer, TOKEN_LCURLY); break;
    case '}': append_lexed(lexer, TOKEN_RCURLY); break;

    case '+': one_or_default(lexer, TOKEN_PLUS, '=', TOKEN_PLUS_EQ); break;
    case '-': one_or_default(lexer, TOKEN_MINUS, '=', TOKEN_MINUS_EQ); break;
    case '/': one_or_default(lexer, TOKEN_SLASH, '=', TOKEN_SLASH_EQ); break;
    case '%': one_or_default(lexer, TOKEN_MODULO, '=', TOKEN_MODULO_EQ); break;
    case '=': one_or_default(lexer, TOKEN_EQ, '=', TOKEN_EQ_EQ); break;
    case '!': one_or_default(lexer, TOKEN_BANG, '=', TOKEN_BANG_EQ); break;
    case '^': one_or_default(lexer, TOKEN_CARET, '=', TOKEN_CARET_EQ); break;
    case '~': one_or_default(lexer, TOKEN_TILDE, '=', TOKEN_TILDE_EQ); break;
    case '.': one_or_default(lexer, TOKEN_DOT, '.', TOKEN_DOT_DOT); break;
    
    case '&': 
        two_or_default(lexer, TOKEN_AMPER, '&', TOKEN_AMPER_AMPER, '=', TOKEN_AMPER_EQ); break;
    case '|': 
        two_or_default(lexer, TOKEN_BAR, '|', TOKEN_BAR_BAR, '=', TOKEN_BAR_EQ); break;

    case '*':
        two_compound_assigns(lexer, '*', TOKEN_STAR, TOKEN_STAR_EQ, TOKEN_EXPO, TOKEN_EXPO_EQ);
        break;
    case '>':
        two_compound_assigns(
            lexer, '>', TOKEN_GREATER, TOKEN_GREATER_EQ, TOKEN_RSHIFT, TOKEN_RSHIFT_EQ
        );
        break;
    case '<':
        two_compound_assigns(lexer, '<', TOKEN_LESS, TOKEN_LESS_EQ, TOKEN_LSHIFT, TOKEN_LSHIFT_EQ);
        break;

    case '#':
    case '$':
    case '\'':
    case '"':
        RETREAT(lexer);
        lex_string(lexer);
        break;
    
    default:
        if (IS_ALPHA(current)) {
            lex_name(lexer);
            return;
        }
        if (IS_DIGIT(current)) {
            RETREAT(lexer);
            lex_number(lexer);
            return;
        }
        unsupported_syntax(lexer);
    }
}

/** 
 * Lex tokens from the lexer's source while ignoring whitespaces until EOF.
 * The lexed tokens are appended to the lexer's TokenArray with an EOF token always at the end. 
 */
bool lex(Lexer *lexer) {
    while (true) {
        ignore_whitespace(lexer);
        lex_token(lexer);
        Token lastToken = lexer->tokens.tokens[lexer->tokens.length - 1];
        if (TOKEN_IS_TYPE(lastToken, TOKEN_EOF)) {
            break;
        }
    }
    
    return !lexer->program->hasErrored;
}
