#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "char_buffer.h"
#include "data_structures.h"
#include "lexer.h"

#define CURRENT_TOKEN_LENGTH(lexer) ((lexer)->current - (lexer)->tokenStart)
#define START_TOKEN(lexer) \
    ((lexer)->tokenStart = (lexer)->current, (lexer)->tokenColumn = (lexer)->column)

#define PEEK(lexer) (*(lexer)->current)
#define PEEK_NEXT(lexer) \
    ((lexer)->current + 1 >= (lexer)->source + (lexer)->sourceLength ? '\0' : (lexer)->current[1])

#define ADVANCE_PEEK(lexer) ((lexer)->column++, *(lexer)->current++)
#define ADVANCE(lexer) ((lexer)->column++, (lexer)->current++)
#define ADVANCE_DOUBLE(lexer) ((lexer)->column += 2, (lexer)->current += 2)

#define MATCH(lexer, expected) (PEEK(lexer) == (expected) ? (ADVANCE(lexer), true) : false)
#define RETREAT(lexer) ((lexer)->column--, (lexer)->current--)

#define LOWERED_CHAR(ch) ((ch) >= 'A' && (ch) <= 'Z' ? (ch) + 32 : (ch))
#define IS_ALPHA(ch) ((LOWERED_CHAR(ch) >= 'a' && LOWERED_CHAR(ch) <= 'z') || (ch) == '_')
#define IS_DIGIT(ch) ((ch) >= '0' && (ch) <= '9')

#define IS_EOF(lexer) (*(lexer)->current == '\0')

/** Signature of a function used for checking if a character is within a base. */
typedef bool (*BaseCheckFunc)(const char ch);

/** A struct made specifically for storing information about a string that's being lexed. */
typedef struct {
    bool isRaw;
    bool isInterpolated;
    
    char *interpolationStart;
    int interpolationColumn;
    int interpolationDepth;

    int escapes;
    char quote;
} StringLexer;

/** Represents how setting some metadata went. */
typedef enum {
    METADATA_SUCCESS,
    METADATA_END,
    METADATA_REPEATED_ERROR,
    METADATA_MISSING_STRING_ERROR
} MetadataStatus;

static void lex_token(Lexer *lexer);

/** Returns an initialized lexer with the source code passed. */
Lexer create_lexer(ZmxProgram *program, char *source) {
    Lexer lexer = {
        .source = source, .sourceLength = strlen(source),
        .current = source, .line = 1, .column = 1,
        .tokenStart = source, .tokenColumn = 1,
        .program = program,
        .tokens = CREATE_DA()
    };
    return lexer;
}

/** Frees all the memory the lexing stage of Zymux has used. */
void free_lexer(Lexer *lexer) {
    free_tokens_contents(&lexer->tokens);
    FREE_DA(&lexer->tokens);
}

/** Returns an initialized string lexer. */
static StringLexer create_string_lexer() {
    StringLexer string = {0};
    return string;
}

/** Allocates and returns the current token's lexeme in a NUL terminated heap string. */
static char *alloc_current_lexeme(Lexer *lexer) {
    const int length = lexer->current - lexer->tokenStart;
    char *string = ZMX_ARRAY_ALLOC(length + 1, char);
    
    strncpy(string, lexer->tokenStart, length);
    string[length] = '\0';
    return string;
}

/** 
 * Appends an implicit token.
 * 
 * it's an empty token of length 0 that the user didn't explicitly write,
 * and is only useful internally.
 */
static void append_implicit(Lexer *lexer, const TokenType type) {
    Token implicit = {
        .lexeme = "", .pos = create_src_pos(lexer->line, lexer->column, 0), .type = type
    };
    APPEND_DA(&lexer->tokens, implicit);
}

/** Appends the token we've been lexing as an error with the passed message. */
static void append_lexed_error(Lexer *lexer, const char *format, ...) {
    Token error = {
        .lexeme = lexer->tokenStart,
        .pos = create_src_pos(lexer->line, lexer->tokenColumn, CURRENT_TOKEN_LENGTH(lexer)),
        .type = TOKEN_ERROR,
    };
    APPEND_DA(&lexer->tokens, error);

    va_list args;
    va_start(args, format);
    zmx_user_error(lexer->program, error.pos, "Syntax error", format, &args);
    va_end(args);
}

/** Appends an error token in a specific position in the source code with the message. */
static void append_error_at(
    Lexer *lexer, char *errorStart, const SourcePosition pos, const char *format, ...
) {
    Token error = {.lexeme = errorStart, .pos = pos, .type = TOKEN_ERROR};
    APPEND_DA(&lexer->tokens, error);

    va_list args;
    va_start(args, format);
    zmx_user_error(lexer->program, error.pos, "Syntax error", format, &args);
    va_end(args);
}

/** Appends a token that was lexed (advanced over) of the passed type. */
static void append_lexed(Lexer *lexer, const TokenType type) {
    Token token = {
        .lexeme = lexer->tokenStart,
        .pos = create_src_pos(lexer->line, lexer->tokenColumn, CURRENT_TOKEN_LENGTH(lexer)),
        .type = type
    };
    APPEND_DA(&lexer->tokens, token);
}

/** Appends an integer that was lexed and sets its integer union to the passed literal value. */
static void append_lexed_int(Lexer *lexer, const ZmxInt integer) {
    Token token = {
        .lexeme = lexer->tokenStart,
        .pos = create_src_pos(lexer->line, lexer->tokenColumn, CURRENT_TOKEN_LENGTH(lexer)),
        .type = TOKEN_INT_LIT, .intVal = integer
    };
    APPEND_DA(&lexer->tokens, token);
}

/** Appends a float that was lexed and sets its float union to the passed literal value. */
static void append_lexed_float(Lexer *lexer, const ZmxFloat floatVal) {
    Token token = {
        .lexeme = lexer->tokenStart,
        .pos = create_src_pos(lexer->line, lexer->tokenColumn, CURRENT_TOKEN_LENGTH(lexer)),
        .type = TOKEN_FLOAT_LIT, .floatVal = floatVal
    };
    APPEND_DA(&lexer->tokens, token);
}

/** Appends a string that was lexed by making the string union the passed buffer. */
static void append_lexed_string(Lexer *lexer, StringLexer *string, CharBuffer buffer) {
    Token token = {
        .lexeme = lexer->tokenStart, 
        .pos = create_src_pos(
            lexer->line, lexer->tokenColumn,
            buffer.length + string->escapes - 1 // -1 because CharBuffer counts NULL.
        ),
        .type = TOKEN_STRING_LIT, .stringVal = {.length = buffer.length, .text = buffer.text}
    };
    APPEND_DA(&lexer->tokens, token);
}

/** One line comments, which start with 2 slashes then ignore everything until EOF or newline. */
static void comment(Lexer *lexer) {
    ADVANCE_DOUBLE(lexer);
    while (!IS_EOF(lexer) && PEEK(lexer) != '\n') {
        ADVANCE(lexer);
    }
}

/** Starts with slash-asterisk and skips characters until EOF or a closing asterisk-slash. */
static void multiline_comment(Lexer *lexer) {
    START_TOKEN(lexer);
    ADVANCE_DOUBLE(lexer);

    while (true) {
        switch (ADVANCE_PEEK(lexer)) {
        case '\0':
            append_error_at(
                lexer, lexer->tokenStart, create_src_pos(lexer->line, lexer->tokenColumn, 2),
                "Unterminated multiline comment."
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
            if (PEEK_NEXT(lexer) == '/') {
                comment(lexer);
                break;
            }
            if (PEEK_NEXT(lexer) == '*') {
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
 * The other 2 possibilities are lexed if their expected character is found.
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
 *     TOKEN_EXPONENT is extended, and TOKEN_EXPONENT_EQUAL is extendedCompound.
 */
static void two_compound_assigns(
    Lexer *lexer, const char overloadedChar, 
    const TokenType original, const TokenType originalCompound,
    const TokenType extended, const TokenType extendedCompound
) {
    if (PEEK(lexer) == overloadedChar && PEEK_NEXT(lexer) == '=') {
        ADVANCE_DOUBLE(lexer);
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
        if (is_keyword(lexer, "map")) return TOKEN_MAP_KW;
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

/** Appends the lexed integer token. */
static void append_lexed_base(Lexer *lexer, const int base, const bool hasPrefix) {
    // Create a terminated version of the lexeme to use strtoll on it.
    char *terminatedLexeme = alloc_current_lexeme(lexer);
    ZmxInt integer = strtoll(terminatedLexeme, NULL, base);
    free(terminatedLexeme);

    if (hasPrefix) {
        lexer->tokenStart -= 2;
        lexer->tokenColumn -= 2;
    }
    append_lexed_int(lexer, integer);
}

/** Errors a number literal covering the number until next isn't alphabetical or numerical. */
static void invalid_literal(Lexer *lexer, const bool hasPrefix, const char *message) {
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
 * It advances through characters as long as they return true on an is_within_base call,
 * then appends the lexed base.
 */
static void lex_base(
    Lexer *lexer, const BaseCheckFunc is_within_base, const int base, const char *errorMessage
) {
    ADVANCE_DOUBLE(lexer);
    START_TOKEN(lexer);
    if (!is_within_base(PEEK(lexer))) {
        invalid_literal(lexer, true, errorMessage);
        return;
    }

    while (is_within_base(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    if (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        invalid_literal(lexer, true, errorMessage);
    } else {
        append_lexed_base(lexer, base, true);   
    }
}

/**
 * Finishes the lexing of a float after the whole number and dot.
 * 
 */
static void finish_float(Lexer *lexer) {
    while (IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    if (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        invalid_literal(lexer, false, "Invalid float literal.");
    } else {
        // Create a terminated version of the lexeme to use strtod on it.
        char *terminatedLexeme = alloc_current_lexeme(lexer);
        ZmxFloat floatVal = strtod(terminatedLexeme, NULL);
        free(terminatedLexeme);
        append_lexed_float(lexer, floatVal);
    }
}

/** Lexes and appends a "normal" number, which is a decimal integer or float. */
static void normal_number(Lexer *lexer) {
    START_TOKEN(lexer);
    while (IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    
    if (PEEK(lexer) == '.' && IS_DIGIT(PEEK_NEXT(lexer))) {
        ADVANCE_DOUBLE(lexer);
        finish_float(lexer);
        return;
    }

    if (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        invalid_literal(lexer, false, "Invalid integer literal.");
        return;
    }
    append_lexed_base(lexer, 10, false);
}

/** Returns whether or not ch is binary. */
static bool is_bin_digit(const char ch) {
    return ch == '0' || ch == '1';
}

/** Returns whether or not ch is octal. */
static bool is_oct_digit(const char ch) {
    return ch >= '0' && ch <= '7';
}

/** Returns whether or not ch is hexadecimal. */
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
    while (PEEK(lexer) == '0' && PEEK_NEXT(lexer) == '0') {
        ADVANCE(lexer);
    }
    switch (LOWERED_CHAR(PEEK_NEXT(lexer))) {
    case 'b': lex_base(lexer, is_bin_digit, 2, "invalid binary literal."); break;
    case 'o': lex_base(lexer, is_oct_digit, 8, "invalid octal literal."); break;
    case 'x': lex_base(lexer, is_hex_digit, 16, "invalid hexadecimal literal."); break;
    default:
        // Skip last zero if it's not the only thing in the whole number.
        if (PEEK(lexer) == '0' && IS_DIGIT(PEEK_NEXT(lexer))) {
            ADVANCE(lexer);
        }
        normal_number(lexer); break;
    }
}

/** Ignores whitespace for interpolated expressions, where newlines and comments aren't allowed. */
static bool ignore_interpolation_whitespace(Lexer *lexer) {
    while (true) {
        switch (PEEK(lexer)) {
        case '\n':
            UNREACHABLE_ERROR(); // Should've errored earlier, so unreachable.
        case ' ':
        case '\t':
        case '\r':
            ADVANCE(lexer);
            break;
        case '/':
            if (PEEK_NEXT(lexer) == '/' || PEEK_NEXT(lexer) == '*') {
                append_error_at(
                    lexer, lexer->tokenStart, create_src_pos(lexer->line, lexer->tokenColumn, 2),
                    "Can't have comment inside string interpolation."
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
 * Resets the string lexer's interpolation state,
 * and sets the lexer to start scanning after the closing brace.
 */
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
        // Empty/starts with whitespace error, don't interpolate anything.
        finish_interpolation(lexer, string, exprEnd, exprEndColumn);
        return;
    }

    append_implicit(lexer, TOKEN_INTERPOLATE);
    while (ignore_interpolation_whitespace(lexer) && lexer->current < exprEnd) {
        lex_token(lexer);
    }
    finish_interpolation(lexer, string, exprEnd, exprEndColumn);
}

/** 
 * Prepares to start lexing the interpolation's tokens.
 * 
 * It does so by appending the current buffer literal then a format beforehand.
 * Returns whether or not we actually lexed the interpolation.
 */
static bool interpolate(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    string->interpolationDepth--;
    if (string->interpolationDepth > 0) {
        // Not the correct closing brace. Simply append the brace, then keep going.
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
 * It tries to interpolate, if it succeeds then it'll append a TOKEN_INTERPOLATE and keep going.
 * There's an edge case if we see the closing quote though, indicating the string finished
 * on the interpolation, in which case we append STRING_END ourselves and return false to stop.
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
        append_implicit(lexer, TOKEN_INTERPOLATE);
        return true;
    }
}

/** 
 * Starts preparing the string lexer's state for a closing brace to interpolate.
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

/** Appends and advances over an escape character made up of a backslash and another character. */
static void escape_character(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    switch (PEEK_NEXT(lexer)) {
        case 'n': buffer_append_char(buffer, '\n'); break;
        case 't': buffer_append_char(buffer, '\t'); break;
        case 'r': buffer_append_char(buffer, '\r'); break;
        case 'b': buffer_append_char(buffer, '\b'); break;
        default: buffer_append_char(buffer, PEEK_NEXT(lexer)); break;
    }
    string->escapes++;
    ADVANCE_DOUBLE(lexer);
}

/** 
 * Appends one character to the literal char buffer.
 * It may call other scanning helpers if it sees a specific character while some metadata is set.
 * It could also error if it encounters a newline or EOF.
 * 
 * Return whether or not we should continue scanning characters.
 */
static bool scan_string_char(
    Lexer *lexer, StringLexer *string, CharBuffer *buffer, char *quote, const int quoteColumn
) {
    const char current = PEEK(lexer);
    if (current == '\\' && !string->isRaw) {
        escape_character(lexer, string, buffer);
        return true;
    } else if (current == '{' && string->isInterpolated) {
        interpolation_start(lexer, string, buffer);
        return true;
    } else if (current == '}' && string->interpolationStart != NULL) {
        return interpolation_end(lexer, string, buffer);
    } else if (IS_EOF(lexer) || current == '\n') {
        append_error_at(
            lexer, quote, create_src_pos(lexer->line, quoteColumn, 1), "Unterminated string."
        );
        return false;
    }
    buffer_append_char(buffer, ADVANCE_PEEK(lexer));
    return true;
}

/** 
 * Finishes the lexing of a string after the metadata of the string is fully set.
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
            lexer, string->interpolationStart,
            create_src_pos(lexer->line, string->interpolationColumn, 1),
            "Unclosed \"{\" in interpolated string."
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

/** 
 * Sets a piece of metadata on the passed string lexer.
 * 
 * The metadata is expected to be the current character in the lexer.
 * It returns a status of how setting the metadata went.
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
 * 
 * It advances until EOF or encountering valid syntax, then errors.
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
    case '=': one_or_default(lexer, TOKEN_ASSIGN, '=', TOKEN_EQ_EQ); break;
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
 * The lexed tokens are appended to the lexer's token array with an EOF token always at the end.
 * 
 * Returns whether or not lexing was successful.
 */
bool lex(Lexer *lexer) {
    while (true) {
        ignore_whitespace(lexer);
        lex_token(lexer);
        if (lexer->tokens.data[lexer->tokens.length - 1].type == TOKEN_EOF) {
            break;
        }
    }
    
    return !lexer->program->hasErrored;
}
