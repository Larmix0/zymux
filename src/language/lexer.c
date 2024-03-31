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
#define RETREAT(lexer) ((lexer)->column--, (lexer)->current--)

#define LOWERED_CHAR(ch) ((ch) >= 'A' && (ch) <= 'Z' ? (ch) + 32 : (ch))
#define IS_ALPHA(ch) ((LOWERED_CHAR((ch)) >= 'a' && LOWERED_CHAR((ch)) <= 'z') || (ch) == '_')
#define IS_DIGIT(ch) ((ch) >= '0' && (ch) <= '9')

#define IS_EOF(lexer) (*(lexer)->current == '\0')

typedef bool (*BaseCheckFunc)(const char ch);

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

void free_lexer(Lexer *lexer) {
    free_token_array(&lexer->tokens);
}

TokenArray create_token_array() {
    TokenArray tokens = {0};
    return tokens;
}

void append_token(TokenArray *tokens, Token token) {
    APPEND_DA(tokens->tokens, tokens->length, tokens->capacity, token, Token);
}

void free_token_array(TokenArray *tokens) {
    for (int i = 0; i < tokens->length; i++) {
        if (tokens->tokens[i].type == TOKEN_STRING_LIT) {
            free_char_buffer(&tokens->tokens[i].stringVal);
        }
    }
    free(tokens->tokens);
}

static StringLexer create_string_lexer() {
    StringLexer string = {0};
    return string;
}

Token create_token(char *message, const int line, const int column, const TokenType type) {
    Token token = {
        .lexeme = message, .length = strlen(message),
        .line = line, .column = column,
        .type = type
    };
    return token;
}

static Token implicit_token(Lexer *lexer, const TokenType type) {
    return create_token("", lexer->line, lexer->column, type);
}

static void append_lexed_error(Lexer *lexer, char *message) {
    Token errorToken = {
        .lexeme = lexer->tokenStart, .length = CURRENT_TOKEN_LENGTH(lexer),
        .line = lexer->tokenLine, .column = lexer->tokenColumn,
        .errorMessage = message, .type = TOKEN_ERROR,
    };
    append_token(&lexer->tokens, errorToken);
    SYNTAX_ERROR(lexer->program, errorToken);
}

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

static void append_lexed(Lexer *lexer, const TokenType type) {
    Token token = {
        .lexeme = lexer->tokenStart, .length = CURRENT_TOKEN_LENGTH(lexer),
        .line = lexer->tokenLine, .column = lexer->tokenColumn,
        .type = type
    };
    append_token(&lexer->tokens, token);
}

static void append_lexed_int(Lexer *lexer, ZmxInt integer) {
    Token token = {
        .lexeme = lexer->tokenStart, .length = CURRENT_TOKEN_LENGTH(lexer),
        .line = lexer->tokenLine, lexer->tokenColumn,
        .type = TOKEN_INT_LIT, .intVal = integer
    };
    append_token(&lexer->tokens, token);
}

static void append_lexed_string(Lexer *lexer, StringLexer *string, CharBuffer buffer) {
    Token token = {
        .lexeme = lexer->tokenStart, 
        .length = buffer.length + string->escapes - 1, // -1 because CharBuffer counts NULL.
        .line = lexer->tokenLine, lexer->tokenColumn,
        .type = TOKEN_STRING_LIT, .stringVal = buffer
    };
    append_token(&lexer->tokens, token);
}

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

static void comment(Lexer *lexer) {
    ADVANCE_AMOUNT(lexer, 2); // Skip the 2 slashes.
    while (!IS_EOF(lexer) && PEEK(lexer) != '\n') {
        ADVANCE(lexer);
    }
}

static void multiline_comment(Lexer *lexer) {
    START_TOKEN(lexer);
    ADVANCE_AMOUNT(lexer, 2); // Skip the "/*".
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
            if (PEEK(lexer) == '/') {
                ADVANCE(lexer); // Finishes the "*/"
                return;
            }
            break;
        default:
            break;
        }
    }
}

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

static void one_or_default(
    Lexer *lexer, const TokenType defaultType, const char expected, const TokenType expectedType
) {
    if (PEEK(lexer) == expected) {
        ADVANCE(lexer);
        append_lexed(lexer, expectedType);
        return;
    }
    append_lexed(lexer, defaultType);
}

static void two_or_default(
    Lexer *lexer, const TokenType defaultType,
    const char expected1, const TokenType type1, const char expected2, const TokenType type2
) {
    if (PEEK(lexer) == expected1) {
        ADVANCE(lexer);
        append_lexed(lexer, type1);
        return;
    }
    one_or_default(lexer, defaultType, expected2, type2);
}

static void two_compound_assigns(
    Lexer *lexer, const char overloadedChar, 
    const TokenType original, const TokenType originalCompound,
    const TokenType extended, const TokenType extendedCompound
) {
    if (PEEK(lexer) == overloadedChar && PEEK_DISTANCE(lexer, 1) == '=') {
        ADVANCE_AMOUNT(lexer, 2);
        append_lexed(lexer, extendedCompound);
        return;
    }
    two_or_default(lexer, original, overloadedChar, extended, '=', originalCompound);
}

static bool is_keyword(Lexer *lexer, const char *keyword) {
    const int length = strlen(keyword);
    if (CURRENT_TOKEN_LENGTH(lexer) != length) {
        return false;
    }
    return strncmp(lexer->tokenStart, keyword, length) == 0;
}

static TokenType handle_keywords(Lexer *lexer) {
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

static void lex_name(Lexer *lexer) {
    while (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    const TokenType type = handle_keywords(lexer);
    append_lexed(lexer, type);
}

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

static void lex_base(
    Lexer *lexer, const BaseCheckFunc is_within_base, const int base, char *errorMessage
) {
    ADVANCE_AMOUNT(lexer, 2); // Skip the prefix (like 0b and 0x).
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
        return;
    }
    append_lexed_base(lexer, base, true);
}

static void finish_float(Lexer *lexer) {
    while (IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    if (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        invalid_literal(lexer, "Invalid float literal.", false);
        return;
    }
    append_lexed(lexer, TOKEN_FLOAT_LIT);
}

static void normal_number(Lexer *lexer) {
    // Skip zero padding, except the last one if it's the last thing in the whole number.
    while (PEEK(lexer) == '0' && IS_DIGIT(PEEK_DISTANCE(lexer, 1))) {
        ADVANCE(lexer);
    }

    START_TOKEN(lexer);
    while (IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    // Handle floats.
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

static bool is_bin_digit(const char ch) {
    return ch == '0' || ch == '1';
}

static bool is_oct_digit(const char ch) {
    return ch >= '0' && ch <= '7';
}

static bool is_hex_digit(const char ch) {
    const char lowered = LOWERED_CHAR(ch);
    return IS_DIGIT(lowered) || (lowered >= 'a' && lowered <= 'f');
}

static void lex_number(Lexer *lexer) {
    if (PEEK(lexer) != '0') {
        normal_number(lexer);
        return;
    }
    switch (LOWERED_CHAR(PEEK_DISTANCE(lexer, 1))) {
    case 'b': lex_base(lexer, is_bin_digit, 2, "invalid binary literal."); break;
    case 'o': lex_base(lexer, is_oct_digit, 8, "invalid octal literal."); break;
    case 'x': lex_base(lexer, is_hex_digit, 16, "invalid hexadecimal literal."); break;
    default: normal_number(lexer); break;
    }
}

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

static void lex_interpolation(Lexer *lexer, StringLexer *string) {
    char *exprEnd = lexer->current;
    int exprEndColumn = lexer->column;
    lexer->current = string->interpolationStart + 1;
    lexer->column = string->interpolationColumn + 1;

    while (lexer->current < exprEnd) {
        if (!ignore_interpolation_whitespace(lexer)) {
            break;
        }
        lex_token(lexer);
        if (!ignore_interpolation_whitespace(lexer)) {
            break;
        }
    }
    lexer->current = exprEnd + 1;
    lexer->column = exprEndColumn + 1;
    string->interpolationStart = NULL;
    string->interpolationColumn = 0;
}

static bool interpolate(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    string->interpolationDepth--;
    if (string->interpolationDepth > 0) {
        buffer_append_char(buffer, ADVANCE_PEEK(lexer));
        return false;
    }
    buffer_pop_amount(buffer, lexer->current - string->interpolationStart);

    // Responsibility of freeing buffer is passed to the token.
    append_lexed_string(lexer, string, *buffer);
    *buffer = create_char_buffer();
    append_token(&lexer->tokens, implicit_token(lexer, TOKEN_FORMAT));
    lex_interpolation(lexer, string);
    return true;
}

static bool interpolation_end(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    if (!interpolate(lexer, string, buffer)) {
        return true;
    }
    
    START_TOKEN(lexer);
    if (PEEK(lexer) != string->quote) {
        append_token(&lexer->tokens, implicit_token(lexer, TOKEN_FORMAT));
        return true;
    }
    // interpolation ended the string
    ADVANCE(lexer); // Skip closing quote.
    append_token(&lexer->tokens, implicit_token(lexer, TOKEN_STRING_END));
    return false;
}

static void interpolation_start(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    if (PEEK_DISTANCE(lexer, 1) == '}') {
        append_lexed(lexer, TOKEN_STRING_LIT);
        append_token(&lexer->tokens, implicit_token(lexer, TOKEN_FORMAT));
        START_TOKEN(lexer);
        ADVANCE_AMOUNT(lexer, 2);
        append_lexed_error(lexer, "Can't have empty interpolation.");
        return;
    }

    if (string->interpolationStart == NULL) {
        string->interpolationStart = lexer->current;
        string->interpolationColumn = lexer->column;
    }
    string->interpolationDepth++;
    buffer_append_char(buffer, ADVANCE_PEEK(lexer));
}

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
    if (current == '\n' && string->interpolationStart == NULL) {
        append_error_at(lexer, "Unterminated string.", quote, 1, lexer->line, quoteColumn);
        return false;
    }
    buffer_append_char(buffer, ADVANCE_PEEK(lexer));
    return true;
}

static void finish_string(Lexer *lexer, StringLexer *string) {
    char *quote = lexer->current - 1;
    int quoteColumn = lexer->column - 1;
    CharBuffer buffer = create_char_buffer();
    while (PEEK(lexer) != string->quote) {
        if (IS_EOF(lexer)) {
            append_error_at(lexer, "Unterminated string.", quote, 1, lexer->line, quoteColumn);
            free_char_buffer(&buffer);
            return;
        }
        if (!scan_string_char(lexer, string, &buffer, quote, quoteColumn)) {
            free_char_buffer(&buffer);
            return;
        }
    }
    ADVANCE(lexer); // Skip closing quote.
    append_lexed_string(lexer, string, buffer); // Responsibility of freeing the buffer is passed.
    append_token(&lexer->tokens, implicit_token(lexer, TOKEN_STRING_END));
}

static void repeated_metadata_error(Lexer *lexer) {
    while (!IS_EOF(lexer)) {
        char current = ADVANCE_PEEK(lexer);
        if (current != '#' && current != '$') {
            RETREAT(lexer);
            break;
        }
    }
    append_lexed_error(lexer, "Can't repeat \"$\" or \"#\" on string.");
}

typedef enum {
    METADATA_SUCCESS,
    METADATA_END,
    METADATA_REPEATED_ERROR,
    METADATA_MISSING_STRING_ERROR
} MetadataStatus;

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
        // Print the character after #/$ then go back so it can be lexed as a separate token.
        START_TOKEN(lexer);
        ADVANCE(lexer);
        append_lexed_error(lexer, "Expected string after \"#\" or \"$\".");
        RETREAT(lexer);
        START_TOKEN(lexer);
        return METADATA_MISSING_STRING_ERROR;
    }
}

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

static void unsupported_syntax(Lexer *lexer) {
    while (!IS_EOF(lexer) && !valid_syntax(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    append_lexed_error(lexer, "Unsupported syntax.");
    return;
}

static void lex_token(Lexer *lexer) {
    START_TOKEN(lexer);
    if (IS_EOF(lexer)) {
        append_token(&lexer->tokens, implicit_token(lexer, TOKEN_EOF));
        return;
    }

    char current = ADVANCE_PEEK(lexer);
    switch (current) {
    // String-related, should be left to the string lexer.
    case '#':
    case '$':
    case '\'':
    case '"':
        RETREAT(lexer);
        lex_string(lexer);
        break;
    
    // Single character token.
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

    // Single or double character token.
    case '+': one_or_default(lexer, TOKEN_PLUS, '=', TOKEN_PLUS_EQ); break;
    case '-': one_or_default(lexer, TOKEN_MINUS, '=', TOKEN_MINUS_EQ); break;
    case '/': one_or_default(lexer, TOKEN_SLASH, '=', TOKEN_SLASH_EQ); break;
    case '%': one_or_default(lexer, TOKEN_MODULO, '=', TOKEN_MODULO_EQ); break;
    case '=': one_or_default(lexer, TOKEN_EQ, '=', TOKEN_EQ_EQ); break;
    case '!': one_or_default(lexer, TOKEN_BANG, '=', TOKEN_BANG_EQ); break;
    case '^': one_or_default(lexer, TOKEN_CARET, '=', TOKEN_CARET_EQ); break;
    case '~': one_or_default(lexer, TOKEN_TILDE, '=', TOKEN_TILDE_EQ); break;
    case '.': one_or_default(lexer, TOKEN_DOT, '.', TOKEN_DOT_DOT); break;
    
    // Characters that have 2 possible continuation tokens + 1 default one.
    case '&': 
        two_or_default(lexer, TOKEN_AMPER, '&', TOKEN_AMPER_AMPER, '=', TOKEN_AMPER_EQ); break;
    case '|': 
        two_or_default(lexer, TOKEN_BAR, '|', TOKEN_BAR_BAR, '=', TOKEN_BAR_EQ); break;

    // Characters that act like two possibilities, but can be repeated for 2 other possibilities.
    // Eg: "*" and "*=" can be "**" and "**=" because the first character is repeated.
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

bool lex(Lexer *lexer) {
    while (true) {
        ignore_whitespace(lexer);
        lex_token(lexer);
        Token lastToken = lexer->tokens.tokens[lexer->tokens.length - 1];
        if (TOKEN_IS_TYPE(lastToken, TOKEN_EOF)) {
            break;
        }
    }

    if (lexer->program->hasErrored) {
        return false;
    }
    return true;
}
