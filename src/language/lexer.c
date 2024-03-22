#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"
#include "char_buffer.h"
#include "data_structures.h"
#include "errors.h"

#define START_TOKEN(lexer) ((lexer)->tokenStart = (lexer)->current)
#define CURRENT_TOKEN_LENGTH(lexer) ((lexer)->current - (lexer)->tokenStart)

#define PEEK(lexer) (*(lexer)->current)
#define PEEK_DISTANCE(lexer, distance) \
    ((lexer)->current + (distance) >= (lexer)->source + (lexer)->sourceLength \
        ? '\0' : (lexer)->current[(distance)])

#define ADVANCE_PEEK(lexer) (*(lexer)->current++)
#define ADVANCE(lexer) ((lexer)->current++)
#define ADVANCE_AMOUNT(lexer, amount) ((lexer)->current += (amount))
#define RETREAT(lexer) ((lexer)->current--)

#define LOWERED_CHAR(ch) ((ch) >= 'A' && (ch) <= 'Z' ? (ch) + 32 : (ch))
#define IS_ALPHA(ch) ((LOWERED_CHAR((ch)) >= 'a' && LOWERED_CHAR((ch))) || (ch) == '_')
#define IS_DIGIT(ch) ((ch) >= '0' && (ch) <= '9')

#define IS_EOF(lexer) (*(lexer)->current == '\0')

typedef bool (*BaseCheckFunc)(const char ch);

typedef struct {
    bool isRaw;
    bool isInterpolated;
    char *interpolationStart;
    int interpolationDepth;
    char quote;
} StringLexer;

static void lex_until(Lexer *lexer, const char *end);

Lexer create_lexer(ZymuxProgram *program, char *source) {
    Lexer lexer = {
        .current = source, .tokenStart = source, .source = source,
        .sourceLength = strlen(source),
        .program = program,
        .line = 1,
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
        free(tokens->tokens[i].lexeme);
    }
    free(tokens->tokens);
}

static StringLexer create_string_lexer() {
    StringLexer string = {0};
    return string;
}

Token create_token(char *message, const int line, const TokenType type) {
    Token token = {.lexeme = message, .length = strlen(message), .line = line, .type = type};
    return token;
}

Token token_alloc_lexeme(const char *message, const int line, const TokenType type) {
    int length = strlen(message);
    Token token = {
        .lexeme = ZMX_ARRAY_ALLOC(length + 1, char),
        .length = length, .line = line, .type = type
    };
    strncpy(token.lexeme, message, length);
    token.lexeme[length] = '\0';
    return token;
}

static void append_error(Lexer *lexer, const char *message, const int line) {
    SYNTAX_ERROR(lexer->program, line, message);
    append_token(&lexer->tokens, token_alloc_lexeme(message, line, TOKEN_ERROR));
    START_TOKEN(lexer);
}

static void append_lexed(Lexer *lexer, const TokenType type) {
    int length = CURRENT_TOKEN_LENGTH(lexer);
    Token token = {
        .lexeme = ZMX_ARRAY_ALLOC(length + 1, char),
        .length = length, .line = lexer->line, .type = type
    };
    strncpy(token.lexeme, lexer->tokenStart, length);
    token.lexeme[length] = '\0';
    append_token(&lexer->tokens, token);
}

bool tokens_equal(const Token left, const Token right) {
    if (left.type != right.type || left.length != right.length) {
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
    const int commentStart = lexer->line;
    ADVANCE_AMOUNT(lexer, 2); // Skip the "/*".

    while (true) {
        switch (PEEK(lexer)) {
        case '\0':
            SYNTAX_ERROR(lexer->program, commentStart, "Unterminated multiline comment.");
            return;
        case '\n':
            lexer->line++;
            break;
        case '*':
            if (PEEK_DISTANCE(lexer, 1) == '/') {
                ADVANCE_AMOUNT(lexer, 2); // Advances over the "*/"
                return;
            }
            break;
        default:
            break;
        }
        ADVANCE(lexer);
    }
}

static void advance_whitespace(Lexer *lexer) {
    while (true) {
        switch (PEEK(lexer)) {
        case ' ':
        case '\t':
        case '\r':
            ADVANCE(lexer);
            break;
        case '\n':
            lexer->line++;
            ADVANCE(lexer);
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

static void two_possibilities(
    Lexer *lexer, const TokenType defaultType, const char expected, const TokenType expectedType
) {
    if (PEEK(lexer) == expected) {
        ADVANCE(lexer);
        append_lexed(lexer, expectedType);
        return;
    }
    append_lexed(lexer, defaultType);
}

static void three_possibilities(
    Lexer *lexer, const TokenType defaultType,
    const char expected1, const TokenType type1, const char expected2, const TokenType type2
) {
    if (PEEK(lexer) == expected1) {
        ADVANCE(lexer);
        append_lexed(lexer, type1);
        return;
    }
    two_possibilities(lexer, defaultType, expected2, type2);
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
    three_possibilities(lexer, original, overloadedChar, extended, '=', originalCompound);
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

/** 
 * @brief Appends a token from lexed number of a base.
 * 
 * Add to the int sum the base to the power of its place in the number.
 * (For example, tenths are base ^ 2 and hundredths are base ^ 3). 
 * Then, multiply that by the actual number in that place.
 * 
 * @param lexer The lexer.
 * @param base The base of the lexed number.
 */
static void append_lexed_base(Lexer *lexer, const int base) {
    ZmxInt sum = 0;
    for (int i = 0; i < CURRENT_TOKEN_LENGTH(lexer); i++) {
        char place = PEEK_DISTANCE(lexer, -i - 1);
        ZmxInt placeValue = IS_DIGIT(place) ? place - '0' : LOWERED_CHAR(place) - 'a' + 10;
        sum += zmx_power(base, i) * placeValue;
    }
    char numAsString[32];
    snprintf(numAsString, 32, ZMX_INT_FMT, sum);
    append_token(&lexer->tokens, token_alloc_lexeme(numAsString, lexer->line, TOKEN_INT_LIT));
}

static void invalid_literal(Lexer *lexer, const char *stringRepr, const int line) {
    CharBuffer buffer = create_char_buffer();
    buffer_append_strings(&buffer, 3, "Invalid ", stringRepr, " literal.");

    append_error(lexer, buffer.text, line);
    free_char_buffer(&buffer);
}

static void lex_base(
    Lexer *lexer, const BaseCheckFunc is_within_base, const int base, const char *stringRepr
) {
    ADVANCE_AMOUNT(lexer, 2); // Skip the prefix (like 0b and 0x).
    START_TOKEN(lexer);

    if (!is_within_base(PEEK(lexer))) {
        invalid_literal(lexer, stringRepr, lexer->line);
        return;
    }
    while (is_within_base(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    if (IS_ALPHA(PEEK(lexer)) || IS_DIGIT(PEEK(lexer))) {
        invalid_literal(lexer, stringRepr, lexer->line);
        return;
    }
    append_lexed_base(lexer, base);
}

static void normal_number(Lexer *lexer) {
    // Skip zero padding
    while (PEEK(lexer) == '0') {
        ADVANCE(lexer);
    }
    // Except the last one if it's the last thing in the whole number.
    if (!IS_DIGIT(PEEK(lexer))) {
        RETREAT(lexer);
    }

    START_TOKEN(lexer);
    while (IS_DIGIT(PEEK(lexer))) {
        ADVANCE(lexer);
    }
    // Handle floats.
    if (PEEK(lexer) == '.' && IS_DIGIT(PEEK_DISTANCE(lexer, 1))) {
        ADVANCE_AMOUNT(lexer, 2);

        while (IS_DIGIT(PEEK(lexer))) {
            ADVANCE(lexer);
        }
        append_lexed(lexer, TOKEN_FLOAT_LIT);
        return;
    }
    append_lexed(lexer, TOKEN_INT_LIT);
}

static bool is_oct_digit(const char ch) {
    return ch >= '0' && ch <= '7';
}

static bool is_hex_digit(const char ch) {
    const char lowered = LOWERED_CHAR(ch);
    return IS_DIGIT(lowered) || (lowered >= 'a' && lowered <= 'f');
}

static bool is_bin_digit(const char ch) {
    return ch == '0' || ch == '1';
}

static void lex_number(Lexer *lexer) {
    switch (LOWERED_CHAR(PEEK_DISTANCE(lexer, 1))) {
    case 'b': lex_base(lexer, is_bin_digit, 2, "binary"); break;
    case 'o': lex_base(lexer, is_oct_digit, 8, "octal"); break;
    case 'x': lex_base(lexer, is_hex_digit, 16, "hexadecimal"); break;
    default: normal_number(lexer); break;
    }
}

static void flush_buffer_token(Lexer *lexer, CharBuffer *buffer) {
    START_TOKEN(lexer);
    append_token(&lexer->tokens, token_alloc_lexeme(buffer->text, lexer->line, TOKEN_STRING_LIT));
    free_char_buffer(buffer);
}

static bool interpolate(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    string->interpolationDepth--;
    if (string->interpolationDepth > 0) {
        buffer_append_char(buffer, ADVANCE_PEEK(lexer));
        return false;
    }
    buffer_pop_amount(buffer, lexer->current - string->interpolationStart);
    flush_buffer_token(lexer, buffer);
    *buffer = create_char_buffer();

    char *exprEnd = lexer->current + 1;
    lexer->current = string->interpolationStart + 1; // After open brace.

    append_token(&lexer->tokens, token_alloc_lexeme("", lexer->line, TOKEN_FORMAT));
    START_TOKEN(lexer);
    lex_until(lexer, exprEnd - 2); // -2 To stop before closing brace.

    lexer->current = exprEnd;
    START_TOKEN(lexer);
    string->interpolationStart = NULL;
    return true;
}

static bool interpolation_end(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    if (!interpolate(lexer, string, buffer)) {
        return true;
    }
    if (PEEK(lexer) != string->quote) {
        append_token(&lexer->tokens, token_alloc_lexeme("", lexer->line, TOKEN_FORMAT));
        return true;
    }
    // interpolation ended the string
    ADVANCE(lexer); // Skip closing quote.
    append_token(&lexer->tokens, token_alloc_lexeme("", lexer->line, TOKEN_STRING_END));
    return false;
}

static bool interpolation_start(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    if (PEEK_DISTANCE(lexer, 1) == '}') {
        append_error(lexer, "Can't have an empty interpolated string.", lexer->line);
        return false;
    }
    if (string->interpolationStart == NULL) {
        string->interpolationStart = lexer->current;
    }
    string->interpolationDepth++;
    buffer_append_char(buffer, ADVANCE_PEEK(lexer));
    return true;
}

static void escape_character(Lexer *lexer, CharBuffer *buffer) {
    switch (PEEK_DISTANCE(lexer, 1)) {
        case 'n': buffer_append_char(buffer, '\n'); break;
        case 't': buffer_append_char(buffer, '\t'); break;
        case 'r': buffer_append_char(buffer, '\r'); break;
        case 'b': buffer_append_char(buffer, '\b'); break;
        default: buffer_append_char(buffer, PEEK_DISTANCE(lexer, 1)); break;
    }
    ADVANCE_AMOUNT(lexer, 2);
}

static bool scan_string_char(Lexer *lexer, StringLexer *string, CharBuffer *buffer) {
    char current = PEEK(lexer);
    if (current == '\\' && !string->isRaw) {
        escape_character(lexer, buffer);
        return true;
    }
    if (current == '{' && string->isInterpolated) {
        return interpolation_start(lexer, string, buffer);
    }
    if (current == '}' && string->interpolationStart != NULL) {
        return interpolation_end(lexer, string, buffer);
    }
    if (current == '\n' && string->interpolationStart == NULL) {
        lexer->line++;
    }
    buffer_append_char(buffer, ADVANCE_PEEK(lexer));
    return true;
}

static void finish_string(Lexer *lexer, StringLexer *string) {
    const int line = lexer->line; // Line of quote.
    CharBuffer buffer = create_char_buffer();

    while (!IS_EOF(lexer) && PEEK(lexer) != string->quote) {
        if (!scan_string_char(lexer, string, &buffer)) {
            free_char_buffer(&buffer);
            return; // Done.
        }
    }
    if (IS_EOF(lexer)) {
        append_error(lexer, "Unterminated string.", line);
        free_char_buffer(&buffer);
        return;
    }
    ADVANCE(lexer); // Skip closing quote.
    flush_buffer_token(lexer, &buffer);
    append_token(&lexer->tokens, token_alloc_lexeme("", lexer->line, TOKEN_STRING_END));
}

static void repeated_metadata_error(Lexer *lexer) {
    while (!IS_EOF(lexer)) {
        advance_whitespace(lexer);

        char current = ADVANCE_PEEK(lexer);
        if (current != '#' && current != '$') {
            RETREAT(lexer);
            break;
        }
    }
    append_error(lexer, "Can't repeat \"$\" or \"#\" on string.", lexer->line);
}

static void set_string_metadata(Lexer *lexer, StringLexer *string, const char data) {
    switch (data) {
    case '#':
        if (string->isRaw) {
            repeated_metadata_error(lexer);
        } else {
            string->isRaw = true;
        }
        break;
    case '$':
        if (string->isInterpolated) {
            repeated_metadata_error(lexer);
        } else {
            string->isInterpolated = true;
        }
        break;
    case '\'':
    case '"':
        string->quote = data;
        break;
    default:
        append_error(lexer, "Expected string after \"#\" or \"$\".", lexer->line);
        break;
    }
}

static void lex_string(Lexer *lexer) {
    StringLexer string = create_string_lexer();

    char current = '\0';
    while (current != '\'' && current != '"') {
        advance_whitespace(lexer);
        current = ADVANCE_PEEK(lexer);
        set_string_metadata(lexer, &string, current);

        if (current != '$' && current != '#' && current != '\'' && current != '"') {
            RETREAT(lexer);
            return;
        }
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
        return true;
    default:
        if (IS_ALPHA(ch) || IS_DIGIT(ch)) {
            return true;
        }
        return false; // Includes whitespace too, as it should've been skipped beforehand.
    }
}

static void lex_token(Lexer *lexer) {
    if (IS_EOF(lexer)) {
        append_token(&lexer->tokens, token_alloc_lexeme("", lexer->line, TOKEN_EOF));
        return;
    }
    if (!valid_syntax(PEEK(lexer))) {
        while (!valid_syntax(PEEK(lexer))) {
            ADVANCE(lexer);
        }
        append_error(lexer, "Unsupported syntax", lexer->line);
        return;
    }

    START_TOKEN(lexer);
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
    case '+': two_possibilities(lexer, TOKEN_PLUS, '=', TOKEN_PLUS_EQ); break;
    case '-': two_possibilities(lexer, TOKEN_MINUS, '=', TOKEN_MINUS_EQ); break;
    case '/': two_possibilities(lexer, TOKEN_SLASH, '=', TOKEN_SLASH_EQ); break;
    case '%': two_possibilities(lexer, TOKEN_MODULO, '=', TOKEN_MODULO_EQ); break;
    case '=': two_possibilities(lexer, TOKEN_EQ, '=', TOKEN_EQ_EQ); break;
    case '!': two_possibilities(lexer, TOKEN_BANG, '=', TOKEN_BANG_EQ); break;
    case '^': two_possibilities(lexer, TOKEN_CARET, '=', TOKEN_CARET_EQ); break;
    case '~': two_possibilities(lexer, TOKEN_TILDE, '=', TOKEN_TILDE_EQ); break;
    case '.': two_possibilities(lexer, TOKEN_DOT, '.', TOKEN_DOT_DOT); break;
    
    // Characters that have 2 possible continuation tokens + 1 default one.
    case '&': 
        three_possibilities(lexer, TOKEN_AMPER, '&', TOKEN_AMPER_AMPER, '=', TOKEN_AMPER_EQ); break;
    case '|': 
        three_possibilities(lexer, TOKEN_BAR, '|', TOKEN_BAR_BAR, '=', TOKEN_BAR_EQ); break;

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
            RETREAT(lexer); // So lex_number() fully handles the number (including edge cases).
            lex_number(lexer);
            return;
        }
        UNREACHABLE_ERROR();
    }
}

static void lex_until(Lexer *lexer, const char *end) {
    while (lexer->current <= end) {
        advance_whitespace(lexer);
        lex_token(lexer);
        advance_whitespace(lexer);

        Token lastToken = lexer->tokens.tokens[lexer->tokens.length - 1];
        if (TOKEN_IS_TYPE(lastToken, TOKEN_EOF)) {
            return;
        }
    }
}

bool lex(Lexer *lexer) {
    lex_until(lexer, lexer->source + lexer->sourceLength); // Until the "\0".
    if (lexer->program->hasErrored) {
        return false;
    }
    return true;
}
