#include <stdio.h>
#include <stdlib.h>

#include "char_buffer.h"
#include "errors.h"
#include "zymux_program.h"

/** 
 * iterates over some source and returns a string copy where every CRLF and CR turns into LF.
 * 
 * This is because different systems have different signals for a line break.
 * 
 * In windows, a line break is CRLF (\r\n).
 * In older versions of mac, a line break is CR (\r).
 * Although newer versions have (\n) which is also what linux uses. It's also Zymux defaults to.
 */
static char *alloc_fixed_source(char *source) {
    CharBuffer buffer = create_char_buffer();
    int idx = 0;
    while (source[idx] != '\0') {
        if (source[idx] != '\r') {
            buffer_append_char(&buffer, source[idx++]);
            continue;
        }

        idx++;
        if (source[idx] == '\n') {
            idx++;
        }
        buffer_append_char(&buffer, '\n');
    }
    return buffer.text;
}

/** 
 * Returns the text of the passed file as an allocated string.
 * All line breaks are written as "\n" in the returned string, no matter the system.
 */
char *alloc_source(const char *fileName) {
    FILE *file = fopen(fileName, "rb");
    if (file == NULL) {
        // TODO: perhaps change this into a user error, as we don't want to internally error
        // when the user misspells a file name, since internal errors are reserved for our mistakes
        // and not the ones the user makes.
        FILE_ERROR("Couldn't open file \"%s\".", fileName);
    }
    if (fseek(file, 0L, SEEK_END) != 0) {
        FILE_ERROR("Couldn't seek to the end of file \"%s\" to view its length.", fileName);
    }
    
    long fileLength = ftell(file);
    if (fileLength == -1) {
        FILE_ERROR("Failed to read length of file \"%s\".", fileName);
    }
    if (fseek(file, 0L, SEEK_SET) != 0) {
        FILE_ERROR("Couldn't seek to the beginning of file \"%s\" to read the source.", fileName);
    }

    char *source = ZMX_ARRAY_ALLOC(fileLength + 1, char);
    if (fread(source, sizeof(char), fileLength, file) != (size_t)fileLength) {
        FILE_ERROR("Failed to read the source of file \"%s\".", fileName);
    }
    if (fclose(file) != 0) {
        FILE_ERROR("Failed to close file \"%s\" after reading it.", fileName);
    }
    source[fileLength] = '\0';
    char *fixedSource = alloc_fixed_source(source);
    free(source);
    return fixedSource;
}
