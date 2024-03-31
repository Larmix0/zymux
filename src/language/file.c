#include <stdio.h>
#include <stdlib.h>

#include "char_buffer.h"
#include "errors.h"
#include "zymux_program.h"

static char *fixed_source(char *original) {
    CharBuffer buffer = create_char_buffer();
    int idx = 0;
    while (original[idx] != '\0') {
        if (original[idx] != '\r') {
            buffer_append_char(&buffer, original[idx++]);
            continue;
        }

        idx++;
        if (original[idx] == '\n') {
            idx++;
        }
        buffer_append_char(&buffer, '\n');
    }
    return buffer.text; // Pass responsibility of freeing buffer's text to user.
}

char *alloc_source(const char *fileName) {
    FILE *file = fopen(fileName, "rb");
    if (file == NULL) {
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
    char *fixedSource = fixed_source(source);
    free(source);
    return fixedSource;
}
