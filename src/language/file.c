#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "allocator.h"
#include "char_buffer.h"
#include "program.h"
#include "report_error.h"

/** The different types of paths we expect to get. */
typedef enum {
    PATH_FILE,
    PATH_DIRECTORY,
    PATH_OTHER
} PathType;

/** Returns what path is. Like a file or directory for example. */
static PathType get_path_type(const char *path) {
    struct stat statBuffer;
    if (stat(path, &statBuffer) != 0) {
        FILE_ERROR("Couldn't read stat of '%s': %s (Errno %d).", path, strerror(errno), errno);
    }
    
    if (S_ISDIR(statBuffer.st_mode)) {
        return PATH_DIRECTORY;
    } else if (S_ISREG(statBuffer.st_mode)) {
        return PATH_FILE;
    }
    return PATH_OTHER;
}

/** 
 * iterates over some source and returns a string copy where every CRLF and CR turns into LF.
 * 
 * This is because different systems have different signals for a line break.
 * 
 * In windows, a line break is CRLF (\r\n).
 * In older versions of mac, a line break is CR (\r).
 * Although newer versions have (\n) which is also what linux uses. It's also Zymux defaults to.
 */
static char *alloc_fixed_source(const char *source) {
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
 * 
 * All line breaks are written as "\n" in the returned string, no matter the system.
 */
char *alloc_source(const char *path) {
    FILE *file = fopen(path, "rb");
    if (file == NULL) {
        FILE_ERROR("Couldn't open file '%s': %s (Errno %d).", path, strerror(errno), errno);
    }

    const PathType type = get_path_type(path);
    if (type == PATH_DIRECTORY) {
        FILE_ERROR("Expected file, but '%s' is a directory.", path);
    } else if (type == PATH_OTHER) {
        FILE_ERROR("Expected file, but '%s' isn't a file.", path);
    }

    if (fseek(file, 0L, SEEK_END) != 0) {
        FILE_ERROR("Couldn't seek to the end of file '%s' to view its length.", path);
    }
    
    const long fileLength = ftell(file);
    if (fileLength == -1) {
        FILE_ERROR("Failed to read length of file '%s'.", path);
    }
    if (fseek(file, 0L, SEEK_SET) != 0) {
        FILE_ERROR("Couldn't seek to the beginning of file '%s' to read the source.", path);
    }

    char *source = ARRAY_ALLOC(fileLength + 1, char);
    if (fread(source, sizeof(char), fileLength, file) != (size_t)fileLength) {
        FILE_ERROR("Failed to read the source of file '%s'.", path);
    }
    if (fclose(file) != 0) {
        FILE_ERROR("Failed to close file '%s' after reading it.", path);
    }
    source[fileLength] = '\0';
    char *fixedSource = alloc_fixed_source(source);
    free(source);
    return fixedSource;
}
