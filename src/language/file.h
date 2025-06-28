#ifndef FILE_H
#define FILE_H

typedef struct ZmxProgram ZmxProgram;

/** Represents all the possible file mode permissions for a file. */
typedef enum {
    FILE_READ_ONLY, /** Read only modes, such as "r" and "rb" */
    FILE_WRITE_ONLY, /** Write only modes, such as "w" "a", "wb", etc.  */
    FILE_READ_AND_WRITE, /** Read and write, any mode that ends with "+" like "r+" and "w+". */
    FILE_INVALID /** Not a valid file mode, like "xyz", or anything else that's not valid. */
} FileMode;

/** Returns a boolean of whether or not the passed file path exists. */
bool file_exists(const char *path);

/** Returns the size of the passed file. */
long get_file_size(FILE *file, const char *path);

/** 
 * Returns an allocated string of the absolute path that the passed relative path refers to.
 * 
 * Will return NULL if it fails (especially if the file doesn't exist as an absolute path).
 */
char *alloc_absolute_path(const char *relativePath);

/** Returns an allocated string of the directory at the program's current file + the passed path. */
char *alloc_relative_path(ZmxProgram *program, const char *path);

/** Returns a file mode parsed from the passed string. Can be an invalid mode. */
FileMode parse_file_mode(char *modeStr, const u32 length);

/** Returns the passed file's entire text as an allocated string. */
char *alloc_file_source(const char *fileName);

#endif
