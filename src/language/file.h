#ifndef FILE_H
#define FILE_H

typedef struct ZmxProgram ZmxProgram;

/** Returns a boolean of whether or not the passed file path exists. */
bool file_exists(const char *path);

/** 
 * Returns an allocated string of the absolute path that the passed relative path refers to.
 * 
 * Will return NULL if it fails (especially if the file doesn't exist as an absolute path).
 */
char *get_absolute_path(const char *relativePath);

/** Returns an allocated string of the directory at the program's current file + the passed path. */
char *get_relative_path(ZmxProgram *program, const char *path);

/** Returns the passed file's entire text as an allocated string. */
char *get_file_source(const char *fileName);

#endif
