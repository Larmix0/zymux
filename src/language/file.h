#ifndef FILE_H
#define FILE_H

/** Returns a boolean of whether or not the passed file path exists. */
bool file_exists(const char *path);

/** Returns an allocated string of the absolute path that the passed relative path refers to. */
char *get_absolute_path(const char *relativePath);

/** Returns the passed file's entire text as an allocated string. */
char *get_file_source(const char *fileName);

#endif
