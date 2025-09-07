# File object documentation

```
File(path, mode):
    Creates a file object that represents and interacts with the passed path if it exists.
    Allows or disallows certain operations depending on the mode.
    Interacts with the file as a binary if the mode passed is a binary one.

    string 'path':
        The constructed string path used to originally create the file.

    string 'mode':
        The mode which the file is opened in and is operating on.

        Available modes are:
            'r', 'a', 'w', 'rb', 'ab', 'wb', 'r+', 'a+', 'w+', 'rb+', 'ab+', and 'wb+'.

    string read():
        Returns a string of the entire file's contents read.
        Can cause an error if it failed to read, or the file's mode doesn't allow reading.

    null write(text):
        Attempts to write the passed text to the opened file depending on the mode.

        If it's writing mode, then it overwrites the whole file,
        if it's in appending mode then it appends it to the end of the file,
        otherwise it raises an error.

        'text': the text to write to the file.

    null close():
        Closes the file and its resources.

        Would only error if the closing operation went internally wrong.
        Closing a file means it's no longer allowed to perform any operations to it or else it will
        throw an error.
```
