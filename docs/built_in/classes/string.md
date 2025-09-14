# String object documentation

```
String(length):
    A string is a series of characters.

    int length():
        Returns the number of characters in the string.

    string lower():
        Returns a version of the string where all of its characters are lowercase.
    
    string upper():
        Returns a version of the string where all of its characters are uppercase.

    string isnumeric():
        Returns whether or not the string's text represents a number which can be safely converted
        to an int/float.
    
    int charcode():
        Returns the ASCII integer corresponding to the passed string, which is expected to be
        a single character.
        Errors if the string was empty or had more than one character inside it.
```
