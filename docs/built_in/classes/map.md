# Map object documentation

```
Map():
    A map of key value pairs which uses hashing for optimized performance.

    any find(key, fallback=null):
        Using a hashable key, it attempts to find an entry from the map.

        If the key is found, then it returns the corresponding value. Otherwise returns
        whatever fallback was (null by default).

    bool delete(key):
        Using a hashable key, it attempts to delete an entry from the map.
        Returns a boolean of whether or not there was an entry that was removed.
```
