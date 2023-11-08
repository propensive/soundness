### LRU Caches

An LRU cache is for storing a limited number of values which are expensive to compute. Values are added to the
cache when they are computed, and can be accessed in constant time by referencing them with a key. If the cache
already contains its limit of values, an existing value must be "evicted" to make space for the new value. The
value chosen is whichever has been in the cache the longest time without being accessed; in other words, the
_least-recently-used_ value.

### `LruCache`

To construct an LRU cache, specify a key and value type, and maximum size to the `LruCache` constructor:
```scala
import camouflage.*
import galilei.*
val cache = LruCache[PathName, Text](100)
```

This `LruCache` will behave like a `Map[PathName, Text]`: its values are `Text`s, indexed by `PathName`s.

A single `apply` method is used to both retrieve and update a value in the cache, much like `getOrElseUpdate` in
a `Map`. For `LruCache`, the parameters are curried, since the second _ought_ to represent a costly
computation—since that is the purpose of caching.

Here is an example of using the `LruCache` to cache reading two files from disk:
```scala
import turbulence.*

cache(p"intro.txt"):
  (dataDir / p"intro.txt").readAs[Text]

cache(p"chapter1.txt"):
  (dataDir / p"chapter1.txt").readAs[Text]
```

A subsequent call to,
```scala
cache(p"chapter1.txt"):
  (dataDir / p"chapter1.txt").readAs[Text]
```
would retrieve the stored `Text` value from the cache, and the code which reads the file from disk would not be
executed.

This is true _unless_ the cache has filled up in the meantime and the text of `chapter1.txt` has been evicted.
And for an `LruCache` of size `100`, `chapter1.txt` would only be evicted if 100 distinct additions or accesses
of other keys were made since the addition or last access of `chapter1.txt`.

Here is a complete example of a tiny `LruCache`:
```scala
import anticipation.*, gossamer.*, turbulence.*
val cache: LruCache[Int, Text] = LruCache(4)

Out.println(cache(1)(t"one"))
Out.println(cache(2)(t"two"))
Out.println(cache(3)(t"three"))
Out.println(cache(4)(t"four"))
Out.println(cache(1)(t"uno"))  // (1)
Out.println(cache(5)(t"five")) // (2)
Out.println(cache(1)(t"ein"))  // (3)
Out.println(cache(2)(t"dos"))
```

The output would be: `one`, `two`, `three`, `four`, `one`, `five`, `one`, `dos`. Note that values retrieved on
the line marked _1_ is not `uno`, since at this point, the cache already contains the value `one` and the second
parameter (which produces `uno`) is not evaluated.

On the line marked _2_, a fifth value, `five`, is inserted. This would exceed the maximum size of the cache, so
one element must be evicted. That value is `two` because it is the least-recently-used value. Were it not for
the previous line (_1_), `one` would be the least-recently-used value, and the line marked _3_ would result in
the evaluation of `ein` and its insertion into the cache; but since `one` remains in the cache at the same
index, it is retrieved instead.