## Caching

### About

A [cache](https://en.wikipedia.org/wiki/Cache_(computing)) trades memory for time, and the
two shapes which that trade usually takes are provided.

An `LruCache` keeps the most recently used entries of a lookup by key, and evicts the least
recent when it exceeds its capacity. A `Cache` remembers a single value, with an optional
lifetime, and computes it again when it expires. That is the shape of a configuration which
is reloaded every minute, or a token which is refreshed on a schedule.

### On caching

Every cache which is written by hand is a mutable map, together with three decisions which
are made implicitly: when to evict, what to do when two threads want the same missing value,
and when a cached value is too old to trust. The map is easy. The decisions are where the
bugs are:

- With no eviction, the cache grows without bound.
- With no coordination, a missing value is computed many times at once.
- With no expiry, stale data is served.

Here the decisions are the type. An `LruCache` is the bounded, evicting kind. A `Cache` is
the expiring, coordinated kind, whose computation is guarded so that concurrent callers
share one result. The policy is stated where the cache is, and not at each read, which is
[declarative context](../philosophy/declarative-context.md).

```tabular
column  Aspect
column  `LruCache`
column  `Cache`
row  Holds  keyed entries, up to a capacity  one value
row  Created with  `LruCache[Key, Value](capacity)`  `Cache(lifetime)`, or `Cache[Value]()`
row  Read with  `cache(key)(computation)`  `cache.establish(computation)`
row  Discards  the least recently used entry  the value, once its lifetime has passed
row  Concurrent misses  each caller computes  one caller computes, the rest share its result
```

Everything comes from the `soundness` package. The second import lets the samples print
their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### An LRU cache

An `LruCache` holds keyed entries, up to its capacity. Applying it to a key and a
computation gives the cached value, when the key is present. Otherwise it runs the
computation, stores the result, and evicts the least recently used entry if the cache is now
too big.

Here the computation is a lookup which counts how often it runs:

```scala
var lookups = 0

def expensiveLookup(key: Int): Text =
  lookups += 1
  t"value $key"

val cache = LruCache[Int, Text](4)
```

The first use of a key computes and stores its value. The second gives the stored value,
with no computation:

```scala fuel
run
##
Out.println(cache(1)(expensiveLookup(1)))
Out.println(cache(1)(expensiveLookup(1)))
Out.println(lookups.show)
```

The computation is passed by name, so nothing is evaluated when the key is present. That is
the whole point of the shape: a cache whose value must be computed before it can be offered
saves nothing.

Using an entry, whether by reading or by writing, marks it as recent, so the entries which
survive are the ones which the program keeps coming back to. After four further keys, key
`1` has been evicted, and asking for it computes again:

```scala fuel
run
##
List(2, 3, 4, 5).each(key => cache(key)(expensiveLookup(key)))

Out.println(cache.contains(1).inspect)
Out.println(cache.contains(5).inspect)
```

`contains` asks whether a key is held, without computing anything, and without marking the
entry as recent. `remove` drops an entry. That is what invalidation needs, when the
underlying data has changed, and the cached value is known to be stale:

```scala fuel
run
##
cache.remove(5)
Out.println(cache.contains(5).inspect)
```

### An expiring value

A `Cache` remembers one value, with a lifetime after which it is computed again. The
lifetime is any [duration](time.md#durations-and-timespans), here a
[quantity](quantities.md) of seconds, with the import which lets a `Cache` read it. The type
of the cached value is stated in the expected type:

```scala
import durationInterfaces.aviationDuration

case class Configuration(port: Int)

var loads = 0

def loadConfiguration(): Configuration =
  loads += 1
  Configuration(8080)

val config: Cache[Configuration] = Cache(300.0*Second)
```

`establish` gives the current value. It computes the value only when none is held, or when
the one which is held has expired. Here the second call gives the same value, and will until
five minutes pass:

```scala fuel
run
##
Out.println(config.establish(loadConfiguration()).inspect)
Out.println(config.establish(loadConfiguration()).inspect)
Out.println(loads.show)
```

The computation runs under a lock. When the value is missing or expired, and several threads
ask at once, one computes, and the rest receive its result. That prevents the
[stampede](https://en.wikipedia.org/wiki/Cache_stampede) which a bare lazy value cannot.
[Concurrency](concurrency.md) describes the tasks which might be asking.

A `Cache` which is created without a lifetime, as `Cache[Configuration]()`, computes its
value once, and keeps it.

### Related

- [Bloom filters](bloom-filters.md) remember only whether something has been seen.
- [Filesystem](filesystem.md#content-addressed-storage) describes a cache on disk, keyed by
  the digest of its content.
- [Time](time.md) describes the durations which a lifetime is given in.
- [Stack traces](stack-traces.md) describes the codepoint which keys a cached value to its call site.
