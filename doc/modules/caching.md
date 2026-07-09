## Caching

### About

A [cache](https://en.wikipedia.org/wiki/Cache_(computing)) trades memory for time, and Soundness
provides the two shapes the trade usually takes. An `LruCache` keeps the most recently used entries
of a keyed lookup, evicting the least recent when it exceeds its capacity. A `Cache` memoizes a
single value with an optional lifetime, recomputing it when it expires — the shape of a
configuration reloaded every minute or a token refreshed on schedule.

### On caching

Every hand-rolled cache is a mutable map plus three decisions made implicitly: when to evict, what
to do when two threads want the same missing value, and when a cached value is too old to trust.
The map is easy; the decisions are where the bugs are — unbounded growth from no eviction, a
stampede of recomputation from no coordination, stale data from no expiry.

Soundness makes the decisions the type: an `LruCache` is the bounded, evicting kind, and a `Cache`
is the expiring, coordinated kind, its computation guarded so concurrent callers share one result.
Everything comes from the `soundness` package:

```scala
import soundness.*
```

### An LRU cache

An `LruCache` holds up to its capacity of keyed entries. Applying it with a key and a computation
returns the cached value when the key is present, and otherwise runs the computation, stores the
result, and evicts the least recently used entry if the cache is now too big:

```scala
val cache = LruCache[Int, Text](4)

cache(1)(expensiveLookup(1))   // computes and stores
cache(1)(expensiveLookup(1))   // returns the stored value; no computation
```

Using an entry — reading or writing — marks it as recent, so the entries that survive are the ones
the program keeps coming back to.

### An expiring value

A `Cache` memoizes one value, with a lifetime after which it is recomputed. `establish` returns the
current value, computing it only when none is held or the held one has expired:

```scala
val config = Cache[Configuration](5.0*Minute)

config.establish(loadConfiguration())   // loads now
config.establish(loadConfiguration())   // the same value, until five minutes pass
```

The computation runs under a lock, so when the value is missing or expired and several threads ask
at once, one computes and the rest receive its result — the
[stampede](https://en.wikipedia.org/wiki/Cache_stampede) a bare lazy value cannot prevent. A
`Cache` created without a lifetime computes its value once and keeps it.
