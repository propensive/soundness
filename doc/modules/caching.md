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

A cache that names its expiry in its type is a [declarative context](../philosophy/declarative-context.md): the policy is stated where the cache is, not at each read.

Soundness makes the decisions the type: an `LruCache` is the bounded, evicting kind, and a `Cache`
is the expiring, coordinated kind, its computation guarded so concurrent callers share one result.
Everything comes from the `soundness` package:

```scala
import soundness.*
```

### An LRU cache

An `LruCache` holds up to its capacity of keyed entries. Applying it with a key and a computation
returns the cached value when the key is present, and otherwise runs the computation, stores the
result, and evicts the least recently used entry if the cache is now too big. Here the
computation is a lookup that counts how often it runs:

```scala
var lookups = 0

def expensiveLookup(key: Int): Text =
  lookups += 1
  t"value $key"

val cache = LruCache[Int, Text](4)

cache(1)(expensiveLookup(1))   // computes and stores
cache(1)(expensiveLookup(1))   // returns the stored value; no computation
lookups                        // 1
```

Using an entry — reading or writing — marks it as recent, so the entries that survive are the ones
the program keeps coming back to: after four further keys, key `1` has been evicted, and asking
for it computes again.

`contains` asks whether a key is held without computing anything and without marking it recent,
and `remove` drops an entry — which is what an invalidation needs when the underlying data has
changed and the cached value is known to be stale:

```scala
cache.contains(1)   // true
cache.remove(1)
cache.contains(1)   // false
```

The computation is passed by name, so nothing is evaluated on a hit. That is the whole point of
the shape: a cache whose value must be computed before it can be offered saves nothing.

### An expiring value

A `Cache` memoizes one value, with a lifetime after which it is recomputed. The lifetime is any
[duration](time.md) — a quantity of seconds, here, with the interface that lets a `Cache` read it
imported — and the cached type is stated in the expected type. `establish` returns the current
value, computing it only when none is held or the held one has expired:

```scala
import durationInterfaces.aviationDuration

case class Configuration(port: Int)

var loads = 0

def loadConfiguration(): Configuration =
  loads += 1
  Configuration(8080)

val config: Cache[Configuration] = Cache(5.0*Minute)

config.establish(loadConfiguration())   // loads now
config.establish(loadConfiguration())   // the same value, until five minutes pass
loads                                   // 1
```

The computation runs under a lock, so when the value is missing or expired and several threads ask
at once, one computes and the rest receive its result — the
[stampede](https://en.wikipedia.org/wiki/Cache_stampede) a bare lazy value cannot prevent. A
`Cache` created without a lifetime, `Cache[Configuration]()`, computes its value once and keeps
it.
