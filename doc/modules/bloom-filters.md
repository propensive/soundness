## Bloom Filters

### About

A [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter) answers "have I seen this before?" in
constant space, at the price of occasional false positives: a *no* is always right, and a *yes* is
right with a probability the filter's size controls. Soundness constructs one from the two numbers
that actually matter — roughly how many elements it will hold, and the false-positive rate to
tolerate — and derives the bit-array size and hash count from them.

Any [digestible](hashing.md) value can be added, the hash algorithm is chosen as a type, and the
filter is immutable: adding an element yields a new filter, so filters are shared and accumulated
as safely as any other value.

### On probabilistic membership

Storing every element of a large set just to answer membership is often a waste: a cache of
already-fetched URLs, a guard against re-sending notifications, a pre-check before an expensive
lookup all survive the occasional false *yes*. A Bloom filter hashes each element to a handful of
bit positions and remembers only the bits — a few bits per element regardless of the elements'
size — but tuning one means choosing a bit-array size and a number of hash functions, a formula
most code copies from a textbook, sometimes wrongly.

The filter's error rate is fixed by its construction rather than checked at each use, which is [safety by construction](../philosophy/safety-by-construction.md).

Soundness asks instead for the intent — expected size and target error rate — and computes the
parameters. The error rate is a [bounded number](numbers.md), so a rate outside `[0, 1]` does not
compile. Everything comes from the `soundness` package, with a hash provider in scope:

```scala
import soundness.*
import providers.soundnessProvider
import Blake3.hash
```

### Creating and adding

A filter is created for an element type, an expected size, and an error rate, and grows with `+`
for one element or `++` for many:

```scala
val bloom = BloomFilter[Text](100, 0.001) + t"Hello world"

val fuller = bloom ++ List(t"hello", t"world")
```

Anything with a `Digestible` instance can be an element — text, numbers, case classes — since
elements enter the filter through the same hashing machinery used everywhere else.

### Querying

`hits` asks whether an element may be present. A `false` is definitive — the element was never
added; a `true` means *probably*, with the false-positive probability the filter was built for:

```scala
fuller.hits(t"hello")     // true — it was added
fuller.hits(t"goodbye")   // false, almost certainly
```

The asymmetry is the contract: a Bloom filter suits exactly those places where a false *yes* costs
a redundant check, and a false *no* would be a bug.

### Choosing the parameters

The two numbers given at construction are the expected number of elements and the tolerable
false-positive rate, and from them the bit-array size and the number of hash functions follow by
the standard formulae. Stating the intent rather than the parameters means the two cannot
disagree — a filter sized for a hundred elements at one in a thousand is exactly that.

The consequence of getting the *expected size* wrong is worth knowing: a filter given far more
elements than it was built for degrades gracefully in the sense that it keeps working, but its
false-positive rate rises above the one requested. It is a promise about a filter used as
intended, not a guarantee that holds however it is filled.

### Where the bits come from

The hash algorithm is part of the filter's type, and the bit positions are derived from a single
digest, extended by rehashing where more bits are needed than one digest provides. That means the
algorithm in scope decides the filter's behavior, and two filters over the same elements agree
only if they agree on the algorithm.

Because elements enter through the ordinary [hashing](hashing.md) machinery, a case class is
usable as an element with no preparation, and a filter over a structured key needs no
serialization step written for it.

### Immutability

`+` and `++` return new filters rather than mutating in place, so a filter is safe to share across
threads and to hold in a data structure. Building one from a large collection with `++` is a
single pass, rather than the sequence of copies that repeated `+` would suggest.
