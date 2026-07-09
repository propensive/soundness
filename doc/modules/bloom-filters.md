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
