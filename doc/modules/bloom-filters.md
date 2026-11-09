## Bloom Filters

### About

A [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter) answers the question "have I seen
this before?" in constant space, at the price of an occasional false positive. A *no* is always
right. A *yes* is right with a probability which the filter's size controls.

A filter is constructed from the two numbers which actually matter: roughly how many elements
it will hold, and the rate of false positives to tolerate. The size of the bit array and the
number of hashes are worked out from them.

Any [digestible](hashing.md#hashing-your-own-types) value can be added, and the hash algorithm
is chosen as a type. A filter is [immutable]: adding an element gives a new filter, so filters
are shared and accumulated as safely as any other value.

### On probabilistic membership

Storing every element of a large set, just to answer whether something is a member, is often a
waste. A cache of URLs which have already been fetched, a guard against sending a notification
twice, and a check before an expensive lookup all survive the occasional false *yes*.

A Bloom filter hashes each element to a handful of positions in an array of bits, and
remembers only the bits. That is a few bits for each element, whatever the size of the
elements. But tuning a filter means choosing the size of the bit array and the number of hash
functions, by a formula which most code copies from a textbook, sometimes wrongly.

Here the filter is asked for by intent: the expected size, and the target error rate. The
parameters are computed. The error rate is a [bounded number](numbers.md#bounded-numbers), so
a rate outside `[0, 1]` does not compile. The filter's error rate is fixed by how it is
constructed, and is not checked at each use, which is
[safety by construction](../philosophy/safety-by-construction.md).

Everything comes from the `soundness` package. A provider of hash algorithms is in scope, and
the last import lets the samples print their results:

```scala
import soundness.*
import providers.soundnessProvider
import Blake3.hash
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Creating and adding

A filter is created for a type of element, an expected size and an error rate. `+` adds one
element, and `++` adds many:

```scala
val bloom = BloomFilter[Text](100, 0.001) + t"Hello world"

val fuller = bloom ++ List(t"hello", t"world")
```

Anything with a `Digestible` instance can be an element, including text, numbers and case
classes, since elements enter the filter through the same hashing as everything else.

An error rate which is not a probability does not compile:

```scala fuel
compile
errors show
##
BloomFilter[Text](100, 1.5)
```

### Querying

`hits` asks whether an element may be present. A `false` is definitive: the element was never
added. A `true` means *probably*, with the probability of a false positive which the filter
was built for:

```scala fuel
run
##
Out.println(fuller.hits(t"hello").inspect)
Out.println(fuller.hits(t"goodbye").inspect)
Out.println(bloom.hits(t"hello").inspect)
```

The last line shows that `bloom` itself was not changed when `fuller` was made from it.

The asymmetry is the contract. A Bloom filter suits exactly those places where a false *yes*
costs a redundant check, and a false *no* would be a bug.

### Choosing the parameters

The two numbers given at construction are the expected number of elements and the tolerable
rate of false positives. The size of the bit array and the number of hash functions follow
from them, by the standard formulae. Stating the intent, and not the parameters, means that
the two cannot disagree: a filter sized for a hundred elements at one in a thousand is exactly
that.

It is worth knowing what happens when the *expected size* is wrong. A filter which is given
far more elements than it was built for keeps working, but its rate of false positives rises
above the rate requested. The rate is a promise about a filter used as intended, not a
guarantee which holds however the filter is filled.

### Where the bits come from

The hash algorithm is part of the filter's type, and every bit position is derived from a
single digest of the element. The digest's first two 64-bit words, `h₁` and `h₂`, give
position *i* as `h₁ + i·h₂`, modulo the size of the bit array. This is the double-hashing
scheme which Guava also uses, and it costs one digest for each element, however many positions
are needed. An algorithm whose digest is shorter than sixteen bytes, such as a CRC, is applied
again under a counter until enough bytes have been gathered.

The algorithm in scope therefore decides the filter's behavior, and two filters over the same
elements agree only if they agree on the algorithm.

A Bloom filter has no adversary to resist, only a distribution to spread, so the cryptographic
BLAKE3 is more hash than it needs. `Murmur3`, the 128-bit MurmurHash3, costs about a fifth as
much for each element, and is the algorithm which Guava's filters use. It is chosen by naming
it, behind the same [permit](hashing.md#fast-hashing) as the checksums:

```scala
import cryptoPermits.permitNonCryptographicHashes

val fast = BloomFilter[Text](10_000, 0.01)[Murmur3]
```

### Filling a filter in place

A filter fresh from `BloomFilter[Text](n, p)` is *exclusive*. It belongs to the code which
made it, and that code fills it in place, with `add` and `addAll`, with no copying at all, and
then freezes it:

```scala
val filter = BloomFilter[Text](10_000, 0.01)
filter.add(t"first")
filter.addAll(List(t"second", t"third"))
val frozen = BloomFilter.freeze(filter)
```

```scala fuel
run
##
Out.println(frozen.hits(t"second").inspect)
Out.println(frozen.hits(t"fourth").inspect)
```

`freeze` consumes the exclusive filter, and the compiler's separation checking rejects any
later use of `filter`. What it gives is the frozen form, which may be shared across threads,
held in a data structure and queried from anywhere, but never written to. This is the same
discipline as freezing an array, and it is what makes the immutability below free, not copied.

### Immutability

`+` and `++` give new filters, and do not change the original, at the price of one copy of
the bits each. Building a filter from a large collection with `++` is a single pass and a
single copy, not the sequence of copies which repeated `+` would make. Filling an exclusive
filter with `addAll`, before freezing it, makes no copy at all.

The operations on a filter, side by side:

```tabular
column  Operation
column  Applies to
column  Yields
column  Copies the bits
row  `filter + element`  any filter  a new frozen filter  once
row  `filter ++ elements`  any filter  a new frozen filter  once
row  `filter.add(element)`  an exclusive filter  nothing; fills it in place  never
row  `filter.addAll(elements)`  an exclusive filter  nothing; fills it in place  never
row  `BloomFilter.freeze(filter)`  an exclusive filter, consumed  the frozen filter  never
row  `filter.hits(element)`  any filter  `false` if absent, `true` if probably present  never
```

[Caching](caching.md) covers the case where the answer itself must be remembered, not only
whether it has been seen.
