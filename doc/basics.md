### About Bloom Filters

A Bloom filter provides a way to test if a value is _probably_ in a set, or if
it is _certainly_ not in the set. That is, false positives for set membership
are permitted, while false negatives are not. The memory required by a Bloom
filter is dependent on the number of elements it should hold, and the
probability of false positives when testing for set membership, and these
parameters must be specified when the Bloom filter is created.

As well as determining the amount of memory the Bloom filter should use, the
approximate number of elements and the acceptable false-positive rate determine
the number of different hashes that will be used for each operation, with
optimal values chosen for each, transparently.

The Bloom filter only has two core operations: adding a value to the set, and
testing if a value is contained within the set. Since the Bloom filter does not
actually store any values, there are no methods for retrieval.

### Constructing a Bloom Filter

A new Bloom filter can be constructed with, for example:
```scala
import gastronomy.hashFunctions.crc32
val bloom = BloomFilter[Element](1000, 0.01)
```

This creates a new `BloomFilter` instance for storing elements of type
`Element`, optimized for 1000 elements, with a target error rate of 1%
(`0.01`). Additionally, the presence of the contextual value,
`hashFunctions.crc32` menas that the CRC32 hash function, defined in
[Gastronomy](https://github.com/propensive/gastronomy/), will be used to
calculate the hashes for addition and membership checks.

For creation, the Bloom filter additionally requires that its element types are
digestible, that is, a contextual `Digestible` instance exists. Gastronomy
provides `Digestible` instances for primitive types and will derive instances for
product and coproduct types.

A newly-created `BloomFilter` is empty, but an element can be added with the
`+` operator, or multiple elements with the `++` operator. Since `BloomFilter`s
are immutable, these will construct new `BloomFilter` instances.

### Using a Bloom filter

`BloomFilter` also provides the method, `mayContain`, which takes an instance of
the Bloom filter's type, and returns a `Boolean`. The interpretation of the
result should not be mistaken: `false` means that the value is guaranteed not
to be a member of the set represented by this Bloom filter, while `true` means
that the value is _probably_ a member of the set, but may not be.

For the Bloom filter above, constructed for approximately 1000 elements with an
error rate of 1%, if it has, indeed, had 1000 elements added to it, then there
is an estimated 1% chance that the `mayContain` method will return `true` for
an element which has not been added. That false-positive probably will increase
if significantly more elements are added to the Bloom filter, and would be
smaller had significantly fewer elements been added.


