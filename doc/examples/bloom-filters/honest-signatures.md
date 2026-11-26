# A fast hash needs a permit

MurmurHash3 suits a Bloom filter, which has a distribution to spread and no adversary to
resist. It is not a cryptographic hash, though, and a filter does not use it until the code
around it has said that it knows.

```scala fuel
compile
frame
  label A hash which is not cryptographic
  errors show
  code
      import providers.soundnessProvider

      val seen = BloomFilter[Text](10_000, 0.01)[Murmur3]
frame
  label The permit, in scope
  highlight permitNonCryptographicHashes  no adversary is being resisted
  code
      import providers.soundnessProvider
      import cryptoPermits.permitNonCryptographicHashes

      val seen = BloomFilter[Text](10_000, 0.01)[Murmur3]
##
```

[Where the bits come from](../../soundness/modules/bloom-filters.md#where-the-bits-come-from)
has the reasons to choose it, and
[honest signatures](../../philosophy/honest-signatures.md) the reasons to ask.
