# The hash behind a filter

Every bit of a Bloom filter comes from a digest of an element, and the algorithm which makes
the digest is chosen by the context. The algorithm is also part of the filter's type, so two
filters made under different choices are not the same kind of value.

```scala fuel
compile
alternative BLAKE3
  highlight Blake3.hash  a cryptographic hash
  code
      import providers.soundnessProvider
      import Blake3.hash

      val seen = BloomFilter[Text](10_000, 0.01)
      val cast = seen ++ List(t"Viola", t"Feste")
alternative MurmurHash3
  highlight Murmur3.hash  about a fifth of the work
  highlight permitNonCryptographicHashes  it is not a cryptographic hash
  code
      import providers.soundnessProvider
      import Murmur3.hash
      import cryptoPermits.permitNonCryptographicHashes

      val seen = BloomFilter[Text](10_000, 0.01)
      val cast = seen ++ List(t"Viola", t"Feste")
##
```

[Where the bits come from](../../soundness/modules/bloom-filters.md#where-the-bits-come-from)
compares the costs, and [declarative context](../../philosophy/declarative-context.md) explains
the import.
