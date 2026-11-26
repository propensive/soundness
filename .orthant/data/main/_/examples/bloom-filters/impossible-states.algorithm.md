# Two filters, two hashes

A filter's bits mean nothing without the hash which set them, so the hash is part of the
filter's type. A filter made with one algorithm cannot be given where a filter of another is
expected, and a query can never be answered with the wrong hash.

```scala fuel
compile
frame
  label A BLAKE3 filter, where MurmurHash3 is expected
  errors show
  code
      import providers.soundnessProvider
      import cryptoPermits.permitNonCryptographicHashes

      def known(seen: BloomFilter[Text, Murmur3], name: Text): Boolean = seen.hits(name)

      val cast = BloomFilter[Text](100, 0.01)[Blake3] + t"Puck"
      val found = known(cast, t"Puck")
frame
  label The algorithm which the type names
  highlight [Murmur3]  the same algorithm as the parameter
    occurrence 1
  code
      import providers.soundnessProvider
      import cryptoPermits.permitNonCryptographicHashes

      def known(seen: BloomFilter[Text, Murmur3], name: Text): Boolean = seen.hits(name)

      val cast = BloomFilter[Text](100, 0.01)[Murmur3] + t"Puck"
      val found = known(cast, t"Puck")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(found.inspect)
```

[Where the bits come from](../../soundness/modules/bloom-filters.md#where-the-bits-come-from)
explains why two filters agree only if their algorithms do.
