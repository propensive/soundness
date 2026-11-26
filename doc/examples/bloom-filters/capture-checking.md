# A filter is filled by one owner, then frozen

A new Bloom filter is exclusive to the code which made it, which fills it in place. `freeze`
consumes it and gives back a filter which may be shared, without copying a bit. Separation
checking makes the old name unusable from that point, so nothing can write to a filter which
others are reading.

```scala fuel
compile
feature experimental.captureChecking
feature experimental.separationChecking
frame
  label An element added after the filter was frozen
  errors show
  highlight fairies.add
    occurrence 2
  code
      import providers.soundnessProvider, Blake3.hash

      def cast() =
        val fairies = BloomFilter[Text](10_000, 0.01)
        fairies.add(t"Oberon")
        val frozen = BloomFilter.freeze(fairies)
        fairies.add(t"Titania")
        frozen
frame
  label Every element added first
  highlight BloomFilter.freeze(fairies)
  code
      import providers.soundnessProvider, Blake3.hash

      def cast() =
        val fairies = BloomFilter[Text](10_000, 0.01)
        fairies.add(t"Oberon")
        fairies.add(t"Titania")
        BloomFilter.freeze(fairies)
##
```

[Filling a filter in place](../../soundness/modules/bloom-filters.md#filling-a-filter-in-place)
describes the exclusive and the frozen forms.
