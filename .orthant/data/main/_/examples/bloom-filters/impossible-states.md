# An error rate is a probability

A Bloom filter is made from the number of elements it expects and the rate of false positives
it may give. The rate is a number between zero and one, and its type says so, so a rate of
one and a half is not a filter which misbehaves at runtime but a line which does not compile.

```scala fuel
compile
frame
  label A rate above one
  errors show
  code
      import providers.soundnessProvider
      import Blake3.hash

      val seen = BloomFilter[Text](100, 1.5)
frame
  label One in a thousand
  highlight 0.001  within `0.0 ~ 1.0`
  code
      import providers.soundnessProvider
      import Blake3.hash

      val seen = BloomFilter[Text](100, 0.001)
##
```

[Creating and adding](../../soundness/modules/bloom-filters.md#creating-and-adding) describes
the two parameters, and [impossible states](../../philosophy/impossible-states.md) the idea of
a type with no wrong values.
