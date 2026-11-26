# Many elements, from any collection

`++` adds every element of a collection to a filter, and does not care which collection it
is. A list, a set and a lazy chain are added with the same operator, in a single pass and a
single copy of the bits.

```scala fuel
compile
frame
  label From a list
  code
      import providers.soundnessProvider
      import Blake3.hash

      val empty = BloomFilter[Text](100, 0.001)
      val lovers = empty ++ List(t"Hermia", t"Lysander")
frame
  label From a set
  highlight Set  the same `++`
  code
      import providers.soundnessProvider
      import Blake3.hash

      val empty = BloomFilter[Text](100, 0.001)
      val lovers = empty ++ List(t"Hermia", t"Lysander")
      val fairies = lovers ++ Set(t"Oberon", t"Titania")
frame
  label From a chain
  highlight Chain  and again
  code
      import providers.soundnessProvider
      import Blake3.hash

      val empty = BloomFilter[Text](100, 0.001)
      val lovers = empty ++ List(t"Hermia", t"Lysander")
      val fairies = lovers ++ Set(t"Oberon", t"Titania")
      val everyone = fairies ++ Chain(t"Bottom", t"Quince")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(everyone.hits(t"Titania").inspect)
Out.println(everyone.hits(t"Quince").inspect)
Out.println(lovers.hits(t"Quince").inspect)
```

[Creating and adding](../../soundness/modules/bloom-filters.md#creating-and-adding) has the
operators, and [small APIs](../../philosophy/small-apis.md) the reason there is one.
