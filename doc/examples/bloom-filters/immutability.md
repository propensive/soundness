# Adding to a filter gives another filter

A frozen Bloom filter never changes. `+` gives a new filter which holds one element more, and
the filter it was made from still answers as it did before.

```scala fuel
compile
frame
  label A filter with one element
  code
      import providers.soundnessProvider
      import Blake3.hash

      val prologue = BloomFilter[Text](100, 0.001) + t"Verona"
frame
  label Another, with two more
  highlight ++  a new filter, and one copy of the bits
  code
      import providers.soundnessProvider
      import Blake3.hash

      val prologue = BloomFilter[Text](100, 0.001) + t"Verona"
      val play = prologue ++ List(t"Mantua", t"Padua")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(play.hits(t"Mantua").inspect)
Out.println(prologue.hits(t"Mantua").inspect)
Out.println(prologue.hits(t"Verona").inspect)
```

Mantua is in the second filter and not in the first.
[Immutability](../../soundness/modules/bloom-filters.md#immutability) lists what each
operation copies.
