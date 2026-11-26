# A compressed stream, addressed by its digest

Compression and hashing know nothing of each other, but both work on a stream of bytes. A
compressed layer is identified by the digest of its compressed form, in one expression, and
the data is never held between the two.

```scala fuel
compile
frame
  label A layer
  code
      import codepages.utf8Codepage, providers.javaBaseProvider, alphabets.hexLowerCase

      val layer = Chain(t"Elsinore. ".in[Data], t"A platform before the castle.".in[Data])
frame
  label Compressed
  highlight compress[Gzip]  a stage of the stream
  code
      import codepages.utf8Codepage, providers.javaBaseProvider, alphabets.hexLowerCase

      val layer = Chain(t"Elsinore. ".in[Data], t"A platform before the castle.".in[Data])
      val wire = layer.compress[Gzip]
frame
  label And named by what it holds
  highlight checksum[Sha2[256]]  hashed as it passes
  code
      import codepages.utf8Codepage, providers.javaBaseProvider, alphabets.hexLowerCase

      val layer = Chain(t"Elsinore. ".in[Data], t"A platform before the castle.".in[Data])
      val wire = layer.compress[Gzip]
      val name: Digest in Sha2[256] = wire.checksum[Sha2[256]]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(name.serialize[Hex])
```

[Hashing a stream](../../soundness/modules/hashing.md#hashing-a-stream) describes the digest,
and [composability](../../philosophy/composability.md) why no adapter stands between the two.
