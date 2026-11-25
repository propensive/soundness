# A digest which does not depend on the chunks

Data too large to hold is hashed as it passes. `checksum` accumulates over the chunks of a
stream, so the result is the digest of the whole, whatever sizes the chunks arrive in.

```scala fuel
compile
frame
  label A value, hashed whole
  code
      import providers.javaBaseProvider, codepages.utf8Codepage

      val whole = t"Hello world".digest[Sha2[256]]
frame
  label The same bytes, in three chunks
  highlight checksum[Sha2[256]]  accumulated over the chunks
  code
      import providers.javaBaseProvider, codepages.utf8Codepage

      val whole = t"Hello world".digest[Sha2[256]]
      val chunks = Chain(t"Hello ".in[Data], t"wor".in[Data], t"ld".in[Data])
      val streamed = chunks.checksum[Sha2[256]]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println((whole == streamed).inspect)
```

[Hashing a stream](../../soundness/modules/hashing.md#hashing-a-stream) applies the same method
to a file and to a directory.
