# A compressor is a stage of a stream

The names which compress a value also attach to a stream of chunks, where the data is
transformed as it flows. The two forms agree byte for byte, so what one compresses the other
decompresses.

```scala fuel
compile
frame
  label Chunks, as they might arrive
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val chunks = Chain(t"When shall we ".in[Data], t"three meet again?".in[Data])
frame
  label A stage which compresses them
  highlight compress[Gzip]  chunk by chunk
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val chunks = Chain(t"When shall we ".in[Data], t"three meet again?".in[Data])
      val wire = chunks.compress[Gzip]
frame
  label The whole of it, decompressed as one value
  highlight decompress[Gzip]  the whole-value form
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val chunks = Chain(t"When shall we ".in[Data], t"three meet again?".in[Data])
      val wire = chunks.compress[Gzip]
      val question = wire.read[Data].decompress[Gzip].utf8
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(question)
```

[Compressing a stream](../../soundness/modules/compression.md#compressing-a-stream) places the
same stages on either side of a file.
