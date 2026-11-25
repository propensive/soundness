# Compression, as a step between text and bytes

Text becomes bytes under an encoding, bytes are compressed under a format, and each step
names what it wants as a type. The way back is the same steps in the other order.

```scala fuel
compile
frame
  label Text, as bytes
  highlight in[Data]  under the encoding in scope
  code
      import codepages.utf8Codepage

      val line = t"To be, or not to be, that is the question"
      val bytes = line.in[Data]
frame
  label Compressed
  highlight compress[Gzip]  the format is a type
  code
      import codepages.utf8Codepage

      val line = t"To be, or not to be, that is the question"
      val bytes = line.in[Data].compress[Gzip]
frame
  label And back again
  highlight decompress[Gzip].utf8  the same steps, reversed
  code
      import codepages.utf8Codepage

      val line = t"To be, or not to be, that is the question"
      val bytes = line.in[Data].compress[Gzip]
      val again = bytes.decompress[Gzip].utf8
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(again)
```

[Compression](../../soundness/modules/streams.md#compression) shows the same in a pipeline,
and [the tutorial on compression](../../soundness/modules/compression.md) has the formats.
