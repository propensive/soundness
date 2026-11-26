# The format is a type

No codec is constructed, fed and finished. `compress` and `decompress` are methods on the data,
and the format is the type they are given.

```scala fuel
compile
frame
  label Gzip
  highlight compress[Gzip]  the format, as a type
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val line = t"Double, double toil and trouble; "
      val spell = (line + line + line + line).in[Data]
      val gzip = spell.compress[Gzip]
frame
  label Raw DEFLATE, without the framing
  highlight compress[Deflate]  another type, the same method
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val line = t"Double, double toil and trouble; "
      val spell = (line + line + line + line).in[Data]
      val gzip = spell.compress[Gzip]
      val deflate = spell.compress[Deflate]
frame
  label And back again
  highlight decompress[Deflate]  and its inverse
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val line = t"Double, double toil and trouble; "
      val spell = (line + line + line + line).in[Data]
      val gzip = spell.compress[Gzip]
      val deflate = spell.compress[Deflate]
      val restored = deflate.decompress[Deflate].utf8
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(spell.length.show)
Out.println(gzip.length.show)
Out.println(deflate.length.show)
Out.println(restored.keep(31))
```

[The formats](../../soundness/modules/compression.md#the-formats) lists all seven, and
[small APIs](../../philosophy/small-apis.md) says why they share two names.
