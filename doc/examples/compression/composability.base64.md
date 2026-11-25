# Compressed bytes, ready for a line of text

Compression gives bytes and a base encoding takes them. Neither library knows the other, and a
value small enough to paste into a configuration file is one expression long.

```scala fuel
compile
frame
  label A long speech
  code
      import codepages.utf8Codepage, alphabets.base64Standard

      val line = t"Tomorrow, and tomorrow, and tomorrow, "
      val speech = (line + line + line + line).in[Data]
frame
  label Compressed
  highlight compress[Deflate]  the raw algorithm, with no framing
  code
      import codepages.utf8Codepage, alphabets.base64Standard

      val line = t"Tomorrow, and tomorrow, and tomorrow, "
      val speech = (line + line + line + line).in[Data]
      val packed = speech.compress[Deflate]
frame
  label And written as Base64
  highlight serialize[Base64]  text which survives being pasted
  code
      import codepages.utf8Codepage, alphabets.base64Standard

      val line = t"Tomorrow, and tomorrow, and tomorrow, "
      val speech = (line + line + line + line).in[Data]
      val packed = speech.compress[Deflate]
      val pasted: Text = packed.serialize[Base64]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(speech.length.show)
Out.println(pasted)
```

[The formats](../../soundness/modules/compression.md#the-formats) lists the compressors, and
[base encoding](../../soundness/modules/base-encoding.md) the encodings.
