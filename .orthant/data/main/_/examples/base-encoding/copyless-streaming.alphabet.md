# An alphabet is a stage of a stream

Bytes are encoded as they flow, without being gathered first. A Base64 group is three bytes, and a
chunk may end in the middle of one, so the stage carries the remainder into the next chunk, and
the streamed text is the same as the text of the whole value.

```scala fuel
compile
frame
  label The whole value, serialized
  highlight serialize[Base64]
  code
      import codepages.utf8Codepage, alphabets.base64Standard

      val whole = t"All the world's a stage".in[Data].serialize[Base64]
frame
  label Two chunks, encoded as they pass
  highlight via(summon[Alphabet[Base64]])
  code
      import codepages.utf8Codepage, alphabets.base64Standard

      val whole = t"All the world's a stage".in[Data].serialize[Base64]

      val chunks = Chain(t"All the wor".in[Data], t"ld's a stage".in[Data])
      val streamed = Stream(chunks).via(summon[Alphabet[Base64]]).memoize
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(whole)
Out.println(streamed)
```

[Encoding a stream](../../soundness/modules/base-encoding.md#encoding-a-stream) describes the
stage, and its counterpart for decoding.
