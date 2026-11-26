# One method finds the messages in a stream

A stream arrives in chunks which have nothing to do with the messages in it. `frames` recovers
the messages, and takes the convention which separates them as a type: a length prefix, a
header, or a line ending.

```scala fuel
compile
frame
  label Lines, split across chunks
  highlight frames[Linefeed]
  code
      import strategies.throwUnsafely, codepages.utf8Codepage, charsets.utf8Charset

      val chunks = Chain(t"Who's there?\nNay, ans", t"wer me\n")
      val lines = chunks.iterator.frames[Linefeed].to[List]
frame
  label Records with a four-byte length
  highlight frames[LengthPrefix]
  code
      import strategies.throwUnsafely, codepages.utf8Codepage, charsets.utf8Charset

      val chunks = Chain(t"Who's there?\nNay, ans", t"wer me\n")
      val lines = chunks.iterator.frames[Linefeed].to[List]

      val prefixed = Chain(Data(0, 0, 0, 2, 104, 105))
      val records = prefixed.iterator.frames[LengthPrefix].to[List].map(_.utf8)
frame
  label Bodies with a Content-Length header
  highlight frames[ContentLength]
  code
      import strategies.throwUnsafely, codepages.utf8Codepage, charsets.utf8Charset

      val chunks = Chain(t"Who's there?\nNay, ans", t"wer me\n")
      val lines = chunks.iterator.frames[Linefeed].to[List]

      val prefixed = Chain(Data(0, 0, 0, 2, 104, 105))
      val records = prefixed.iterator.frames[LengthPrefix].to[List].map(_.utf8)

      val headed = Chain(t"Content-Length: 4\r\n\r\nExit".in[Data])
      val bodies = headed.iterator.frames[ContentLength].to[List].map(_.utf8)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(lines.inspect)
Out.println(records.inspect)
Out.println(bodies.inspect)
```

[Framing](../../soundness/modules/rpc.md#framing) has the table of conventions, and
[small APIs](../../philosophy/small-apis.md) the reason for one name.
