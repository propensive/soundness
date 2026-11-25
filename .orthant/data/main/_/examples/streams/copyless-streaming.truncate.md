# The first bytes of a stream, and no more

`truncate` bounds a stream without draining it. The stream it gives ends after the count, and
the rest of the source is released unread, so taking a header from a large body costs the header
alone.

```scala fuel
compile
frame
  label A stream of a whole speech
  code
      import codepages.utf8Codepage

      val speech = t"Friends, Romans, countrymen, lend me your ears".in[Data]

      def opening(): Data = speech.stream.memoize
frame
  label Its first seven bytes
  highlight truncate(7)
  code
      import codepages.utf8Codepage

      val speech = t"Friends, Romans, countrymen, lend me your ears".in[Data]

      def opening(): Data = speech.stream.truncate(7).memoize
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(opening().utf8)
```

[Composing a pipeline](../../soundness/modules/streams.md#composing-a-pipeline) describes
`truncate` and its counterpart, `discard`.
