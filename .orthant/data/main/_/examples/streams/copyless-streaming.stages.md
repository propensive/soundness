# Stages which run only when the consumer pulls

A stage turns one stream into another, and attaching it moves no data. The last line pulls, and
each pull is a nested call through the stages on the caller's thread: compressed bytes become
bytes, and bytes become text, in the blocks in which they arrive.

```scala fuel
compile
frame
  label Compressed bytes, as a stream
  highlight .stream
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val wire = t"Double, double toil and trouble".in[Data].compress[Gzip]

      def spell() = wire.stream
frame
  label A stage which decompresses
  highlight viaDuct
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val wire = t"Double, double toil and trouble".in[Data].compress[Gzip]
      val gzip = summon[Gzip is Compression]

      def spell() = wire.stream.viaDuct(gzip.decompressor())
frame
  label A stage which decodes characters
  highlight via(summon[Charset])
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val wire = t"Double, double toil and trouble".in[Data].compress[Gzip]
      val gzip = summon[Gzip is Compression]

      def spell() = wire.stream.viaDuct(gzip.decompressor()).via(summon[Charset])
frame
  label The pull which drives them all
  highlight memoize
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer

      val wire = t"Double, double toil and trouble".in[Data].compress[Gzip]
      val gzip = summon[Gzip is Compression]

      def spell(): Text =
        wire.stream.viaDuct(gzip.decompressor()).via(summon[Charset]).memoize
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(spell())
```

[Composing a pipeline](../../soundness/modules/streams.md#composing-a-pipeline) lists the
terminal operations which may end one.
