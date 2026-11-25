# Lines which do not respect the blocks they arrive in

A network or a disk delivers bytes in blocks of its own choosing, and a line may begin in one and
end in another. `delineate` turns a stream of bytes into a stream of lines, carrying an
unfinished line from each block into the next, and `records` hands them out one at a time.

```scala fuel
compile
frame
  label Three lines, cut into two blocks mid-word
  highlight Guild..enstern  one name, in two blocks
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import lineSeparation.adaptiveLinefeedLineSeparation

      val blocks = Chain(t"Rosencrantz\nGuild".in[Data], t"enstern\nOsric".in[Data])
frame
  label The lines, whole
  highlight delineate.records
  code
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import lineSeparation.adaptiveLinefeedLineSeparation

      val blocks = Chain(t"Rosencrantz\nGuild".in[Data], t"enstern\nOsric".in[Data])
      val courtiers = Stream(blocks).delineate.records.to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(courtiers.inspect)
```

[Records](../../soundness/modules/streams.md#records) describes streams whose elements are
records, not bytes.
