# A document written a block at a time

`show` renders a whole document into one `Text`, which a large document does not need. `Json.emit`
hands each block of the output to a function as it fills, as text or as UTF-8 bytes encoded
straight from the serializer, so the whole of it is never held.

```scala fuel
compile
alternative One text
  highlight show
  code
      import codepages.utf8Codepage, formatting.compactJsonFormatting

      case class Play(title: Text, acts: Int)

      val json = Play(t"The Winter's Tale", 5).in[Json]
      val size = json.show.length
alternative Blocks of text
  highlight emit[Text]
  code
      import codepages.utf8Codepage, formatting.compactJsonFormatting

      case class Play(title: Text, acts: Int)

      val json = Play(t"The Winter's Tale", 5).in[Json]
      var size = 0
      Json.emit[Text](json, block => size += block.length)
alternative Blocks of bytes
  highlight emit[Data]
  code
      import codepages.utf8Codepage, formatting.compactJsonFormatting

      case class Play(title: Text, acts: Int)

      val json = Play(t"The Winter's Tale", 5).in[Json]
      var size = 0
      Json.emit[Data](json, block => size += block.length)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(size.show)
```

[Formatting](../../soundness/modules/json.md#formatting) compares these forms of output with the
ones which a consumer pulls.
