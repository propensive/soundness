# From a spreadsheet to JSON

One case class has a codec for each format, each derived from the same fields. Rows of CSV are
decoded to it and encoded as JSON, and neither format has heard of the other.

```scala fuel
compile
frame
  label Rows, decoded
  code
      import strategies.throwUnsafely
      import dsvFormats.csvWithHeaderFormat

      case class Part(player: Text, lines: Int)

      val sheet = t"player,lines\nBottom,261\nQuince,121".read[Sheet]
      val parts = sheet.rows.to[List].map(_.as[Part])
frame
  label The same values, as a JSON array
  highlight in[Json]  the same derivation, another format
  code
      import strategies.throwUnsafely
      import dsvFormats.csvWithHeaderFormat

      case class Part(player: Text, lines: Int)

      val sheet = t"player,lines\nBottom,261\nQuince,121".read[Sheet]
      val parts = sheet.rows.to[List].map(_.as[Part])
      val json = parts.in[Json]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(json.show)
```

[Decoding to a case class](../../soundness/modules/csv.md#decoding-to-a-case-class) and
[JSON](../../soundness/modules/json.md#case-classes-and-enumerations) describe the two halves.
