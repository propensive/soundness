# Lines of a stream, decoded as numbers

A stream of text is split into lines, and each line is a record. Decoding a record is the
`as` of every other text, so the stream becomes a list of numbers with no parser written
for it.

```scala fuel
compile
frame
  label A stream of text
  code
      import lineSeparation.adaptiveLinefeedLineSeparation, strategies.throwUnsafely

      val ages = t"13\n16\n80".source[Text]
frame
  label Split into lines
  highlight delineate.records  one record for each line
  code
      import lineSeparation.adaptiveLinefeedLineSeparation, strategies.throwUnsafely

      val ages = t"13\n16\n80".source[Text].delineate.records
frame
  label Each line, decoded
  highlight as[Int]  the decoder of any text
  code
      import lineSeparation.adaptiveLinefeedLineSeparation, strategies.throwUnsafely

      val ages = t"13\n16\n80".source[Text].delineate.records.map(_.as[Int]).to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(ages.inspect)
```

[Records](../../soundness/modules/streams.md#records) describes streaming by the record, and
[Decoding](../../soundness/modules/decoding.md) the method which reads each one.
