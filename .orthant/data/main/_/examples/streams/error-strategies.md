# One bad line, or the whole stream

A stream of lines is decoded as numbers, and one line is not a number. Whether that spoils
the whole stream or only its own line is decided by where the failure is caught, and the
decoding itself is the same in both.

```scala fuel
compile
frame
  label The whole stream, or nothing
  highlight safely  around the stream
  code
      import lineSeparation.adaptiveLinefeedLineSeparation

      def ages(): Optional[List[Int]] = safely:
        t"13\nsweet\n80".source[Text].delineate.records.map(_.as[Int]).to[List]
frame
  label Each line, or nothing
  highlight safely(line.as[Int])  around the line
  code
      import lineSeparation.adaptiveLinefeedLineSeparation

      def ages(): List[Optional[Int]] =
        t"13\nsweet\n80".source[Text].delineate.records.map: line =>
          safely(line.as[Int])
        . to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(ages().inspect)
```

[Records](../../soundness/modules/streams.md#records) describes the stream of lines, and
[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) the other answers
to a failure.
