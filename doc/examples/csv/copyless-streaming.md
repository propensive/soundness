# Records from a stream, with no rows between

A file too large to hold is read from a source, a record at a time. A type with a
`Dsv.Parsable` instance is parsed from the cells where they lie in the reader's buffer, and no
row is built for it on the way.

```scala fuel
compile
frame
  label Rows from a source, each decoded
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Part(player: Text, lines: Int)

      val cast = t"Bottom,261\nQuince,121\nFlute,58".source[Text]
      val lines = cast.rows.map(_.as[Part].lines).to[List]
frame
  label Records parsed directly from the source
  highlight rowsOf[Part]  one record at a time
  highlight Dsv.Parsable.derived  a parser for this shape
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Part(player: Text, lines: Int)

      given partParsable: (Part is Dsv.Parsable) = Dsv.Parsable.derived

      val cast = t"Bottom,261\nQuince,121\nFlute,58".source[Text]
      val lines = cast.rowsOf[Part].map(_.lines).to[List]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(lines.inspect)
```

[Parsing directly to a type](../../soundness/modules/csv.md#parsing-directly-to-a-type) says
which fields a directly parsed type may have.
