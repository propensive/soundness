# A cell may hold a comma

Any text is a cell. One which holds the delimiter, a quote or a line break is quoted as it is
written, with its quotes doubled, and is one cell again when it is read. No value has to be
avoided, and none is split.

```scala fuel
compile
frame
  label A row of plain cells
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Weaver(first: Text, last: Text)

      val row: Text = Weaver(t"Nick", t"Bottom").in[Text over Dsv]
frame
  label A cell with a comma and a quote
  highlight t"Bottom..weaver"  quoted when written
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Weaver(first: Text, last: Text)

      val row: Text = Weaver(t"Nick", t"Bottom, \"the weaver\"").in[Text over Dsv]
      val back: Weaver = row.read[Weaver in Dsv]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(row)
Out.println(back.last)
```

[Encoding](../../soundness/modules/csv.md#encoding) has the rules for quoting.
