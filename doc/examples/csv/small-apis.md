# A value is a row, and a list of them is a sheet

`dsv` encodes whatever it is given. A case class becomes a row, a list of them becomes a
sheet, and `show` writes either as delimited text in the format in scope.

```scala fuel
compile
frame
  label One row
  highlight dsv  a Dsv
  code
      import dsvFormats.csvFormat

      case class Part(player: Text, lines: Int)

      val row: Dsv = Part(t"Bottom", 261).dsv
frame
  label A sheet
  highlight dsv  a Sheet
    line 7
  code
      import dsvFormats.csvFormat

      case class Part(player: Text, lines: Int)

      val row: Dsv = Part(t"Bottom", 261).dsv
      val parts = List(Part(t"Bottom", 261), Part(t"Quince", 121), Part(t"Flute", 58))
      val sheet: Sheet = parts.dsv
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(sheet.show)
```

[Encoding](../../soundness/modules/csv.md#encoding) has the forms which write straight to text
or bytes.
