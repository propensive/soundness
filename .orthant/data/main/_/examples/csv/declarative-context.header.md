# Column headers, read as member names

A cell of a row with a header is read as a member, at the type asked for. The headers of a file
seldom look like the names of Scala, so the code must say how one becomes the other, and
does not compile until it has.

```scala fuel
compile
frame
  label A member, and no rule for its header
  errors show
  code
      import strategies.throwUnsafely
      import dsvFormats.csvWithHeaderFormat, dynamicAccess.dynamicDsv

      val sheet = t"Stage Name,First Line\nBottom,Are we all met?".read[Sheet]
      val line = sheet.rows(Prim).let(_.firstLine[Text])
frame
  label A rule which reads capitalized words
  highlight capitalizedWordsRedesignation  firstLine is First Line
  code
      import strategies.throwUnsafely
      import dsvFormats.csvWithHeaderFormat, dynamicAccess.dynamicDsv
      import dsvRedesignations.capitalizedWordsRedesignation

      val sheet = t"Stage Name,First Line\nBottom,Are we all met?".read[Sheet]
      val line = sheet.rows(Prim).let(_.firstLine[Text])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(line.inspect)
```

[Addressing cells by name](../../soundness/modules/csv.md#addressing-cells-by-name) lists the
rules and the headers each expects.
