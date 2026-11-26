# A row which stops short

Rows of a spreadsheet often leave their last cells empty. A trailing field of type `Optional`
takes one column, and reads as `Unset` from a row which does not reach it, where a plain field
would be an error.

```scala fuel
compile
frame
  label A row with both cells
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Cue(line: Text, speaker: Optional[Text])

      val full: Cue = t"Who's there?,Bernardo".read[Cue in Dsv]
frame
  label A row with one
  highlight Optional[Text]  Unset where the row ends first
  code
      import strategies.throwUnsafely
      import dsvFormats.csvFormat

      case class Cue(line: Text, speaker: Optional[Text])

      val full: Cue = t"Who's there?,Bernardo".read[Cue in Dsv]
      val short: Cue = t"Who's there?".read[Cue in Dsv]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(full.inspect)
Out.println(short.inspect)
```

[Optional columns](../../soundness/modules/csv.md#optional-columns) has the policies which make
a missing column an error.
