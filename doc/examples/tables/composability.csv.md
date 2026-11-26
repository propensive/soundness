# From CSV to a table

One case class describes a row twice: to the CSV reader, as the fields to decode, and to the
table, as the columns to draw. Nothing is written to join the two.

```scala fuel
compile
frame
  label Rows of CSV, decoded
  highlight as[Play]  decoded by the fields of Play
  code
      import dsvFormats.csvFormat, strategies.throwUnsafely

      case class Play(title: Text, genre: Text, acts: Int)

      val csv = t"Hamlet,Tragedy,5\nTwelfth Night,Comedy,5\nThe Tempest,Romance,5"
      val plays = csv.read[Sheet].rows.map(_.as[Play]).to[List]
frame
  label The same rows, tabulated
  highlight tabulation  drawn by the fields of Play
  code
      import dsvFormats.csvFormat, strategies.throwUnsafely
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, genre: Text, acts: Int)

      val csv = t"Hamlet,Tragedy,5\nTwelfth Night,Comedy,5\nThe Tempest,Romance,5"
      val plays = csv.read[Sheet].rows.map(_.as[Play]).to[List]
      val table = plays.tabulation.grid(40)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

table.render.each(Out.println(_))
```

[CSV](../../soundness/modules/csv.md) reads the rows, and
[Tables](../../soundness/modules/tables.md#a-table-from-a-case-class) draws them.
