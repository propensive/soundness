# A table from a case class

A list of case classes is a table already. Its columns are the fields, titled from their
names, with numbers aligned to the right, and a field added to the class is a column added to
the table.

```scala fuel
compile
frame
  label Two fields, two columns
  highlight tabulation  derived from the fields of Play
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, acts: Int)

      val plays = List(Play(t"Hamlet", 5), Play(t"Twelfth Night", 5))
      val table = plays.tabulation.grid(48)
frame
  label A third field
  highlight firstPerformed:  becomes the title "First performed"
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, acts: Int, firstPerformed: Int)

      val plays = List(Play(t"Hamlet", 5, 1601), Play(t"Twelfth Night", 5, 1602))
      val table = plays.tabulation.grid(48)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

table.render.each(Out.println(_))
```

[A table from a case class](../../soundness/modules/tables.md#a-table-from-a-case-class) names
the three types which are at work.
