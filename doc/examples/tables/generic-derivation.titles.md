# A title which is not the field's name

A derived table titles each column from its field. Where another title is wanted, a
`Table.Relabelling` supplies it by name, and the derivation picks it up: the field keeps its
name in the code, and the table gets its own.

```scala fuel
compile
frame
  label The derived title
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, firstPerformed: Int)

      val plays = List(Play(t"Hamlet", 1601), Play(t"Twelfth Night", 1602))
      val table = plays.tabulation.grid(40)
frame
  label A title of its own
  highlight Table.Relabelling[Play]  found by the derivation, as it compiles
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, firstPerformed: Int)

      given relabelling: Table.Relabelling[Play]:
        def relabelling() = Map(t"firstPerformed" -> t"Year")

      val plays = List(Play(t"Hamlet", 1601), Play(t"Twelfth Night", 1602))
      val table = plays.tabulation.grid(40)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

table.render.each(Out.println(_))
```

[Titles](../../soundness/modules/tables.md#titles) has the other ways to rename a column.
