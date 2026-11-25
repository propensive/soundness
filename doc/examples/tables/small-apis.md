# One `Column` for every type of cell

A column is a title and a function from a row to a value. The value may be text, a number or
anything else which can be shown: the title's type decides what a cell is, and each value is
shown as that. A column which is not essential says so, and gives way when the table is narrow.

```scala fuel
compile
frame
  label A column of text
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, acts: Int, scenes: Int)

      val plays = List(Play(t"Hamlet", 5, 20), Play(t"Macbeth", 5, 28))

      val table = Scaffold[Play](Column(t"Play")(_.title))
frame
  label A column of numbers
  highlight _.scenes  an Int, shown as text
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, acts: Int, scenes: Int)

      val plays = List(Play(t"Hamlet", 5, 20), Play(t"Macbeth", 5, 28))

      val table = Scaffold[Play](Column(t"Play")(_.title), Column(t"Scenes")(_.scenes))
frame
  label A column which may be dropped
  highlight columnar.Collapsible(0.5)  vanishes when width is short
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, acts: Int, scenes: Int)

      val plays = List(Play(t"Hamlet", 5, 20), Play(t"Macbeth", 5, 28))

      val table = Scaffold[Play]
        ( Column(t"Play")(_.title),
          Column(t"Scenes")(_.scenes),
          Column(t"Scenes per act", sizing = columnar.Collapsible(0.5))(play => play.scenes/play.acts) )
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

table.tabulate(plays).grid(48).render.each(Out.println(_))
table.tabulate(plays).grid(24).render.each(Out.println(_))
```

[Explicit columns](../../soundness/modules/tables.md#explicit-columns) lists the sizings, and
[renderings at different widths](../../soundness/modules/tables.md#renderings-at-different-widths)
shows them giving way.
