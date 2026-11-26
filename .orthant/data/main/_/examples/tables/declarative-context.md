# The same table, in another style

How a table is ruled is not part of its definition, nor an argument to the method which
draws it. It is a given, chosen by an import, and every table drawn in that scope agrees.

```scala fuel
compile
alternative Thick
  highlight thickTableStyle
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, genre: Text, acts: Int)

      val plays = List(Play(t"Hamlet", t"Tragedy", 5), Play(t"The Tempest", t"Romance", 5))
      val table = plays.tabulation.grid(40)
alternative Rounded
  highlight thinRoundedTableStyle
  code
      import tableStyles.thinRoundedTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, genre: Text, acts: Int)

      val plays = List(Play(t"Hamlet", t"Tragedy", 5), Play(t"The Tempest", t"Romance", 5))
      val table = plays.tabulation.grid(40)
alternative Minimal
  highlight minimalTableStyle
  code
      import tableStyles.minimalTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Play(title: Text, genre: Text, acts: Int)

      val plays = List(Play(t"Hamlet", t"Tragedy", 5), Play(t"The Tempest", t"Romance", 5))
      val table = plays.tabulation.grid(40)
##
```

In the first of the three styles, the table is drawn so:

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

table.render.each(Out.println(_))
```

[Styles](../../soundness/modules/tables.md#styles) shows each of them drawn, and
[declarative context](../../philosophy/declarative-context.md) explains the import.
