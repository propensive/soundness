# A table which breaks its words at syllables

A table knows nothing of syllables, and the hyphenation patterns know nothing of tables. With
the patterns of a language in scope, a column which is too narrow for its text breaks each
word where the language allows.

```scala fuel
compile
frame
  label A table of parts
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, strategies.throwUnsafely

      case class Part(name: Text, description: Text)

      val cast = List
        ( Part(t"Rosencrantz", t"a courtier and former schoolfellow"),
          Part(t"Guildenstern", t"another, indistinguishable" ) )

      val grid = cast.tabulation.grid(34)
frame
  label The English patterns
  highlight hyphenations.englishHyphenation  all that the table needs
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, strategies.throwUnsafely
      import hyphenations.englishHyphenation

      case class Part(name: Text, description: Text)

      val cast = List
        ( Part(t"Rosencrantz", t"a courtier and former schoolfellow"),
          Part(t"Guildenstern", t"another, indistinguishable" ) )

      val grid = cast.tabulation.grid(34)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

grid.render.each(Out.println(_))
```

[Tables](../../soundness/modules/tables.md#explicit-columns) has the column sizings which
wrap, and [Hyphenating](../../soundness/modules/hyphenation.md#hyphenating) the operation
beneath.
