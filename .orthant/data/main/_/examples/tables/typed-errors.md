# A table too wide to draw

What happens to a table which cannot fit its width is a policy, chosen by an import. Under
`ignoreAttenuation` it is drawn as well as it can be, and nothing can fail. Under
`failAttenuation` the same line raises a `Table.Error`, and no longer compiles until the error
is handled.

```scala fuel
compile
frame
  label Drawn regardless
  highlight ignoreAttenuation  cannot fail, so nothing is handled
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import hyphenations.englishHyphenation

      case class Play(title: Text, genre: Text)

      val plays = List(Play(t"A Midsummer Night's Dream", t"Comedy"))

      def draw(width: Int): Text =
        import columnAttenuation.ignoreAttenuation
        plays.tabulation.grid(width).render.join(t"\n")
frame
  label Refused, and not handled
  errors show
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import hyphenations.englishHyphenation

      case class Play(title: Text, genre: Text)

      val plays = List(Play(t"A Midsummer Night's Dream", t"Comedy"))

      def draw(width: Int): Text =
        import columnAttenuation.failAttenuation
        plays.tabulation.grid(width).render.join(t"\n")
frame
  label Refused, with an answer
  highlight Table.Error(minimum,..available)  the two widths, as numbers
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import hyphenations.englishHyphenation

      case class Play(title: Text, genre: Text)

      val plays = List(Play(t"A Midsummer Night's Dream", t"Comedy"))

      def draw(width: Int): Text =
        import columnAttenuation.failAttenuation

        recover:
          case Table.Error(minimum, available) => t"$available is too narrow: $minimum is needed"
        . protect(plays.tabulation.grid(width).render.join(t"\n"))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

Out.println(draw(10))
Out.println(draw(40))
```

[When the table does not fit](../../soundness/modules/tables.md#when-the-table-does-not-fit)
says which policy suits which output.
