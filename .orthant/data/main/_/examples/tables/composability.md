# Styled text in a cell

A table does not know about styled text, and styled text does not know about tables. A column
whose title is written with `e"…"` is a column of styled cells, and the layout measures what
will be seen, and not the escape codes.

```scala fuel
compile
frame
  label Plain titles, plain cells
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Role(name: Text, play: Text)

      val roles = List(Role(t"Feste", t"Twelfth Night"), Role(t"Puck", t"A Midsummer Night's Dream"))

      val table = Scaffold[Role](Column(t"Role")(_.name), Column(t"Play")(_.play))
frame
  label Styled titles, styled cells
  highlight e"$Bold(Role)"  the title decides the type of the column
  highlight e"$Italic(${role.play})"  a cell with a style of its own
  code
      import tableStyles.thickTableStyle, textMetrics.uniformMetric
      import columnAttenuation.ignoreAttenuation, hyphenations.englishHyphenation

      case class Role(name: Text, play: Text)

      val roles = List(Role(t"Feste", t"Twelfth Night"), Role(t"Puck", t"A Midsummer Night's Dream"))

      val table = Scaffold[Role]
        ( Column(e"$Bold(Role)")(_.name),
          Column(e"$Bold(Play)")(role => e"$Italic(${role.play})") )
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

table.tabulate(roles).grid(36).render.each(Out.println(_))
```

[Explicit columns](../../soundness/modules/tables.md#explicit-columns) explains how the title
fixes the type, and [Terminal](../../soundness/modules/terminal.md#styled-text) the styles.
