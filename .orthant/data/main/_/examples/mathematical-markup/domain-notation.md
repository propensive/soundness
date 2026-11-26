# The quadratic formula on one line

MathML says that an equation is a tree, at ten times the length of the equation. The same
tree is written in Ergo with one glyph for each structure: `↗` raises, `/` divides and `√`
takes a root.

```scala fuel
compile
frame
  label A power, as a tree
  code
      import Mathml.*

      val formula = Math(Msup(Mi(t"b"), Mn(t"2")))
frame
  label The same power, in Ergo
  highlight ↗  a superscript
  code
      import Mathml.*

      val formula = ergo"(b↗2)"
frame
  label Under a root
  highlight √  a square root, of the group which follows
  code
      import Mathml.*

      val formula = ergo"(√(b↗2 - 4 a c))"
frame
  label Over a denominator
  highlight /  a fraction, of the groups either side
  code
      import Mathml.*

      val formula = ergo"(x = (-b ± √(b↗2 - 4 a c))/(2 a))"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(formula.draw)
```

`draw` sets the tree in the terminal, and `in[Xml]` writes it for a browser.
[Structural glyphs](../../soundness/modules/mathematical-markup.md#structural-glyphs) has the
whole table.
