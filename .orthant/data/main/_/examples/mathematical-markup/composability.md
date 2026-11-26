# A quantity set as mathematics

A value substituted into an Ergo expression becomes a part of the tree. A number is a number
of the formula, and a quantity brings its units with it, set as the mathematics they are.

```scala fuel
compile
frame
  label A formula of symbols
  code
      val gravity = ergo"(g = G M/r↗2)"
frame
  label A number in it
  highlight $radius  an `Int`, set as a number
  code
      val radius = 6371
      val gravity = ergo"(g = G M/$radius↗2)"
frame
  label A quantity
  highlight $surface  a quantity, set with its units
  code
      val surface = 9.81*Metre/(Second*Second)
      val gravity = ergo"(g = $surface)"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(gravity.in[Xml].show)
```

[The MathML tree](../../soundness/modules/mathematical-markup.md#the-mathml-tree) describes
what is written, and [quantities](../../soundness/modules/quantities.md) the value which was
substituted.
