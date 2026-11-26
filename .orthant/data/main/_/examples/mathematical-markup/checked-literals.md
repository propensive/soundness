# A formula with a bracket left open

An Ergo expression written as `ergo"…"` is parsed by the compiler, and the value it stands
for is the MathML tree. A group which is never closed is therefore a compile error, and not a
page which shows half a formula.

```scala fuel
compile
frame
  label The exponent's group is not closed
  errors show
  code
      val circle = ergo"(x↗2 + y↗2 = r↗(2)"
frame
  label Every group is closed
  highlight r↗(2)
  code
      val circle = ergo"(x↗2 + y↗2 = r↗(2))"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(circle.in[Xml].show)
```

[Ergo](../../soundness/modules/mathematical-markup.md#ergo) describes the notation, and
[safety by construction](../../philosophy/safety-by-construction.md) the principle.
