# A fraction has two parts

In MathML text, an `<mfrac>` with one child is a mistake which the browser discovers. As a
value, a fraction is made from a numerator and a denominator, and there is no way to write
one without the other.

```scala fuel
compile
frame
  label A fraction with no denominator
  errors show
  code
      import Mathml.*

      val half = Math(Mfrac(Mn(t"1")))
frame
  label A numerator and a denominator
  highlight Mn(t"2")  the second part, which the type asks for
  code
      import Mathml.*

      val half = Math(Mfrac(Mn(t"1"), Mn(t"2")))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(half.in[Xml].show)
```

[The MathML tree](../../soundness/modules/mathematical-markup.md#the-mathml-tree) lists the
elements, and [impossible states](../../philosophy/impossible-states.md) the principle.
