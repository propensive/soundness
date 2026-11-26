# One verb for three ways of keeping

`keep` keeps part of a text. What it is given says which part: a number of characters, a
number and a direction, or a predicate which each kept character satisfies.

```scala fuel
compile
alternative A number of characters
  highlight keep(4)  the first four
  code
      val kept = t"KINGlear".keep(4)
alternative From the right
  highlight Rtl  the last four
  code
      val kept = t"KINGlear".keep(4, Rtl)
alternative Those which match
  highlight _.isUpper  every capital letter
  code
      val kept = t"KINGlear".keep(_.isUpper)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(kept)
```

[Slicing](../../soundness/modules/text.md#slicing) and
[Transforming](../../soundness/modules/text.md#transforming) describe the forms, and
[small APIs](../../philosophy/small-apis.md) the reason for one verb.
