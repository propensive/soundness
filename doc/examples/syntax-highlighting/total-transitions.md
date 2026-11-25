# Broken source is still highlighted

Code is highlighted while it is being written, which is when it is wrong. `highlight` is
therefore defined for every text: it raises nothing and needs no error strategy, and what the
tokenizer could not read is a token like any other, with an accent which says so.

```scala fuel
compile
frame
  label Well-formed source
  code
      val whole = Scala.highlight(t"val line = \"Exit, pursued by a bear\"")
frame
  label A string which never ends
  highlight Scala.highlight  no raises, and no strategy imported
    line 2
  code
      val whole = Scala.highlight(t"val line = \"Exit, pursued by a bear\"")
      val broken = Scala.highlight(t"val line = \"Exit, pursued by")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(whole(1).map(_.accent.show).join(t" "))
Out.println(broken(1).map(_.accent.show).join(t" "))
```

[Types and diagnostics](../../soundness/modules/syntax-highlighting.md#types-and-diagnostics)
describes ill-typed source, which is highlighted with its errors, and
[total transitions](../../philosophy/total-transitions.md) the principle.
