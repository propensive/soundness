# One method splits text three ways

Java's `split` takes a regular expression, even where a literal was meant. `cut` takes a
`Text`, a `Char` or a regular expression, and its type says which.

```scala fuel
compile
alternative At a piece of text
  highlight t",..")  taken literally
  code
      val cast = t"Lear, Kent, Fool".cut(t", ")
alternative At a character
  highlight ','  a single character
  code
      val cast = t"Lear,Kent,Fool".cut(',')
alternative At a pattern
  highlight r"[,;]..*"  read as a regular expression
  code
      val cast = t"Lear, Kent;Fool".cut(r"[,;] *")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(cast.inspect)
```

[Splitting and joining](../../soundness/modules/text.md#splitting-and-joining) has `words`,
`lines` and `join`, and [small APIs](../../philosophy/small-apis.md) the case for one name.
