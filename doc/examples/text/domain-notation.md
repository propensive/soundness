# The first to the third characters

A range of characters is written with ordinals, which count from one as people do: `Prim`,
`Sec`, `Ter`, and so on. A zero-based index is not an ordinal, so the off-by-one error has
nowhere to start.

```scala fuel
compile
frame
  label A range of numbers
  errors show
  code
      val line = t"Now is the winter of our discontent"
      val word = line.segment(0 thru 2)
frame
  label A range of ordinals
  highlight Prim..Ter  the first to the third, inclusive
  code
      val line = t"Now is the winter of our discontent"
      val word = line.segment(Prim thru Ter)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(word)
```

[Slicing](../../soundness/modules/text.md#slicing) has `keep`, `skip` and `snip`, which count
characters instead.
