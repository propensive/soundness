# Three ways to say which positions

A range of positions is written as it is spoken: from the third `thru` the sixth, from the
third `till` the sixth, or three positions from the third. The word says whether the end is
included, so there is no `+ 1` to forget.

```scala fuel
compile
frame
  label Including the end
  highlight thru  third, fourth, fifth and sixth
  code
      val scenes = Ter thru Sen
frame
  label Excluding the end
  highlight till  third, fourth and fifth
  code
      val scenes = Ter till Sen
frame
  label By length
  highlight span  three positions, from the third
  code
      val scenes = Ter span 3
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(scenes.inspect)
```

[Ordinals](../../soundness/modules/numbers.md#ordinals) describes intervals and what they
prove.
