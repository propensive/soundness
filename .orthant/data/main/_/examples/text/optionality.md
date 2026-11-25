# A position which may not exist

A search of text for a substring has no position to give when the substring is absent.
`offsetOf` returns an optional position, so a missing one cannot be mistaken for a number.

```scala fuel
compile
frame
  label A search treated as a position
  errors show
  code
      val line = t"To be, or not to be"
      val comma: Ordinal = line.offsetOf(t",")
frame
  label A search which may find nothing
  highlight Optional[Ordinal]  a position, or nothing
  code
      val line = t"To be, or not to be"
      val comma: Optional[Ordinal] = line.offsetOf(t",")
frame
  label What absence means, decided
  highlight .or(line)  the whole line, if there is no comma
  code
      val line = t"To be, or not to be"
      val comma: Optional[Ordinal] = line.offsetOf(t",")
      val clause = comma.let { end => line.segment(Prim thru end) }.or(line)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(clause)
```

[Searching](../../soundness/modules/text.md#searching) lists the other questions text can be
asked, and [optionality](../../philosophy/optionality.md) explains why there is no `-1`.
