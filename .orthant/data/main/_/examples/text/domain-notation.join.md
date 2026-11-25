# A list which reads as English

A list in a sentence has commas between its items, and a different word before the last.
`join` takes the separator, and optionally the final one, so the sentence is not assembled
by hand.

```scala fuel
compile
frame
  label One separator
  highlight join(t",..")  between every pair
  code
      val witches = List(t"Hecate", t"Graymalkin", t"Paddock")
      val line = witches.join(t", ")
frame
  label A different one before the last
  highlight and  the word before the last item
  code
      val witches = List(t"Hecate", t"Graymalkin", t"Paddock")
      val line = witches.join(t", ", t" and ")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(line)
```

[Splitting and joining](../../soundness/modules/text.md#splitting-and-joining) sets `join`
against `mkString`.
