# The third version

An evolution follows the elements of a sequence through many versions, and a version is asked
for by its position, as an ordinal. `Prim`, `Sec` and `Ter` are the first, second and third,
so there is no question of whether counting began at zero.

```scala fuel
compile
frame
  label Three drafts of a word
  code
      val drafts = List(List('s', 'o', 'l', 'i', 'd'), List('s', 'a', 'l', 'l', 'i', 'e', 'd'),
          List('s', 'u', 'l', 'l', 'i', 'e', 'd'))

      val history = evolve(drafts)
frame
  label The third of them
  highlight Ter  the third, not the element at index 3
  code
      val drafts = List(List('s', 'o', 'l', 'i', 'd'), List('s', 'a', 'l', 'l', 'i', 'e', 'd'),
          List('s', 'u', 'l', 'l', 'i', 'e', 'd'))

      val history = evolve(drafts)
      val folio: List[Char] = history(Ter)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(folio.inspect)
```

[Evolution](../../soundness/modules/diffing.md#evolution) describes what an evolution keeps.
