# A sort which names its algorithm

The algorithm decides whether equal elements keep their order, and how a sort behaves on data
which is nearly sorted. It is chosen by an import, and with none in scope a sort does not
compile.

```scala fuel
compile
frame
  label No algorithm has been chosen
  errors show
  code
      val titles = List(t"Othello", t"Lear", t"Hamlet", t"Macbeth")
      val byLength = titles.order(_.length)
frame
  label A stable sort
  highlight sortingAlgorithms.timsort  stable, and good on ordered data
  code
      import sortingAlgorithms.timsort

      val titles = List(t"Othello", t"Lear", t"Hamlet", t"Macbeth")
      val byLength = titles.order(_.length)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(byLength.inspect)
```

Timsort is stable, so `Othello` stays ahead of `Macbeth`, which is as long.
[Sorting](../../soundness/modules/collections.md#sorting) lists the algorithms, and
[declarative context](../../philosophy/declarative-context.md) explains choices of this kind.
