# An edit gives a new graph

A `Dag` is a value. Adding a dependency or taking one away gives another `Dag`, and the graph
which was edited still answers as it did.

```scala fuel
compile
frame
  label A graph of dependencies
  code
      import strategies.throwUnsafely

      val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())
frame
  label One dependency fewer, in a new graph
  highlight dag.remove(8..4)  `dag` is not changed
  code
      import strategies.throwUnsafely

      val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())
      val pruned: Dag[Int] = dag.remove(8, 4)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(pruned.reachable(8).inspect)
Out.println(dag.reachable(8).inspect)
```

Node 4 is no longer reached in the new graph, and still is in the old.
[Editing](../../soundness/modules/graphs.md#editing) has the other edits, and
[immutability](../../philosophy/immutability.md) the reasons.
