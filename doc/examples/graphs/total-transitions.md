# An order which cannot fail

A topological order exists only for a graph with no cycle. `linearized` is therefore not a
method which fails on some graphs: it is offered on a `Dag`, which has no cycle, and is not
offered on a `Digraph`, which may.

```scala fuel
compile
frame
  label A graph which may hold a cycle
  errors show
  code
      val scenes = Digraph(t"duel" -> t"play", t"play" -> t"ghost")
      val order: List[Text] = scenes.linearized
frame
  label A graph which does not
  highlight acyclic  the one place a cycle is looked for
  code
      import strategies.throwUnsafely

      val scenes = Digraph(t"duel" -> t"play", t"play" -> t"ghost")
      val order: List[Text] = scenes.acyclic.linearized
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(order.inspect)
```

Every scene follows the scenes it depends on.
[Ordering and reachability](../../soundness/modules/graphs.md#ordering-and-reachability) has the
other queries, and [total transitions](../../philosophy/total-transitions.md) the principle.
