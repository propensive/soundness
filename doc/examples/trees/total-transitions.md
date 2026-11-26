# A drawing which cannot fail

A graph is drawn in the order of its dependencies, and a graph with a cycle has no such order.
The diagrams therefore take only a graph which is known to be acyclic, and a `Digraph`, which
may not be, has no drawing until it has been checked.

```scala fuel
compile
frame
  label A graph which may hold a cycle
  errors show
  code
      import laneDagStyles.boxDrawingLaneDagStyle, textMetrics.uniformMetric

      val feud = Digraph(t"Tybalt" -> t"Mercutio", t"Romeo" -> t"Tybalt")
      val lanes = LaneDagDiagram(feud).render(name => t" $name")
frame
  label The same graph, checked
  highlight acyclic  the one place a cycle is looked for
  code
      import laneDagStyles.boxDrawingLaneDagStyle, textMetrics.uniformMetric
      import strategies.throwUnsafely

      val feud = Digraph(t"Tybalt" -> t"Mercutio", t"Romeo" -> t"Tybalt")
      val lanes = LaneDagDiagram(feud.acyclic).render(name => t" $name")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

lanes.each(Out.println(_))
```

[Choosing a graph layout](../../soundness/modules/trees.md#choosing-a-graph-layout) says which
graphs can be drawn, and [total transitions](../../philosophy/total-transitions.md) why the
check is in the type.
