# A graph, as Graphviz reads it

A `Dag` of text is a graph, and DOT is a language for graphs. `dot` gives the one as a value
of the other, which is serialized like any other document, and no line of DOT is written by
hand.

```scala fuel
compile
frame
  label A graph
  code
      import strategies.throwUnsafely

      val court = Dag(t"Hamlet" -> Set(t"Gertrude"), t"Gertrude" -> Set())
frame
  label The same graph, as DOT
  highlight court.dot  a DOT document, as a value
  code
      import strategies.throwUnsafely

      val court = Dag(t"Hamlet" -> Set(t"Gertrude"), t"Gertrude" -> Set())
      val picture: Dot = court.dot
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(picture.serialize)
```

[Drawing](../../soundness/modules/graphs.md#drawing) has the other ways to draw a graph, and
[composability](../../philosophy/composability.md) the reason they fit.
