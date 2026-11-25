# Why a graph operation failed

A `Dag.Error` holds its reason as a value: a cycle, or a node which the graph does not have,
with the node's name. Code which handles the error matches on the reason, and has no message
to read.

```scala fuel
compile
frame
  label Any failure
  code
      def after(plot: Dag[Text], scene: Text): Text =
        recover:
          case Dag.Error(_) => t"unknown"
        . protect(plot.reachable(scene).join(t", "))
frame
  label The node which was missing
  highlight NodeMissing(name)  the reason, and what it holds
  code
      def after(plot: Dag[Text], scene: Text): Text =
        recover:
          case Dag.Error(reason) => reason match
            case Dag.Error.Reason.NodeMissing(name) => t"there is no scene called $name"
            case Dag.Error.Reason.Cyclic            => t"the plot goes round in a circle"
        . protect(plot.reachable(scene).join(t", "))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val plot = unsafely(Dag(t"duel" -> t"play", t"play" -> t"ghost"))

Out.println(after(plot, t"duel"))
Out.println(after(plot, t"shipwreck"))
```

[Ordering and reachability](../../soundness/modules/graphs.md#ordering-and-reachability) shows
the error as a message, and [expressive errors](../../philosophy/expressive-errors.md) why it
is also data.
