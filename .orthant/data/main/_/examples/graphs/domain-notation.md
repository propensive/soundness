# Dependencies, drawn as arrows

A graph is written as it would be drawn on paper. `a -> b` says that `a` depends on `b`, and
a `Dag` is a list of such arrows, or of nodes each pointing at the set of its dependencies.

```scala fuel
compile
frame
  label One arrow for each edge
  highlight ->  depends on
    occurrence 1
  code
      import strategies.throwUnsafely

      val tragedy = Dag(t"duel" -> t"play", t"play" -> t"ghost", t"duel" -> t"voyage")
frame
  label One arrow for each node
  highlight Set(t"play"..t"voyage")  everything the duel depends on
  code
      import strategies.throwUnsafely

      val tragedy = Dag
        ( t"duel" -> Set(t"play", t"voyage"),
          t"play" -> Set(t"ghost"),
          t"voyage" -> Set(),
          t"ghost" -> Set() )
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(tragedy.linearized.inspect)
Out.println(tragedy.successors(t"duel").inspect)
```

[Building a graph](../../soundness/modules/graphs.md#building-a-graph) has the third form, which
takes a function.
