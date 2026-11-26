# Whether one node reaches another

Asking whether one node depends on another, however indirectly, is a search on a `Dag`, and a
single bit on a frozen graph, which keeps a matrix of reachability. `reaches` is offered only
where it is cheap, so the search cannot be written by accident inside a loop.

```scala fuel
compile
frame
  label Asked of a graph which would have to search
  errors show
  code
      import strategies.throwUnsafely

      val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())
      val depends: Boolean = dag.reaches(8, 2)
frame
  label Asked of a frozen graph
  highlight freeze  laid out once, for querying
  code
      import strategies.throwUnsafely

      val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())
      val frozen = dag.freeze
      val depends: Boolean = frozen.reaches(8, 2)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(depends.inspect)
Out.println(frozen.reaches(2, 8).inspect)
```

[Freezing](../../soundness/modules/graphs.md#freezing) describes the frozen form and what it
keeps ready.
