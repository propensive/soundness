# The dependants of a node

A `Dag` stores each node's dependencies, and not its dependants. Finding what depends on a
node means reading every edge, and a query which costs that much does not compile until the
cost is acknowledged, or the graph is put into a form which has the answer ready.

```scala fuel
compile
frame
  label A scan of every edge, unacknowledged
  errors show
  code
      import strategies.throwUnsafely

      val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())

      val dependants = dag.predecessors(2)
frame
  label The cost, acknowledged
  highlight linearScan  every edge will be read
  code
      import strategies.throwUnsafely
      import dysasymptotics.linearScan

      val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())

      val dependants = dag.predecessors(2)
frame
  label Or a frozen graph, which keeps both directions
  highlight freeze  dependants are stored, so nothing is scanned
  code
      import strategies.throwUnsafely

      val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())

      val dependants = dag.freeze.predecessors(2)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(dependants.inspect)
```

[Freezing](../../soundness/modules/graphs.md#freezing) describes the form which answers without
a scan.
