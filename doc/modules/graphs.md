## Graphs

### About

A [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) is a set of
nodes with dependencies between them, and no cycles. Build systems, task schedules, type
hierarchies and package dependencies all have this shape.

A `Dag` represents one as an [immutable] value. It has the operations which such graphs need:
topological ordering, reachability, transitive closure and reduction, and editing which keeps
the graph acyclic. Two other graph types sit beside it:

```tabular
column  Type
column  Cycles
column  Editing
column  Suited to
row  `Dag`  never  returns a new value  dependency structures as values
row  `Digraph`  allowed  returns a new value  graphs that may hold a cycle
row  `Topology[node]^`  never  in place  graphs built up edge by edge
row  `Topology[node]^{}`  never  none: frozen  graphs built once and queried often
```

All of these share one vocabulary through the `Nodal` [typeclass], so the same operations apply
to each. So does anything else with the shape of a graph, such as a `Hasse` diagram, or a `Map`
from nodes to their successors.

### On acyclic graphs

Dependency structures have the same needs wherever they appear: an order in which to process the
nodes, the set which a change can reach, the removal of redundant edges. A general-purpose graph
library carries the weight of arbitrary graphs, cycles included. More often, a project writes
its own topological sort over a `Map`, and inherits its edge cases.

A `Dag` cannot be constructed with a cycle. A cyclic dependency graph is an
[impossible state](../philosophy/impossible-states.md), ruled out where the graph is built, and
so the topological order of a `Dag` has no way to fail.

Everything comes from the `soundness` package. The other imports choose a
[strategy](errors.md#choosing-a-strategy) for errors, and let the samples print their results:

```scala fuel
feature experimental.captureChecking
##
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Building a graph

A `Dag` is built from nodes with their sets of dependencies:

```scala
val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())
```

An edge `a -> b` means that `a` depends on `b`. A `Dag` is also built from edges alone, and both
ends of every edge become nodes:

```scala
val edges = Dag(8 -> 4, 8 -> 6, 6 -> 3)
```

A third form takes a set of nodes, and a function which gives the dependencies of each.

Each of these checks for a cycle, and raises a `Dag.Error` if it finds one:

```scala fuel
run
errors show
##
Dag(1 -> 2, 2 -> 3, 3 -> 1)
```

A graph which may hold a cycle is a `Digraph`. It is built in the same ways, without the check.
`cycle` finds a cycle, as a list of nodes which ends where it began:

```scala
val graph = Digraph(1 -> 2, 2 -> 3, 3 -> 1)
```

```scala fuel
run
##
Out.println(graph.cycle.inspect)
```

`acyclic` is the one checked conversion from a `Digraph` to a `Dag`:

```scala
val checked = Digraph(1 -> 2).acyclic
```

`explore` builds a `Digraph` by following a dependency function outward from one node. The
result is a `Digraph` because the function may lead back on itself. This is the graph of
divisors beneath 12:

```scala fuel
run
##
val divisors = 12.explore(n => (1 until n).filter(n % _ == 0)).acyclic
Out.println(divisors.linearized.inspect)
```

### Ordering and reachability

`linearized` gives a [topological order](https://en.wikipedia.org/wiki/Topological_sorting).
Every node comes after its dependencies, and the nodes are otherwise in the order they were
given:

```scala fuel
run
##
Out.println(dag.linearized.inspect)
```

`reachable` gives a node and everything it depends on, directly or through other nodes:

```scala fuel
run
##
Out.println(dag.reachable(8).inspect)
Out.println(dag.reachable(4).inspect)
```

`reachable` raises a `Dag.Error` for a node which the graph does not have. It is defined on a
`Dag` and a `Digraph` alike, since reachability is well defined whether or not there is a cycle:

```scala fuel
run
errors show
##
dag.reachable(5)
```

These are the queries which a graph answers:

```tabular
column  Query
column  Answers
column  On a `Dag`
row  `sources`  the nodes depending on nothing, where a build starts
row  `sinks`  the nodes nothing depends on
row  `successors(n)`  the dependencies of `n`
row  `predecessors(n)`  the dependants of `n`  needs `linearScan`
row  `reachable(n)`  `n` and everything it depends on, transitively
row  `descendants(n)`  the sub-graph beneath `n`
row  `ancestors(n)`  the sub-graph that depends on `n`  needs `linearScan`
row  `lineage(n)`  ancestors and descendants together  needs `linearScan`
row  `linearized`  a topological order
row  `invert`  the graph with every edge reversed
row  `reaches(a, b)`  whether `a` depends on `b`, transitively  frozen `Topology` only
```

As a collection, a graph is its nodes, and those of a `Dag` are in topological order. The
[collection](collections.md) vocabulary applies through the usual typeclasses:

```scala fuel
run
##
Out.println(dag.has(4).inspect)
Out.println(dag.size.show)
```

`dag.map(_.toString)` gives a `Digraph`, not a `Dag`. Two nodes may map to the same value, and
merging them could close a cycle.

A `Dag` stores only the forward direction. Finding the predecessors of a node means scanning
every edge, which is a cost out of proportion to the question. The operations which need it are
`predecessors`, `ancestors`, `lineage`, `bypass` and `-`. Without an acknowledgement of the
cost, they do not compile:

```scala fuel
compile
errors show
##
dag.predecessors(2)
```

An import gives the acknowledgement:

```scala
import dysasymptotics.linearScan
```

```scala fuel
run
##
Out.println(dag.predecessors(2).inspect)
Out.println(dag.ancestors(2).linearized.inspect)
Out.println(dag.lineage(4).linearized.inspect)
```

### Folding over a graph

A traversal computes a value for every node from the values of the nodes it depends on. The
dependencies are always computed first. This traversal gives each node one more than the sum of
the values beneath it:

```scala
val counts = dag.traversal[Int]((childValues, node) => childValues.total + 1)
```

Most real work over a dependency graph has this shape: the cost of a build, a version constraint
passed along, a transitive set gathered up. A traversal expresses it once, without recursion
written by hand or a set of visited nodes to keep.

### Closure and reduction

The transitive `closure` adds an edge wherever a path exists. The transitive `reduction` removes
every edge which a path already implies. The reduction is the smallest graph with the same
reachability, and is the form in which a dependency graph is usually drawn:

```scala
val closed = dag.closure
val reduced = dag.reduction
```

Both are answered from the reachability matrix of a frozen topology, which the next section
describes. A graph which is queried this way more than once is best frozen once.

### Freezing

Whether a `Topology` can be edited is a matter of its capture set, as it is for arrays. A
`Topology[node]^` can be edited. A `Topology[node]^{}` cannot, since the editing methods need an
exclusive handle. `dag.freeze` gives the frozen form, as does `Topology.freeze`, which consumes
an editable topology.

The frozen form is laid out for querying. It holds both directions of adjacency in dense arrays,
and maintains its sources and sinks. It builds a bit matrix of reachability on first use, so
whether one node reaches another is a single bit:

```scala
val frozen = dag.freeze
```

```scala fuel
run
##
Out.println(frozen.reaches(8, 2).inspect)
Out.println(frozen.reaches(2, 8).inspect)
Out.println(frozen.predecessors(2).inspect)
```

`predecessors` needs no acknowledgement here. `frozen.snapshot` gives a `Dag` back.

`closure`, `reduction` and `reaches` read the matrix. They are offered only on the frozen form,
since an edit would make the matrix stale. To edit, take an editable copy with
`Topology(frozen)`, edit it, and freeze again.

### Editing

An edit to a `Dag` gives a new `Dag` where the edit cannot create a cycle, and a `Digraph` where
it might:

```tabular
column  Edit
column  Returns
column  Acknowledgement
row  `add(a, b)`  a `Dag`, or raises `Dag.Error`
row  `remove(a, b)`  a `Dag`
row  `- n`  a `Dag`  `linearScan`
row  `bypass(n)`  a `Dag`  `linearScan`
row  `bypassAll(nodes)`  a `Dag`  `linearScan`
row  `subgraph(nodes)`  a `Dag`  `linearSize`
row  `+ (a -> b)`  a `Digraph`
row  `++ other`  a `Digraph`
row  `map(f)`  a `Digraph`
```

`add` adds a dependency, and `remove` takes one away:

```scala fuel
run
##
Out.println(dag.add(8, 3).reachable(8).inspect)
Out.println(dag.remove(8, 4).reachable(8).inspect)
```

`add` refuses an edge which would close a cycle:

```scala fuel
run
errors show
##
dag.add(2, 8)
```

`+` adds the same edge without complaint, because its result is a `Digraph`:

```scala fuel
run
##
Out.println((dag + (2 -> 8)).cycle.inspect)
```

`bypass` drops a node, and reroutes each of its dependants to each of its dependencies. Every
path through the node survives. `-` drops the node and its edges, and the paths with them:

```scala fuel
run
##
Out.println(dag.bypass(6).reachable(8).inspect)
Out.println((dag - 6).reachable(8).inspect)
```

`bypassAll` bypasses a set of nodes, one after another. `subgraph` keeps only the edges among
the chosen nodes. It rebuilds the whole map, which is the cost that `linearSize` acknowledges:

```scala
import dysasymptotics.linearSize

val part = dag.subgraph(Set(8, 4, 2))
```

### Editing in place

A graph is sometimes built up one edge at a time, as when a build system discovers tasks or a
scheduler accepts jobs. A `Topology` keeps its topological order valid as edges arrive. A new
edge disturbs only the nodes between its two ends.

A `Topology` is a mutable value with one exclusive owner. It is edited through its methods,
which do not produce new values:

```scala
val topology = Topology[Text]()
topology.add(t"app", t"lib")
topology.add(t"lib", t"core")
```

```scala fuel
run
##
Out.println(topology.linearized.inspect)
```

An edge which would close a cycle is refused with a `Dag.Error`, and the graph is left as it
was:

```scala fuel
typecheck
##
topology.add(t"core", t"app")
```

When the editing is done, the topology is given up. `Topology.freeze(topology)` and
`Dag(topology)` both consume it, so the result can never be edited behind its back. A `Dag`
thaws into a `Topology` with `thaw`, and `snapshot` takes a `Dag` without giving up the handle.

### Partial orders

A `Hasse` diagram takes a set and an ordering relation, and computes the covering structure: for
each element, the elements immediately above and below it, with every implied comparison
dropped. These are the divisors of 12, ordered by divisibility:

```scala
val divisors = Hasse(Set(1, 2, 3, 4, 6, 12))((a, b) => b % a == 0)
```

```scala fuel
run
##
Out.println(divisors.parents(2).inspect)
Out.println(divisors.children(6).inspect)
Out.println(divisors.maxima.inspect)
```

A `Hasse` diagram is itself a graph, in which each element points at the elements it covers. It
linearizes from its bottom to its top, and `divisors.dag` is the same relation as a `Dag`.

### Layering

`layered` arranges a `Dag` for drawing. Each node is assigned to a layer, with dependencies
above dependents. The nodes within each layer are ordered so that the lines which join adjacent
layers cross as rarely as possible. This is the layered approach of Sugiyama, which Graphviz's
`dot` uses.

```scala
val layering = dag.layered
```

A node with no dependencies is in the first layer. Node `8` is two layers below, one for each
dependency on its longest path. No link in this graph crosses another:

```scala fuel
run
##
Out.println(layering.rank(2).show)
Out.println(layering.rank(8).show)
Out.println(layering.crossings.show)
```

`layering.layers(1)` gives the vertices of the second layer in their drawn order, and
`layering.links(0)` gives the positions joined between the first two layers. An edge which spans
more than one layer passes through a `Layering.Vertex.Virtual` in each layer between, so every
link joins adjacent layers.

By default a node sits one layer below its deepest dependency, so every node without
dependencies shares the first layer:

```scala
val deep = Dag(1 -> Set(), 5 -> Set(), 2 -> Set(1), 3 -> Set(2, 5))
```

```scala fuel
run
##
Out.println(deep.layered.rank(5).show)
```

`rankings.balancedRanking` instead pulls each node which has dependents down to the layer above
its shallowest dependent. That shortens the long edge from a source which feeds only something
deep. Node `5` feeds only node `3`, two layers down, so it sinks to the layer above:

```scala fuel
run
##
locally:
  import rankings.balancedRanking
  Out.println(deep.layered.rank(5).show)
```

### Drawing

[Trees](trees.md#graphs) draws a graph as text, in lanes, in layers or as a dense grid.

A `Dag` of text is also rendered as
[DOT](https://en.wikipedia.org/wiki/DOT_(graph_description_language)), ready for
[Graphviz](https://graphviz.org/):

```scala fuel
run
##
Out.println(dag.map(_.show).acyclic.dot.serialize)
```

### Related

- [Trees](trees.md) draws trees and graphs as text.
- [Collections](collections.md) describes the vocabulary which a graph shares, and the
  acknowledgements of cost in `dysasymptotics`.
- [Concurrency](concurrency.md) runs the tasks which a dependency graph orders.
- [Impossible states](../philosophy/impossible-states.md) explains why a `Dag` cannot hold a
  cycle.
- [Charts](charts.md) draws a graph as a doughnut, and [compiler](compiler.md) builds a toolchain as one.
