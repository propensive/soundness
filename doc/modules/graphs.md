## Graphs

### About

A [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) — a set of nodes
with dependencies and no cycles — is the shape of build systems, task schedules, type hierarchies
and package dependencies. Soundness represents one as a `Dag`, an immutable value with the
operations such graphs need: topological ordering, reachability, transitive closure and reduction,
and editing that keeps the graph acyclic. Beside it sit a `Digraph`, the general directed graph
that may contain a cycle, and a `Topology`, a mutable graph edited in place that keeps a
topological order live as edges arrive — which, frozen, is also the form for graphs built once
and queried often.

All of these, and anything else graph-shaped — a `Hasse` diagram, a `Map` from nodes to their
successors — share one vocabulary through the `Nodal` typeclass, so the same operations apply to
each.

### On acyclic graphs

Dependency structures accumulate the same needs everywhere they appear: an order in which to
process the nodes, the set a change can reach, the removal of redundant edges. General-purpose
graph libraries carry the weight of arbitrary graphs — cycles included — and hand back algorithms
rather than values; more often, projects hand-roll a topological sort over a `Map` and inherit its
edge cases.

A graph that cannot be constructed with a cycle is an [impossible state](../philosophy/impossible-states.md) ruled out at the point of construction.

A `Dag` is a value: immutable, transformable, and acyclic by construction, so its topological
order has no failure mode. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Building a graph

A `Dag` is built from edges, from nodes with their dependency sets, or from a set of nodes and a
function giving each one's dependencies. An edge `a -> b` means `a` depends on `b`, and both ends
of every edge become nodes:

```scala
val dag = Dag(8 -> Set(4, 6), 6 -> Set(3, 2), 4 -> Set(2), 3 -> Set(), 2 -> Set())

Dag(8 -> 4, 8 -> 6, 6 -> 3)   // built from edges alone
```

Each of these checks for a cycle and raises a `Dag.Error` if it finds one. A graph that may hold
a cycle is a `Digraph`, built the same ways without a check, and `acyclic` is the one checked
conversion:

```scala
val graph = Digraph(1 -> 2, 2 -> 3, 3 -> 1)
graph.cycle                  // the cycle, as a list of nodes ending where it began
Digraph(1 -> 2).acyclic      // a Dag
```

Exploring outward from a node through a dependency function materialises a `Digraph`, since
the function may lead back on itself:

```scala
12.explore(n => (1 until n).filter(n % _ == 0)).acyclic   // the divisibility graph beneath 12
```

### Ordering and reachability

`linearized` gives a [topological order](https://en.wikipedia.org/wiki/Topological_sorting) —
every node after its dependencies, and otherwise in the order the nodes were given — and
reachability queries slice the graph around a node:

```scala
dag.linearized         // List(2, 3, 4, 6, 8): dependencies before dependents
dag.reachable(8)       // Set(8, 2, 3, 4, 6): 8 and everything it depends on, transitively
dag.descendants(8)     // the sub-graph beneath 8
dag.invert             // the graph with every edge reversed
```

`sources` are the nodes depending on nothing, where a build or an installation starts, and
`sinks` the nodes nothing depends on. `successors` are a node's dependencies, and `predecessors`
its dependants.

As a collection, a graph is its nodes — in topological order, for a `Dag` — so the collection
vocabulary applies to it through the usual typeclasses: `dag.has(4)`, `dag.size`,
`dag.each(println(_))`, `dag.map(_.toString)` (which answers a `Digraph`, since two nodes may
merge into one and close a cycle).

A `Dag` stores only the forward direction, so finding a node's predecessors means scanning every
edge — a cost out of proportion to the question. The operations that need it (`predecessors`,
`ancestors`, `lineage`, `bypass` and `-`) therefore ask for an acknowledgement, which an import
gives:

```scala
import dysasymptotics.linearScan

dag.predecessors(2)    // Set(4, 6)
dag.ancestors(2)       // the sub-graph that depends on 2
dag.lineage(4)         // both directions together
```

`reachable` raises a `Dag.Error` only for a node the graph does not have; on a `Digraph` it is
total, since reachability is well defined whether or not the graph has a cycle.

### Folding over a graph

A traversal computes a value for every node from the values of the nodes it depends on, in an
order that guarantees the dependencies are computed first:

```scala
dag.traversal[Int]((childValues, node) => childValues.sum + 1)   // one more than the sum beneath
```

This is the shape of most real work over a dependency graph — computing a build order's costs,
propagating a version constraint, accumulating a transitive set — expressed once rather than as a
hand-written recursion with its own visited-set bookkeeping.

### Closure and reduction

The transitive `closure` adds an edge wherever a path exists; the transitive `reduction` removes
every edge already implied by a path, giving the minimal graph with the same reachability — the
form in which a dependency graph is usually drawn:

```scala
dag.closure     // all implied edges made explicit
dag.reduction   // only the essential edges
```

Both are answered through a frozen topology's reachability matrix, which the next section
describes; a graph queried this way more than once is best frozen once.

### Freezing

Mutability is a matter of the capture set, as it is for arrays: a `Topology[node]^` can be
edited, and a `Topology[node]^{}` — the result of `dag.freeze`, or of `Topology.freeze`, which
consumes an editable one — cannot, since its editing methods need an exclusive handle. The
frozen form is the one laid out for querying: both directions of adjacency in dense arrays,
sources and sinks maintained, and a bit matrix of reachability built on first use, so that
whether one node reaches another is a single bit:

```scala
val frozen: Topology[Int]^{} = dag.freeze
frozen.reaches(8, 2)      // true
frozen.predecessors(2)    // Set(4, 6), with no acknowledgement needed
frozen.snapshot           // back to a Dag
```

A frozen topology's `closure`, `reduction` and `reaches` read the matrix, which is why they are
offered only on the frozen form: an edit would make it stale. To edit, take an editable copy
with `Topology(frozen)`, edit, and freeze again.

### Editing

Editing a `Dag` gives a new `Dag` where the edit cannot create a cycle, and a `Digraph` where it
might:

```scala
dag.add(8, 3)              // a new dependency; raises Dag.Error if it would close a cycle
dag.remove(8, 4)           // one edge fewer
dag - 4                    // the node and its edges gone
dag.bypass(4)              // 4 gone, and 8 now depends directly on 2
dag.subgraph(Set(8, 4, 2)) // the edges among those nodes
dag + (2 -> 8)             // a Digraph: this edge closes a cycle
dag.map(_.toString)        // a Digraph of the same shape, since two nodes might merge
```

`bypass` drops a node while rerouting each of its dependants to each of its dependencies, so
every path through it survives; `bypassAll` does the same for a set of nodes, one after another.
`subgraph` keeps only the edges among the chosen nodes, and asks for the `linearSize`
acknowledgement, since a bounded choice of nodes rebuilds the whole map.

### Editing in place

Where a graph is built up or edited incrementally — a build system discovering tasks, a
scheduler accepting jobs — a `Topology` keeps its topological order valid as edges arrive,
touching only the nodes between the two ends of a new edge, and refuses an edge that would close
a cycle. It is a mutable value, exclusively owned, edited through methods rather than by
producing new values:

```scala
val topology: Topology[Text]^ = Topology()
topology.add(t"app", t"lib")
topology.add(t"lib", t"core")
topology.linearized          // List(core, lib, app)
```

An edge that would close a cycle is refused with a `Dag.Error`, and the graph is left as it was:

<!-- doccheck: skip -->
```scala
topology.add(t"core", t"app")   // raises Dag.Error
```

When the editing is done, the topology is given up — `Topology.freeze(topology)` or
`Dag(topology)` consume it — so that the result can never be edited behind its back. A `Dag`
thaws into a `Topology` with `thaw`, and `snapshot` takes a `Dag` without giving up the handle.

### Partial orders

A `Hasse` diagram takes a set and an ordering relation and computes the covering structure — for
each element, the ones immediately above and below, with all implied comparisons dropped. The
divisors of 12, ordered by divisibility:

```scala
val divisors = Hasse(Set(1, 2, 3, 4, 6, 12))((a, b) => b % a == 0)

divisors.parents(2)    // Set(4, 6) — the covers of 2
divisors.children(6)   // Set(2, 3)
divisors.maxima        // Set(12)
```

A `Hasse` diagram is itself a graph, each element pointing at the elements it covers, so it
linearizes from its bottom to its top, and `divisors.dag` is the same relation as a `Dag`.

### Drawing

A `Dag` of text renders to [DOT](https://en.wikipedia.org/wiki/DOT_(graph_description_language)),
ready for [Graphviz](https://graphviz.org/):

```scala
dag.map(_.show).acyclic.dot.serialize   // a DOT digraph as Text
```
