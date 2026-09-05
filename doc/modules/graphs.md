## Graphs

### About

A [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) — a set of nodes
with dependencies and no cycles — is the shape of build systems, task schedules, type hierarchies
and package dependencies. Soundness represents one as a `Dag`, an immutable value with the
operations such graphs need: topological ordering, reachability, transitive closure and reduction,
and the monadic `map`, `flatMap` and `filter` that let a graph be transformed like a collection.

Alongside it, a `Hasse` diagram derives the covering structure of a
[partial order](https://en.wikipedia.org/wiki/Partially_ordered_set) — which elements sit directly
above and below which — and a `Dag` of names renders to
[DOT](https://en.wikipedia.org/wiki/DOT_(graph_description_language)) for Graphviz to draw.

### On acyclic graphs

Dependency structures accumulate the same needs everywhere they appear: an order in which to
process the nodes, the set a change can reach, the removal of redundant edges. General-purpose
graph libraries carry the weight of arbitrary graphs — cycles included — and hand back algorithms
rather than values; more often, projects hand-roll a topological sort over a `Map` and inherit its
edge cases.

A `Dag` is a value: immutable, transformable, and specific to the acyclic case, so a cycle is a
typed error rather than an infinite loop. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Building a graph

A `Dag` is built from edges, from nodes with their dependency sets, or by exploring outward from a
node through a dependency function:

```scala
val dag = Dag(8 -> 4, 8 -> 6, 6 -> 3, 6 -> 2, 4 -> 2)

12.explore(n => (1 until n).filter(n % _ == 0).to(Set))
// the divisibility graph beneath 12
```

### Ordering and reachability

`sorted` gives a [topological order](https://en.wikipedia.org/wiki/Topological_sorting) — every
node after its dependencies — and reachability queries slice the graph around a node:

```scala
dag.sorted             // dependencies before dependents
dag.reachable(8)       // everything 8 depends on, transitively
dag.descendants(8)     // the sub-graph beneath 8
dag.invert             // the graph with every edge reversed
```

`ancestors` is the counterpart of `descendants`, giving what depends on a node rather than what it
depends on, and `lineage` gives both together — the node's whole causal neighbourhood, which is
what "why is this here, and what breaks if I remove it" asks for.

`sources` gives the nodes with no dependencies, which is where a build or an installation starts,
and `subgraph` restricts the graph to a chosen set of nodes.

Everything that could encounter a cycle says so: `sorted`, `reachable`, `descendants` and their
kin raise a `Dag.Error`, since a cyclic graph has no topological order and no finite reachable set.
`hasCycle` asks the question directly, for code that would rather check than handle.

### Folding over a graph

A traversal computes a value for every node from the values of the nodes it depends on, in an
order that guarantees the dependencies are computed first:

```scala
dag.traversal[Int]((childValues, node) => childValues.sum + 1)
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

### Transforming

`map`, `flatMap` and `filter` treat the graph as a collection whose structure follows the values:
mapping renames the nodes, flat-mapping substitutes a graph for each node and knits the edges
together, and filtering removes nodes while preserving the connectivity through them:

```scala
dag.map(_.show)                 // a Dag[Text] of the same shape
dag.filter(_ != 6)              // 8 now depends directly on 3 and 2
```

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

### Drawing

A `Dag` of text renders to DOT, ready for [Graphviz](https://graphviz.org/):

```scala
dag.map(_.show).dot.serialize   // a DOT digraph as Text
```
