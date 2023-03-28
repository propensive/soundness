Acyclicity provides a monadic representation of a directed, acyclic graph (DAG) called `Dag`, and support for
generating [DOT](https://bit.ly/3vFumLW) files which can be rendered with tools such as
[GraphViz](https://graphviz.org/).

The `Dag[T]` type represents a mapping from nodes of type `T` to zero, one or many other nodes in the graph, and
can be constructed by providing the mapping from each node to its `Set` of dependent nodes, or by calling,
```scala
Dag.from(nodes)(fn)
```
where `nodes` is a `Set` of nodes, and `fn` is a function from each node to its dependencies.

For example,
```scala
val factors = Dag(
  30 -> Set(2, 3, 4, 5, 10, 15),
  15 -> Set(5, 3),
  10 -> Set(5, 2),
  5 -> Set(),
  3 -> Set(),
  2 -> Set()
)
```

### Monadic Operations

A `Dag[T]` may be mapped to a `Dag[S]` with a function `T => S`, like so:
```scala
factors.map(_*10)
```

Care should be taken when more than one node in the domain maps to a single node in the range, but both incoming
and outgoing edges will be merged in such cases.

It's also possible to `flatMap` with a function `T => Dag[S]`. This will replace every node of type `T` with a
subgraph, `Dag[S]` with incoming edges attached to all source nodes of the subgraph, and pre-existing outgoing
edges attached to all destination nodes of the subgraph.

A `Dag[T]` may also be filtered with a predicate, `T => Boolean`. The removal of a node during filtering will
reattach every incoming edge to every outgoing edge of that node.

### Other Operations

The method `Dag#reduction` will calculate the transitive reduction of the graph, removing any direct edge
between two nodes when transitive edges exist between those nodes.

The dual of this operation is the transitive closure, which adds direct edges between each pair of nodes between
which a transitive path exists. This is available with the `Dag#closure` method.

A list of nodes will be returned in topologically-sorted order by calling `Dag#sorted`.

### DOT output

An extension method, `dot`, on `Dag`s of `String`s will produce a `Dot` instance, an AST of the DOT code
necessary to render a graph. This can then be serialized to a `String` with the `serialize` method.

Typical usage would be to first convert a `Dag[T]` to a `Dag[String]`, then produce the `Dot` instance and
serialize it, for example:
```scala
println(dag.map(_.name).dot.serialize)
```

### Limitations

This library is incomplete, inadequately tested and subject to further development, and is recommended to be
used by developers who do not mind examining the source code to diagnose unexpected behavior.

