[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/acyclicity/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/acyclicity/actions)
[<img src="https://img.shields.io/badge/gitter-discuss-f00762?style=for-the-badge" height="24">](https://gitter.im/propensive/acyclicity)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/CHCPjERybv)
[<img src="https://img.shields.io/matrix/propensive.acyclicity:matrix.org?label=MATRIX&color=0dbd8b&style=for-the-badge" height="24">](https://app.element.io/#/room/#propensive.acyclicity:matrix.org)
[<img src="https://img.shields.io/twitter/follow/propensive?color=%2300acee&label=TWITTER&style=for-the-badge" height="24">](https://twitter.com/propensive)
[<img src="https://img.shields.io/maven-central/v/com.propensive/acyclicity-core_2.12?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/acyclicity-core_2.12)
[<img src="https://vent.dev/badge/propensive/acyclicity" height="24">](https://vent.dev/)

<img src="/doc/images/github.png" valign="middle">

# Acyclicity

Acyclicity provides a single data structure, `Dag[T]`, representing a graph of nodes of type `T`, with monadic operations and several other utility methods.

## Features

- provides a simple immutable monadic implementation of a DAG
- implements `map`, `flatMap` and `filter` for `Dag`s
- can deduce a partial order on a graph
- can find the transitive closure, transitive reduction and inverse of a graph
- methods for addition and subtraction of graph nodes


## Getting Started

Acyclicity provides a monadic representation of a directed, acyclic graph (DAG) called `Dag`.

The `Dag[T]` type represents a mapping from nodes of type `T` to zero, one or many other nodes in the graph, and can be constructed by providing a `Map[T, Set[T]]` or calling,
```scala
Dag.from(nodes)(fn)
```
where `nodes` is a `Set` of nodes, and `fn` is a function from each node to its dependencies.

For example,
```scala
val factors = Dag(Map(
  30 -> Set(2, 3, 4, 5, 10, 15),
  15 -> Set(5, 3),
  10 -> Set(5, 2),
  5 -> Set(),
  3 -> Set(),
  2 -> Set()
))
```

## Monadic Operations

A `Dag[T]` may be mapped to a `Dag[S]` with a function `T => S`, like so:
```scala
factors.map(_*10)
```

Care should be taken when more than one node in the domain maps to a single node in the range, but both incoming and outgoing edges will be merged in such cases.

It's also possible to `flatMap` with a function `T => Dag[S]`. This will replace every node of type `T` with a subgraph, `Dag[S]` with incoming edges attached to all source nodes of the subgraph, and pre-existing outgoing edges attached to all destination nodes of the subgraph.

A `Dag[T]` may also be filtered with a predicate, `T => Boolean`. The removal of a node during filtering will reattach every incoming edge to every outgoing edge of that node.

## Other Operations

The method `Dag#simplify` will calculate the transitive reduction of the graph, removing any direct edge between two nodes when transitive edges exist between those nodes.

The duel of this operation is the transitive closure, which adds direct edges between each pair of nodes between which a transitive path exists. This is available with the `Dag#closure` method.

A list of nodes will be returned in topologically-sorted order by calling `Dag#sorted`.

## Limitations

This library is incomplete, inadequately tested and subject to further development, and should only be used by developers who do not mind examining the source code to diagnose unexpected behavior.


## Status

Acyclicity is classified as __embryonic__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without guarantee of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement of designs
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Acyclicity&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/acyclicity`.
```
fury layer clone -i propensive/acyclicity
```
or imported into an existing layer with,
```
fury layer import -i propensive/acyclicity
```
A binary is available on Maven Central as `com.propensive:acyclicity-core_<scala-version>:0.1.0`. This may be added
to an [sbt](https://www.scala-sbt.org/) build with:
```
libraryDependencies += "com.propensive" %% "acyclicity-core" % "0.1.0"
```

## Contributing

Contributors to Acyclicity are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/acyclicity/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Acyclicity easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
the answers.

## Author

Acyclicity was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).



## License

Acyclicity is copyright &copy; 2021-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
