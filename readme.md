[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/dendrology/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/dendrology/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Dendrology

__Rendering of trees and DAGs in the console__

_Dendrology_ provides methods for rendering data in a tree or directed acyclic graph
structure as lines, for rendering in a monospaced font, typically in a terminal.

## Features

- display any tree-structured data as a tree in a console
- tree data can be of any type whose children can be accessed recursively
- display directed acyclic graphs in a console
- output is a linearized sequence of any type
- tree input is processed lazily, and output is a stream
- can be easily adapted to any grid-like layout, e.g. an HTML table
- custom textual renderings are available


## Availability

Dendrology has not yet been published as a binary.

## Getting Started

Dendrology can render tree-like structures as text, such as the following,
```
├─● Plantae
├─● Fungi
│ ├─● Rozellomyceta
│ ├─● Aphelidiomyceta
│ └─● Eumycota
├─● Protozoa
├─● Bacteria
└─● Animalia
  └─● Chordata
    └─● Mammalia
      └─● Carnivora
        ├─● Feliadae
        ├─● Canidae
        │ └─● Canis
        └─● Ursidae
```
and DAGs such as this:
```
▪ Any
└─▪ Matchable
  ├─▪ AnyRef
  │ ├─▪ List[Int]
  └─│─│─▪ AnyVal
    │ │ ├─▪ Boolean
    │ │ ├─│─▪ Int
    │ │ └─│─│─▪ Unit
    └─│───│─│─│─▪ String
      └───│─│─│─┴─▪ Null
          └─┴─┴───┴─▪ Nothing
```

Dendrology is versatile, and can represent data in a variety of input and output types, so long as methods are
specified for working with those types.

All Dendrology terms and types are in the `dendrology` package.
```scala
import dendrology.*
```

## Trees

To create a tree, all we need is the root node (or nodes), of some type, and a way to access a node's children
(of the same type). This can then be applied recursively to the root node to fully expand the tree. This
approach has the advantage that it does not require the data to be reshaped into a particular data structure to
be used.

Dendrology makes it possible to define the method for accessing a node's children in two ways: either as a
lambda when a `TreeDiagram` is constructed, like so,
```scala
import anticipation.Text

case class Person(name: Text, age: Int, children: List[Person])
val daughter = Person(t"Jill", 7, List())
val son = Person(t"Jack", 9, List())
val headOfFamily = Person(t"John", 37, List(son, daughter))

val diagram = TreeDiagram.by[Person](person => person.children)(headOfFamily)
```
or alternatively through a contextual instance of the typeclass `Expandable` for the given node type. Types
which are naturally hierarchical can, of course, define their own `Expandable` instances so they can be used in
tree-structures without the need to specify how child nodes should be accessed. For example:
```scala
given Expandable[Person] = _.children
val diagram2 = TreeDiagram(headOfFamily)
```

It's possible to include multiple root nodes as parameters to `TreeDiagram`, which will appear as top-level
siblings in the tree.

Instances of `TreeDiagram` provide a few methods to help with rendering a diagram. The `render` method will
meet most requirements for rendering a tree diagram as a series of lines. The `render` method takes a single
parameter, a lambda for converting from the type of the nodes, `NodeType`, to a serialized type of each line,
`LineType`. Typical choices might be a `NodeType` of some user-defined type like `Person`, and a `LineType` of
`Text`.

For example, we could write,
```scala
import treeStyles.default

val lines = diagram2.render(_.name)
@main
def run(): Unit =
  lines.foreach(Out.println(_))
```

The algorithm performs a depth-first traversal of the data, mapping each node to a line, and flattening the data
in the process. The output will be a `LazyList[LineType]`.

The parameter to the `render` method, `_.name`, will determine the `LineType` is `Text`, which will resolve a
contextual `TreeStyle[Text]`, which is needed to render the horizontal and vertical lines of the diagram as
`Text`. The `dendrology.treeStyles` package provides `default`, `rounded` and `ascii` renderings for `Text` and
other `Textual` types.

In addition to `render`, the method `TreeDiagram#nodes` can recover the node value used to generate each line in
the diagram. A common way to make use of this is to zip it with the output from `render` to get a
`LazyList[(LineType, NodeType)]` which could be used to perform additional post-processing of each line, based
on information from its corresponding node.

### Laziness

The `drawTree` implementation accesses the tree data structure mostly lazily, but _does_ need to know the number
of elements in each ancestor of the current node, yet does not need to know anything about the descendants of
subsequent nodes in the traversal until they are reached in their natural order.

This is necessary because any subsequent siblings of any ancestor nodes will require an additional descending
vertical line to be rendered in the appropriate column of the current line, whereas that vertical line should be
absent for each ancestor that is the last of its siblings.

## Directed Acyclic Graphs

A DAG diagram is represented by the `DagDiagram` class, and should be constructed from a `Dag` instance from
[Acyclicity](https://github.com/propensive/acyclicity/).

For example, given a value `dag`, an instance of `Dag[Person]`, we can construct a new `DagDiagram` with
`DagDiagram(dag)`. Unlike `TreeDiagram`, `DagDiagram` always takes exactly one parameter.

Like `TreeDiagram`, though, `DagDiagram` provides `render` and `nodes` methods with the same purpose. While
`TreeDiagram` returns a `LazyList`, `DagDiagram` cannot (due to the nature of the data it represents) evaluate
lazily, and provides a strict `List`.

Here is the full code used to create the example DAG above:
```scala
import acyclicity.Dag
import gossamer.t
import turbulence.Out, turbulence.stdioSources.jvm
import dagStyles.default

val dag = Dag(
  t"Any"       -> Set(),
  t"Matchable" -> Set(t"Any"),
  t"AnyVal"    -> Set(t"Matchable"),
  t"AnyRef"    -> Set(t"Matchable"),
  t"Unit"      -> Set(t"AnyVal"),
  t"Boolean"   -> Set(t"AnyVal"),
  t"Int"       -> Set(t"AnyVal"),
  t"String"    -> Set(t"AnyRef"),
  t"List[Int]" -> Set(t"AnyRef"),
  t"Null"      -> Set(t"String", t"List[Int]"),
  t"Nothing"   -> Set(t"Null", t"Unit", t"Boolean", t"Int")
)

@main
def run2(): Unit =
  DagDiagram(dag).render { node => t"▪ $node" }.foreach(Out.println(_))
```


## Status

Dendrology is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Dendrology is designed to be _small_. Its entire source code currently consists
of 165 lines of code.

## Building

Dendrology can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Dendrology are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/dendrology/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Dendrology easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Dendrology was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Dendrology_ is the study of trees.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows a pattern of leaves from a tree; trees being the subject of Dendrology.

## License

Dendrology is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
