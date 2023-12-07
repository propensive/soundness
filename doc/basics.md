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

## Trees

To create a tree, all we need is the root node (or nodes), of some type, and a way to access a node's children
(of the same type). This can then be applied recursively to the root node to fully expand the tree. This
approach has the advantage that it does not require the data to be reshaped into a particular data structure to
be used.

Dendrology makes it possible to define the method for accessing a node's children in two ways: either as a
lambda when a `TreeDiagram` is constructed, like so,
```scala
val diagram = TreeDiagram.by[Person](person => person.children)(headOfFamily)
```
or alternatively through a contextual instance of the typeclass `Expandable` for the given node type. Types
which are naturally hierarchical can, of course, define their own `Expandable` instances so they can be used in
tree-structures without the need to specify how child nodes should be accessed. For example:
```scala
given Expandable[Person] = _.children
val diagram = TreeDiagram(headOfFamily)
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
given Expandable[Person] = _.children
val diagram = TreeDiagram(headOfFamily)

import treeStyles.default
val lines = diagram.render(_.name)
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
import acyclicity.*
import dendrology.*, dagStyles.default

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

DagDiagram(dag).render { node => t"▪ $node" }.foreach(Out.println(_))
```