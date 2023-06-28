Dendrology can render tree-like structures as text, such as the following:
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

To convert some data in a tree-like structure into visual tree, we need a few
things:
 - a type, `NodeType`, which represents each node in the tree,
 - a type, `LineType`, for each line of output (often just a string),
 - a way to access the children of each node, as a `Seq[NodeType]`, called `getChildren`,
 - a way to construct each line, `LineType`, from its node plus a sequence of "tiles"
   representing the lines and spaces of the tree layout, called `mkLine`, and
 - the data to convert.

The algorithm performs a depth-first traversal of the data, mapping each node
to a line, and flattening the data in the process. The output will be a
`LazyList[LineType]`. This traversal and conversion would be trivial, except for the
additional `List[TreeTile]` that is provided for the construction of each line.

The `List[TreeTile]` parameter is a sequence of tiles representing the lines
and spacing needed to render the lines making up the tree, intended to be
printed left-to-right, and left-aligned, with the variable number of elements
in the sequence corresponding to the depth of that node in the tree, and
effectively providing an appropriate indentation for each node, which would
normally be rendered immediately after the tiles.

For example, the lambda,
```scala
(tiles, node) -> t"${tiles.map(_.text).join}> ${node.title}"
```
could define a `mkLine` which returns a `Text` line by appending the `Node`'s
`title` field to the tiles, serialized and joined as text.

Four different kinds of tile are necessary to render any table:
 - a space (`Space`),
 - a vertical bar (`Extender`),
 - a vertical bar with a branch to the right (`Branch`), and
 - an L-shaped final branch (`Last`)

These are defined in the `TreeTile` enumeration, and have a corresponding
`Show` instance to convert them to text, however this requires a contextual
`TreeStyle` instance in scope; a value which defines exactly which characters
should be used to render the tiles as text. Three implementations are provided
in the `dendrology.treeStyles` package: `default`, `rounded` and `ascii`.

Calling `drawTree` with appropriate `getChildren` and this `mkLine`
lambda will produce a `LazyList[Text]` instance which could, for example, be
printed to standard output, like so:

```scala
import dendrology.*
import treeStyles.rounded

def mkLine(tiles: List[TreeTile], node: Node): Text =
  t"${tiles.map(_.text).join}> ${node.title}"

val lines = drawTree(_.children, mkLine)(myNodes)
lines.foreach(println(_))
```

### Laziness

The `drawTree` implementation accesses the tree data structure mostly
lazily, but _does_ need to know the number of elements in each ancestor of the
current node, but does not need to know anything about the descendants of
subsequent nodes in the traversal until they are reached in their natural
order.

This is necessary because any subsequent siblings of any ancestor nodes will
require an additional descending vertical line to be rendered in the
appropriate column of the current line, whereas that vertical line should be
absent for each ancestor that is the last of its siblings.

