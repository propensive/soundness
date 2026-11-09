## Trees

### About

A hierarchy, such as a file listing or an organization, is drawn as text with box-drawing
characters. A `TreeDiagram` draws a tree. The DAG diagrams draw [graphs](graphs.md) whose nodes
may have several parents, in a dense grid or in the lanes familiar from `git log --graph`.

Each diagram is a value which produces lines. Its output is printed, styled or embedded like any
other text.

### On drawing hierarchies

The `└─` and `├─` of a directory listing look trivial, and are not. The prefix of each line
depends on which of its ancestors have later siblings, so the state of the drawing is threaded
through the whole traversal. A directed acyclic graph is harder. A node with two parents cannot
be drawn as a tree at all, and laying out the lanes which connect the nodes is a real algorithm.

Layout and rendering are kept apart. A diagram computes rows of *tiles*, and a style maps each
tile to characters. The same layout is therefore drawn in Unicode or in ASCII, plain or styled.
This is [decoupling](../philosophy/decoupling.md) of structure from appearance.

Everything comes from the `soundness` package. The other imports choose a style for trees, and
let the samples print their results:

```scala
import soundness.*
import treeStyles.squareTreeStyle
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
```

### Trees

A tree is given as its roots, and a way to reach the children of any node. Here the way is a
function, `_.children`:

```scala
case class Taxon(name: Text, children: List[Taxon] = Nil)

val animalia = Taxon
  ( t"animalia",
    List(Taxon(t"chordata", List(Taxon(t"mammalia"))), Taxon(t"arthropoda")) )

val diagram = TreeDiagram.by[Taxon](_.children)(animalia)
```

`render` takes a function from a node to its label, and gives the lines to print:

```scala fuel
run
##
diagram.render(_.name).each(Out.println(_))
```

Another style draws the same diagram differently. `roundedTreeStyle` curves the corners:

```scala fuel
run
##
locally:
  import treeStyles.roundedTreeStyle
  diagram.render(_.name).each(Out.println(_))
```

`asciiTreeStyle` uses only ASCII characters, for output which must survive the plainest
terminal:

```scala fuel
run
##
locally:
  import treeStyles.asciiTreeStyle
  diagram.render(_.name).each(Out.println(_))
```

### Graphs

A [`Dag`](graphs.md) is drawn in two layouts. `DagDiagram` is the dense form, with every edge
drawn in a grid of connecting tiles. `LaneDagDiagram` is the lane form, which is how the history
of a repository is usually drawn: nodes are bullets on vertical lanes, and edges flow between
them.

```scala
import laneDagStyles.boxDrawingLaneDagStyle
import textMetrics.uniformMetric
import strategies.throwUnsafely

val history = Dag(t"C" -> Set(t"B"), t"B" -> Set(t"A"), t"A" -> Set())
```

```scala fuel
run
##
LaneDagDiagram(history).render(node => t" $node").each(Out.println(_))
```

A history which branches and merges needs a second lane:

```scala
val merged = Dag
  ( t"merge" -> Set(t"feature", t"fix"),
    t"feature" -> Set(t"base"),
    t"fix" -> Set(t"base"),
    t"base" -> Set() )
```

```scala fuel
run
##
LaneDagDiagram(merged).render(node => t" $node").each(Out.println(_))
```

A glyph function, given per node, marks chosen nodes such as the current commit or a failing
build. `compact` tightens the layout. `LayeredDagDiagram` arranges the nodes in levels instead
of lanes.

### Structure and style apart

A diagram exposes its rows: the tiles of each line, and the node which the line is for. A
consumer can therefore render the lines itself, coloring the labels of one kind of node or
adding a note to another. The box-drawing characters remain the business of the style, and the
meaning remains the caller's.

```scala
import dysasymptotics.linearSize
```

```scala fuel
run
##
diagram.map { tiles => node => t"${tiles.size} ${node.name}" }.each(Out.println(_))
```

`nodes` gives the node of each line, in drawing order, and `tiles` gives the tiles of each line.
Here each label is preceded by the number of tiles on its line, which is the depth of its node.

A `TreeTile` is one of four things, and a style says what each becomes. A `TextualTreeStyle`
says it with four short pieces of text, which is all that a new style needs:

```tabular
column  Tile
column  Is
column  Argument
column  In `myStyle`
row  `Space`  a space  `space`  two spaces
row  `Last`  a last branch  `last`  `└─`
row  `Branch`  a branch  `branch`  `├─`
row  `Extender`  a line carried down past a node  `extender`  `│` and a space
```

```scala
val myStyle = TextualTreeStyle[Text](t"  ", t"└─", t"├─", t"│ ")
```

A style is parameterized by the type of line, and is not fixed to `Text`. A diagram can
therefore be rendered directly into styled [terminal output](terminal.md), with the lines of the
tree in one color and the labels in another, and with the widths still computed correctly.

### Supplying the tree

A tree is given in one of two ways. `TreeDiagram.by` takes a function from a node to its
children. This suits a structure which is already in hand.

`TreeDiagram.apply` takes only the roots, and finds children through an `Expandable` instance on
the node type. This suits a node type which has one natural meaning of "children": a filesystem
path, a dependency, a syntax node.

```scala fuel
alternative `TreeDiagram.by`
  code
      TreeDiagram.by[Taxon](_.children)(animalia)
alternative `Expandable`
  code
      given Taxon is Expandable = _.children

      TreeDiagram(animalia)
##
```

The lines are produced lazily. A large or deep tree is drawn as far as it is consumed, and no
further.

The laziness has one limit, which belongs to the drawing itself. To render a line, the diagram
must know how many children each *ancestor* of the current node has. A node whose grandparent
has later siblings needs a vertical line running down the grandparent's column. A node whose
grandparent was the last of its siblings needs that column blank. Nothing about the descendants
of *later* nodes is needed until the traversal reaches them, so an infinite tree can still be
drawn.

### Choosing a graph layout

The three graph diagrams answer different questions:

```tabular
column  Diagram
column  Draws
column  Suits
row  `DagDiagram`  every edge, in a grid of connecting tiles  a small graph with complicated edges
row  `LaneDagDiagram`  each node on a vertical lane, as version-control history is drawn  a long graph with few concurrent branches
row  `LayeredDagDiagram`  nodes in levels by depth, ordered within each level so that as few connecting lines cross as possible  showing what depends on what, rather than what happened in what order
```

The levels of a `LayeredDagDiagram` come from the `layered` arrangement of the graph. So
`import rankings.balancedRanking` changes the drawing, as it changes the
[layering](graphs.md).

All three take any graph which has a topological order: a `Dag`, a frozen `Topology` or a
`Hasse`. Drawing one cannot fail. A `Digraph` may hold a cycle, and has no drawing until
`acyclic` has converted it.

### Related

- [Graphs](graphs.md) describes `Dag`, `Digraph` and the layering which these diagrams draw.
- [Terminal](terminal.md) describes the styled text which a diagram can be rendered into.
- [Filesystem](filesystem.md) and [paths](paths.md) describe a common source of trees.
- [Git](git.md) reads the commit history which a lane diagram suits.
- [Tables](tables.md) lays out tabular data as text, in the same spirit.
