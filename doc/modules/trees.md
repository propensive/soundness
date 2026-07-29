## Trees

### About

Hierarchies — file listings, dependency graphs, organisational structures — draw as text with
box-drawing characters: a `TreeDiagram` renders a tree, and the DAG diagrams render
[graphs](graphs.md) whose nodes may have several parents, in the dense style or the lane style
familiar from `git log --graph`. Each diagram is a value producing lines, so the output prints,
styles or embeds like any other text.

### On drawing hierarchies

The `└─` and `├─` of a directory listing look trivial and are not: each line's prefix depends on
which of its ancestors have later siblings, so the drawing state threads through the whole
traversal. Directed acyclic graphs are harder still — a node with two parents cannot be drawn as a
tree at all, and laying out the connecting lanes is a real algorithm.

Soundness separates the layout from the rendering: a diagram computes rows of *tiles*, and a style
maps tiles to characters, so the same layout renders in Unicode or ASCII, plain or styled.
Everything comes from the `soundness` package:

```scala
import soundness.*
import treeStyles.defaultTreeStyle
```

### Trees

A tree is supplied as its roots and a way to reach children — a function, or an `Expandable`
instance on the node type — and renders through a function from node to label:

```scala
case class Taxon(name: Text, children: List[Taxon] = Nil)

val diagram = TreeDiagram.by[Taxon](_.children)(animalia)

diagram.render(_.name).each(Out.println(_))
// animalia
// ├─chordata
// │ └─mammalia
// └─arthropoda
```

`roundedTreeStyle` curves the corners and `asciiTreeStyle` uses only ASCII, for output that must
survive the plainest terminals.

### Graphs

A [Dag](graphs.md) draws in two layouts. `DagDiagram` is the dense form, every edge drawn in a
grid of connecting tiles; `LaneDagDiagram` is the lane form — nodes as bullets on vertical lanes,
edges flowing between them — the shape version-control history is drawn in:

```scala
import laneDagStyles.defaultLaneDagStyle
import strategies.throwUnsafely

val history = Dag(t"C" -> Set(t"B"), t"B" -> Set(t"A"), t"A" -> Set())

LaneDagDiagram(history).render(node => t" $node").each(Out.println(_))
// ●  A
// │
// ●  B
// │
// ●  C
```

A per-node glyph function marks chosen nodes — the current commit, a failing build — and `compact`
tightens the layout; `LayeredDagDiagram` arranges nodes in levels instead of lanes.

### Structure and style apart

A diagram exposes its rows — the tiles and the node of each line — so a consumer can render lines
itself: colored labels for one kind of node, annotations appended to another. The box-drawing
characters stay the style's business, and the meaning stays the caller's.

```scala
diagram.nodes    // the node of each line, in drawing order
diagram.tiles    // the tiles of each line
diagram.map { tiles => node => renderMyself(tiles, node) }
```

A `TreeTile` is one of four things: a branch, a last branch, an extender carrying a line down past
a node, or a space. A style says what each becomes, and a `TextualTreeStyle` says it as four short
pieces of text, which is all a new style needs:

```scala
val myStyle = TextualTreeStyle[Text](t"  ", t"└─", t"├─", t"│ ")
```

Because the style is parameterized by the line type rather than fixed to `Text`, a diagram renders
directly into styled [terminal output](terminal.md), with the tree furniture in one colour and the
labels in another, and the widths still computed correctly.

### Supplying the tree

A tree can be given in two ways. `TreeDiagram.by` takes a function from node to children, which
suits a structure that is already in hand:

```scala
TreeDiagram.by[Taxon](_.children)(root)
```

`TreeDiagram.apply` instead takes roots and finds children through an `Expandable` instance on the
node type, which suits a node type whose children are what it means by children — a filesystem
path, a dependency, a syntax node:

```scala
given Taxon is Expandable = _.children

TreeDiagram(root)
```

The lines are produced lazily, so a large or deep tree is drawn as far as it is consumed rather
than all at once.

### Choosing a graph layout

The three graph diagrams answer different questions. `DagDiagram` is dense: every edge is drawn,
in a grid of connecting tiles, which is what a small graph with complicated edges needs.
`LaneDagDiagram` puts each node on a vertical lane, as version-control history is drawn, which
suits a long graph with few concurrent branches. `LayeredDagDiagram` arranges nodes into levels
by depth, which suits showing what depends on what rather than what happened in what order.

All three take a `Dag`, and all three raise a `DagError` where the graph is not acyclic — a cycle
has no drawing, so it is reported rather than approximated.
