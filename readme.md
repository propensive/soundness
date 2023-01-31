[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/dendrology/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/dendrology/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Dendrology

_Dendrology_ provides a single method, `drawTree`, to produce a
line-by-line visual representation of tree-structured data in a monospaced
font, typically in a console, but potentially in any grid-based layout.

## Features

- display any tree-structured data as a tree in a console
- input data can be of any type whose children can be accessed recursively
- output is a linearized sequence of any type
- input is processed lazily, and output is a stream
- can be easily adapted to any grid-like layout, e.g. an HTML table


## Availability

Dendrology has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

To convert some data in a tree-like structure into visual tree, we need a few
things:
 - a type, `N`, which represents each node in the tree,
 - a type, `L`, for each line of output (often just a string),
 - a way to access the children of each node, as a `Seq[N]`, called `getChildren`,
 - a way to construct each line, `L`, from its node plus a sequence of "tiles"
   representing the lines and spaces of the tree layout, called `mkLine`, and
 - the data to convert.

The algorithm performs a depth-first traversal of the data, mapping each node
to a line, and flattening the data in the process. The output will be a
`LazyList[L]`. This traversal and conversion would be trivial, except for the
additional `List[TreeTile]` that is provided for the construction of each line.

The `List[TreeTile]` parameter is a sequence of tiles representing the lines
and spacing needed to render the lines making up the tree, intended to be
printed left-to-right, and left-aligned, with the variable number of elements
in the sequence corresponding to the depth of that node in the tree, and
effectively providing an appropriate indentation for each node, which would
normally be rendered immediately after the tiles.

For example, the lambda,
```scala
(tiles, node) -> t"${tiles.map(_.show).join}> ${node.title}"
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

Calling `drawText` with appropriate `getChildren` and this `mkLine`
lambda will produce a `LazyList[Text]` instance which could, for example, be
printed to standard output, like so:

```scala
import dendrology.*
import treeStyles.rounded

def mkLine(tiles: List[TreeTile], node: Node): Text =
  t"${tiles.map(_.show).join}> ${node.title}"

val lines = drawText(_.children, mkLine)(myNodes)
lines.foreach(println(_))
```

### Laziness

The `drawTree` implementation accesses the tree data structure mostly
lazily, but _does_ need to know the number of elements in each ancestor of the
current node, but does not need to know anything about the descendants of
subsequent nodes in the traversal until they are reached in their natural
order.



## Related Projects

The following _Scala One_ libraries are dependencies of _Dendrology_:

[![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp;

The following _Scala One_ libraries are dependents of _Dendrology_:

[![Chiaroscuro](https://github.com/propensive/chiaroscuro/raw/main/doc/images/128x128.png)](https://github.com/propensive/chiaroscuro/) &nbsp; [![Hyperbole](https://github.com/propensive/hyperbole/raw/main/doc/images/128x128.png)](https://github.com/propensive/hyperbole/) &nbsp;

## Status

Dendrology is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Dendrology is designed to be _small_. Its entire source code currently consists
of 23 lines of code.

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

## License

Dendrology is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
