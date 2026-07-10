## Markdown

### About

[Markdown](https://commonmark.org/) is the plain-text prose format of readmes, comments and
documentation. Soundness parses it, following the CommonMark specification, into a typed syntax tree
that keeps block-level content — headings, paragraphs, lists — distinct from the inline content —
emphasis, links, code spans — within them. From that tree it renders HTML, writes the Markdown back
out, or produces styled output for a terminal.

Because block and inline content are different types, a `Markdown of Layout` (a whole document) and a
`Markdown of Prose` (a run of inline content) cannot be confused, and each renders to the kind of
[HTML](html.md) that is valid in its place — flow content for the one, phrasing content for the
other.

### On Markdown

Markdown has two levels that a single untyped tree tends to blur. A paragraph and the emphasis inside
it are not interchangeable: a heading may contain a link, but a link may not contain a heading. A
representation that treats every node alike lets such nonsense be built, and pushes the check to
whatever consumes the tree.

Soundness keeps the levels apart in the types. The block structure is the `Layout` enumeration and the
inline structure the `Prose` enumeration, and a document's type records which it holds. Parsing
produces a complete CommonMark tree, and rendering it to HTML yields exactly the content model each
node belongs to. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Parsing

`Parser.parse` reads Markdown text into a document — a `Markdown of Layout`:

```scala
val document = Parser.parse(t"# Title\n\nSome **bold** text.")
```

### Rendering to HTML

A parsed document renders to HTML with `html`, and the result shows as an HTML string. A block
document becomes flow content, and the inline content within it becomes phrasing content, so the
output is well-formed by construction:

```scala
Parser.parse(t"# Title\n").html.show
// t"<h1>Title</h1>"
```

### The syntax tree

The document's `children` are `Layout` nodes, and each carries its inline content as `Prose`, so a
document can be inspected or transformed by matching on the tree:

```scala
document.children.head match
  case Layout.Heading(_, level, Prose.Textual(text)) => (level, text)
  case _                                             => (0, t"")
```

`Layout` covers headings, paragraphs, block quotes, ordered and bullet lists, code blocks and
thematic breaks; `Prose` covers emphasis, strong emphasis, code spans, links, images and line
breaks.

### Writing Markdown back

A document renders back to CommonMark text with `show`. A formatting choice in scope can wrap the
output to a column width, for generating readable Markdown:

```scala
given Markdown.Formatting = Markdown.Formatting.bounded(80)

document.show   // the document as wrapped Markdown source
```

### Terminal output

A document also renders to styled terminal text with `terminal`, turning headings, emphasis and code
into ANSI styling for display in a console at a given width:

```scala
document.terminal(width = 80)
```
