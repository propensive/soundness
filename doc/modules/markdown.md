## Markdown

### About

[Markdown](https://commonmark.org/) is the plain-text format for prose in readmes, comments
and documentation. It is parsed, following the CommonMark specification, into a typed tree.
The tree keeps content at the level of blocks, such as headings, paragraphs and lists,
distinct from the inline content within them, such as emphasis, links and code spans. From
that tree, HTML is produced, the Markdown is written back out, or styled output is produced
for a terminal.

Block content and inline content are different types. A `Markdown of Layout`, which is a
whole document, and a `Markdown of Prose`, which is a run of inline content, cannot be
confused. Each becomes the kind of [HTML](html.md#precise-types) which is valid in its
place: flow content for the one, and phrasing content for the other.

### On Markdown

Markdown has two levels, which a single untyped tree tends to blur. A paragraph, and the
emphasis inside it, are not interchangeable. A heading may contain a link, but a link may
not contain a heading. A representation which treats every node alike lets such nonsense be
built, and leaves the check to whatever consumes the tree.

Here the levels are kept apart in the types. The structure of blocks is the enumeration
`Layout`, the structure of inline content is the enumeration `Prose`, and the type of a
document records which it holds. Parsing gives a complete CommonMark tree, and converting it
to HTML gives exactly the content which each node may be. A document which is parsed to a
typed tree, and not converted straight to HTML, is what makes it
[composable](../philosophy/composability.md) with the rest of a program.

Everything comes from the `soundness` package. The last import lets the samples print their
results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Parsing

`Parser.parse` reads Markdown text into a document, which is a `Markdown of Layout`:

```scala
val document = Parser.parse(t"# Title\n\nSome **bold** text.")
```

Markdown is also parsed incrementally, from a [stream](streams.md), reading lines as they
arrive, without holding the source. Only the tree is retained, and the raw text of each
paragraph, which is parsed later. A long document is therefore parsed without ever being
held whole:

```scala
import dysasymptotics.linearSize

val readme = t"# Readme\n\nThe first paragraph.\n\nThe second."
```

```scala fuel
run
##
Out.println(Parser.parse(readme.stream).children.size.show)
Out.println(readme.read[Markdown of Layout].children.size.show)
```

The whole CommonMark conformance suite passes through the streaming entry, a character at a
time, so a document which is split at any point parses to the same tree as the whole text.

### Rendering to HTML

`html` converts a parsed document to HTML, and `show` writes the result as text. A document
of blocks becomes flow content, and the inline content within it becomes phrasing content,
so the output is well-formed by construction:

```scala fuel
run
##
Out.println(document.html.show)
```

### The syntax tree

The `children` of a document are `Layout` nodes, and each has its inline content as `Prose`.
A document can therefore be inspected, or transformed, by matching on the tree:

```scala
val heading = document.children.head match
  case Layout.Heading(_, level, Prose.Textual(text)) => (level, text)
  case _                                             => (0, t"")
```

```scala fuel
run
##
Out.println(heading.inspect)
```

The block nodes are the cases of `Layout`, and each has the line on which it starts:

```tabular
column  Layout
column  Is
row  `Heading`  a heading, with its level from 1 to 6
row  `Paragraph`  a paragraph
row  `BlockQuote`  a block quote
row  `OrderedList`  an ordered list
row  `BulletList`  a bullet list
row  `CodeBlock`  a code block, with its info string
row  `ThematicBreak`  a thematic break
row  `HtmlBlock`  a block of raw HTML
```

The inline nodes are the cases of `Prose`:

```tabular
column  Prose
column  Is
row  `Textual`  plain text
row  `Emphasis`  emphasis
row  `Strong`  strong emphasis
row  `Code`  a code span
row  `Link`  a link, with its destination and optional title
row  `Image`  an image, with its destination and optional title
row  `Softbreak`  a soft line break
row  `Linebreak`  a hard line break
row  `HtmlInline`  inline raw HTML
```

### Writing Markdown back

`show` writes a document back as CommonMark text. A choice of formatting in scope can wrap
the output to a width in columns, to generate Markdown which is readable:

```scala fuel
run
##
locally:
  given formatting: Markdown.Formatting = Markdown.Formatting.bounded(80)
  Out.println(document.show)
```

### Terminal output

`terminal` converts a document to [styled terminal text](terminal.md#styled-text). Headings,
emphasis and code become ANSI styling, for display in a console at a given width:

```scala fuel
run
##
Out.println(document.terminal(width = 80))
```

### Related

- [HTML](html.md) describes the HTML which a document converts to.
- [Terminal](terminal.md) describes the styled text of the terminal form.
- [Syntax highlighting](syntax-highlighting.md) colors the code in a code block.
- [Errors](errors.md#defining-an-error) describes messages, which can be written as
  Markdown.
