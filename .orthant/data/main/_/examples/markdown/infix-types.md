# A document, or a run of inline content

`Markdown of Layout` is a whole document of blocks, and `Markdown of Prose` is a run of inline
content. The word after `of` is part of the type, so one is not accepted where the other is
wanted.

```scala fuel
compile
frame
  label A document of blocks
  highlight Markdown..Layout  headings, paragraphs, lists
  code
      val document: Markdown of Layout = Parser.parse(t"Pursued by a *bear*.")
frame
  label A run of inline content
  highlight Markdown..Prose  text, emphasis, links
  code
      val document: Markdown of Layout = Parser.parse(t"Pursued by a *bear*.")
      val phrase: Markdown of Prose = Markdown(Prose.Emphasis(Prose.Textual(t"bear")))
frame
  label Inline content, taken for a document
  errors show
  code
      val phrase: Markdown of Prose = Markdown(Prose.Emphasis(Prose.Textual(t"bear")))
      val document: Markdown of Layout = phrase
##
```

[Markdown](../../soundness/modules/markdown.md#on-markdown) explains the two levels, and
[infix types](../../philosophy/infix-types.md) the word `of`.
