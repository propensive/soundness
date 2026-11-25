# A heading has six levels, and holds no blocks

The tree of a Markdown document has one type for blocks, `Layout`, and another for the inline
content within them, `Prose`. A heading's level is one of six numbers, and its content is
prose, so a seventh level and a heading within a link are not values.

```scala fuel
compile
frame
  label A heading of the second level
  code
      val heading = Layout.Heading(Prim, 2, Prose.Textual(t"Dramatis Personae"))
frame
  label A seventh level
  errors show
  code
      val heading = Layout.Heading(Prim, 7, Prose.Textual(t"Dramatis Personae"))
frame
  label A heading inside a link
  errors show
  code
      val heading = Layout.Heading(Prim, 2, Prose.Textual(t"Dramatis Personae"))
      val link = Prose.Link(t"https://example.com/cast", Unset, heading)
##
```

[The syntax tree](../../soundness/modules/markdown.md#the-syntax-tree) lists the nodes of each
kind, and [impossible states](../../philosophy/impossible-states.md) argues for types like
these.
