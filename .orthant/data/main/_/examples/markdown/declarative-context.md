# The width of written Markdown

Writing a document back as Markdown is `show`, and how its lines are broken is not an argument
to it. A formatting given in scope sets the width, for every document written there.

```scala fuel
compile
frame
  label Lines as long as their paragraphs
  code
      val speech = Parser.parse(t"All the world's a stage, and all the men and women merely players.")
      val text: Text = speech.show
frame
  label Lines wrapped at thirty columns
  highlight Markdown.Formatting.bounded(30)  for every show in scope
  code
      given formatting: Markdown.Formatting = Markdown.Formatting.bounded(30)

      val speech = Parser.parse(t"All the world's a stage, and all the men and women merely players.")
      val text: Text = speech.show
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(text)
```

[Writing Markdown back](../../soundness/modules/markdown.md#writing-markdown-back) has the
rest, and [declarative context](../../philosophy/declarative-context.md) the reasoning.
