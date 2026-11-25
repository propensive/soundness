# Markdown which becomes typed HTML

A parsed document is a tree, and `html` converts the tree to the HTML library's own nodes.
Blocks become flow content and inline content becomes phrasing content, so the page is
well-formed before a character of it is written.

```scala fuel
compile
frame
  label A document, parsed
  code
      val notice = Parser.parse(t"# Exit\n\nPursued by a *bear*.")
frame
  label The same document, as HTML
  highlight html  flow content
  code
      val notice = Parser.parse(t"# Exit\n\nPursued by a *bear*.")
      val page = notice.html
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(page.show)
```

[Rendering to HTML](../../soundness/modules/markdown.md#rendering-to-html) has the detail, and
[HTML](../../soundness/modules/html.md#precise-types) the types of content.
