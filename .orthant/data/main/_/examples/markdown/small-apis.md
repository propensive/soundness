# One tree, written three ways

A parsed document is a value, and each form of output is a method of it. `html` gives typed
HTML, `show` gives Markdown again, and `terminal` gives styled text for a console, all from the
tree which was parsed once.

```scala fuel
compile
frame
  label Parsed once
  code
      val notice = Parser.parse(t"# Exit\n\nPursued by a *bear*.")
frame
  label As HTML
  highlight html  typed nodes, then text
  code
      val notice = Parser.parse(t"# Exit\n\nPursued by a *bear*.")
      val page: Text = notice.html.show
frame
  label As Markdown, and for a terminal
  highlight terminal(width..40)  styled text, wrapped
  code
      val notice = Parser.parse(t"# Exit\n\nPursued by a *bear*.")
      val page: Text = notice.html.show
      val source: Text = notice.show
      val screen = notice.terminal(width = 40)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(page)
Out.println(source)
```

[Writing Markdown back](../../soundness/modules/markdown.md#writing-markdown-back) and
[terminal output](../../soundness/modules/markdown.md#terminal-output) have the detail.
