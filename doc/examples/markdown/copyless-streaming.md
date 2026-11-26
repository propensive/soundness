# A document parsed as it arrives

Markdown is parsed from a stream a line at a time, and the source is never held whole. The
tree is the same as the one which the whole text gives, wherever the stream is divided.

```scala fuel
compile
frame
  label A document, parsed from text
  code
      import strategies.throwUnsafely

      val readme = t"# Readme\n\nThe first paragraph.\n\nThe second."
      val whole = Parser.parse(readme)
frame
  label The same document, parsed from a stream
  highlight readme.stream  read a line at a time
  code
      import strategies.throwUnsafely

      val readme = t"# Readme\n\nThe first paragraph.\n\nThe second."
      val whole = Parser.parse(readme)
      val streamed = Parser.parse(readme.stream)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(streamed.html.show)
Out.println((whole.html.show == streamed.html.show).inspect)
```

[Parsing](../../soundness/modules/markdown.md#parsing) says what is kept while a long document
is read.
