# Prose wrapped for the reader of the source

A paragraph in source code is wrapped and indented to suit the code around it, and none of
that belongs in the output. `txt"…"` joins the lines of a paragraph with single spaces, drops
the indentation, and keeps a blank line as a paragraph break.

```scala fuel
compile
frame
  label One long line
  highlight t"If..mended."  one line, as long as the sentence
  code
      val closing = t"If we shadows have offended, think but this, and all is mended."
frame
  label Wrapped in the source, and not in the result
  highlight txt  line breaks and indentation are removed
  code
      val closing = txt"""If we shadows have offended,
                          think but this,
                          and all is mended."""
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(closing)
```

[Writing text](../../soundness/modules/text.md#writing-text) describes both interpolators.
