# Completions, where there is a caret

Highlighted source has completions only if it was highlighted with a caret. The caret is an
optional parameter and the completions are an optional result, so source which was merely
displayed cannot be mistaken for source in which nothing can follow.

```scala fuel
compile
frame
  label No caret, and so no completions
  code
      val source = t"val cue = line mat"

      val displayed = Scala.highlight(source).completions
frame
  label A caret at the end
  highlight caret  an ordinal position in the source
  code
      val source = t"val cue = line mat"

      val displayed = Scala.highlight(source).completions
      val edited = Scala.highlight(source, caret = source.length.z).completions
frame
  label The names offered
  highlight lay(Nil)  no completions is not the same as an empty list
  code
      val source = t"val cue = line mat"

      val displayed = Scala.highlight(source).completions
      val edited = Scala.highlight(source, caret = source.length.z).completions
      val offered = edited.lay(Nil)(_.items.map(_.name))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(displayed.absent.inspect)
Out.println(offered.inspect)
```

The source is only tokenized here, so the offer is of keywords.
[Completions](../../soundness/modules/syntax-highlighting.md#completions) adds a compiler, and
with it the members of a type.
