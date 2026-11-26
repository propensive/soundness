# A type, as its author would write it

A macro which reports a type should show it as it appears in the source: infix where it was
written infix, and by the names which the imports make available. `Syntax.name` renders a
type that way, so an error which mentions `Addable by Int to Double` says exactly that.

```scala fuel
compile
frame
  label A union and an intersection
  code
      val either = Syntax.name[Int | Text & Double]
frame
  label A typeclass, read as a phrase
  highlight Addable..Double  infix in the source, infix in the result
  code
      val either = Syntax.name[Int | Text & Double]
      val phrase = Syntax.name[Addable by Int to Double]
frame
  label A dependent function
  highlight text.type
  code
      val either = Syntax.name[Int | Text & Double]
      val phrase = Syntax.name[Addable by Int to Double]
      val function = Syntax.name[(text: Text) => text.type]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(either)
Out.println(phrase)
Out.println(function)
```

[Types as source text](../../soundness/modules/metaprogramming.md#types-as-source-text) has
more, and [infix types](../../philosophy/infix-types.md) explains why the phrase is worth
keeping.
