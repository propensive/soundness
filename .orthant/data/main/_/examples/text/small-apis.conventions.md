# Words between naming conventions

A conversion from one naming convention to another is two steps: one method takes an
identifier apart into words, and another joins words in a style. Three methods which split and
four which join give every conversion between them.

```scala fuel
compile
frame
  label The words of an identifier
  highlight uncamel  taken apart at each capital
  code
      val words = t"twelfthNight".uncamel
frame
  label Joined with hyphens
  highlight kebab  joined with hyphens
  code
      val words = t"twelfthNight".uncamel
      val file = words.kebab
frame
  label Joined in two more styles
  code
      val words = t"twelfthNight".uncamel
      val file = words.kebab
      val column = words.snake
      val title = words.pascal
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(file)
Out.println(column)
Out.println(title)
```

[Case conventions](../../soundness/modules/text.md#case-conventions) has the full table.
