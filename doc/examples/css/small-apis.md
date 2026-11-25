# An inline style from named arguments

The declarations of an inline style are named arguments, each taking the type of value its
property has, and the result is the text of a `style` attribute.

```scala fuel
compile
frame
  label One declaration
  code
      val accent = Css.Style(color = Srgb(1.0, 0.0, 0.0))
frame
  label Two
  highlight width  a property, as a named argument
  code
      val accent = Css.Style(color = Srgb(1.0, 0.0, 0.0), width = 4.0*Px)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(accent.text)
```

[Writing CSS](../../soundness/modules/css.md#writing-css) has whole stylesheets.
