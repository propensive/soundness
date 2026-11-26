# A color and a length, in a stylesheet

A `css"…"` literal takes typed values where CSS has values. A color is written as its hex code
and a length with its unit, so neither is ever assembled from text.

```scala fuel
compile
frame
  label A stylesheet, as text
  code
      val sheet = css"a { color: #ff0000; width: 4px }"
frame
  label The color, as a color
  highlight $red  written as hex
  code
      val red = Srgb(1.0, 0.0, 0.0)
      val sheet = css"a { color: $red; width: 4px }"
frame
  label The width, as a length
  highlight $width  written with its unit
  code
      val red = Srgb(1.0, 0.0, 0.0)
      val width = 4.0*Px
      val sheet = css"a { color: $red; width: $width }"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactCssFormatting

Out.println(sheet.show)
```

[Writing CSS](../../soundness/modules/css.md#writing-css) describes the literal, and
[colors](../../soundness/modules/colors.md) the values it takes.
