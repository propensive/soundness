# An adjustment gives a new color

A color is a value, and lightening it or draining its saturation gives another. The color
which was adjusted is as it was, so a palette can be derived from one color without a copy
being kept of it.

```scala fuel
compile
frame
  label A color
  code
      val tomato = WebColors.Tomato.to[Hsl]
frame
  label A paler one
  highlight lighten(0.5)  halfway toward white
  code
      val tomato = WebColors.Tomato.to[Hsl]
      val pale = tomato.lighten(0.5)
frame
  label A darker one, from the same color
  highlight tomato.darken(0.5)  tomato has not changed
  code
      val tomato = WebColors.Tomato.to[Hsl]
      val pale = tomato.lighten(0.5)
      val deep = tomato.darken(0.5)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(tomato.lightness.inspect)
Out.println(pale.lightness.inspect)
Out.println(deep.lightness.inspect)
```

[Adjusting](../../soundness/modules/colors.md#adjusting) has the other adjustments, and
[immutability](../../philosophy/immutability.md) the principle.
