# Paint, measured in parts

A mixture is written as a painter measures it out: so many parts of one color, and so many of
another. `5*Red` is five parts of red, and the sum of two amounts is a color.

```scala fuel
compile
frame
  label Five parts of red
  highlight 5*Red  an amount of a color
  code
      import mixing.proportionalMixing
      import WebColors.{Red, Yellow, White}

      val paint = 5*Red
frame
  label Three parts of yellow
  highlight 3*Yellow  weighed against the five
  code
      import mixing.proportionalMixing
      import WebColors.{Red, Yellow, White}

      val paint = 5*Red + 3*Yellow
frame
  label A little white
  highlight 0.5*White  weighed against the eight
  code
      import mixing.proportionalMixing
      import WebColors.{Red, Yellow, White}

      val paint = 5*Red + 3*Yellow + 0.5*White
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(paint.to[Srgb].inspect)
Out.println(paint.parts.inspect)
```

[Mixing by parts](../../soundness/modules/colors.md#mixing-by-parts) explains how each amount
is weighed against the total.
