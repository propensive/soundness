# Any color is a style

The interpolator for styled text does not define colors of its own. A color from any color
space is a substitution, and so is one which has been converted, rotated or mixed on the way.

```scala fuel
compile
frame
  label A named color
  code
      val rose = WebColors.Crimson

      val line = e"$rose(a rose) by any other name"
frame
  label Its complement, computed in another space
  highlight to[Hsl].complement  a color in HSL, used as it is
  code
      val rose = WebColors.Crimson
      val leaf = rose.to[Hsl].complement

      val line = e"$rose(a rose) by any other $leaf(name)"
frame
  label As a background
  highlight Bg(rose)  the same color, in the other role
  code
      val rose = WebColors.Crimson
      val leaf = rose.to[Hsl].complement

      val line = e"$rose(a rose) by any other ${Bg(rose)}($leaf(name))"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xtermTrueColorTermcap

Out.println(line)
```

[Colors](../../soundness/modules/colors.md#adjusting) has the adjustments, and
[styled text](../../soundness/modules/terminal.md#styled-text) the substitutions.
