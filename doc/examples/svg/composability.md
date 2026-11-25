# A color, a style and an angle in one figure

SVG has little languages for colors, styles and angles, and each belongs to another library.
A figure takes their values as they are: a typed color in a typed CSS style, and an angle
which knows its unit.

```scala fuel
compile
frame
  label A figure
  code
      import Svg.Rectangle

      val banner = Rectangle((0, 0), 40, 10)
frame
  label A fill, from the colors and CSS libraries
  highlight Css.Style(fill..))  a typed declaration, holding a typed color
  code
      import Svg.Rectangle

      val banner = Rectangle((0, 0), 40, 10, style = Css.Style(fill = Srgb(1, 0, 0)))
frame
  label A turn, by an angle
  highlight Angle.degrees(45)  an angle, not a bare number
  code
      import Svg.Rectangle

      val banner = Rectangle((0, 0), 40, 10, style = Css.Style(fill = Srgb(1, 0, 0)))
        . rotate(Angle.degrees(45))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(banner.in[Xml].show)
```

[Groups, polylines and text](../../soundness/modules/svg.md#groups-polylines-and-text) shows
the style of a figure, and [composability](../../philosophy/composability.md) why no adapter
is needed.
