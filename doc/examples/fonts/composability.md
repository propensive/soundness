# A drawing which declares the font of its lettering

The lettering of an SVG drawing takes a `Font in Web`, which cannot be made without a
provision for its typeface. The drawing reads the provision from the font, and writes the
`@font-face` rule itself.

```scala fuel
compile
frame
  label Lettering
  code
      import strategies.throwUnsafely
      import Svg.Lettering

      val title = Lettering((10, 30), t"Elsinore")
      val drawing = Svg(120, 40, figures = List(title))
frame
  label Lettering in a typeface, with its provision
  highlight Font(Typeface["Menlo"].bold)  the provision in scope is paired with the face
  code
      import strategies.throwUnsafely
      import Svg.Lettering

      given menlo: ((Typeface of "Menlo") is Typesettable in Web) = Web.local()

      val title = Lettering((10, 30), t"Elsinore", font = Font(Typeface["Menlo"].bold))
      val drawing = Svg(120, 40, figures = List(title))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(drawing.in[Xml].show)
```

[Fonts](../../soundness/modules/fonts.md#fonts) says what each kind of document does with its
fonts, and [SVG](../../soundness/modules/svg.md#groups-polylines-and-text) describes
lettering.
