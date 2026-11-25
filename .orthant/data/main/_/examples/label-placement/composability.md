# Lettering set where the arrangement put it

The arrangement knows only boxes and points, and an SVG drawing only figures. The answer to
`position` is plain coordinates, so the lettering of a drawing is built in the body of
`arrange`, and comes out of it already placed.

```scala fuel
compile
frame
  label Two names at one point
  code
      import Svg.Lettering

      val names = arrange:
        val verona = position(60.0, 12.0, 100.0, 100.0)
        val mantua = position(60.0, 12.0, 100.0, 100.0)
        List(verona, mantua)
frame
  label Each name, as lettering at its position
  highlight (mantua.x,..mantua.y)  a pair of numbers is a point of the drawing
  code
      import Svg.Lettering

      val names = arrange:
        val verona = position(60.0, 12.0, 100.0, 100.0)
        val mantua = position(60.0, 12.0, 100.0, 100.0)

        List
          ( Lettering((verona.x, verona.y), t"Verona"),
            Lettering((mantua.x, mantua.y), t"Mantua", Lettering.Anchor.End) )
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

names.each: name =>
  Out.println(name.in[Xml].show)
```

The second name was moved to the west of the point, so it is anchored at its end.
[Captions and targets](../../soundness/modules/label-placement.md#captions-and-targets)
describes the position which is answered, and [SVG](../../soundness/modules/svg.md) the
lettering.
