# A transform gives a new figure

A figure is a value, and a transform does not move it: `translate` and `rotate` give another
figure, which records the transform beside the geometry it was defined with.

```scala fuel
compile
frame
  label A square
  code
      import Svg.Rectangle

      val square = Rectangle((0, 0), 10, 10)
frame
  label The square, moved
  highlight translate  gives a new figure
  code
      import Svg.Rectangle

      val square = Rectangle((0, 0), 10, 10)
      val moved = square.translate(Delta(30, 0))
frame
  label The moved square, turned
  highlight rotate  takes a typed angle
  code
      import Svg.Rectangle

      val square = Rectangle((0, 0), 10, 10)
      val moved = square.translate(Delta(30, 0))
      val diamond = moved.rotate(Angle.degrees(45))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(square.in[Xml].show)
Out.println(moved.in[Xml].show)
Out.println(diamond.in[Xml].show)
```

All three figures have the same corner and size, and `square` is as it was.
[Transforms](../../soundness/modules/svg.md#transforms) explains why the transform is recorded
and not applied.
