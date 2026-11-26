# A turn which needs no resampling

A grid of pixels turns exactly by quarter turns, and by no other angle without inventing
pixels. The parameter of `rotate` is a type with three values, so a turn which would blur the
image cannot be asked for.

```scala fuel
compile
frame
  label An eighth of a turn
  errors show
  code
      val sunset = Raster(4, 2)((x, y) => Chroma(255, 60*x, 0))
      val turned = sunset.rotate(45)
frame
  label A quarter turn
  highlight 90  one of `90`, `180` and `270`
  code
      val sunset = Raster(4, 2)((x, y) => Chroma(255, 60*x, 0))
      val turned = sunset.rotate(90)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(turned.width.show)
Out.println(turned.height.show)
```

[Transforming](../../soundness/modules/images.md#transforming) has the other operations, and
[impossible states](../../philosophy/impossible-states.md) the principle.
