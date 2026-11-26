# Two colors must share a space to mix

Each color space is a type of its own, so the compiler knows which space a color is in. Mixing
the coordinates of one space with those of another means nothing, and cannot be written.

```scala fuel
compile
frame
  label A red in sRGB and a slate in HSL
  errors show
  code
      val red = Srgb(0.8, 0.2, 0.2)
      val slate = Hsl(208.8.deg, 0.3, 0.4)

      val blend = red.mix(slate)
frame
  label Both in sRGB
  highlight to[Srgb]  converted before it is mixed
  code
      val red = Srgb(0.8, 0.2, 0.2)
      val slate = Hsl(208.8.deg, 0.3, 0.4)

      val blend = red.mix(slate.to[Srgb])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(blend.inspect)
```

[Mixing](../../soundness/modules/colors.md#mixing) compares a mixture in sRGB with one in
CIELAB, and [impossible states](../../philosophy/impossible-states.md) has the principle.
