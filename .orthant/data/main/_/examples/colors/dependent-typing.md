# A pixel knows which channels it has

A pixel's layout is a tuple of channel types, and its accessors are computed from the layout.
A layout with no alpha channel has no alpha to read, and the compiler says so.

```scala fuel
compile
frame
  label Three channels, and a fourth is asked for
  errors show
  code
      val pixel: Pixel[Rgb] = Pixel[Rgb](WebColors.Tomato)
      val opacity: Int = pixel.alpha
frame
  label Four channels
  highlight Pixel[Rgba](  a layout with an alpha channel
  code
      val pixel: Pixel[Rgba] = Pixel[Rgba](WebColors.Tomato)
      val opacity: Int = pixel.alpha
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(pixel.red.inspect)
Out.println(opacity.inspect)
```

Each accessor is a shift and a mask whose operands the compiler works out from the position of
the channel in the tuple. [Pixel layouts](../../soundness/modules/colors.md#pixel-layouts) has
the layouts.
