# One method converts to every color space

There is no `toHsl`, `rgbToLab` or `asCmyk`. A color is converted with `to`, which takes the
space that is wanted, and reaches it from whichever space the color is in.

```scala fuel
compile
frame
  label For adjustment
  highlight to[Hsl]  hue, saturation and lightness
  code
      given daylight: Colorimetry = colorimetry.daylightColorimetry

      val tomato = WebColors.Tomato
      val hsl = tomato.to[Hsl]
frame
  label For print
  highlight to[Cmyk]  four inks
  code
      given daylight: Colorimetry = colorimetry.daylightColorimetry

      val tomato = WebColors.Tomato
      val hsl = tomato.to[Hsl]
      val ink = tomato.to[Cmyk]
frame
  label For perceptual distance
  highlight to[Cielab]  needs the light in scope
  code
      given daylight: Colorimetry = colorimetry.daylightColorimetry

      val tomato = WebColors.Tomato
      val hsl = tomato.to[Hsl]
      val ink = tomato.to[Cmyk]
      val lab = tomato.to[Cielab]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(hsl.inspect)
Out.println(ink.inspect)
Out.println(lab.inspect)
```

[Converting between spaces](../../soundness/modules/colors.md#converting-between-spaces) has
the conversions, and [small APIs](../../philosophy/small-apis.md) the reasons for one name.
