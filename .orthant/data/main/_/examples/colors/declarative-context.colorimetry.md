# The same ivory, under another light

The coordinates of a color in CIELAB depend on the light the eye has adapted to. The light is
a given, and the conversion beneath it is the same line under each.

```scala fuel
compile
alternative Daylight
  highlight daylightColorimetry  the illuminant D65
  code
      given light: Colorimetry = colorimetry.daylightColorimetry

      val ivory = WebColors.Ivory.to[Cielab]
alternative A tungsten bulb
  highlight incandescentTungstenColorimetry  the illuminant A
  code
      given light: Colorimetry = colorimetry.incandescentTungstenColorimetry

      val ivory = WebColors.Ivory.to[Cielab]
alternative A cool fluorescent tube
  highlight coolFluorescentColorimetry  the illuminant F2
  code
      given light: Colorimetry = colorimetry.coolFluorescentColorimetry

      val ivory = WebColors.Ivory.to[Cielab]
##
```

[Color profiles](../../soundness/modules/colors.md#color-profiles) lists the illuminants, and
[declarative context](../../philosophy/declarative-context.md) explains why the light is not
an argument.
