# A conversion which depends on the light

CIELAB describes a color relative to the light the eye has adapted to. A conversion into it
therefore needs to know the light, and does not compile until a `Colorimetry` is in scope.

```scala fuel
compile
frame
  label Under no light in particular
  errors show
  code
      val ivory = WebColors.Ivory.to[Cielab]
frame
  label In daylight
  highlight daylightColorimetry  the light the eye has adapted to
  code
      given daylight: Colorimetry = colorimetry.daylightColorimetry

      val ivory = WebColors.Ivory.to[Cielab]
frame
  label A conversion which needs no light
  highlight to[Hsl]  defined without a reference white
  code
      val ivory = WebColors.Ivory.to[Hsl]
##
```

[Color profiles](../../soundness/modules/colors.md#color-profiles) lists the illuminants, and
[honest signatures](../../philosophy/honest-signatures.md) explains why the dependency is
declared.
