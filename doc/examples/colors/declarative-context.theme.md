# A theme, light or dark

A `Theme` is a coordinated set of colors, and comes in a light and a dark variant. Which
theme, and which variant, are two imports, and the code which asks for the background asks
the same question under each.

```scala fuel
compile
alternative Dark
  highlight darkBrightness  the variant
  code
      import themes.solarizedTheme, luminosity.darkBrightness

      val paper = summon[Theme].background
      val ink = summon[Theme].foreground
alternative Light
  highlight lightBrightness  the variant
  code
      import themes.solarizedTheme, luminosity.lightBrightness

      val paper = summon[Theme].background
      val ink = summon[Theme].foreground
##
```

[Themes and palettes](../../soundness/modules/colors.md#themes-and-palettes) describes the
roles of a palette, and [declarative context](../../philosophy/declarative-context.md) the
reasons for an import.
