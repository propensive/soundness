## Colors

### About

Soundness represents [colors](https://en.wikipedia.org/wiki/Color_model) and converts
between the models used to describe them. A color written in one model — sRGB for a
screen, CIELAB for perceptual distance, HSL for intuitive adjustment, CMYK for print —
converts to any other with a single method, and each model is a distinct type, so the
compiler knows which one a value is in.

Around 140 named web colors come built in, and a color can be mixed with another,
lightened or darkened, saturated, or rotated around the color wheel. Where a conversion
depends on viewing conditions — the same coordinates look different under daylight and
under a tungsten bulb — the conditions are supplied as a color profile in scope.

### On color

A color has no single true representation. A monitor mixes red, green and blue; a printer
lays down cyan, magenta, yellow and black; a designer thinks in hue, saturation and
lightness; and measuring how _different_ two colors look calls for a model, CIELAB, built
so that equal distances correspond to equal perceived differences. Each model suits a
different task, and moving between them is arithmetic that is easy to get subtly wrong.

Most libraries settle on one model, usually RGB, and leave the rest to the programmer.
Soundness gives each model its own type and converts between them with one operation, so
a value's model is always known, and a conversion that needs a profile cannot be written
without one.

Profiles exist because some models are anchored to human perception rather than to a
device, and perception depends on the light. CIELAB and XYZ describe a color relative to
a *reference white* — the color of the illumination the eye has adapted to — and that
reference differs between daylight, a tungsten bulb, and a fluorescent tube. The same
surface reflects the same physics under each, yet looks different, so a conversion into
or out of these models must be told which conditions it assumes. Everything comes from
the `soundness` package, with a color profile chosen in scope:

```scala
import soundness.*

given Colorimetry = colorimetry.daylight
```

### Color spaces

Each model is a type whose fields are its coordinates, given as fractions between 0
and 1. A color is constructed directly in whichever model is convenient:

```scala
val red = Srgb(0.8, 0.2, 0.2)
val slate = Hsl(0.58, 0.3, 0.4)
```

`Srgb`, `Hsl`, `Hsv`, `Cielab`, `Xyz`, `Cmy` and `Cmyk` are the models available, and a
value in any of them is a `Color`.

### Converting between spaces

The `in` method converts a color to another model:

```scala
val lab = red.in[Cielab]
val back = lab.in[Srgb]      // the original red, within rounding
```

Conversions compose, so a color reaches any model from any other, passing through the
intermediate spaces without the caller arranging it. Converting to CIELAB or XYZ from
sRGB draws on the `Colorimetry` in scope, since those models are defined relative to a
reference white.

### Named colors

The named web colors live on `WebColors`, each a `Color in Srgb`:

```scala
WebColors.Ivory        // Srgb(1, 1, 0.941)
WebColors.DeepPink
WebColors.SteelBlue
```

`WebColors.colors` is the whole list, useful for iterating over the palette.

### Mixing

Two colors mix into one, by default halfway between them, or in any ratio:

```scala
Srgb(0.2, 0.4, 0.6).mix(Srgb(0.8, 0.6, 0.4))        // Srgb(0.5, 0.5, 0.5)
WebColors.Ivory.in[Cielab].mix(WebColors.DeepPink.in[Cielab])
```

Mixing in sRGB blends the screen values; mixing in CIELAB blends what the eye perceives,
which gives a more natural midpoint between two colors.

### Adjusting

Hue, saturation and lightness are what a person reaches for to adjust a color, so those
operations live on `Hsl` and `Hsv`. Convert into one, adjust, and convert back:

```scala
val tomato = WebColors.Tomato.in[Hsl]

tomato.rotate(180).in[Srgb]     // the complementary color
tomato.lighten(0.2).in[Srgb]    // a fifth of the way toward white
tomato.desaturate.in[Srgb]      // the same lightness, no color
```

`rotate` turns the hue by a number of degrees, `complement` turns it halfway round,
`saturate` and `desaturate` push saturation to its extremes, and `lighten` and `darken`
move lightness by a fraction. On `Hsv`, `tint`, `shade` and `tone` mix toward white,
toward black, or both.

### Perceptual distance

CIELAB exists so that the distance between two colors matches how different they look.
`delta` gives that distance:

```scala
WebColors.DeepPink.in[Cielab].delta(WebColors.Tomato.in[Cielab])
```

A small delta means two colors are hard to tell apart; a large one means they contrast.
This is the right measure for choosing a readable foreground against a background, where
comparing raw RGB values would mislead.

### Hex and the terminal

The `rgb"…"` interpolator reads a [hexadecimal](https://en.wikipedia.org/wiki/Web_colors)
color, with or without a leading hash, and checks it as the code compiles:

```scala
rgb"#abcdef"   // Chroma(171, 205, 239)
rgb"ffffff"    // Chroma(255, 255, 255)
```

The result is a `Chroma` — a color with whole-number channels from 0 to 255 — which is
the form a terminal expects, so a color computed in any model renders as styled terminal
output once converted.

### Color profiles

A `Colorimetry` describes the illuminant and observer that a profile-dependent conversion
assumes. `colorimetry.daylight` suits most screen work; others model incandescent,
fluorescent and standard-illuminant conditions. The profile is a given, so every
conversion within its scope uses it, and none has to name it:

```scala
given Colorimetry = colorimetry.incandescentTungsten

WebColors.Ivory.in[Cielab]   // computed for tungsten light
```
