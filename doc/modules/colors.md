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

given Colorimetry = colorimetry.daylightColorimetry
```

A color's space living in its type is a small instance of [impossible states](../philosophy/impossible-states.md): mixing two spaces without converting cannot be written.

### Color spaces

Each model is a type whose fields are its coordinates, given as fractions between 0
and 1 — except hue, which is a point on a circle and is given as an `Angle`. A color
is constructed directly in whichever model is convenient:

```scala
val red = Srgb(0.8, 0.2, 0.2)
val slate = Hsl(208.8.deg, 0.3, 0.4)
```

Seven models are available, and a value in any of them is a `Color`:

```tabular
column  Model
column  Coordinates
column  Suits
row  `Srgb`  `red`, `green`, `blue`  a screen
row  `Hsl`  `hue`, `saturation`, `lightness`  intuitive adjustment
row  `Hsv`  `hue`, `saturation`, `value`  intuitive adjustment, and tints and shades
row  `Cielab`  `lightness`, `blueYellow`, `greenRed`  perceptual distance and mixing
row  `Xyz`  `x`, `y`, `z`  a device-independent reference
row  `Cmy`  `cyan`, `magenta`, `yellow`  print
row  `Cmyk`  `cyan`, `magenta`, `yellow`, `key`  print, with a black ink
```

### Converting between spaces

The `to` method converts a color to another model:

```scala
val lab = red.to[Cielab]
val back = lab.to[Srgb]      // the original red, within rounding
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
WebColors.Ivory.to[Cielab].mix(WebColors.DeepPink.to[Cielab])
```

Mixing in sRGB blends the screen values; mixing in CIELAB blends what the eye perceives,
which gives a more natural midpoint between two colors.

### Mixing by parts

`mix` takes two colors at a time. To combine several, multiply each by the number of
parts of it going into the mixture and add them up, the way a painter measures paint
onto a palette. A mixing mode is chosen by importing one:

```scala
import mixing.proportionalMixing

5*WebColors.Red + 3*WebColors.Yellow   // Srgb(1, 0.375, 0) — an orange
```

`5*Red` is a `Daub`: an amount of a color, which is still that color — one part of red
and five parts of red are both red. A `Daub` is a `Color`, so a mixture converts,
renders and measures like any other, and the parts matter only when more paint is added:

```scala
val mixture = 5*WebColors.Red + 3*WebColors.Yellow
mixture.to[Cielab]
mixture.to[Hsl].hue.degrees   // 22.5
```

Parts are proportions, so `5*Red + 3*Yellow` and `50*Red + 30*Yellow` are the same color,
and a third daub is weighted against the total so far rather than against its neighbor:

```scala
1*WebColors.Red + 1*WebColors.Lime + 1*WebColors.Blue   // an even third of each
```

### Mixing modes

`proportionalMixing` averages the colors, which is what `mix` does. The other modes are the
blend modes of Photoshop and GIMP, as the W3C compositing specification defines them. Each
computes a channel from the backdrop, *b*, and the layer, *l*:

```tabular
column  Mode
column  Each channel becomes
row  `proportionalMixing`  *l*, so the result is a weighted average
row  `multiplyMixing`  *b*·*l*
row  `screenMixing`  *b* + *l* − *b*·*l*
row  `overlayMixing`  2·*b*·*l* where *b* ≤ ½, otherwise 1 − 2·(1 − *b*)·(1 − *l*)
row  `hardLightMixing`  as overlay, with *l* rather than *b* deciding
row  `softLightMixing`  a gentler hard light, darkening where *l* ≤ ½ and lightening above
row  `darkenMixing`  the lesser of *b* and *l*
row  `lightenMixing`  the greater of *b* and *l*
row  `colorDodgeMixing`  *b*/(1 − *l*), at most 1
row  `colorBurnMixing`  1 − (1 − *b*)/*l*, at least 0
row  `differenceMixing`  the absolute difference of *b* and *l*
row  `exclusionMixing`  *b* + *l* − 2·*b*·*l*
row  `linearDodgeMixing`  *b* + *l*, at most 1
row  `linearBurnMixing`  *b* + *l* − 1, at least 0
```

Each is imported the same way, from the `mixing` package:

```scala
import mixing.multiplyMixing

1*WebColors.Yellow + 1*WebColors.Cyan   // Srgb(0.5, 1, 0) — a green, as paint mixes
```

A daub's share of the total parts acts as its opacity: the mode is applied at full
strength and the result mixed back in that proportion, exactly as a layer's opacity
slider works. Only proportional mixing is therefore commutative — under any other mode
`5*Red + 3*Yellow` differs from `3*Yellow + 5*Red`, just as reordering two layers does.

Every mode but the proportional one reads coordinates as fractions of full intensity, so they
are offered only for sRGB, CMY and CMYK. Asking for `multiplyMixing` in CIELAB, where lightness
runs to 100, will not compile rather than quietly meaning something else.

### Adjusting

Hue, saturation and lightness are what a person reaches for to adjust a color, so those
operations live on `Hsl` and `Hsv`. Convert into one, adjust, and convert back:

```scala
val tomato = WebColors.Tomato.to[Hsl]

tomato.rotate(180.deg).to[Srgb]   // the complementary color
tomato.lighten(0.2).to[Srgb]      // a fifth of the way toward white
tomato.desaturate.to[Srgb]        // the same lightness, no color
```

`rotate` turns the hue by an `Angle`, so the unit is written down rather than assumed:
`180.deg` and `π.rad` are the same half-turn. The adjustments are:

```tabular
column  Operation
column  On
column  Gives
row  `rotate(angle)`  `Hsl`, `Hsv`  the hue turned by an angle
row  `complement`  `Hsl`, `Hsv`  the hue turned by a half-turn
row  `saturate`, `desaturate`  `Hsl`, `Hsv`  full saturation, or none
row  `pure`  `Hsl`, `Hsv`  the hue at full saturation and middle lightness, or full value
row  `lighten(amount)`  `Hsl`  lightness moved that fraction of the way toward white
row  `darken(amount)`  `Hsl`  lightness moved that fraction of the way toward black
row  `tint(white)`  `Hsv`  saturation reduced by a fraction, toward white
row  `shade(black)`  `Hsv`  value reduced by a fraction, toward black
row  `tone(black, white)`  `Hsv`  both
row  `mix(other, ratio)`  `Srgb`, `Cielab`  a blend of two colors, halfway by default
row  `delta(other)`  `Cielab`  the perceptual distance between two colors
```

A hue is a point on a circle rather than a quantity, so a rotation always lands back in
the range the eye reads as a color: turning past a full circle, or backwards past zero,
wraps round.

```scala
tomato.rotate((-30).deg).hue.degrees   // 339.1, never negative
```

### Perceptual distance

CIELAB exists so that the distance between two colors matches how different they look.
`delta` gives that distance:

```scala
WebColors.DeepPink.to[Cielab].delta(WebColors.Tomato.to[Cielab])
```

A small delta means two colors are hard to tell apart; a large one means they contrast.
This is the right measure for choosing a readable foreground against a background, where
comparing raw RGB values would mislead.

### Hex and the terminal

The `rgb"…"` interpolator reads a [hexadecimal](https://en.wikipedia.org/wiki/Web_colors)
color, with or without a leading hash, and checks it as the code compiles:

```scala
rgb"#abcdef"         // Chroma(171, 205, 239)
rgb"ffffff"          // Chroma(255, 255, 255)
rgb"#abcdef".color   // Srgb(0.671, 0.804, 0.937)
```

The result is a `Chroma` — a color with whole-number channels from 0 to 255 — which is
the form a terminal expects, so a color computed in any model renders as styled terminal
output once converted. `color` converts the other way, to the `Color in Srgb` that every
perceptual conversion, `Palette` role and `Daub` accepts, so a hex literal is a starting
point for everything above as well as an endpoint.

### Color profiles

A `Colorimetry` describes the illuminant and observer that a profile-dependent conversion
assumes. `colorimetry.daylightColorimetry` suits most screen work; others model incandescent,
fluorescent and standard-illuminant conditions. The profile is a given, so every
conversion within its scope uses it, and none has to name it:

```scala
given Colorimetry = colorimetry.incandescentTungstenColorimetry

WebColors.Ivory.to[Cielab]   // computed for tungsten light
```

The `colorimetry` package names each profile after its CIE standard illuminant, or after a
lamp that approximates one:

```tabular
column  Given
column  Illuminant
column  Also named
row  `incandescentTungstenColorimetry`  A
row  `oldDirectSunlightAtNoonColorimetry`  B
row  `oldDaylightColorimetry`  C
row  `iccProfilePcsColorimetry`  D50
row  `midMorningDaylightColorimetry`  D55
row  `daylightColorimetry`  D65  `srgbColorimetry`, `adobeRgbColorimetry`
row  `northSkyDaylightColorimetry`  D75
row  `equalEnergyColorimetry`  E
row  `daylightFluorescentF1Colorimetry`  F1
row  `coolFluorescentColorimetry`  F2
row  `whiteFluorescentColorimetry`  F3
row  `warmWhiteFluorescentColorimetry`  F4
row  `daylightFluorescentF5Colorimetry`  F5
row  `liteWhiteFluorescentColorimetry`  F6
row  `daylightFluorescentF7Colorimetry`  F7  `d65SimulatorColorimetry`
row  `sylvaniaF40Colorimetry`  F8  `d50SimulatorColorimetry`
row  `coolWhiteFluorescentColorimetry`  F9
row  `philipsTl85Colorimetry`  F10  `ultralume50Colorimetry`
row  `philipsTl84Colorimetry`  F11  `ultralume40Colorimetry`
row  `philipsTl83Colorimetry`  F12  `ultralume30Colorimetry`
```

### Themes and palettes

A `Theme` names a coordinated set of colors — a background, a foreground, a list of accents,
and whether it is a light or a dark theme — and a `Palette` selects from one by role, offering
`subdue` and `accent` to push a color toward the background or away from it. The Solarized
theme is built in and chosen by import:

```scala
import luminosity.darkBrightness
import themes.solarizedTheme

summon[Theme].background       // Srgb(0.000, 0.169, 0.212)
summon[Theme].luminosity       // Brightness.Dark
```

`luminosity.darkBrightness` and `luminosity.lightBrightness` select the variant of a theme
that offers both, so terminal output styled through a palette adapts to the terminal's
background.

A palette's roles are `Color in Srgb`, so a palette written from hex literals gives each role
a `rgb"…".color`.

### Pixel layouts

A color on screen is not usually a triple of doubles but a packed integer, and how it is packed
varies: eight bits per channel with or without alpha, five-six-five for a 16-bit display, ten bits
per channel for high dynamic range, a single channel for grayscale. A *layout* states that packing
as a tuple of channel types, most significant first, and the compiler works out the rest:

```scala
type Rgb565 = (Red[5], Green[6], Blue[5])

scala.compiletime.constValue[anticipation.Channel.TotalBits[Rgb]]   // 24
summon[anticipation.Channel.Storage[Rgb565] =:= Short]   // the narrowest type that fits
summon[anticipation.Channel.Storage[Tuple1[Grey[8]]] =:= Byte]
```

Three layouts are predefined:

```tabular
column  Layout
column  Channels
column  Bits
row  `Rgb`  `(Red[8], Green[8], Blue[8])`  24
row  `Rgba`  `(Red[8], Green[8], Blue[8], Alpha[8])`  32
row  `Cmyk8`  `(Cyan[8], Magenta[8], Yellow[8], Key[8])`  32
```

`Channel` is reached through its library package here because the umbrella already exports a
`Channel` of its own, the WebSocket channel.

Each channel's shift and mask follow from its position in the tuple, computed as the code
compiles, so reading a channel from a packed pixel is a single shift-and-mask instruction with no
runtime dispatch. A `Pixel[layout]` is an unboxed integer carrying the packed value, and converts
to and from `Chroma`, `Srgb` and `Cmyk` exactly where its layout supports it — a layout with no
alpha channel has no alpha to read.

This is what [images](images.md) use to give a raster typed pixel access, and it is where a
color computed in one of the perceptual spaces above ends up when it reaches a screen.
