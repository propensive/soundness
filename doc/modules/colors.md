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

### Color spaces

Each model is a type whose fields are its coordinates, given as fractions between 0
and 1 — except hue, which is a point on a circle and is given as an `Angle`. A color
is constructed directly in whichever model is convenient:

```scala
val red = Srgb(0.8, 0.2, 0.2)
val slate = Hsl(208.8.deg, 0.3, 0.4)
```

`Srgb`, `Hsl`, `Hsv`, `Cielab`, `Xyz`, `Cmy` and `Cmyk` are the models available, and a
value in any of them is a `Color`.

### Converting between spaces

The `in` method converts a color to another model:

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
and a third daub is weighted against the total so far rather than against its neighbour:

```scala
1*WebColors.Red + 1*WebColors.Lime + 1*WebColors.Blue   // an even third of each
```

### Mixing modes

`proportional` averages the colors, which is what `mix` does. The other modes are the
blend modes of Photoshop and GIMP — `multiply`, `screen`, `overlay`, `hardLight`,
`softLight`, `darken`, `lighten`, `colorDodge`, `colorBurn`, `difference`, `exclusion`,
`linearDodge` and `linearBurn` — and each is imported the same way:

```scala
import mixing.multiplyMixing

1*WebColors.Yellow + 1*WebColors.Cyan   // Srgb(0.5, 1, 0) — a green, as paint mixes
```

A daub's share of the total parts acts as its opacity: the mode is applied at full
strength and the result mixed back in that proportion, exactly as a layer's opacity
slider works. Only `proportional` is therefore commutative — under any other mode
`5*Red + 3*Yellow` differs from `3*Yellow + 5*Red`, just as reordering two layers does.

Every mode but `proportional` reads coordinates as fractions of full intensity, so they
are offered only for sRGB, CMY and CMYK. Asking for `multiply` in CIELAB, where lightness
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
`180.deg` and `π.rad` are the same half-turn. `complement` is that half-turn.
`saturate` and `desaturate` push saturation to its extremes, and `lighten` and `darken`
move lightness by a fraction. On `Hsv`, `tint`, `shade` and `tone` mix toward white,
toward black, or both.

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
rgb"#abcdef"   // Chroma(171, 205, 239)
rgb"ffffff"    // Chroma(255, 255, 255)
```

The result is a `Chroma` — a color with whole-number channels from 0 to 255 — which is
the form a terminal expects, so a color computed in any model renders as styled terminal
output once converted.

### Color profiles

A `Colorimetry` describes the illuminant and observer that a profile-dependent conversion
assumes. `colorimetry.daylightColorimetry` suits most screen work; others model incandescent,
fluorescent and standard-illuminant conditions. The profile is a given, so every
conversion within its scope uses it, and none has to name it:

```scala
given Colorimetry = colorimetry.incandescentTungstenColorimetry

WebColors.Ivory.to[Cielab]   // computed for tungsten light
```

### Pixel layouts

A colour on screen is not usually a triple of doubles but a packed integer, and how it is packed
varies: eight bits per channel with or without alpha, five-six-five for a 16-bit display, ten bits
per channel for high dynamic range, a single channel for greyscale. A *layout* states that packing
as a tuple of channel types, most significant first, and the compiler works out the rest:

```scala
type Rgb565 = (Red[5], Green[6], Blue[5])

compiletime.constValue[Channel.TotalBits[Rgb]]      // 24
summon[Channel.Storage[Rgb565] =:= Short]           // the narrowest type that fits
summon[Channel.Storage[Tuple1[Grey[8]]] =:= Byte]
```

Each channel's shift and mask follow from its position in the tuple, computed as the code
compiles, so reading a channel from a packed pixel is a single shift-and-mask instruction with no
runtime dispatch. A `Pixel[layout]` is an unboxed integer carrying the packed value, and converts
to and from `Chroma`, `Srgb` and `Cmyk` exactly where its layout supports it — a layout with no
alpha channel has no alpha to read.

This is what [images](images.md) use to give a raster typed pixel access, and it is where a
colour computed in one of the perceptual spaces above ends up when it reaches a screen.
