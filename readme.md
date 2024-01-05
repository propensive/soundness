[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/iridescence/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/iridescence/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Iridescence

__Sophisticated representation of color__

_Iridescence_ implements several algorithms for working with colors represented in different forms.

## Features

- represents colors using a variety of different color models
- work with colors in RGB, HSV, CMY, CMYK, HSL, CIELAB and XYZ
- convert between any colors
- utilize color profiles (where necessary)
- provides a standard palette of named colors
- print colors as CSS, Hex or ANSI
- brighten, lighten, darken and blend colors
- calculate perceptual deltas between colors


## Availability Plan

Iridescence has not yet been published. The medium-term plan is to build Iridescence
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Iridescence.

Subsequently, Iridescence will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

_Iridescence_ provides seven different ways of representing colors:
- `Srgb`: [sRGB](https://en.wikipedia.org/wiki/SRGB), 
- `Xyz`: [CIE 1931 XYZ](https://en.wikipedia.org/wiki/CIE_1931_color_space)
- `Cielab`: [L*a*b*](https://en.wikipedia.org/wiki/CIELAB_color_space) or CIELAB
- `Cmy`: [CMY](https://en.wikipedia.org/wiki/CMY_color_model)
- `Cmyk`: [CMKY](https://en.wikipedia.org/wiki/CMYK_color_model)
- `Hsl`: [HSL](https://en.wikipedia.org/wiki/HSL_and_HSV)
- `Hsv`: [HSV](https://en.wikipedia.org/wiki/HSL_and_HSV)

Each color model uses either three or four continuous coordinates, all represented in Iridescence as `Double`s
in the unit interval (0 ≤ *c* ≤ 1), to describe an apparently full spectrum of colors perceived by the human
eye.

Given the complex nature of sight and color, different models make different tradeoffs in their representations
of different colors. While sRGB is the most direct representation of the colored light emitted by a computer
monitor, and indeed the most common representation for computers, CMY and CMYK are more common in printing.

Meanwhile, the HSL and HSV representations representations use the natural qualititative properties of hue,
saturation, lightness and brightness, and the XYZ and CIELAB color spaces are derived empirically. CIELAB
attempts to maintain the property that the Euclidean distance between two colors is proportional to the
perceptual difference between those colors, as determined by experimentation.

The particular color model should be chosen according to the requirements of the particular task.

### A Quick Example

```scala
import iridescence.*

given profile = profiles.Ultralume50

val pink: Cielab = colors.Ivory.cielab.mix(colors.DarkMagenta.cielab)
val palePink: Srgb = pink.srgb.hsv.tint(0.5).srgb
println(s"${color.ansiFg24}Hello World!")
```

### Types

Iridescence provides case classes to immutably represent each of the seven color models, above. Colors in one
representation can be directly converted into many of the other representations, and the remaining conversions
can be performed indirectly.

In general, every color representation provides the `Color#srgb` method to convert it to an `Srgb` value.
Conversely, the `Srgb` type provides the methods `cmy`, `cmyk`, `cielab`, `xyz`, `hsv` and `hsl` to convert to
these alternative representations.

While it would be possible to provide an n×n set of methods for converting between any pair of representations,
conversions which rely on an unspecified intermediate representation (for example converting between HSL and
CMYK) are generally _not_ provided unless the intermediate representation is a necessary step in the
calculation. This is to make it clear when conversions are happening.

For example, the methods `Hsl#srgb` and `Srgb#xyz` both exist, but `Hsl#xyz` is not implemented. However,
`Srgb#cielab` _is_ provided, even though the conversion is made via an intermediate XYZ value.

Here are some examples:
```scala
val DeepPink: Srgb = Srgb(1, 0.078, 0.576)
val Gold: Hsv = Srgb(1, 0.843, 0).hsv
val Gold2: Cmyk = Gold.srgb.cmyk
```

### Palette

The `colors` object provides a standard palette of about 140 named colors defined in sRGB space.

### Color profiles

Certain color representations rely on additional information that characterizes the conditions under which the
colors are encoded, and this information is necessary for conversions between certain color spaces.

For example, to convert from `Srgb` to `Cielab` requires a profile. Profiles are provided through the `Profile`
type, and several are provided in the `profiles` object. These should be specified, implicitly or explicitly
with each conversion, like so:

```scala
val color = DeepPink.cielab(using profiles.MidMorningDaylight)
```
or,
```scala
given Profile = profiles.CoolFluorescent
val color = LawnGreen.xyz
```

For generality, conversions to `Srgb` _always_ require a profile to be given (even for conversions where it is
not used). This restriction may be lifted later. A good default profile to use is the `Daylight` profile.
```scala
given Profile = profiles.Daylight
```

### Color methods

Additional methods are provided on certain color types for producing new colors from old. In general, these
methods are particular to the color model being used.

For example, the methods `saturate`, `desaturate`, `pure` and `rotate` (for changing the hue) are provided on
`Hsl` and `Hsv` types, while `Hsv` additionally provides `shade`, `tint` and `tone` methods. These latter
methods take `black` and/or `white` parameters to specify the amount of shading, tinting or toning to be
applied.

`Cielab` provides a `delta` method for comparing two colors (returning a `Double` in the unit interval), and the
`mix` method for combining two colors. `Cielab#mix` takes another `Cielab` color as its first parameter, and
a mix ratio (again, in the unit interval) as an optional second parameter. If left unspecified, it defaults to
the midpoint between the two colors.

Use of these methods might typically involve converting a color to the model which defines them, then applying
them as necessary, before converting back. For example,
```scala
colors.IndianRed.hsv.tone(0.2, 0.4).srgb
```

### Serialization

Different formats, languages and protocols will represent colors as strings in a number of different ways.
Iridescence provides serialization methods to the following formats:
- 24-bit ANSI foreground and background escape codes,
- RGB CSS, in the form `rgb(100, 78, 12)`,
- HSL CSS, in the form `hsl(310, 12%, 84%)`,
- 12-bit and 24-bit hexadecimal, e.g. `#afc` or `#ffed00`

These are available on the `Srgb` type, with the exception of `Hsl#css`.

### Limitations

There is no support for transparency.




## Status

Iridescence is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Iridescence is designed to be _small_. Its entire source code currently consists
of 443 lines of code.

## Building

Iridescence will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Iridescence?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Iridescence's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Iridescence and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `iridescence`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Iridescence's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Iridescence are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/iridescence/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Iridescence
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Iridescence was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

The word _iridescent_, defined as "having a play of changeable colors", also describes the functionality of Iridescence.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo illustrates a color wheel.

## License

Iridescence is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

