[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/escapade/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/escapade/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Escapade

__The adventure of ANSI escape codes__

__Escapade__ makes it easy to work safely with strings containing ANSI escape codes.

## Features

- provides a representation of strings containing ANSI escape codes
- support for 24-bit color, integrated with [Iridescence](https://github.com/propensive/iridescence)
  color representations
- introduces the `ansi""` string interpolator
- constant-time reduction to "plain" strings and printed length
- extensible support for different substitution types
- introduces "virtual" escapes with stack-based region tracking

## Availability Plan

Escapade has not yet been published. The medium-term plan is to build Escapade
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Escapade.

Subsequently, Escapade will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### About ANSI Codes

[ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code) provide a variety of features in
terminal emulators for performing operations such as positioning the cursor, changing the appearance
of text with styles like bold, italic and strike-through, as well as foreground and background
colors.

### Creating ANSI strings

To create an ANSI string, we use the `ansi""` interpolator. This works like an ordinary string
interpolator, allowing substitutions of stringlike values.

But substitutions may also be `Escape` instances, which do not insert any visible characters, but
may change the appearance or style of subsequent characters. Many `Escape` instances are defined in
the `escapes` object, for example, `Bold`, `Underline` and `BrightRedFg`.

These escapes may be used in an ANSI string, like so:
```scala
import escapes.*
val txt = ansi"This text is ${Bold}bold, ${Underline}underlined and ${BrightRedFg}bright red."
```

This introduces the bold and underline styles and the bright red color to the string, but once
introduced, they continue indefinitely.

Thankfully, Escapade provides a convenient way to terminate styles introduced by ANSI escapes. If
the character immediately following the escape is a recognized opening bracket (`(`, `[`, `{`, `
«`,
`<`), then the style will continue until a corresponding closing brace is found in the string, i.e.
`)`, `]`, `}`, `
»` or `>`.

For example,
```scala
ansi"This text is $Bold[bold], $Underline{underlined} and $BrightRedFg<bright red>."
```
will apply each style only to the words inside the brackets.

Plenty of choice is given over which type of brackets to use, so that a choice can (hopefully) be
made which does not conflict with the real content of the string. Regions may be nested arbitrarily
deep, and different bracketing styles may be used, but nested regions form a stack which must be
terminated in order. So any closing bracket other than the type corresponding to the most recent
opening bracket will not be given special treatment.

For example,
```scala
ansi"This text is $Bold[bold and $Italic{italic] text.}"
```
might be intending to display the final word, `text`, in italic but not bold. But the mismatched
brackets would treat `italic] text.` as literal text, rendered in italic. And, in fact, the ANSI
string would not compile due to the unclosed `[` bracket.

### Combining colors

While styles such as _bold_, _italic_, _underline_ and _reverse_ may be combined independently in a
string, the situation is more complex with colors, as a new color simply replaces an old one, and
it is not normally possible to restore the previous color; only to "reset" the color.

Indeed, this is what happens using the standard ANSI escapes provided in the `escapes` object.

But Escapade also provides stack-based tracking for colored text, so that regions may be nested, and
the underlying color may be restored. This uses colors from
[Iridescence](https://github.com/propensive/iridescence/) which may be substituted straight into an
ANSI string, like so:

```scala
import iridescence.*
import colors.*
ansi"$Gold[gold, $Indigo[indigo, $HotPink[hot pink], indigo] $White[and] gold]"
```

### Manipulating colors

Each substitution into an `ansi""` string interpolator may apply a change to the existing style,
represented by and tracked throughout the string as an instance of the case class, `Style`.
Typically, these changes will specify the new state of properties such as _bold_, _italic_ or the
background color.

But the changes may also be a transformation of the existing style information. For example, the
bold state could be flipped depending on what it was before, or the foreground color could be
mixed with black to give a "faded" or "darkened" effect to the text, without changing its hue.

Any such transformation requires an object to be used as a substitution to an interpolated string
to introduce it, plus a corresponding contextual `Stylize` instance, for example:
```scala
case class Fade(amount: Double)

given Stylize[Fade] = fade =>
  Stylize { style =>
    style.copy(fg = style.fg.hsv.shade(fade.amount).srgb)
  }
```





## Status

Escapade is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Escapade is designed to be _small_. Its entire source code currently consists
of 564 lines of code.

## Building

Escapade will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Escapade?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Escapade's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Escapade and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `escapade`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Escapade's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Escapade are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/escapade/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Escapade
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Escapade was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

An __escapade__ is a "wild and exciting undertaking" which is "not necessarily lawful"; like the variety of _escape_ codes that are only valid inside an ANSI terminal.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meaningsâ€”since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows a hot-air balloon, indicative of an escapade.

## License

Escapade is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

