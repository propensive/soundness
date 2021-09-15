[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
[<img src="https://vent.dev/badge/propensive/escapade" height="24">](https://vent.dev/)
<img src="/doc/images/github.png" valign="middle">

# Escapade

__Escapade__ makes it easy to work safely with strings containing ANSI escape codes.

## Features

- provides a representation of strings containing ANSI escape codes
- support for 24-bit color, integrated with [Iridescence](https://github.com/propensive/iridescence)
  color representations
- introduces the `ansi""` string interpolator
- constant-time reduction to "plain" strings and printed length
- extensible support for different substitution types
- introduces "virtual" escapes with stack-based region tracking

## Getting Started

## About ANSI Codes

[ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code) provide a variety of features in
terminal emulators for performing operations such as positioning the cursor, changing the appearance
of text with styles like bold, italic and strike-through, as well as foreground and background
colors.

## Creating ANSI strings

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
<<<<<<< HEAD
will apply each style only to the words inside the brackets.
=======
will apply each style only to the words inside the brackets.
>>>>>>> b739b35

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

## Combining colors

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

## Manipulating colors

TBC

## Status

Escapade is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Escapade&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/escapade`.
```
fury layer clone -i propensive/escapade
```
or imported into an existing layer with,
```
fury layer import -i propensive/escapade
```

## Contributing

Contributors to Escapade are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/escapade/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Escapade easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
answers given in private.

## Author

Escapade was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

TBC

## License

Escapade is copyright &copy; 2019-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
