[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/hieroglyph/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/hieroglyph/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Hieroglyph

__Tools for working with characters in Scala__

Hieroglyph provides facilities for working with characters, in particular, by providing typesafe
support for different character encodings, and utilizing additional Unicode metadata.

## Features

- specification of character encodings
- warnings issued if using an encoding which may not be available
- provides additional character metadata from the Unicode database
- facilitates accurate text width calculations, particularly for East Asian scripts


## Availability

Hieroglyph has not yet been published as a binary.

## Getting Started

### Character Display Width

Hieroglyph provides an extension method, `displayWidth`, on `Char` which will
return the amount of space the glyph for that character will take up, when
rendered in a mono-spaced font.

Unsurprisingly, this will usually be `1`, but many characters in alphabets that
are not based on the Latin Alphabet will need two normal character widths of
space when rendered.

For example, compare,
```scala
'x'.displayWidth   // returns 1
'好'.displayWidth  // returns 2
```

However, calculating the width of a character (and, in particular a string of
characters) will be much slower if every character must be checked individually,
and totalled, when the `length` field of a string can provide the same value in
constant (and fast) time, for strings which are known not to contain any "wide"
characters.

[Gossamer](https://github.com/propensive/gossamer/) provides a corresponding
`displayWidth` extension method on all text-like types, which calculates the
display width of the entire string by summing its character widths, or, with
`textWidthCalculation.uniform` in scope, simply returns the `length` value.

Therefore, methods which need to perform text width calculations can use either
a `uniform` mode or an `eastAsianScripts` mode, depending on the contextual
value imported from the `textWidthCalculation` package.



## Status

Hieroglyph is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Hieroglyph is designed to be _small_. Its entire source code currently consists
of 323 lines of code.

## Building

Hieroglyph can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Hieroglyph are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/hieroglyph/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Hieroglyph easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Hieroglyph was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Hieroglyphics are elaborate characters, whose meaning requires interpretation, while _Hieroglyph_ is a library
which provides encodings to translate between characters and their binary representations.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo is a stylised rendering of the [Unicode](https://home.unicode.org/) logo.

## License

Hieroglyph is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
