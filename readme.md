[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/lithography/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/lithography/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Lithography

__Tools for working with characters in Scala__

Lithography provides facilities for working with characters, in particular, by providing support for
different character encodings, and utilizing additional Unicode metadata.

## Features

- provides additional character metadata from the Unicode database
- facilitates accurate text width calculations, particularly for East Asian scripts


## Availability

Lithography has not yet been published as a binary. It is currently waiting for the
final release of Scala 3.3.

## Getting Started

### Character Display Width

Lithography provides an extension method, `displayWidth`, on `Char` which will
return the amount of space the glyph for that character will take up, when
rendered in a mono-spaced font.

Unsurprisingly, this will usually be `1`, but many characters in alphabets that
are not based on the Latin Alphabet will need two normal character widths of
space when rendered.

However, calculating the width of a character (and, in particular a string of
characters) will be much slower if every character must be checked individually,
and totalled, when the `length` field of a string can provide the same value in
constant (and fast) time, for strings which are known not to contain any "wide"
characters.

Therefore, methods which need to perform text width calculations can use either
a `uniform` mode or an `eastAsianScripts` mode, depending on the contextual
value imported from the `textWidthCalculation` package.

For example, compare,
```scala
import textWidthCalculation.uniform
'x'.displayWidth   // returns 1
'好'.displayWidth  // returns 1
```
and,
```scala
import textWidthCalculation.eastAsianScripts
'x'.displayWidth   // returns 1
'好'.displayWidth  // returns 2
```

[Gossamer](https://github.com/propensive/gossamer/) provides a corresponding
`displayWidth` extension method on all text-like types, which calculates the
display width of the entire string by summing its character widths, or, with
`textWidthCalculation.uniform` in scope, simply returns the `length` value.


## Status

Lithography is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Lithography is designed to be _small_. Its entire source code currently consists
of 243 lines of code.

## Building

Lithography can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Lithography are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/lithography/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Lithography easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Lithography was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Lithography is a process used for printing ink on paper. This library provides information on
how to print characters on screen.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## License

Lithography is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
