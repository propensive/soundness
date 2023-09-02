[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/chiaroscuro/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/chiaroscuro/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Chiaroscuro

__Compare and contrast similar objects and highlight their differences__

A simple equality test can determine whether two values are identical or not,
but it offers no explanation for _how_ two unequal values are different.
_Chiaroscuro_ provides a means to compare two objects and see their structural
differences.

## Features

TBC


## Availability

Chiaroscuro has not yet been published as a binary.

## Getting Started

Two values of the same type can be compared with the `contrastWith` method,
provided by Chiaroscuro. This will return an instance of `Semblance`,
describing the similarity of one value with the other, and will be one of three
cases:
- `Identical`, if the two values are the same
- `Different`, if the two values are different, without any further detail
- `Similar`, if the two values are different, with a breakdown of their
  similarities and differences

The last of these three cases is the most interesting, though it is only a
possible result for values of certain types, namely types which can be
destructured into components which can themselves be compared, recursively.
Simple types like `Text` (or `String`) and primitive types like `Int` or
`Double` can only ever be `Identical` or `Different`, but product types, like
case class instances, sum types, like enums or sealed traits, and sequence
types, like `List` or `IArray` can result in semblances which are neither
`Identical` nor `Different` but `Similar`.

The three cases of `Semblance` are defined (recursively) as follows:
- `Identical(value: Text)`
- `Different(left: Text, right: Text)`
- `Similar(comparison: IArray[(Text, Semblance)], left: Text, right: Text)




## Status

Chiaroscuro is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Chiaroscuro is designed to be _small_. Its entire source code currently consists
of 194 lines of code.

## Building

Chiaroscuro can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Chiaroscuro are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/chiaroscuro/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Chiaroscuro easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Chiaroscuro was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Chiaroscuro_ is a Renaissance painting technique of expressing strong contrasts between light and dark, while Chiaroscuro provides the means to highlight the contrasts between two values.

### Pronunciation

`/kɪˌɑːɹəˈskʊəɹəʊ/`

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## License

Chiaroscuro is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
