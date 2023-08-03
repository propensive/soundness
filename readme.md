[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/harlequin/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/harlequin/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Harlequin

__Syntax highlighting for Scala code__

Scala source code is nontrivial to parse, and while many syntax-highlighting scripts or
configurations exist (for example, for editors), the complexity of the Scala language means that
they can make mistakes when encountering unusual combinations of tokens. _Harlequin_ uses the
actual Scala compiler to ensure that code is parsed exactly as it would be during compilation.

The result of parsing code contains only three different types of token—spaces, newlines and
content—which lend themselves to conversion to HTML or another format.

## Features

- Parses fragments of Scala code using the Scala compiler
- Takes a string and returns a `List` of simple tokens
- Tokens represent newlines, spaces and content, for easy transformation into other formats, like HTML
- Content includes metadata to indicate its nature
- Erroneous content may also be parsed
- Parsing is fast; a short fragment will typically take a few milliseconds


## Availability

Harlequin has not yet been published as a binary.

## Getting Started

_Harlequin_ provides only a single method, `ScalaSyntax.highlight`, which takes a `Text` value and
returns a `List[Token]`, like so:
```scala
import harlequin.*, gossamer.*

val code = t"def inc(x: Int): Unit =\n  x + 1"
val tokens = ScalaSyntax.highlight(code)
```

The returned value, `tokens`, for this example will be the following `List`:
```scala
List(Code("def", Keyword), Space(1), Code("inc", Term), Code("(", Parens), Code("x", Term),
    Code(":", Symbol), Space(1), Code("Int", Type), Code(")", Parens), Code(":", Symbol), Space(1),
    Code("Unit", Type), Space(1), Code("=", Symbol), Newline, Space(2), Code("x", Ident), Space(1),
    Code("+", Ident), Space(1), Code("1", Number))
```

Here, each token is either, a space of a particular size, for example, `Space(2)` represents two
space characters, a `Newline`, or a fragment of code, consisting of the text with an "accent"; an
indication of the meaning of that code token.

### Accents

The accent will be one of the following possible values:

- `Error`, an erroneous token,
- `Ident`, a reference to a term,
- `Keyword`, a non-modifier keyword,
- `Modifier`, a modifier keyword,
- `Number`, a number literal,
- `Parens`, parenthesis,
- `String`, a string literal (including interpolated strings),
- `Term`, a term definition,
- `Type`, a type definition.

Typically, these would be mapped to different colors during conversion to markup.

It is likely that as Harlequin evolves, the set of `Accent` values will grow.



## Status

Harlequin is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Harlequin is designed to be _small_. Its entire source code currently consists
of 94 lines of code.

## Building

Harlequin can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Harlequin are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/harlequin/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Harlequin easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Harlequin was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

A harlequin's clothes are a patchwork of bright colors, much as highlighted source code is.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## License

Harlequin is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
