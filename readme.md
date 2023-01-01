[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/punctuation/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/punctuation/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/punctuation-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/punctuation-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Punctuation

_Punctuation_ provides a convenient representation of Markdown which disambiguates between inline-
and block-level content, with converters to
[Honeycomb](https://github.com/propensive/honeycomb) HTML and
[Escapade](https://github.com/propensive/escapade) ANSI-rendered text.

## Features

- parse Markdown content
- use different types for inline- and block-level content
- static typechecking of Markdown using interpolated strings
- support for substitutions of different types as formatted Markdown
- simple Scala AST representing Markdown
- convert inline- or block-level Markdown to HTML (using Honeycomb)
- convert Markdown to styled ANSI terminal text (using Escapade)


## Availability

The current latest release of Punctuation is __0.4.0__.

## Getting Started

Markdown may be read from a source such as a string or file with the `Markdown.parse` method. For example,
```scala
import punctuation.*
val md = Markdown.parse(t"## This is a subheading")
```
will return an instance of `Markdown[Block]`, which is a wrapper for a sequence of block-level Markdown
AST elements. In the example above, the result would be equal to,
`Markdown(Heading(2, Textual(t"This is a subheading")))`.

Block-level AST elements are:
- `ThematicBreak()`
- `Paragraph(inline*)`
- `Heading(level, inline*)`
- `FencedCode(lang, meta, text)`
- `BulletList(numbered, loose, items*)`
- `BlockQuote(block*)`
- `Reference(id, location)`
- `Table(parts*)`

Using [Honeycomb](https://github.com/propensive/honeycomb/) a `Markdown[Block]` instance may be converted
to HTML just by calling the `html` extension method on it. This extension method returns an instance of
`Seq[Html[Flow]].

### Inline Markdown

Often block-level Markdown elements are not desired, and the subset of Markdown that is valid in "inline"
contexts is required. The `Markdown.parseInline` method will parse an inline fragment of Markdown, returning
an instance of `Markdown[Inline]`.

Inline AST elements are:
- `Break()`
- `Emphasis(inline*)`
- `HtmlNode(text)`
- `Image(alt, src)`
- `Code(text)`
- `Strong(inline*)`
- `Textual(text)`
- `Link(location, inline*)`

Likewise, a `Markdown[Inline]` instance can be converted to HTML with the `html` extension method. Since
inline Markdown uses a more limited set of HTML tags, this method returns a `Seq[Html[Phrasing]]` which
allows it to be embedded within nodes such as `P`, which would not be the case for block-level Markdown
converted to HTML.

### `md""` Interpolator

In addition to parsing Markdown at runtime, it's possible to construct Markdown literals with the `md""`
interpolator. This is as simple as writing markdown inside quotes preceded by `md`, and it will be
parsed at compiletime. For example, `md"This is _emphasised_."`

The type of a `md""` literal will depend upon its content. If it includes block-level elements, its
return type will be `Markdown[Block]`, whereas if it uses only inline elements, it will be typed as
`Markdown[Inline]`.

Any errors in the Markdown content will be detected at compiletime, causing compile errors.

Substitutions into Markdown literals are also supported, provided the type of the substitution can be
converted to Markdown, and can appear at the position it's substituted. Conversion of a particular type
to Markdown is permitted by the existence of a contextual typeclass instance.


## Related Projects

The following _Scala One_ libraries are dependencies of _Punctuation_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp; [![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp; [![Harlequin](https://github.com/propensive/harlequin/raw/main/doc/images/128x128.png)](https://github.com/propensive/harlequin/) &nbsp; [![Honeycomb](https://github.com/propensive/honeycomb/raw/main/doc/images/128x128.png)](https://github.com/propensive/honeycomb/) &nbsp; [![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/) &nbsp;

No other _Scala One_ libraries are dependents of _Punctuation_.

## Status

Punctuation is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Punctuation is designed to be _small_. Its entire source code currently consists of 535 lines of code.

## Building

Punctuation can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Punctuation are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/punctuation/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Punctuation easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Punctuation was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

To _punctuate_ is "to mark with points" (or more specifically, symbolic characters) and also, "to stress or single out as important", which describes Markdown in both the literal sense that punctuation characters are added to text, and in the figurative sense that this punctuates a document with stress and other styled emphasis.

## License

Punctuation is copyright &copy; 2020-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
