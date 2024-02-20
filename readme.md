[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/punctuation/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/punctuation/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Punctuation

__Typesafe parsing and rendering of markdown__

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


## Availability Plan

Punctuation has not yet been published. The medium-term plan is to build Punctuation
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Punctuation.

Subsequently, Punctuation will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

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





## Status

Punctuation is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Punctuation is designed to be _small_. Its entire source code currently consists
of 617 lines of code.

## Building

Punctuation will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Punctuation?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Punctuation's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Punctuation and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `punctuation`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Punctuation's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Punctuation are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/punctuation/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Punctuation
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Punctuation was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

To _punctuate_ is "to mark with points" (or more specifically, symbolic characters) and also, "to stress or single out as important", which describes Markdown in both the literal sense that punctuation characters are added to text, and in the figurative sense that this punctuates a document with stress and other styled emphasis.

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

The logo shows a downward arrow overlaid on a capital letter M, representing the "*M*ark-down" format.

## License

Punctuation is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

