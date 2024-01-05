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


## Availability Plan

Harlequin has not yet been published. The medium-term plan is to build Harlequin
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Harlequin.

Subsequently, Harlequin will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

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

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Harlequin is designed to be _small_. Its entire source code currently consists
of 94 lines of code.

## Building

Harlequin will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Harlequin?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Harlequin's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Harlequin and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `harlequin`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Harlequin's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Harlequin are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/harlequin/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Harlequin
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Harlequin was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

A harlequin's clothes are a patchwork of bright colors, much as highlighted source code is.

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

The logo shows the motley pattern of a harlequin.

## License

Harlequin is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

