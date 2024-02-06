[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/savagery/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/savagery/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Savagery

__Scala-ble Vector Graphics__

While applications like [Inkscape](https://inkscape.org/) make it easy to
create vector graphics in SVG format, this approach is optimized for one-off
productions by graphic artists; it is not possible to create graphics
programmatically, dynamically or at scale from an input, for creating visual
languages or stylized diagrams. This is the role that _Savagery_ aims to fill.

## Features

- construct Scalable Vector Graphics programmatically
- compose low-level components into larger components as easily as applying functions
- renders generated SVG as XML


## Availability Plan

Savagery has not yet been published. The medium-term plan is to build Savagery
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Savagery.

Subsequently, Savagery will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

TBC




## Status

Savagery is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Savagery is designed to be _small_. Its entire source code currently consists
of 132 lines of code.

## Building

Savagery will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Savagery?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Savagery's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Savagery and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `savagery`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Savagery's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Savagery are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/savagery/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Savagery
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Savagery was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Savagery was chosen because it contains the letters S, V and G in order, and offers a pessimistic commentary on the nature of vector graphics.

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

The logo is a stylized variation of the official SVG logo.

## License

Savagery is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

