[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/hallucination/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/hallucination/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Hallucination

__A library for working with images in Scala__

Images are commonplace in many programming environments, so they ought to be as
easy to work with as text. _Hallucination_ aspires to make this possible.

## Features

- simple immutable API for reading and writing image files
- supports standard Java ImageIO formats: GIF, JPEG, BMP and PNG
- read directly from any [Turbulence](https://github.com/propensive/turbulence) source
- access image metadata and individual pixel colors


## Availability Plan

Hallucination has not yet been published. The medium-term plan is to build Hallucination
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Hallucination.

Subsequently, Hallucination will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

All terms and types are define in the `hallucination` package, and can be used with:
```scala
import hallucination.*
```

### Reading images

To read an image of a known type, the `read` method of its codec object should
be used. Currently, four codecs are defined:
 - `Png`
 - `Gif`
 - `Jpeg`, and
 - `Bmp`

The source of the image may be any source that can be read as `Bytes` by
Turbulence, for example:
```scala
import galilei.*

val image = Png.read(% / p"home" / p"work" / p"image.png")
```

The resultant value will be an instance of `Image[Png]`, that is, an `Image`
parameterized with the erased phantom type `Png`.

It's possible to use Turbulence's `readAs` method to read an `Image[?]` from a
source of `Bytes`, for example:
```scala
import turbulence.*
import hellenism.*

val icon = (Classpath / p"icon.bmp").readAs[Image[Bmp]]
```

### Accessing `Image` data

The width and height of the image are available through the `width` and
`height` methods of `Image[?]`.

The color of the pixel at given coordinates within the image can be accessed,
as an `Rgb24` value, using `Image`'s `apply` method, i.e. `image(x, y)`.



## Status

Hallucination is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Hallucination is designed to be _small_. Its entire source code currently consists
of 60 lines of code.

## Building

Hallucination will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Hallucination?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Hallucination's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Hallucination and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `hallucination`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Hallucination's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Hallucination are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/hallucination/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Hallucination
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Hallucination was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

An _hallucination_ is a sensation of something appearing real, but existing
only in ones mind. An image is a visual hallucination.

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

Hallucination's logo is a simplistic eye, the organ that is stimulated by images.

## License

Hallucination is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

