[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/cacophony/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/cacophony/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Cacophony

__A library for working with audio in Scala__

Audio is commonplace in many programming environments, so it ought to be as
easy to work with as text. _Cacophony_ aspires to make this possible.

## Features

- simple immutable API for reading and writing PCM audio files
- supports standard Java Sound formats: WAVE, AIFF, AIFC, AU and SND
- read directly from any [Turbulence](https://github.com/propensive/turbulence) source
- non-PCM input streams readable by `javax.sound.sampled` are auto-converted to 16-bit signed PCM
- access audio metadata (sample rate, duration, channels, bit depth) and individual samples


## Availability







## Getting Started

All terms and types are defined in the `cacophony` package, and can be used with:
```scala
import cacophony.*
```

### Reading audio

To read an audio file of a known type, the `read` method should be used with
the appropriate codec object. Currently, five codecs are defined:
 - `Wave`
 - `Aiff`
 - `Aifc`
 - `Au`, and
 - `Snd`

The source of the audio may be any source that can be read as `Bytes` by
Turbulence, for example:
```scala
import galilei.*

val audio = (% / "home" / "work" / "sound.wav").read[Audio in Wave]
```

The resultant value will be an instance of `Audio in Wave`, that is, an `Audio`
parameterized with the erased phantom type `Wave`.

### Accessing `Audio` data

The number of channels, frame count, sample rate and duration are available as
`channels`, `frames`, `sampleRate` and `duration` methods on `Audio`. The
`sampleRate` and `duration` are returned as `Quantity` values from
[Quantitative](https://github.com/propensive/quantitative).

A single PCM sample, expressed as a raw signed integer, can be accessed using
the `apply` method, i.e. `audio(channel, frame)`.



## Status

Cacophony is classified as __embryotic__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Cacophony is designed to be _small_.

## Building

Cacophony will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Cacophony?".

1. *Copy the sources into your own project*

   Read the `fury` file in the repository root to understand Cacophony's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Cacophony and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.

   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `cacophony`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Cacophony's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Cacophony are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/cacophony/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Cacophony
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
respond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Cacophony was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

A _cacophony_ is a harsh, discordant mixture of sounds. The library deals with
representing arbitrary audio in Scala, including, perhaps, cacophonies.

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

Cacophony's logo is a stylized speaker, the device that produces audible sound.

## License

Cacophony is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).
