[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/surveillance/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/surveillance/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Surveillance

__To make I/O in Scala elegant and typesafe__

__Surveillance__ watches directories for changes, and provides a `LazyList` of streaming `WatchEvent`s. While it
works well with [Galilei](https://github.com/propensive/galilei/), it can work with any representation of
paths.

## Features

- simple API for most common filesystem operations
- read from and write to files on disk with a variety of different types
- simple streaming to and from disk with `LazyList`s
- employs `IArray[Byte]` for fast, random-access, immutable chunking of byte data
- encoding-aware operations involving `String`s


## Availability

Surveillance has not yet been published as a binary.

## Getting Started

Surveillance watches directories for changes to their contents, through an extension method, `watch()`, on a
type representing a directory or a `Seq` of such types. For [Galilei](https://github.com/propensive/galilei)'s
`Directory` type, this works straight away. Other libraries which provide directory-like types can integrate with
Surveillance just by defining two simple [typeclass instances](#defining-typeclass-instances).

### Watching

A simple setup for watching a directory looks like this:
```scala
import galilei.*
import serpentine.*
import surveillance.*

val dir = (Unix / p"home" / p"work" / p"updates").directory()
val watcher = dir.watch()
```

Constructing a new `Watcher` on a directory will register that directory with the filesystem's filewatching service
and start a new thread to respond to updates.

The most important method of a `Watcher` is its `stream` method, which will return a `LazyList[WatchEvent]`




## Status

Surveillance is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Surveillance is designed to be _small_. Its entire source code currently consists
of 109 lines of code.

## Building

Surveillance can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Surveillance are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/surveillance/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Surveillance easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Surveillance was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

This library watches directories for changes, which is to say it keeps them under __surveillance__.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows a glistening eye, watching or _surveilling_.

## License

Surveillance is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
