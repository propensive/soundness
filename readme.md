[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/surveillance/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/surveillance/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/surveillance-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/surveillance-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Surveillance

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

The current latest release of Surveillance is __0.4.0__.

## Getting Started

Surveillance watches directories for changes to their contents, through an extension method, `watch()`, on a
type representing a directory or a `Seq` of such types. For [Galilei](https://github.com/propensive/galilei)'s
`Directory` type, this works straight away. Other libraries which provide directory-like types can integrate with
Surveillance just by defining two simple [typeclass instances](#defining-typeclass-instances).

## Watching

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

## Defining Typeclass Instances


## Related Projects

The following _Scala One_ libraries are dependencies of _Surveillance_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp; [![Eucalyptus](https://github.com/propensive/eucalyptus/raw/main/doc/images/128x128.png)](https://github.com/propensive/eucalyptus/) &nbsp; [![Serpentine](https://github.com/propensive/serpentine/raw/main/doc/images/128x128.png)](https://github.com/propensive/serpentine/) &nbsp;

No other _Scala One_ libraries are dependents of _Surveillance_.

## Status

Surveillance is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Surveillance is designed to be _small_. Its entire source code currently consists of 103 lines of code.

## Building

Surveillance can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Surveillance are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/surveillance/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Surveillance easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Surveillance was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

This library watches directories for changes, which is to say it keeps them under __surveillance__.

## License

Surveillance is copyright &copy; 2022-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
