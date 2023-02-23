[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/diuretic/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/diuretic/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Diuretic

_Diuretic_ provides typeclass instances for
[Anticipation](https://github.com/propensive/anticipation/) typeclasses for the
Java Standard Library. Many of these types have better alternatives in the
[Scala One](https://github.com/propensive/one/) ecosystem, but there are times
when it is necessary to work with types from the Java Standard Library.

## Features

- provides typeclasses for working with time, filesystem objects and URLs
- implementations for `java.time.Instant` & `java.util` time types, `java.io` &
  `java.nio` filesystem types, and `java.net.URL`
- enables Java URLs to be used with the
  [Telekinesis](https://github.com/propensive/telekinesis/) HTTP client
- enables numerous projects like
  [Surveillance](https://github.com/propensive/surveillance/) to work with Java
IO and NIO types like `File` and `Path`
- allows numerous projects to use `java.time` or `java.util` types
- integration with other libraries that use
  [Anticipation](https://github.com/propensive/anticipation/) is seamless


## Availability

Diuretic has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

Using Diuretic is usually as simple as adding an import, alongside another
project which implements
[Anticipation](https://github.com/propensive/anticipation/) typeclasses.
Currently six contextual imports are available for three different Anticipation
modules.

### Time Representation

Diuretic provides a choice of three different representations of durations and instants:

- `timeRepresentation.javaTime`—uses `java.time.Instant` for instants and
  `Long` for durations
- `timeRepresentation.javaLongTime`—uses `Long` for both instants and durations
- `timeRepresentation.javaUtil`—uses `java.util.Date` for instants and `Long`
  for durations

### File Representation

Diuretic provides a choice of two different file representations:

- `fileRepresentation.javaNio`—uses `java.nio.file.Path` for files, directories and paths
- `fileRepresentation.javaIo`—uses `java.io.File` for files, directories and paths

A possible future version may offer representations which use distinct types
for paths (which may not relate to a file or directory) and files/directories.

### URL Representation

- `urlRepresentation.javaNet`—uses `java.net.URL` for URLs


## Related Projects

The following _Scala One_ libraries are dependencies of _Diuretic_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp;

The following _Scala One_ libraries are dependents of _Diuretic_:

[![Guillotine](https://github.com/propensive/guillotine/raw/main/doc/images/128x128.png)](https://github.com/propensive/guillotine/) &nbsp; [![Parasitism](https://github.com/propensive/parasitism/raw/main/doc/images/128x128.png)](https://github.com/propensive/parasitism/) &nbsp; [![Profanity](https://github.com/propensive/profanity/raw/main/doc/images/128x128.png)](https://github.com/propensive/profanity/) &nbsp;

## Status

Diuretic is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Diuretic is designed to be _small_. Its entire source code currently consists
of 56 lines of code.

## Building

Diuretic can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Diuretic are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/diuretic/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Diuretic easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Diuretic was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

The name _Diuretic_ alludes to a side-effect of drinking coffee, or in our case, Java.

## License

Diuretic is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
