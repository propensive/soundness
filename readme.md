[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/jovian/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/jovian/actions)
[<img src="https://img.shields.io/badge/gitter-discuss-f00762?style=for-the-badge" height="24">](https://gitter.im/propensive/jovian)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/CHCPjERybv)
[<img src="https://img.shields.io/matrix/propensive.jovian:matrix.org?label=MATRIX&color=0dbd8b&style=for-the-badge" height="24">](https://app.element.io/#/room/#propensive.jovian:matrix.org)
[<img src="https://img.shields.io/twitter/follow/propensive?color=%2300acee&label=TWITTER&style=for-the-badge" height="24">](https://twitter.com/propensive)
[<img src="https://img.shields.io/maven-central/v/com.propensive/jovian-core_2.12?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/jovian-core_2.12)
[<img src="https://vent.dev/badge/propensive/jovian" height="24">](https://vent.dev/)

<img src="/doc/images/github.png" valign="middle">

# Jovian

__Jovian__ is a simple library for performing disk I/O with Scala. It provides access to most filesystem operations through the `Path` type—an abstract representation of a file or a directory—plus typeclass-based `read` and `write` methods which can use ad-hoc types, including streaming types like `LazyList`. Jovian is designed to take advantage of Scala 3's safer exceptions.

## Features

- simple API for most common filesystem operations
- read from and write to files on disk with a variety of different types
- simple streaming to and from disk with `LazyList`s
- employs `IArray[Byte]` for fast, random-access, immutable chunking of byte data
- encoding-aware operations involving `String`s


## Getting Started

## The `Path` type

Most operations are accessible through the `Path` type. This is a canonicalized string representation of a path
on disk, which may be a directory or file, or may not exist.

## Reading

Files can be read with the `Path#read` method, which takes the type it should return, for example,
`path.read[String]()` or `path.read[LazyList[String]]()`. If used in a position with an expected type, this type
parameter may be omitted, for example:
```scala
def contents: String = path.read()
```

The `read` method takes an optional `limit` value parameter which specifies a limit on the number of byte that
may be read. This defaults to the conservative figure of `65536`. If this is exceeded, a `TooMuchData` exception
is thrown.

## Writing

It's possible to write to a file using the `Path#write` and `Path#append` methods. These each take a single
`content` parameter, which can be one of a variety of types. As standard, these include,
- `IArray[Byte]`
- `LazyList[IArray[Byte]]`
- `String`
- `LazyList[String]`
which together support complete and streamed byte and character data.

## Status

Jovian is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without guarantee of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement of designs
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Jovian&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/jovian`.
```
fury layer clone -i propensive/jovian
```
or imported into an existing layer with,
```
fury layer import -i propensive/jovian
```
A binary is available on Maven Central as `com.propensive:jovian-core_<scala-version>:0.1.0`. This may be added
to an [sbt](https://www.scala-sbt.org/) build with:
```
libraryDependencies += "com.propensive" %% "jovian-core" % "0.1.0"
```

## Contributing

Contributors to Jovian are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/jovian/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Jovian easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
the answers.

## Author

Jovian was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).



## License

Jovian is copyright &copy; 2019-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
