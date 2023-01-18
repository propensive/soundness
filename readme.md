[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/galilei/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/galilei/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Galilei

__Galilei__ is a simple library for performing disk I/O with Scala. It provides access to most filesystem
operations through the `Path` type—an abstract representation of a file or a directory—plus typeclass-based
`read` and `write` methods which can use ad-hoc types, including streaming types like `LazyList`. Galilei is
designed to take advantage of Scala 3's safer exceptions.

## Features

- simple API for most common filesystem operations
- read from and write to files on disk with a variety of different types
- simple streaming to and from disk with `LazyList`s
- employs `IArray[Byte]` for fast, random-access, immutable chunking of byte data
- encoding-aware operations involving `String`s


## Availability

Galilei has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

Unlike many filesystem APIs, __Galilei__ provides different types for `Path`s, `File`s, `Directory`s
and `Symlink`s. While a `Path` (which is defined in [Serpentine](https://github.com/propensive/serpentine/))
represents some location within a filesystem—which may or may not exist and may be either a file,
directory or symlink—instances of `File`, `Directory` and `Symlink` should only exist when the
corresponding file, directory or symlink exists on disk.

While there is always the possibility that the existence or nature of a `File`, `Directory` or
`Symlink` could change between the instance being created—after all, we are using _immutable_ heap
objects to represent _mutable_ disk objects which could be changed by other operating system
processes—this is a race condition (and would be with other designs, too), and is _recoverable_
through exception handling.

### Filesystems

`Path`s may be relative or absolute, and must be rooted against a particular filesystem. On Linux
Mac OS X, and other UNIX-like systems, there is a single root (with the type `Filesystem`, called
`Unix`, whereas Windows provides multiple roots, indicated by a single alphabetic drive letter, such
as `windows.DriveC`, which corresponds to the drive, `C:\`.

The `/` operator (regardless of the operating system's standard path separator) may be used on
`Filesystem` and `Path` objects to navigate the directory sturcture. For example,
```scala
val filesPath = Unix / "usr" / "share" / "files"
```
or,
```scala
val programsPath = windows.DriveC / "Program Files"
```

### Path-dependent Types

The types `Path`, `File`, `Directory` and `Symlink` are all path-dependent types, defined within
a particular `Filesystem` object, and the type system will not allow them to be mixed arbitrarily
between different filesystems.

For example, a `Unix.Path` instance may produce a `Unix.File` value, whose `hardLinkTo` method
can accept a `Unix.Path`, but not a `windows.DriveD.Path`.

However, we could still put a `Unix.Path` in the same `List` as a `windows.DriveC.Path`. That `List`
would have the type `List[Filesystem#Path]`, and many of its members' methods, such as `parent` or
`uriString`, could still be used.

For example, the expression,
```scala
List(Unix / "home" / "work", windows.DriveC / "Documents").map(_.directory.parent)
```
has the type `List[Filesystem#Directory]`

### Reading

`File`s can be read with the `File#read` method, which takes, as a parameter, the type it should
return, for example, `path.read[String]()` or `path.read[LazyList[String]]()`. If used in a position
with an expected type, this type parameter may be omitted, for example:
```scala
def contents: String = path.read()
```

The `read` method takes an optional `limit` value parameter which specifies a limit on the number of
bytes that may be read. This defaults to the conservative figure of `65536`. If this is exceeded, a
`TooMuchData` exception is thrown.

### Writing

It's possible to write to a file using the `File#write` and `File#append` methods. These each take a
single `content` parameter, which can be one of a variety of types. As standard, these include,
- `IArray[Byte]`
- `LazyList[IArray[Byte]]`
- `String`
- `LazyList[String]`
which together support complete and streamed byte and character data.


## Related Projects

The following _Scala One_ libraries are dependencies of _Galilei_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp; [![Eucalyptus](https://github.com/propensive/eucalyptus/raw/main/doc/images/128x128.png)](https://github.com/propensive/eucalyptus/) &nbsp; [![Gastronomy](https://github.com/propensive/gastronomy/raw/main/doc/images/128x128.png)](https://github.com/propensive/gastronomy/) &nbsp; [![Kaleidoscope](https://github.com/propensive/kaleidoscope/raw/main/doc/images/128x128.png)](https://github.com/propensive/kaleidoscope/) &nbsp; [![Serpentine](https://github.com/propensive/serpentine/raw/main/doc/images/128x128.png)](https://github.com/propensive/serpentine/) &nbsp;

The following _Scala One_ libraries are dependents of _Galilei_:

[![Exoskeleton](https://github.com/propensive/exoskeleton/raw/main/doc/images/128x128.png)](https://github.com/propensive/exoskeleton/) &nbsp; [![Merino](https://github.com/propensive/merino/raw/main/doc/images/128x128.png)](https://github.com/propensive/merino/) &nbsp; [![Oubliette](https://github.com/propensive/oubliette/raw/main/doc/images/128x128.png)](https://github.com/propensive/oubliette/) &nbsp;

## Status

Galilei is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Galilei is designed to be _small_. Its entire source code currently consists
of 447 lines of code.

## Building

Galilei can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Galilei are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/galilei/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Galilei easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Galilei was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Galilei's primary focus is handling Input and Output, or _I/O_, and is a pun based on the name of the moon _Io_, one of the four moons of Jupiter discovered by Galileo Galilei.

## License

Galilei is copyright &copy; 2020-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
