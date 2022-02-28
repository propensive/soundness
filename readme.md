[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/slalom/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/slalom/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/slalom-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/slalom-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
<img src="/doc/images/github.png" valign="middle">

# Slalom

__Slalom__ is a small library for handling the abstract notion of paths, distinguishing between
relative and absolute forms, rooted on some value. This may be useful for representing paths on a
filesystem, resources on an HTTP server, descendants in a family tree, or many other concepts which
follow a hierarchical pattern.

## Features

- representations of hierarchical paths
- designed for extension and use in many concrete contexts
- distinguishes between absolute and relative paths

## Availability

The current latest release of Slalom is __0.4.0__.

## Getting Started

The type `Root` defines a "root", beneath which any number of possible `Path` instances may exist in
a hierarchy, and the methods `parent`, `ancestor` and `/` may be used to navigate between them. For
abstract paths, the value `Base` can serve as a root node for a path hierarchy.

Here are some examples:
```scala
val crustaceans = Base / "eukaryota" / "animalia" / "arthropods" / "crustaceans"
val arthropods = crustaceans.parent
val animalia = crustaceans.ancestor(2)
```

`Path`s may be further distinguished as `Path.Relative` or `Path.Absolute`, where a `Relative` may
be converted into an `Absolute` by calling the `Path#absolute` method, and passing an absolute path
to which the relative path should be considered relative to. The result is typed as `Path.Absolute`.

`Path` objects, whether absolute or relative, serialize with `toString` according to the delimiters
in their root, which are defined in terms of a base name (for example, `/` or `classpath:`) and a
separator (for example, `\`, `/` or `.`).

Any implementation of `Root` should define these values, `prefix` and `separator`, respectively. For
example, the definition,
```scala
object Domain extends Root(prefix = "", separator = ".")
```
would ensure that,
```scala
Domain / "www" / "example" / "com"
```
would serialize to the string `"www.example.com"`.

Note that the `separator` is not included between the `prefix` and the first path element when
serializing, so _may_ need to be included in the `prefix` value itself.

## Other Methods

The method `++` can add a `Relative` path to an `Absolute` path, and return a new `Absolute` path.

Similarly, `Absolute#relativeTo` takes another `Absolute` path and returns a `Relative` instance
that, when applied with `++` to the first path, produces the second path.

The `Absolute#conjunction` method will find the closest common parent of the path and its parameter.

Note that `Path`s are not aware of their children, so there is no `children` method, but this may be
provided by individual implementations.

## Exceptions

Many operations on `Path`s may attempt (directly or indirectly) to access the parent of the root.
This is not possible, and if this happens, a `RootBoundaryExceeded` exception will be thrown.

## Generic relative paths

Given that a relative path is (by definition) not attached to any particular root, all instances of
`Root#Path.Relative` inherit from `GenericRelative` which gives users the choice, when implementing
APIs that accept relative paths, between accepting _any_ kind of relative path (regardless of its
origin) and accepting just those originating from a particular root.

## Related Projects

The following _Scala One_ libraries are dependencies of _Slalom_:

[![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp;

The following _Scala One_ libraries are dependents of _Slalom_:

[![Jovian](https://github.com/propensive/jovian/raw/main/doc/images/128x128.png)](https://github.com/propensive/jovian/) &nbsp; [![Scintillate](https://github.com/propensive/scintillate/raw/main/doc/images/128x128.png)](https://github.com/propensive/scintillate/) &nbsp;

## Status

Slalom is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Slalom is designed to be _small_. Its entire source code currently consists of 99 lines of code.

## Building

Slalom can be built on Linux or Mac OS with Vex, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Slalom are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/slalom/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Slalom easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Slalom was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

A _slalom_ is a challenging path around obstacles, usually taken downhill on skis, and Slalom provides functionality for the challenge of working with paths.

## License

Slalom is copyright &copy; 2021-22 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
