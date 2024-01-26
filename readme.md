[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/serpentine/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/serpentine/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Serpentine

__Precise navigation of paths__

__Serpentine__ is a small library for handling the abstract notion of paths, distinguishing between
relative and absolute forms, rooted on some value. This may be useful for representing paths on a
filesystem, resources on an HTTP server, descendants in a family tree, or many other concepts which
follow a hierarchical pattern.

## Features

- representations of hierarchical paths
- designed for extension and use in many concrete contexts
- distinguishes between absolute and relative paths

## Availability Plan

Serpentine has not yet been published. The medium-term plan is to build Serpentine
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Serpentine.

Subsequently, Serpentine will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

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

### Other Methods

The method `++` can add a `Relative` path to an `Absolute` path, and return a new `Absolute` path.

Similarly, `Absolute#relativeTo` takes another `Absolute` path and returns a `Relative` instance
that, when applied with `++` to the first path, produces the second path.

The `Absolute#conjunction` method will find the closest common parent of the path and its parameter.

Note that `Path`s are not aware of their children, so there is no `children` method, but this may be
provided by individual implementations.

### Exceptions

Many operations on `Path`s may attempt (directly or indirectly) to access the parent of the root.
This is not possible, and if this happens, a `RootBoundaryExceeded` exception will be thrown.

### Generic relative paths

Given that a relative path is (by definition) not attached to any particular root, all instances of
`Root#Path.Relative` inherit from `GenericRelative` which gives users the choice, when implementing
APIs that accept relative paths, between accepting _any_ kind of relative path (regardless of its
origin) and accepting just those originating from a particular root.





## Status

Serpentine is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Serpentine is designed to be _small_. Its entire source code currently consists
of 958 lines of code.

## Building

Serpentine will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Serpentine?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Serpentine's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Serpentine and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `serpentine`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Serpentine's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Serpentine are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/serpentine/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Serpentine
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Serpentine was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

A path which is _serpentine_ may be a challenge to navigate, which is where _Serpentine_ can help.

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

The logo shows a serpentine section of river, meandering.

## License

Serpentine is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

