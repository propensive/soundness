[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/hellenism/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/hellenism/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Hellenism

__Classpath management facilities for Scala__

Classpaths are fundamental to the the operation of the JVM, as the means by
which code and data is made available to the JVM. A classpath is specified
linearly as a sequence of directories or archives, which are aggregated into a
unified hierarchy. Working with these two forms can be tricky, and _Hellenism_
improves on the typesafety and syntax for accessing, manipulating and
constructing classpaths.

## Features

- navigate the classpath like a filesystem in [Galilei](https://github.com/propensive/galilei)
- read classpath resources like files with [Turbulence](https://github.com/propensive/turbulence) typeclasses
- access the underlying classpath from a classloader
- use polykinded types to get `ClassRef`s, for slightly more concise code


## Availability Plan

Hellenism has not yet been published. The medium-term plan is to build Hellenism
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Hellenism.

Subsequently, Hellenism will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### Getting a `Classpath`

A `Classpath` instance is an immutable value containing references to the sources of classes and classpath
resources. There are two variants which may be relevant:
 - `LocalClasspath`, which can include local filesystem directories and JAR files, as well as the core JVM
   classes, and,
 - `OnlineClasspath`, which can additionally include remote URLs

Both define an `entries` field, which is a `List[ClasspathEntry]`. `ClasspathEntry` is an enumeration including,
`Directory`, `Jarfile`, `Url` types, plus the singleton object, `JavaRuntime`. `Url` may only appear in an
`OnlineClasspath`. The distinction is made in the classpath's type because sometimes it is necessary to use only
locally-defined resources.

A `Classpath` can be constructed from a Java `URLClassLoader` instance, a subclass of the more general
`ClassLoader` whose sources may be inspected to build the `Classpath` instance.

#### Classpath Entries

Amongst `ClasspathEntry`s, `Directory`s, `Jarfile`s and `Url`s may be resolved to rich types representing
directories, files and URLs respectively, using typeclasses provided by
[Anticipation](https://github.com/propensive/anticipation/), just by calling their `apply()` methods.

### Classpath References

A resource, that is, some binary object accessed by a path name which is resolved on a classpath, is represented
by a `ClasspathRef`. This is an immutable value which represents the unresolved path to the resource, without
reference to particular classpath or the data it refers to.

A `ClasspathRef` can be accessed using a [Serpentine](https://github.com/propensive/serpentine/) path from the
`Classpath` object, for example:
```scala
val ref = Classpath / p"com" / p"example" / p"data.file"
```

### Getting a Classpath Resource

The classpath resource, an instance of `Resource`, can be accessed from a `ClasspathRef` just by calling its
`apply()` method, like so,
```scala
val resource: Resource = ref()
```
so long as there is a contextual `Classloader` instance in scope. This is necessary because different
classloaders may resolve the same path to different resources.

#### Reading Resources

Classpath `Resource`s are readable, using typeclasses provided by
[Turbulence](https://github.com/propensive/turbulence/). So it is possible to call `readAs[Bytes]` or
`readAs[Json]` (with [Jacinta](https://github.com/propensive/jacinta/) available) and get a useful value
directly.

#### Choosing a `Classloader`

The choice of classloader is determined by a contextual `Classloader` value. This value can be constructed from
a Java `ClassLoader` (note the capital `L` in the Java standard class, and the lower-case `l` in Hellenism's
type name), but for most purposes, one of the following imports is sufficient:

- `classloaders.threadContext`: use the current thread's context classloader
- `classloaders.system`: use the _system_ classloader
- `classloaders.platform`: use the _platform_ classloader
- `classloaders.scala`: use the classloader which was used to load the first Scala classes

The `threadContext` classloader is a reasonable default choice for most purposes.

## Status

Hellenism is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Hellenism is designed to be _small_. Its entire source code currently consists
of 112 lines of code.

## Building

Hellenism will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Hellenism?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Hellenism's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Hellenism and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `hellenism`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Hellenism's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Hellenism are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/hellenism/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Hellenism
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Hellenism was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

The term _classical_ could equally be a reference to "classes" (in the Java sense) or the antiquity of Ancient Greece (and Rome); _Hellenism_, however, refers less ambiguously to the latter, while alluding to the _dependency hell_ of classloader hierarchies.

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

The logo shows a simplistic rendering of the Parthenon on the Acropolis in
Athens, a symbol of Hellenic culture.

## License

Hellenism is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

