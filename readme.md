[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/imperial/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/imperial/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Imperial

__Access the operating system's standard directory structure__

_Imperial_ provides access to the standard directory structure in UNIX and Linux, as defined by the
[systemd](https://www.freedesktop.org/software/systemd/man/file-hierarchy.html) and [XDG Base
Directory](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) specifications.

## Features

- Typesafe representation of standard UNIX directories
- Supports any directory representation through typeclasses
- Resolves environment variables to correctly use user-specified directory locations
- Supports alternative sources of environment variables


## Availability Plan

Imperial has not yet been published. The medium-term plan is to build Imperial
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Imperial.

Subsequently, Imperial will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### Standard directory layout

_Imperial_ provides a structure of nested objects inside the `Root` and `Home` objects which mirror the standard directory layout on
modern UNIX systems, as described in the
(XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html). This allows standard
directories to be referenced in code without the risk of typos.

Additionally, certain environment variables may be set which change the standard location, particularly within a user's home
directory. Imperial will take these into account, according to the specification.

### Accessing Directories

Objects have the same names as their corresponding UNIX directory, but by convention begin with a capital letter. Any `.` prefixes in
a directory's name are removed since, of course, `.` is used in place of `/` to access a nested object. So `/var/lib` would be
represented by the object, `Root.Var.Lib` and `$HOME/.local/bin` would be `Home.Local.Bin`.

Conversely, `Home.Config` would normally resolve to `$HOME/.config`, but if the user has the `XDG_CONFIG_HOME` environment
variable set to something else, it would simply resolve to `$XDG_CONFIG_HOME`.

The type of each of these objects is `BaseLayout`, which provides a `BaseLayout#apply()` method for constructing an instance of
a directory type (such as `java.io.File` or `galilei.Directory`) representing the directory. Imperial is designed to be
compatible with different file and directory representations, but it works automatically with
[Galilei](https://github.com/propensive/galilei).

For example, it would be possible to access a `myapp` configuration directory with,
```scala
import galilei.*
val dir: Directory = Home.Config() / t"myapp"
```

### Source of Environment Variables

By default, Imperial will take its environment variables directly from the JVM, since they are globally accessible. But this
is not always the correct source, for example in a long-running server which may receive multiple command-line requests from
different shells over its lifetime, each with a potentially different set of environment variables.

To cater for this scenario, it's possible to provide an alternative `EnvVarProvider` instance. This is a
single-abstract-method trait which takes a `Text` value and returns an `Option[Text]` of the value (returning `None` if the
environment variable is unset).

For example, if the `envvars` is a `Map` of values, a new EnvVarProvider may be specified in-scope with,
```scala
given EnvVarProvider = envvars.get(_)
```





## Status

Imperial is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Imperial is designed to be _small_. Its entire source code currently consists
of 149 lines of code.

## Building

Imperial will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Imperial?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Imperial's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Imperial and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `imperial`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Imperial's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Imperial are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/imperial/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Imperial
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Imperial was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

This project is particular to Linux operating systems, whos mascot is a penguin. While __Imperial__ defines the layout of the
operating system's own (little) empire, "Emperor" is also a species of penguin.

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

The logo shows the colors of an emperor (suggesting _imperial_) penguin.

## License

Imperial is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

