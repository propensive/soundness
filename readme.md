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


## Availability

Imperial has not yet been published as a binary.

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

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Imperial is designed to be _small_. Its entire source code currently consists
of 140 lines of code.

## Building

Imperial can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Imperial are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/imperial/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Imperial easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Imperial was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

This project is particular to Linux operating systems, whos mascot is a penguin. While __Imperial__ defines the layout of the
operating system's own (little) empire, "Emperor" is also a species of penguin.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows the colors of an emperor (suggesting _imperial_) penguin.

## License

Imperial is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
