[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/imperial/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/imperial/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/imperial-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/imperial-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Imperial

_Imperial_ provides access to the standard directory structure in UNIX and Linux, as defined by the
[systemd](https://www.freedesktop.org/software/systemd/man/file-hierarchy.html) and [XDG Base
Directory](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) specifications.

## Features

- Typesafe representation of standard UNIX directories
- Supports any directory representation through typeclasses
- Resolves environment variables to correctly use user-specified directory locations
- Supports alternative sources of environment variables


## Availability

The current latest release of Imperial is __0.4.0__.

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
a directory type (such as `java.io.File` or `joviality.Directory`) representing the directory. Imperial is designed to be
compatible with different file and directory representations, but it works automatically with
[Joviality](https://github.com/propensive/joviality).

For example, it would be possible to access a `myapp` configuration directory with,
```scala
import joviality.*
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


## Related Projects

The following _Scala One_ libraries are dependencies of _Imperial_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp; [![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp;

The following _Scala One_ libraries are dependents of _Imperial_:

[![Oubliette](https://github.com/propensive/oubliette/raw/main/doc/images/128x128.png)](https://github.com/propensive/oubliette/) &nbsp;

## Status

Imperial is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Imperial is designed to be _small_. Its entire source code currently consists of 90 lines of code.

## Building

Imperial can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Imperial are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/imperial/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Imperial easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Imperial was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

This project is particular to Linux operating systems, whos mascot is a penguin. While __Imperial__ defines the layout of the
operating system's own (little) empire, "Emperor" is also a species of penguin.

## License

Imperial is copyright &copy; 2022-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
