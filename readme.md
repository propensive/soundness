[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/adversaria/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/adversaria/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/adversaria-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/adversaria-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Adversaria

__Adversaria__ is a tiny library which provides a few tools to make it easier to work with static
_annotations_ in Scala, by making them available through _typeclass interfaces_.

## Features

- access all annotations on a type through a typeclass
- resolve a typeclass instance only if a type has an annotated field
- makes annotations more useful and accessible in Scala
- no macro code is required to use annotations


## Availability

The current latest release of Adversaria is __0.4.0__.

## Getting Started

The nature of annotations in Scala is that they are very rarely the best solution for any task, but
can be convenient in certain circumstances, and this small domain is where Adversaria aims to help.

Currently three use cases are supported:

- getting all the annotations applied to a particular type
- finding the parameter of a case class to which a particular annotation has been applied
- getting all the annotations applied to a particular case class field

The list of supported use cases may grow.

## Examples

If we were to define the following annotations,
```scala
import scala.annotation.StaticAnnotation

final case class id() extends StaticAnnotation
final case class count(n: Int) extends StaticAnnotation
```

we could apply them to some case classes, such as,
```scala
@count(10)
case class Company(name: String)

case class Person(name: String, @id email: String)
```


## Related Projects

The following _Scala One_ libraries are dependencies of _Adversaria_:

[![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp;

No other _Scala One_ libraries are dependents of _Adversaria_.

## Status

Adversaria is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Adversaria is designed to be _small_. Its entire source code currently consists of 92 lines of code.

## Building

Adversaria can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Adversaria are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/adversaria/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Adversaria easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Adversaria was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Adversaria_ are miscellaneous collections of notes or _annotations_, after which the library is named.

## License

Adversaria is copyright &copy; 2019-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
