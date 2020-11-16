[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/adversaria/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/adversaria/actions)
[<img src="https://img.shields.io/badge/gitter-discuss-f00762?style=for-the-badge" height="24">](https://gitter.im/propensive/adversaria)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/CHCPjERybv)
[<img src="https://img.shields.io/matrix/propensive.adversaria:matrix.org?label=MATRIX&color=0dbd8b&style=for-the-badge" height="24">](https://app.element.io/#/room/#propensive.adversaria:matrix.org)
[<img src="https://img.shields.io/twitter/follow/propensive?color=%2300acee&label=TWITTER&style=for-the-badge" height="24">](https://twitter.com/propensive)
[<img src="https://img.shields.io/maven-central/v/com.propensive/adversaria-core_2.12?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/adversaria-core_2.12)
[<img src="https://img.shields.io/badge/vent-propensive%2Fadversaria-f05662?style=for-the-badge" height="24">](https://vent.dev)

<img src="/doc/images/github.png" valign="middle">

# Adversaria

__Adversaria__ is a tiny library which provides a few tools to make it easier to work with static _annotations_ in Scala, by making them available through _typeclass interfaces_.

## Features

- access all annotations on a type through a typeclass
- resolve a typeclass instance only if a type has an annotated field
- makes annotations more useful and accessible in Scala
- no macro code is required to use annotations


## Getting Started

The nature of annotations in Scala is that they are very rarely the best
solution for any task. The can, however, be convenient in certain
circumstances, and this small domain is where Adversaria aims to help.

Currently three use cases are supported:

- getting all the annotations applied to a particular type
- finding the parameter of a case class to which a particular annotation has been applied
- getting all the annotations applied to a particular case class field

The list of supported use cases may grow as additional suggestions are received.

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

Perhaps we would like to find out the annotations on `Company`. We can get this
information by requesting an implicit `TypeMetadata[Company]` value, and
accessing its `annotations` field, like so,

```scala
import adversaria._

val info = implicitly[TypeMetadata[Company]]
println(info.annotations)

> List(count(10))
```

The `TypeMetadata` implicit should resolve for any type, regardless of
whether it has any annotations or not. Its `annotations` method will return a
list of annotations on that case class.

Another supported use case is to find the field of a case class which has been
annotated with a particular annotation, _if and only if_ that annotation exists.
We use the `FindMetadata` typeclass for this. It takes two type parameters:
the type of the annotation, and the type to check for, respectively,

```scala
import adversaria._

val idField = implicitly[FindMetadata[id, Person]]
println(idField.get(Person("John Smith", "test@example.com)))

> test@example.com
```
However, attempting to resolve such an implicit on a case class which has no
field annotated with that annotation, for example,
```scala
val idField = implicitly[FindMetadata[id, Company]]
```
will fail with at compiletime with the message,
```
adversaria: could not find a parameter annotated with type @id
```


## Status

Adversaria is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without guarantee of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement of designs
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Adversaria&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/adversaria`.
```
fury layer clone -i propensive/adversaria
```
or imported into an existing layer with,
```
fury layer import -i propensive/adversaria
```
A binary is available on Maven Central as `com.propensive:adversaria-core_<scala-version>:0.3.0`. This may be added
to an [sbt](https://www.scala-sbt.org/) build with:
```
libraryDependencies += "com.propensive" %% "adversaria-core" % "0.3.0"
```

## Contributing

Contributors to Adversaria are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/adversaria/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Adversaria easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
the answers.

## Author

Adversaria was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).



## License

Adversaria is copyright &copy; 2017-20 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
