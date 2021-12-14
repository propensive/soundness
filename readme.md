[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
[<img src="https://vent.dev/badge/propensive/polyvinyl" height="24">](https://vent.dev/)
<img src="/doc/images/github.png" valign="middle">

# Polyvinyl

_Polyvinyl_ makes it easy to define schemas for and use record types, originating from a
variety of possible sources. These could originate from a database, a file or some other
source, loaded at compiletime, and utilized in a later phase of compilation.

## Features

- provides support for record types in Scala
- enforces namespace-safety on field access
- record schemas may be defined programmatically, without writing explicit case classes
- schemas can be defined dynamically, taking strings as field names
- schemas may be defined in the same module, provided they are not used in the same file
- uses Scala 3's `Selectable` trait internally


## Getting Started

## Quick Start

A schema may be implemented as a singleton object extending `Schema[T]`, where `T` is the fixed
type that each field will return. Additionally, the object should also define the method `fields`,
returning a `List[String]` of the valid field names for record instances of that schema. Whichever
string values this method returns when the code to construct a record is invoked (at compiletime)
will be considered valid field names for that record.

Accordingly, different schemas must be implemented as different singletons.

Furthermore, an `apply` method should be implemented as a macro on the singleton object, exactly
as follows:
```scala
transparent inline def apply(inline fn: String => T): Record[T] = ${build('fn)}
```

Invoking this method will construct a new record, an instance of `Record[T]`, whose field values
will be obtained by calling the `fn` function with the field's name, as a `String`.

Here is a full, albeit uninteresting, example:
```scala
object Numbers extends Schema[Int]:
  def fields = List("one", "two", "three", "four")
  transparent inline def apply(inline fn: String => Int): Record[Int] = ${build('fn)}
```

The field names are defined in the source code, but they could be obtained from anywhere
(provided the code will produce the desired output when it is invoked inside the compiler at
compiletime).

In a different file, this `Numbers` schema object may be used to construct new `Record[Int]`
objects. These instances must be backed by some means of obtaining the field values, given a
field name; this is just a lambda, so the implementation is up to the programmer.

Here, we implement a record using a `Map[String, Int]`:
```scala
val numberMap = Map(
  "one"   -> 1,
  "two"   -> 2,
  "three" -> 3,
  "four"  -> 4,
  "five"  -> 5
)

val record = Numbers(numberMap)
```

Given the value, `record`, any fields defined in the `fields` method may be invoked on it, and
will return appropriate values from the map.

For example, `record.one` will return the `Int`, `1`. But `record.six` will be a compile error,
as will `record.five`: even though the runtime `numberMap` object contains a value for the
`String` `"five"`, the schema does not define it.

## Compilation Order

It is crucial that the schema singleton is defined in a different file from the invocation. This
is to guarantee that the object (and hence the fields it defines) is available as a runtime object
during later compilations which use it. Scala is able to compile a macro definition and its usage
in the correct order so long as they are defined in separate files (and no cyclic dependencies
exist between those files).

## Limitations

It is not currently possible for different fields to return different types.


## Related Projects

_Polyvinyl_ has no dependencies.

No other _Niveau_ libraries are dependents of _Polyvinyl_.

## Status

Polyvinyl is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Polyvinyl is designed to be _small_. Its entire source code currently consists of 13 lines of code.

## Availability

Polyvinyl&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/polyvinyl`.
```
fury layer clone -i propensive/polyvinyl
```
or imported into an existing layer with,
```
fury layer import -i propensive/polyvinyl
```

## Contributing

Contributors to Polyvinyl are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/polyvinyl/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Polyvinyl easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Polyvinyl was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Polyvinyl is the substance from which records (LPs) are made; the purpose of this library is to produce record types.

## License

Polyvinyl is copyright &copy; 2021 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
