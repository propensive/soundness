[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
[<img src="https://vent.dev/badge/propensive/rudiments" height="24">](https://vent.dev/)
<img src="/doc/images/github.png" valign="middle">

# Rudiments

_Rudiments_ provides a small collection of tiny but useful utilities for everyday programming in Scala, and
could be considered an enhanced "predef".

## Features

- implementation of an easier-to-use Y-combinator method, `fix` with a recursion helper method, `recur`
- typesafe and mutation-safe reimplementations of several `String` operations
- `String` extractors for primitive types
- `unit`, `only`, `twin` and `triple` convenience methods to make some code patterns slightly more concise
- a typesafe `str""` string interpolator where all substitutions must be `String`s


## Getting Started

Utilities in _Rudiments_ are mostly provided through extension methods, and importing the `rudiments` package
will bring all utility methods into scope.

## Y-Combinator

An implementation of a [Y-Combinator](https://shorturl.at/jqKOY), called `fix`, is provided, implemented using a
Scala 3 context function, which enables slightly more favorable syntax than was possible in Scala 2. This method
makes it easier to write code in a point-free style.

The `fix` method takes a type parameter (which must be explicitly specified for type inference) and a lambda as
its first parameter, to which an additional parameter should be supplied as its initial value. Crucially, in the
body of `fix`'s lambda, a call to `recur` should be used to signal recursion.

This is best illustrated with an example. Here is an implementation of a factorial function.
```scala
def factorial(n: Int): Int =
  fix[Int] { i => if i <= 0 then 1 else i*recur(i - 1) } (n)
```

This avoids the explicit definition of a private or nested helper function, which would normally be necessary
for a definition such as this.

## Primitive `String` Extractors

Extractors for all the primitive types are provided for matching on `String`s. These are defines as extensions
to the (virtual) companion objects of the primitive types, so they have the same names as the types they
extract.

Here is an example of them in use:
```scala
def parse(number: String): Boolean | Int | Double =
  number match
    case Boolean(b) => b
    case Int(i)     => i
    case Double(d)  => d
    case _          => 0
```

## Typesafe `String` operations

The extension method `String#cut` has identical semantics to `String#split`, but returns an immutable `IArray`
instead of an `Array`. Likewise, the methods `String#bytes` and `String#chars` mirror `String#getBytes` and
`String#toCharArray`, returning an `IArray[Byte]` and `IArray[Char]` respectively.

The `bytes` method currently uses the `UTF-8` in all cases, though this may change if there is sufficient
demand.

Four variants of the extension method `join` are provided on `Traversable[String]` instances, which provide the
same functionality as `mkString` (but only operating on `String`s) with one additional two-parameter version
that's specialized for natural language lists where each element is separated by a comma except the last, which
is preceded by a word such as `and` or `or`.

For example,
```scala
List("one", "two", "three").join(", ", " or maybe ")
```
will produce the string `one, two or maybe three`.

## Lightweight system property access

Many JVM system properties are available in the map, `System.getProperties` and are typically given identifiers
written in a dot-notation style, such as `user.dir`. Rudiments provides syntactic sugar for accessing these
dynamically through the `Sys` object, for example,
```scala
val pwd: Option[String] = Sys.user.dir()
```

## `unit`

Often a side-effecting expression returns a value which is not used at a particular call site, and can be
discarded. However, the expression's return type can result in type-inference choosing an undesired return type,
when `Unit` would be preferable, or a compile-time warning about discarded values may be produced.

The `unit` extension method silently discards the return value of any expression, and instead produces a `Unit`,
`()`.

## `only`

The `only` extension method applies a partial function to a value and lifts the result into an option.

For example,
```scala
val result: Option[Int] = string.only { case Int(i) => i*i }
```

## `str""` Interpolator

The `s""` interpolator takes parameters of `Any` type as substitutions, calling `String#toString` on them as
necessary. This may be considered too permissive, so `str""` is provided as a typesafe alternative that requires
every substitution to be a `String`.

## `twin` and `triple`

These two extension methods produce a two-tuple and a three-tuple (respectively) of repetitions of the value it
is applied to. This can be useful in a subsequent `map` operation.


## Status

Rudiments is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Rudiments&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/rudiments`.
```
fury layer clone -i propensive/rudiments
```
or imported into an existing layer with,
```
fury layer import -i propensive/rudiments
```

## Contributing

Contributors to Rudiments are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/rudiments/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Rudiments easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
answers given in private.

## Author

Rudiments was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

The word _rudiment_ is defined as, "the principle which lies at the bottom of any development; an unfinished beginning", which is apt for a library whose purpose is to provide such common functionality that it might lie at the start of many other libries.

## License

Rudiments is copyright &copy; 2020-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
