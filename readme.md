[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/rudiments/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/rudiments/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Rudiments

__Rudimentary utilities for writing everyday Scala__

_Rudiments_ provides a small collection of tiny but useful utilities for everyday programming in Scala, and
could be considered an enhanced "predef".

## Features

- implementation of an easier-to-use Y-combinator method, `fix` with a recursion helper method, `recur`
- typesafe and mutation-safe reimplementations of several `String` operations
- `String` extractors for primitive types
- `unit`, `only`, `twin` and `triple` convenience methods to make some code patterns slightly more concise
- a typesafe `str""` string interpolator where all substitutions must be `String`s


## Availability

Rudiments has not yet been published as a binary.

## Getting Started

Utilities in _Rudiments_ are mostly provided through extension methods, and importing the `rudiments` package
will bring all utility methods into scope.

### Y-Combinator

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

### Primitive `String` Extractors

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

### Typesafe `String` operations

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

### Lightweight system property access

Many JVM system properties are available in the map, `System.getProperties` and are typically given identifiers
written in a dot-notation style, such as `user.dir`. Rudiments provides syntactic sugar for accessing these
dynamically through the `Sys` object, for example,
```scala
val pwd: Option[String] = Sys.user.dir()
```

### `unit`

Often a side-effecting expression returns a value which is not used at a particular call site, and can be
discarded. However, the expression's return type can result in type-inference choosing an undesired return type,
when `Unit` would be preferable, or a compile-time warning about discarded values may be produced.

The `unit` extension method silently discards the return value of any expression, and instead produces a `Unit`,
`()`.

### `only`

The `only` extension method applies a partial function to a value and lifts the result into an option.

For example,
```scala
val result: Option[Int] = string.only { case Int(i) => i*i }
```

### `str""` Interpolator

The `s""` interpolator takes parameters of `Any` type as substitutions, calling `String#toString` on them as
necessary. This may be considered too permissive, so `str""` is provided as a typesafe alternative that requires
every substitution to be a `String`.

### `twin` and `triple`

These two extension methods produce a two-tuple and a three-tuple (respectively) of repetitions of the value it
is applied to. This can be useful in a subsequent `map` operation.



## Status

Rudiments is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Rudiments is designed to be _small_. Its entire source code currently consists
of 995 lines of code.

## Building

Rudiments can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Rudiments are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/rudiments/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Rudiments easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Rudiments was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

The word _rudiment_ is defined as, "the principle which lies at the bottom of any development; an unfinished beginning", which is apt for a library whose purpose is to provide such common functionality that it might lie at the start of many other libraries.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows a burning sun, the basis for life in our solar system; it represents the foundational nature of Rudiments.

## License

Rudiments is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
