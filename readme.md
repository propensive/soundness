[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
[<img src="https://vent.dev/badge/propensive/gossamer" height="24">](https://vent.dev/)
<img src="/doc/images/github.png" valign="middle">

# Gossamer

Gossamer provides the `Text` type, a typesafe opaque type alias of `String` and a `Show` typeclass
instance.

## Features

- provides a `Show` typeclass with instances for common types, including product types
- reimplements common methods on `String` with more typesafe variants
- implementation of the Minimum Edit Distance algorithm
- convenient converters to common encodings like URL encodings and Punycode
- implements a stricter `str""` interpolator for strings
- implements the `txt""` interpolator to ignore spurious whitespace in strings which flow onto multiple lines


## Getting Started

_Gossamer_ provides a collection of useful methods and constructors for working with strings.

## `Show` typeclass

A standard `Show` typeclass is provided which will convert values of different types into `String`s.

Many types, such as `Int`s, only have a single reasonable presentation as a `String`, while others,
for example instances of case classes, may be presented in different ways depending on the context.
Gossamer's `Show` typeclass does not prescribe exactly where and when it should be used, but
instances of `Show` should produce strings which meaningfully present a value as a string, usually
for human consumption.

Using [Wisteria](https://github.com/propensive/wisteria), `Show` instances for product types (such
as case classes and tuples) and coproduct types (such as enumerations and sealed traits) will be
automatically derived.

## `Text`, a typesafe `String`

The `Text` type is provided as an opaque alias of `String`, duplicating most of the functionality
of `String` (and its associated extension methods), but without the typesafety risks associated
with `String`. `Text` instances may only be combined with other types when a `Show` typeclass
instance exists for that type.

Furthermore, every method of `Text` is guaranteed not to be `null` and declares any exceptions it
may throw.

### Interpolators

Scala's standard library provides the `s` interpolator which allows elements of any type to be
substituted into a `String`. This presents a typesafety hole, since `toString` must be applied to
each one, without any guarantee that it produces a reasonable presentation of that value as a
`String`.

So Gossamer introduces the `str""` interpolator which only permits types with a corresponding
`Show` typeclass instance to be substituted into a string: other types will result in an error.
The `toString` method will never be called on these substitutions.

### Long strings

Additionally, a `txt""` interpolator is provided for constructing "long" strings which need to be
split across several lines of code, but where any whitespace (such as indentation and newlines)
should always be read as a single space character, unless it contains two adjacent newlines, in
which case it should be interpreted as a "new paragraph", represented as a single newline (`'\n'`)
character.

This is particularly useful for embedding long messages in code while not breaking the consistency
of indentation. For example:
```scala
val msg: String = txt"""This is a long message which will not fit into a
                        standard line of code, and needs to be split across
                        several lines.

                        But at least it is aligned nicely within the code."""
```

The `String` `msg` will contain a single `'\n'` character, between `lines.` and `But`.

### `DebugString` typeclass

In addition to `Show`, Gossamer provides a `DebugString` single-abstract-method typeclass which is
designed to provide `String` representations of values as valid Scala expressions that could be
copied and pasted into code.

Like the `Show` typeclass, product and coproduct instances of `DebugString` are automatically
derived.

## Encodings

Simple extension methods which provide a number of string-based encodings are provided. The
`urlEncode` and `urlDecode` methods will convert to and from (respectively) strings in the
URL encoding scheme. The `punycode` method will convert the string (most commonly, a domain name)
into a ASCII-only representation of the string, encoding any non-ASCII characters as Punycode.

## Safer `String` methods

Safer alternatives to many of the commonly-used methods of `String` are provided. These typically
delegate to existing methods on `String`, but will:
- never return `null`
- never return mutable arrays
- never accept `Any` as a parameter type, or implicitly use `String#toString` to convert
  non-`String` types to `String`s

## Minimum Edit Distance

An implementation of the _Minimum Edit Distance_ or [Levenshtein
distance](https://en.wikipedia.org/wiki/Levenshtein_distance), `lev` is provided as an extension
method on `String`s. The method takes another `String` as a parameter, and returns the minimum
number of edits (character additions, deletions or replacements) required to change one string to
the other.

For example, `"Hello".lev("Hallo!")` returns `2`: the replacement of `e` with `a` counts as one
edit, and the addition of `!` counts as the second edit. The algorithm is symmetrical.

## Joining

Scala's standard library provides the `mkString` method on collection types, but this unfortunately
calls `toString` on every element in the collection, without warning. Gossamer provides a `join`
method which may only be applied to values that are already `String`s.

This is further generalized with a `Joinable` typeclass: if an instance exists for other
`String`-like types, they may also be `join`ed like a collection of `String`s, where every
parameter to `join` is of the same type as the elements of the collection.

In addition to the zero-, one- and three-parameter variants of `join` which behave like their
`mkString` equivalents, two- and four-parameter versions are also provided. These allow a different
separator to be used between the penultimate and last elements of the collection.

For example,
```scala
List("one", "two", "three", "four").join(", ", " and ")
```
will evaluate to `"one, two, three and four"`, and,
```scala
List("one", "two", "three").join("Choose ", ", ", " or ", ".")
```
results in, `"Choose one, two or three."`.


## Related Projects

The following _Niveau_ libraries are dependencies of _Gossamer_:

[![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp; [![Wisteria](https://github.com/propensive/wisteria/raw/main/doc/images/128x128.png)](https://github.com/propensive/wisteria/) &nbsp;

The following _Niveau_ libraries are dependents of _Gossamer_:

[![Gesticulate](https://github.com/propensive/gesticulate/raw/main/doc/images/128x128.png)](https://github.com/propensive/gesticulate/) &nbsp; [![Profanity](https://github.com/propensive/profanity/raw/main/doc/images/128x128.png)](https://github.com/propensive/profanity/) &nbsp;

## Status

Gossamer is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Gossamer is designed to be _small_. Its entire source code currently consists of 682 lines of code.

## Availability

Gossamer&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/gossamer`.
```
fury layer clone -i propensive/gossamer
```
or imported into an existing layer with,
```
fury layer import -i propensive/gossamer
```

## Contributing

Contributors to Gossamer are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/gossamer/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Gossamer easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Gossamer was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Gossamer is lightweight and stringlike.

## License

Gossamer is copyright &copy; 2021 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
