[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/gossamer/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/gossamer/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Gossamer

__Lightweight string utilities__

Gossamer provides the `Text` type, a typesafe opaque type alias of `String` and a `Show` typeclass
instance.

## Features

- provides a `Show` typeclass with instances for common types, including product types
- reimplements common methods on `String` with more typesafe variants
- implementation of the Minimum Edit Distance algorithm
- convenient converters to common encodings like URL encodings and Punycode
- implements a stricter `str""` interpolator for strings
- implements the `txt""` interpolator to ignore spurious whitespace in strings which flow onto multiple lines


## Availability Plan

Gossamer has not yet been published. The medium-term plan is to build Gossamer
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Gossamer.

Subsequently, Gossamer will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

_Gossamer_ provides a collection of useful methods and constructors for working with strings.

All Gossamer terms and types are defined in the `gossamer` package:
```scala
import gossamer.*
```

### `Show` typeclass

A standard `Show` typeclass is provided which will convert values of different types into `String`s.

Many types, such as `Int`s, only have a single reasonable presentation as a `String`, while others,
for example instances of case classes, may be presented in different ways depending on the context.
Gossamer's `Show` typeclass does not prescribe exactly where and when it should be used, but
instances of `Show` should produce strings which meaningfully present a value as a string, usually
for human consumption.

Using [Wisteria](https://github.com/propensive/wisteria), `Show` instances for product types (such
as case classes and tuples) and coproduct types (such as enumerations and sealed traits) will be
automatically derived.

### `Text`, a typesafe `String`

The `Text` type in `anticipation` is provided as an opaque alias of `String`,
duplicating most of the functionality of `String` (and its associated extension
methods), but without the typesafety risks associated with `String`. `Text`
instances may only be combined with other types when a `Show` typeclass
instance exists for that type.

Furthermore, every method of `Text` is guaranteed not to be `null` and declares any exceptions it
may throw.

#### Interpolators

Scala's standard library provides the `s` interpolator which allows elements of any type to be
substituted into a `String`. This presents a typesafety hole, since `toString` must be applied to
each one, without any guarantee that it produces a reasonable presentation of that value as a
`String`.

So Gossamer introduces the `str""` interpolator which only permits types with a corresponding
`Show` typeclass instance to be substituted into a string: other types will result in an error.
The `toString` method will never be called on these substitutions.

#### Long strings

Additionally, a `txt""` interpolator is provided for constructing "long" strings which need to be
split across several lines of code, but where any whitespace (such as indentation and newlines)
should always be read as a single space character, unless it contains two adjacent newlines, in
which case it should be interpreted as a "new paragraph", represented as a single newline (`'\n'`)
character.

This is particularly useful for embedding long messages in code while not breaking the consistency
of indentation. For example:
```scala
import anticipation.Text

val msg: Text = txt"""This is a long message which will not fit into a
                      standard line of code, and needs to be split across
                      several lines.

                      But at least it is aligned nicely within the code."""
```

The `String` `msg` will contain a single `'\n'` character, between `lines.` and `But`.

#### `DebugString` typeclass

In addition to `Show`, Gossamer provides a `DebugString` single-abstract-method typeclass which is
designed to provide `String` representations of values as valid Scala expressions that could be
copied and pasted into code.

Like the `Show` typeclass, product and coproduct instances of `DebugString` are automatically
derived.

### Encodings

Simple extension methods which provide a number of string-based encodings are provided. The
`urlEncode` and `urlDecode` methods will convert to and from (respectively) strings in the
URL encoding scheme. The `punycode` method will convert the string (most commonly, a domain name)
into a ASCII-only representation of the string, encoding any non-ASCII characters as Punycode.

### Safer `String` methods

Safer alternatives to many of the commonly-used methods of `String` are provided. These typically
delegate to existing methods on `String`, but will:
- never return `null`
- never return mutable arrays
- never accept `Any` as a parameter type, or implicitly use `String#toString` to convert
  non-`String` types to `String`s

### Minimum Edit Distance

An implementation of the _Minimum Edit Distance_ or [Levenshtein
distance](https://en.wikipedia.org/wiki/Levenshtein_distance), `lev` is provided as an extension
method on `Text`s. The method takes another `Text` as a parameter, and returns the minimum
number of edits (character additions, deletions or replacements) required to change one string to
the other.

For example, `t"Hello".lev(t"Hallo!")` returns `2`: the replacement of `e` with `a` counts as one
edit, and the addition of `!` counts as the second edit. The algorithm is symmetrical.

### Joining

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
val numbers = List(t"one", t"two", t"three", t"four").join(t", ", t" and ")
```
will evaluate to `"one, two, three and four"`, and,
```scala
val numbers2 = List(t"one", t"two", t"three").join(t"Choose ", t", ", t" or ", t".")
```
results in, `t"Choose one, two or three."`.






## Status

Gossamer is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Gossamer is designed to be _small_. Its entire source code currently consists
of 941 lines of code.

## Building

Gossamer will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Gossamer?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Gossamer's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Gossamer and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `gossamer`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Gossamer's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Gossamer are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/gossamer/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Gossamer
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Gossamer was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Gossamer is lightweight and stringlike.

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

The logo shows the glowing tip of a gossamer-thin fibreoptic cable.

## License

Gossamer is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

