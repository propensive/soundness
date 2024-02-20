[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/fulminate/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/fulminate/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Fulminate

__Rich and composable error messages__

While plain text is a sufficient medium for communicating with users (whether
they be other programmers, or end-users), it lacks a versatile way of marking
up parts of the text in a way that could be rendered in different ways as
markdown, HTML or console text: usually, it's necessary to commit early to
the output format. _Fulminate_ fills this gap by providing a convenient
representation of a message, with constructors which automatically contrast
the dynamic parts of the message from the static parts.

## Features

- rich representation of user-facing messages, particularly for errors
- demarcation of fixed and variable parts of a message
- provides a general-purpose `Error` type for immutable exception objects


## Availability Plan

Fulminate has not yet been published. The medium-term plan is to build Fulminate
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Fulminate.

Subsequently, Fulminate will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### `Message`s

A `Message` is a string of text which may have other `Message`s embedded within
it. Unlike a string, though, `Message`s retain their structure, and references
to the embedded `Message`s are retained, unchanged.

The simplest way to construct a `Message` is with the `msg""` interpolator, for example:
```scala
val message = msg"this is a message"
```

Such a message can be embedded within another, as in,
```scala
val message2 = msg"We can see that $message."
```
which would represent the text, `We can see that this is a message.`.

This exact text can be produced by calling `Message`'s `text` method, but this
flattens the structure of the message. Other libraries, such as
[Punctuation](https://github.com/propensive/punctuation/),
[Honeycomb](https://github.com/propensive/honeycomb/) and
[Escapade](https://github.com/propensive/escapade/) can render `Message`s as
richer presentations of text, such as Markdown, HTML and console output, and in
doing so, can highlight those parts of the `Message` which were embedded.

The `msg""` interpolator also allows other types to be embedded, provided an
`MessageShow` typeclass instance exists for that type. By default, that includes
primitive types, `Text` strings and any type for which a `Show` typeclass
instance exists, all of which will be automatically converted to `Message`s
when they are substituted into the interpolator. While the `Message` that gets
provided by an `MessageShow` instance may be essentially the same as the `Text`
that is provided by a `Show` instance in most cases, `MessageShow` can provide
additional structure to the text content, that becomes apparent when rendered
as Markdown, HTML or console text.

### `Error`s

An `Error` is a subclass of `java.lang.Exception`, not be confused with
`java.lang.Error`, whose error message is expressed as a `Message`.

This will typically be subclassed with a case class whose parameters will be
substituted into the message, for example:
```scala
case class SizeError(expected: Size, actual: Size)
extends Error(msg"expected a size $expected, but the actual size was $actual")
```

This would require an appropriate `MessageShow[Size]` (or a `Show[Size]`)
instance in scope for the substitution to be acceptable.

### `fail`

When writing macros, with a `Quotes` instance in scope, the `fail` method takes
a `Message` and will produce a compile error showing that message.

If the compiler is running in a terminal with color capability, then color will
be used to highlight embeddings in the failure message, by including ANSI
escape codes in the output message.

### `Mistake`

Sometimes we need to raise exceptions which are not intended to be handled, and
not even intended to be thrown, for example when a code branch is run which
should be impossible to reach. The `Mistake` class provides a standard way of
handling such situation, and takes a `Message` parameter which should briefly
explain the reason why the situation was believed to be impossible.

Should a `Mistake` be thrown, it should represent a programming mistake: the
manifestation of a misconception of impossibility on the part of the
programmer.





## Status

Fulminate is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Fulminate is designed to be _small_. Its entire source code currently consists
of 166 lines of code.

## Building

Fulminate will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Fulminate?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Fulminate's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Fulminate and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `fulminate`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Fulminate's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Fulminate are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/fulminate/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Fulminate
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Fulminate was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

To _fulminate_ is to express vehement protest, while _Fulminate_ provides the means to express protestations at errors.

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

The logo shows a boiling, bubbling liquid; fulminating.

## License

Fulminate is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

