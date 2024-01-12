[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/larceny/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/larceny/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Larceny

____

Unlike runtime errors, compilation errors prevent successful compilation, which
makes them harder to test, since we can't even compile the units tests we want
to write and run to test them!

_Larceny_ makes it possible to write those tests. Code which would normally
fail compilation, for any reason (provided it parses as well-formed Scala) is
permitted inside certain blocks of code, but instead of being compiled and run,
instead returns a list of compilation errors, as runtime values, which are
perfect for testing.

## Features

- suppresses compilation errors on ordinary code blocks
- code must at least parse, but all errors will be lifted to runtime values
- allows compilation errors to be tested in unit testing frameworks
- unit tests on compilation errors can be written in the most natural way


## Availability Plan

Larceny has not yet been published. The medium-term plan is to build Larceny
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Larceny.

Subsequently, Larceny will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

Larceny is a compiler plugin, and can be included in a compilation with the
`-Xplugin:larceny.jar` parameter to `scalac`:
```sh
scalac -d bin -Xplugin:larceny.jar -classpath larceny.jar *.scala`
```

The compiler plugin identifies code blocks whose compilation errors should be
suppressed, which are inside a `demilitarize` block (using any
valid Scala block syntax), for example:
```scala
package com.example

import larceny.*

@main def run(): Unit =
  demilitarize("Hello world".substring("5"))

  demilitarize:
    val x = 8
    println(x.missingMethod)
```

Here, the code inside each `demilitarize` block will never compile:
the first, because `substring` takes an `Int` as a parameter, and the second
because `missingMethod` is not a member of `Int`.

But despite this, if the Larceny plugin is enabled, then the code will compile.

And any invalid code that is _not_ within a `demilitarize` block will
still result in the expected compilation errors.

The compilation error from each `demilitarize` block will be
returned (in a `List`) from each block. We could adjust the code to see them,
like so:
```scala
@main def run(): Unit =
  val errors = demilitarize:
    "Hello world".substring("5")

  errors.foreach:
    case CompileError(id, message, code, offset) =>
      println(s"[$id] Found error '$message' in the code '$code' with offset $offset")
```

The four parameters of `CompileError` need some explanation:
- `id` is an integer representing the type of error
- `message` is the human-readable error message text that would be output by
  the compiler
- `code` is the fragment of code which would be marked as problematic (often
  with a wavy red underline)
- `offset` is the number of characters from the start of `code` that is
  indicated as the exact point of the error

Taking the second example above,
```scala
demilitarize:
  val x = 8
  println(x.missingMethod)
```
the `message` would be:
```
value missingMethod is not a member of Int
```
while the `code` value would be `x.missingMethod` (note that the surrounding
`println` is not considered erroneous), and the `offset` would be `2`. The
value `2` is because the erroneous code begins `x.`, but the point of the error
is considered to be the `m` of `missingMethod`, which is character `2`.

The error IDs are defined in the Scala compiler and correspond to an
enumeration of values. For convenience, these values are exported into the
`ErrorId` object, and can be accessed by the `errorId` method of
`CompileError`.

`ErrorId` is also an extractor on `CompileError`, so it's possible to write:
```scala
demilitarize(summon[Ordering[Exception]]) match
  case ErrorId(ErrorId.MissingImplicitArgumentID) => "expected"
  case _                                          => "unexpected"
```

### Implementation

Larceny runs on each source file before typechecking, but after parsing. Any
blocks named `demilitarize` found in the the untyped AST will trigger
a new and independent compilation of the same source file (with the same
classpath, but without the Larceny plugin) from _within_ the main compilation.

Since the `demilitarize` blocks should contain compile errors, this
child compilation is expected to fail, but its compilation errors will be
captured. Each compilation error which is positioned within a
`demilitarize` block will be converted to static code which constructs
a new `CompileError` instance, and inserts it into the `demilitarize`
block, in place of entire erroneous contents.

If there are multiple `demilitarize` blocks in the same source file,
some errors which occur in earlier phases of compilation may prevent later
phases from running, and the errors from those later phases will not be
captured during the first compilation. Larceny will rerun the compiler as
many times as necessary to capture errors from later phases, each time
removing more code which would have precluded these later phases.

The main compilation is then allowed to continue to typechecking, which will
only see the `CompileError` constructions, not the original code. As long as
there are no compilation errors _outside_ of a `demilitarize` block,
compilation should succeed. When the code is run, each `demilitarize`
block will simply return a list of `CompileError`s.

### Probably

Larceny works well with [Probably](https://github.com/propensive/probably/).

For example, we could write a compile error test with,
```scala
test(t"cannot sort data without an Ordering"):
  demilitarize(data.sorted).head.message
.assert(_.startsWith("No implicit Ordering"))
```





## Status

Larceny is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Larceny is designed to be _small_. Its entire source code currently consists
of 135 lines of code.

## Building

Larceny will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Larceny?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Larceny's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Larceny and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `larceny`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Larceny's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Larceny are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/larceny/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Larceny
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Larceny was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Larceny is the act of unlawfully taking something from someone. _Larceny_ unlawfully takes errors from compiletime and gives them to runtime.

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

The logo shows a shape of a medieval fortification, alluding to a "demilitarized zone" akin to the `demilitarized` scopes Larceny provides.

## License

Larceny is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

