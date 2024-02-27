[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/hyperbole/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/hyperbole/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Hyperbole

__Assistance with Scala 3 metaprogramming__

Hyperbole is a nascent set of tools to make it easier to write macros in Scala.
To begin, it includes an `introspect` method which can provide useful
reflection detail about how the source of an expression translates into an AST.

## Features

- See a full introspection of an expression's AST
- Renders the AST structure in an easy-to-read tree format
- View source code alongside the AST
- Also view the "expanded" source code alongside, showing "invisible" syntax
- Produces output at compiletime or runtime, invoked inside or outside of a macro


## Availability Plan

Hyperbole has not yet been published. The medium-term plan is to build Hyperbole
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Hyperbole.

Subsequently, Hyperbole will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

Hyperbole runs at compiletime to inspect an expression, but can present its
output in different ways:
- as a message reported at compiletime
- as a terminal compile error
- as a value which can be used in some way at runtime

It can be invoked on an expression in ordinary code, or in an inline context
where a `Quotes` instance is available, on a lifted `Expr` or a `Tree` value.

In all cases, the macro is invoked with an `introspect` method, either passing
the expression to be inspected, or a lifted `Expr` expression value in a quoted
context, disambiguated by overloading. In a quoted context, an additional
optional parameter, `terminate`, may be specified as `true` to indicate that
the output should be reported as a compiler error rather than an informational
message at compiletime; by default it is `false`.

A contextual `Introspection` value determines what result should be yielded
from a call to `introspect`. This given value can determine not only the result
type, but whether the expression is evaluated or its value retained. Three
implementations offered:

- `introspection.println`—records the introspection details to `stdout` with
  `println`, and returns the expression value
- `introspection.text`—constructs a `Text` value containing the introspection
  details, ignoring the expression without evaluating it

The default is to log using Eucalyptus. Note that this contextual value is not
necessary for the `introspect` method that take an `Expr` value.

For example, in a macro method body,
```scala
import hyperbole.*
def macroImpl(expr: Expr[T])(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  introspect(expr, terminate = true)
```
or, for example, in a [Probably](https://github.com/propensive/probably/) test,
```scala
import hyperbole.*, introspection.log
import probably.*
import eucalyptus.*, logging.stdout

test(t"Two joined lists are not empty"):
  val xs = List(1)
  val ys = List(2)
  introspect(xs ++ ys)
.assert(!_.isEmpty)

```





## Status

Hyperbole is classified as __embryonic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Hyperbole is designed to be _small_. Its entire source code currently consists
of 78 lines of code.

## Building

Hyperbole will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Hyperbole?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Hyperbole's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Hyperbole and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `hyperbole`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Hyperbole's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Hyperbole are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/hyperbole/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Hyperbole
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Hyperbole was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Hyperbole_ is a tool for working with macros (short for _macroinstructions_) whose name implies a large—or hyperbolic—size.

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

The logo shows a set of shapes formed from hyperbolic curves, reminiscent of a butterfly.

## License

Hyperbole is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

