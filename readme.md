[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/hyperbole/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/hyperbole/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Hyperbole

Hyperbole is a nascent set of tools to make it easier to write macros in Scala.
To begin, it includes an `introspect` method which can provide useful
reflection detail about how the source of an expression translates into an AST.

## Features

- See a full introspection of an expression's AST
- Renders the AST structure in an easy-to-read tree format
- View source code alongside the AST
- Also view the "expanded" source code alongside, showing "invisible" syntax
- Produces output at compiletime or runtime, invoked inside or outside of a macro


## Availability

Hyperbole has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

Hyperbole runs at compiletime to inspect an expression, but can present its
output in different ways:
- as a message reported at compiletime
- as a terminal compile error
- as a value which can be used in some way at runtime
- logged or printed as a side-effect at runtime

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

- `introspection.log`—uses a contextual
  [Eucalyptus](https://github.com/propensive/eucalyptus/) log to record the
  introspection details as a side-effect, and returns the expression value
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


## Related Projects

The following _Scala One_ libraries are dependencies of _Hyperbole_:

[![Dendrology](https://github.com/propensive/dendrology/raw/main/doc/images/128x128.png)](https://github.com/propensive/dendrology/) &nbsp; [![Escapade](https://github.com/propensive/escapade/raw/main/doc/images/128x128.png)](https://github.com/propensive/escapade/) &nbsp; [![Escritoire](https://github.com/propensive/escritoire/raw/main/doc/images/128x128.png)](https://github.com/propensive/escritoire/) &nbsp; [![Harlequin](https://github.com/propensive/harlequin/raw/main/doc/images/128x128.png)](https://github.com/propensive/harlequin/) &nbsp;

No other _Scala One_ libraries are dependents of _Hyperbole_.

## Status

Hyperbole is classified as __embryonic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Hyperbole is designed to be _small_. Its entire source code currently consists
of 224 lines of code.

## Building

Hyperbole can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Hyperbole are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/hyperbole/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Hyperbole easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Hyperbole was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Hyperbole_ is a tool for working with macros (short for _macroinstructions_) whose name implies a large—or hyperbolic—size.

## License

Hyperbole is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
