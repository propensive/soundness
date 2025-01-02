[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/symbolism/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/symbolism/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Symbolism

__A general mechanism to implement overloaded symbolic operators__

Many different types support, in some way, the concept of arithmetic
operations: they can be added, subtracted, multiplied and divided. Or perhaps
just some of these. We want to use the familiar binary arithmetic operators
(`+`, `-`, `*` and `/`) to work with these types, so that is the first thing
__Symbolism__ provides, though typeclass definitions for the four main arithmetic
operations, plus negation and square and cube root methods.

## Features

- uses typeclasses as a modular way to implement symbolic operators
- avoids overloading errors when mixing different projects which define symbolic extension methods
- provides typeclasses for the arithmetic operators, `+`, `-`, `*` and `/`
- provides a single inequality typeclass for the comparison operators, `<`, `<=`, `>` and `>=`


## Availability

Symbolism has not yet been published. The medium-term plan is to build it with
[Fury](https://github.com/propensive/fury) and to publish it as a source build
on [Vent](https://github.com/propensive/vent). This will enable ordinary users
to write and build software which depends on Symbolism.





## Getting Started


These work out-of-the-box: if your type provides an appropriate arithmetic typeclass
instance, you can use its arithmetic operator. The typeclasses are:
- `Addable`
- `Subtractable`
- `Multiplicable`
- `Divisible`
- `Negatable`, and
- `Rootable`

But these typeclasses provide another important facility: they allow us to
abstract over the arithmetic operations. That means it's possible to write code
against these typeclasses which will work on any type which implements them.

For example, [Mosquito](https://github.com/propensive/mosquito/) is a library
for working with Euclidean vectors (called `Euclidean`) and matrices of
arbitrary types, such as `Double`s or even `Exception`s. With Mosquito, you can
add two vectors if, and only if, you can add their elements. So you can add two
vectors of `Double` (as long as they have the same dimension), but you can't
add two vectors of `Exception` because `Exception`s have no `Addable` instance.

We can go further, and calculate the scalar (dot) product if we also have a
`Multiplicable` typeclass, and the vector (cross) product if we have a
`Subtractable` typeclass too.

Similarly, [Baroque](https://github.com/propensive/baroque/) provides an
implementation of complex numbers, which are generic in the common type used
for their real and imaginary parts. Operations on Baroque complex numbers are
provided if their generic types support the necessary operations.

[Quantitative](https://github.com/propensive/quantitative/) offers
representations of physical quantities, such as _10Nm_ (10 Newton Metres), and
supports all arithmetic operations on these values through Symbolism typeclass
instances. The types of the operands do not need to be the same, and the units
of the result type can be computed from the input units. For example, _10Nm /
5s_ would produce a value of _2Nm/s_. This is no problem at all for Symbolism.
Nor is it problematic for Mosquito which would be happy to calculate the cross
product of two vectors of different units, and compute the resultant units.

The power of this abstraction is demonstrated by Mosquito, Baroque and
Quantitative all having a dependency on Symbolism, but no dependencies on each
other.

### Defining Binary Arithmetic Typeclasses

Symbolism's binary arithmetic typeclasses are determined by three types
representing the left operand, the right operand and the result type. Often
these will all be the same type, but it's possible for them to differ, for example
when multiplying a `String` by an `Int`, with a `String` as the result.

The type of each typeclass can be specified is the form,
`LeftOperand is Operation[RightOperand] into Result`. Hence, we have:
- `Augend is Addable by Addend into Result`
- `Minuend is Subtractable by Subtrahend into Result`
- `Multiplicand is Multiplicable by Multiplier into Result`
- `Dividend is Divisible by Divisor into Result`
and have the methods, `add`, `subtract`, `multiply` and `divide` defined upon them.

Implementing a `given` instance is as simple as defining. For example, imagine
adding a length of time (a `Duration`) to a point in time (an `Instant`). We
could write,
```scala
given Instant is Addable by Duration into Instant:
  def add(left: Instant, right: Duration): Instant = left.plus(right)
```
or even,
```scala
given Instant is Addable by Duration into Instant = _.plus(_)
```
taking advantage of the fact that `Addable` is a SAM type.

If we were defining our own `Instant` and `Duration` types, we might write this:
```scala
object Duration:
  given Duration is Addable by Duration into Duration = _.plus(_)
  given Duration is Subtractable by Duration into Duration = _.minus(_)
  given Duration is Multiplicable by Double into Duration = _.times(_)
  given Duration is Divisible by Double into Dulation = _.divide(_)

object Instant:
  given Instant is Addable by Duration into Instant = _.plus(_)
  given Instant is Subtractable by Instant into Duration = _.minus(_)
  given Instant is Subtractable by Duration into Instant = _.to(_)
```

The typeclasses are defined in the companion object of the type of their left
operand, because this is how they will be resolved; not by the right operand.
But notice how it's possible to define two different `Subtractable` instances
on `Instant`. The correct typeclass will be chosen depending on the type of the
right operand.

### Writing Generic Code

Imagine, instead of providing new types with arithmetic operations, we want to
write a generic method which can work with any such type. We should demand the
appropriate typeclass instance in the method's signature, as a `using`
parameter.

Here's what that might look like for a method that needs to multiply two values
of the same type:
```scala
def op[Operand, Result](lefts: List[Operand], right: List[Operand])
    (using Operand is Multiplicable by Operand into Result)
        : List[Result] =
  lefts.zip(rights).map(_*_)
```

We do not need to name the using parameter, as the `*` operator will resolve it
automatically.

### Note on Types

The nature of these types, composed using the infix type constructors, `is`,
`by` and `into`, is that any part may be omitted when writing the type. For
example, `Instant is Addable` is a type, but it's a type which leaves its right
operand and result type unspecified.

Equally, `Multiplicable by Int into String` is a type which leaves the left
operand's type unspecified.

Omitting part of the type can be useful, usally with the result type. If we
have an instance of an underspecified type, such as `String is Multiplicable by
Int`, say `mul`, then we can access its `Result` type as a member of `mul`,
with `mul.Result`.

This comes into its own in method signatures of methods which chain together
several arithmetic operations. Here's an example which multiplies two pairs of
numbers, all potentially different types, then subtracts one product from the
other:
```scala
def op2[A, B, C, D](a: A, b: B, c: C, d: D)
    (using product1: A is Multiplicable by B,
           product2: C is Multiplicable by D,
           sum:      product1.Result is Subtractable by product2.Result)
        : sum.Result
  a*b - c*d
```

### Other Operations

Two other operations are provided: negation and rooting.

#### Negation

Negation is a unary operator, provided by the `Negatable` typeclass. We can
define a `Negatable` without a `by` clause in its type, for example,
```scala
given PosInt is Negatable into NegInt = _.negate()
```

#### Rooting

TBC


## Status

Symbolism is classified as __embryotic__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Symbolism is designed to be _small_. Its entire source code currently consists
of 166 lines of code.

## Building

Symbolism will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Symbolism?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Symbolism's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Symbolism and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `symbolism`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Symbolism's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Symbolism are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/symbolism/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Symbolism
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Symbolism was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

__Symbolism__ helps work with _symbolic_ operators

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows three of the four arithmetic operators overlaid upon each other.

## License

Symbolism is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

