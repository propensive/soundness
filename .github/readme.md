[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/abacist/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/abacist/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Abacist

__Calculations with non-decimal units and mixed bases__

Many units of measurement in use today have not yet been decimalized, and work
in non-decimal and mixed bases. Arithmetic using these units can be convoluted,
requiring either conversion or error-prone carrying. _Abacist_ builds upon the
representations of physical quantities provided by
[Quantitative](https://github.com/propensive/quantitative/) to support compound
units in mixed bases.

## Features

- allows arbitrary constructions of cascading units of the same dimension, such as (miles, yards, feet and inches)
- provides addition, subtraction and scalar multiplication operations on those units
- allows conversion to and from decimal `Quantity` values
- unit types are opaque aliases of `Long` for performance


## Availability







## Getting Started

All terms and types are defined in the `abacist` package, and are exported to
the `soundness` package, and build upon
definitions in [Quantitative](https://github.com/propensive/quantitative/). You
can import from either:
```amok
syntax scala
transform
  before   selective imports
  after    universal soundness import
  replace  abacist.*, quantitative.*  soundness.*
##
import abacist.*, quantitative.*
```

### Discrete Cascading Units

Prior to the adoption of the metric system for quantities of mass and length, other systems based
on non-decimal (but exact) multiples of other units were widely used. These are commonly called
"the Imperial System", particularly for lengths, and for masses, "Avoirdupois", but actually
represent several different systems (often with units of the same name representing different
physical amounts, confusingly) adopted in various jurisdictions with varying degrees of
officiality.

Abacist can accommodate all such systems through a single _type_ which defines a cascade of
units (of the same dimension), in a tuple. The unit types are defined in
Quantitative, and Abacist makes it
possible to use them in discrete multiples.

For example, one variant of the Imperial System measuring
human heights could be defined as,
```amok
syntax scala
##
type ImperialHeight = (Feet[1], Inches[1])
```
or for longer distances,
```amok
syntax scala
##
type ImperialDistance = (Miles[1], Yards[1], Inches[1])
```
that is, a number of miles, yards and inches, represented as a `Tuple` of these units' types
(each raised to the power `1`). Another example for mass is,
```amok
syntax scala
##
type Avoirdupois = (Hundredweights[1], Stones[1], Pounds[1], Ounces[1], Drams[1])
```
or alternatively:
```amok
syntax scala
##
type SimpleAvoirdupois = (Pounds[1], Ounces[1])
```

Each type, a tuple of subtypes of `Measure`, statically represents a system of discrete cascading units,
provided that,
- each element of the tuple has the same dimensionality (i.e. represents the same sort of physical quantity)
- the elements are ordered by decreasing magnitude
and furthermore, for most useful operations, that contextual `Ratio` instances exist between each unit and
the principal unit for their common dimension.

With a valid definition, such as one of the above, we can represent values in its units, called a `Count`
(because it's a count of integer multiples of each of the units).

### Construction

To construct a new `Count`, simply call its factory method with the appropriate tuple type, and as many
integer arguments as necessary. The rightmost `Int` argument will be interpreted as the multiple of the
rightmost unit in the tuple, and additional arguments will represent (right-to-left) multiples of units of
increasing magnitude. For example, `Count[ImperialDistance](180, 24)` represents, "180 yards and 24 inches",
while, `Count[ImperialDistance](1, 180, 24)` represents, "1 mile, 180 yards and 24 inches".

### Extraction

Individual units from a `Count` may be extracted by applying the units value
to extract, without its dimension, to the `Count` value, like so:
```amok
syntax scala
##
type Height = (Feet[1], Inches[1])
val height: Count[Height] = Count(6, 4)
val feet = height[Feet]
val inches = height[Inches]
```

The value `feet` will be set to `6`, and the value `inches` will be `4`.

`Count`s of identical units may be added and subtracted, and multiplied and divided by numbers, but not
by other quantities. They may be converted to `Quantity`s with the `quantity` method, much as a `Quantity` can
be converted, or constructed from a `Quantity` by calling `count` on the
`Quantity`, e.g.
```amok
syntax scala
##
(18*Kilo(Gram)).count[Avoirdupois]
```

### Rounding

Note that in many cases, the discrete units of a `Count` will not be able to
precisely represent a `Quantity`. A `Quantity` will be rounded to the nearest
whole `Count` value. Converting back to a `Quantity` will include this
rounding error.

For example, note how the error changes when more precise `Count` units are
used:
```amok
syntax scala
transform
  before  Imprecise height
  after   Precise height
  replace  Inches[1]  Inches[1], Points[1]
  replace  0.00460  0.0000139
##
type Height = (Feet[1], Inches[1])
val height: Quantity[Metres[1]] = Quantity(1.3)
val error = height - height.count[Height].quantity

assert(error == 0.00460)
```


### Underlying Representation

A `Count` is an opaque type alias for a `Long`, meaning that operations involving `Count`s do not involve
any heap objects. The underlying value of a count represents an integer
multiple of the smallest unit in the cascade. For example, a length of
`1ft 3in` would be stored as `15`, being the sum of the 12 inches in one foot,
plus 3 inches.


## Status

Abacist is classified as __fledgling__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Abacist is designed to be _small_. Its entire source code currently consists
of 311 lines of code.

## Building

Abacist will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Abacist?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Abacist's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Abacist and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `abacist`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Abacist's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Abacist are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/abacist/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Abacist
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Abacist was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

An _abacist_ is a person who operates an abacus, for counting and arithmetic.
Counting of mixed-base units is the purpose of _Abacist_.

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

The logo shows a single bead on a rod of an abacus, the device an _abacist_ specializes in.

## License

Abacist is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

