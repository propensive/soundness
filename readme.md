[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/mosquito/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/mosquito/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Mosquito

____

Euclidean vectors, in contrast to scalars and arbitrary collections, represent
values in _a fixed multiple_ number of dimensions. _Mosquito_ provides a
representation of vectors, `Euclidean`, whose generic type encapsulates both
its element type and size. In some sense, a `Euclidean` can be considered a
hybrid of a `Tuple` (whose size in known) and a collection (whose elements are
homogeneous). Mosquito supports the use case of _generic programming_ with
`Euclidean` vectors, but facilitates linear algebra operations, including
working with matrices and scalar and vector products.

## Features

- representation of Euclidean vectors and matrices
- provides many common linear algebraic operations
- vectors and matrices are generically-typed
- linear algebraic operations abstract over arithmetic operations
- results are dependently-typed on inputs


## Availability Plan

Mosquito has not yet been published. The medium-term plan is to build Mosquito
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Mosquito.

Subsequently, Mosquito will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

All terms and types are defined in the `mosquito` package, so it's sufficient
to import,

```scala
import mosquito.*
```

### Constructing vectors

A Euclidean vector, primarily to distinguish it from Scala's `Vector`
collection type, is called `Euclidean`. It represents an ordered, generic
collection of elements whose length is known, and furthermore encoded in its
type. So a three-dimensional vector of `Double` values would have the type,
`Euclidean[Double, 3]`.

But since both the element type and the size of a Euclidean vector can be
inferred from its values, we can create such a vector with just,
```scala
val vector = Euclidean(2.7, -0.3, 0.8)
```
and it's type will be known to be `Euclidean[Double, 3]`.

### Constructing matrices

A matrix has the type `Matrix`, and like a `Euclidean`, contains homogeneous
elements, and encodes its size in its type. However, a `Matrix`'s size is
determined by a number of rows and a number of columns. So a 3×4 matrix of
`Int`s would have the type, `Matrix[Int, 3, 4]`. Conventionally for matrices,
the number of rows is given before the number of columns. This is in contrast
to the more general convention that a width precedes height. So we avoid the
terminology of _width_ and _height_ when describing a matrix, and talk instead
of _rows_ and _columns_.

When constructing a new `Matrix`, its rows and columns should be specified as
type parameters (since they cannot be inferred at present), and organised into
equal-length tuples for each row of the matrix. The number and length of the
tuples must, of course, match the rows and columns specified during
construction, and a mismatch will result in a compile error. Here is an example
of constructing a 2×3 `Int` matrix:
```scala
val matrix = Matrix[2, 3](
  (1, -2, 4),
  (3, 8, -1)
)
```

### Multiplication operations

Vectors may be multiplied in several different ways:
- multiplication of a vector by a scalar, yielding a new vector
- a _dot product_ between two equal-dimension vectors
- a _cross product_ between two 3- or 7-dimensional vectors
- matrix multiplication
- left- and right-multiplication of a vector by a matrix

#### Scalar multiplication

A vector may be multiplied by a scalar intuitively with the `*`. The result
will be a new vector, of the same dimension, and elements of the type resulting
from multiplying the original element types by the scalar value.

Often, as for `Double`s, this would be the same type: the product of a
`Euclidean[Double, 3]` and a `Double` will be another `Euclidean[Double, 3]`.
But this will not always be the case.

If using [Quantitative](https://github.com/propensive/quantitative/) with
Mosquito, a vector of values in metres per second, multiplied by a time value,
would yield a vector distance. Specifically, a `Euclidean[Quantity[Metres[1] &
Seconds[-1]]]` multiplied by a `Quantity[Seconds[1]]` would produce a
`Euclidean[Quantity[Metres[1]]]`.

#### Dot Products

The extension method `dot` is available for combining two `Euclidean` instances
of equal dimension, and whose scalar elements have a `*` operation defined,
whose result type has a `+` operation that returns the same type. Although
slightly complex, these criteria correspond closely to the low-level operations
needed to carry out a dot product.

The result type of a dot product will therefore be determined by the element
types of each vector. Using another _Quantitative_ example, the dot product of
two vectors of distance `Quantity` values in metres would be a single scalar
value in square metres.

#### Cross Products

The cross product is only defined for vectors of dimension 3 or 7. Currently,
_Mosquito_ only provides an implementation for 3-dimensional `Euclidean`
values, but cross products between 7-dimensional vectors will be provided at a
later date.

#### Matrix multiplication

A pair of matrices may be multiplied with the `*` operator, if they share the
same inner dimension, that is, the number of columns of the left matrix is the
same as the number of rows of the right matrix. The result will be a new square
matrix, whose elements' type will be determined by the result of `*` and `+`
operations on their elements.

#### Multiplication of a vector by a matrix

Left- and right-multiplication by a matrix is not yet implemented, but is a
logical and simple inclusion, since the operation is equivalent to matrix
multiplication if the vector is interpreted as a single-row or single-column
matrix.

[Github issue #3](https://github.com/propensive/mosquito/issues/3) is tracking
the implementation of multiplication of a vector by a matrix.

### Other operations

Vectors and matrices of equal dimension may also be added and subtracted.
Usually, that will result in a vector or matrix of the same element type, but
in some cases, result types may be different. Quantitative would, for example,
produce a value in metres per second when adding units in feet per second and
metres per minute.



## Status

Mosquito is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Mosquito is designed to be _small_. Its entire source code currently consists
of 249 lines of code.

## Building

Mosquito will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Mosquito?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Mosquito's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Mosquito and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `mosquito`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Mosquito's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Mosquito are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/mosquito/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Mosquito
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Mosquito was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

A mosquito is a typical example of a vector: an animal that transmits a pathogen or disease.

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

The logo represents the _x_, _y_ and _z_ axes of a vector space.

## License

Mosquito is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

