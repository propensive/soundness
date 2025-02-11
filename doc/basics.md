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

