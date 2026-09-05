## Mathematics

### About

Beyond ordinary [numbers](numbers.md), Soundness provides the structured values of mathematics:
[complex numbers](https://en.wikipedia.org/wiki/Complex_number), whose real and imaginary parts may
be any suitably numeric type; [vectors](https://en.wikipedia.org/wiki/Euclidean_vector) and
[matrices](https://en.wikipedia.org/wiki/Matrix_(mathematics)) whose dimensions live in their
types, so a dimension mismatch is a compile error; and
[permutations](https://en.wikipedia.org/wiki/Permutation), reversible rearrangements stored in a
compact canonical form.

All three compose with the rest of Soundness's arithmetic: a vector of lengths dotted with a vector
of lengths gives an area, and a complex number of [quantities](quantities.md) keeps its units,
because the operations are defined through the same arithmetic typeclasses everywhere.

### On mathematical structure

A vector is not a list. A list of three numbers and a list of four concatenate happily; vectors of
different dimensions cannot even be added, and a matrix product is defined only when the inner
dimensions agree. Libraries that model these as arrays defer every such rule to runtime, where a
shape mismatch becomes an exception — or worse, silent nonsense — long after the mistake.

Soundness puts the dimensions in the types. A `Vector[Int, 3]` and a `Vector[Int, 2]` are different
types; a `Matrix[…, 2, 3]` multiplies a `Matrix[…, 3, 2]` and nothing else; and the element type is
generic over anything with the right arithmetic, which is how units flow through. Everything comes
from the `soundness` package, and arithmetic between plain numbers and these types relies on the
compiler's `into` conversions, so that language feature is enabled too:

```scala
import soundness.*
import scala.language.experimental.into
```

Dimensions in the types of vectors and matrices make a mismatched multiplication a compile error — [impossible states](../philosophy/impossible-states.md) for linear algebra.

### Complex numbers

A `Complex` pairs a real and an imaginary component, in Cartesian or polar form, and supports the
usual arithmetic; `i` is the imaginary unit:

```scala
Complex(1, 3).show           // t"1 + 3ℐ"
Complex(1, 3)*Complex(2, 4)  // Complex(-10, 10)
-Complex(10.0, 7.0)          // Complex(-10.0, -7.0)
```

The components may be any type with the arithmetic the operation needs — including quantities, so a
complex impedance keeps its units:

```scala
val real = 1.0*Metre/Second
val imaginary = 9.0*Metre/Second

Complex(real, imaginary).show   // t"(1.00 + 9.00ℐ) m·s¯¹"
```

A complex number can equally be written by adding to a multiple of the imaginary unit, or given in
polar form as a modulus and an `Angle`:

```scala
Complex(0.8, 0.0) + i*1.8            // Complex(0.8, 1.8)
Complex(12*Kilo(Gram), 0.3845.rad)   // from modulus and argument
```

`modulus` and `argument` give the polar form back, the argument as a typed angle, and the prefix
`~` operator gives the complex conjugate.

### Vectors

A `Vector` is a fixed number of components, the dimension in the type — in effect a hybrid of a
`Tuple`, whose size is known statically, and a collection, whose elements share a type. That is
what a Euclidean vector is, and it is why neither of the two ordinary choices quite fits it.
Construction infers both the element type and the size, and a value of the wrong size does not
compile:

```scala
val v = Vector(1, 2, 3)               // Vector[Int, 3]
val w: Vector[Int, 2] = Vector(1, 3, 4)   // does not compile: three components
```

Vectors of one dimension add and subtract; `dot` and, in three dimensions, `cross` compute the
products, carrying units through where the components are quantities:

```scala
Vector(1, 2, 3).dot(Vector(4, 3, 7))     // 31
Vector(1, 2, 3).cross(Vector(4, 3, 7))   // Vector(5, 5, -5)

Vector(5*Inch, 2*Inch, Inch).dot(Vector(2*Inch, 3*Inch, 6*Inch))   // 22 square inches
```

### Matrices

A `Matrix` carries its row and column counts in its type, written as type arguments and filled
row by row:

```scala
val m1 = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
val m2 = Matrix[3, 2]((7, 8), (9, 10), (11, 12))

m1*m2   // Matrix[2, 2]((58, 64), (139, 154))
m1*Vector(7, 8, 9)   // Vector(50, 122)
```

A product whose dimensions do not agree is a compile error, not a runtime shape check. Square
matrices add `determinant`, `trace`, `inverse` — an `Optional`, absent for a singular matrix — and
`solve` for a linear system:

```scala
Matrix[3, 3]((2, -3, 1), (2, 0, -1), (1, 4, 5)).determinant   // 49
Matrix[2, 2]((1.0, 2.0), (3.0, 4.0)).inverse
// Matrix[2, 2]((-2.0, 1.0), (1.5, -0.5))
```

The `adjugate` — the transpose of the cofactor matrix — is available in its own right, since it is
defined over any ring and not only where division is possible, so an integer matrix has an
adjugate where it has no inverse:

```scala
Matrix[2, 2]((1, 2), (3, 4)).adjugate   // Matrix[2, 2]((4, -2), (-3, 1))
```

`frobeniusNorm` gives the matrix's magnitude — the square root of the sum of the squares of its
entries — and `eigenvalues` computes the values for which the matrix has an eigenvector, as an
`Optional` since not every matrix has real eigenvalues:

```scala
Matrix[2, 2]((3.0, 0.0), (0.0, 4.0)).frobeniusNorm   // 5.0
Matrix[2, 2]((2.0, 1.0), (1.0, 2.0)).eigenvalues     // 1.0 and 3.0
```

### Elements that are not numbers

Nothing above requires the entries to be `Double`s. Vectors and matrices are generic in their
element type, and the operations resolve through the arithmetic typeclasses, so anything with the
right structure works — including typed [quantities](quantities.md), where the dimensions compose
as the arithmetic does:

```scala
val v1 = Vector(5*Inch, 2*Inch, Inch)
val v2 = Vector(2*Inch, 3*Inch, 6*Inch)

v1.dot(v2)   // 22 square inches — the units multiplied too
```

A dot product of lengths is an area, and the type says so. The same holds for complex entries, for
exact rationals, and for any other element type with the operations the computation needs.

### Permutations

A `Permutation` is a reversible rearrangement, built from the reordered indexes and applied to any
list of at least that length. An index sequence that is not a permutation — a duplicate, a gap — is
a typed error:

```scala
import strategies.throwUnsafely

val shuffle = Permutation(Sequence(3, 1, 4, 2, 0, 5))
val items = List(t"zero", t"one", t"two", t"three", t"four", t"five")

shuffle(items)   // List(t"three", t"one", t"four", t"two", t"zero", t"five")

shuffle.inverse(shuffle(items)) == items   // always true
```

Internally a permutation is stored as its [Lehmer code](https://en.wikipedia.org/wiki/Lehmer_code)
in factorial-base form — a single integer that uniquely identifies it — which makes permutations
compact to store and enumerate: `Permutation.bySize(n)` streams all `n!` of them. A permutation of
*n* elements occupies O(*n* log *n*) space, which is the information-theoretic minimum, since
there are *n*! of them to tell apart.

Since the number *is* the permutation, it can be given directly, and both representations can be
read back:

```scala
Permutation(Factoradic(45)) == Permutation(Sequence(1, 4, 2, 3, 0))   // true

shuffle.lehmer      // the Lehmer code, as a List[Int]
shuffle.expansion   // the reordered indexes
```

Applying a permutation to a value out of range is not an error, either: a permutation fixes every
point outside its domain, so applying it to a longer sequence rearranges the tail and leaves the
excess prefix where it was.
