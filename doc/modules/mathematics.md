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
from the `soundness` package:

```scala
import soundness.*
```

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
Complex(1.0*Metre/Second, 9.0*Metre/Second).show   // t"(1.00 + 9.00ℐ) m·s¯¹"
```

`modulus` and `argument` give the polar form, the argument as a typed angle.

### Vectors

A `Vector` is a fixed number of components, the dimension in the type. Construction infers both the
element type and the size, and a value of the wrong size does not compile:

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

### Permutations

A `Permutation` is a reversible rearrangement, built from the reordered indexes and applied to any
list of at least that length. An index sequence that is not a permutation — a duplicate, a gap — is
a typed error:

```scala
import strategies.throwUnsafely

val shuffle = Permutation(Series(3, 1, 4, 2, 0, 5))
shuffle(List(t"zero", t"one", t"two", t"three", t"four", t"five"))
// List(t"three", t"one", t"four", t"two", t"zero", t"five")

shuffle.inverse(shuffle(items)) == items   // always true
```

Internally a permutation is stored as its [Lehmer code](https://en.wikipedia.org/wiki/Lehmer_code)
in factorial-base form — a single integer that uniquely identifies it — which makes permutations
compact to store and enumerate: `Permutation.bySize(n)` streams all `n!` of them.
