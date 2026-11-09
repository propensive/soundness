## Mathematics

### About

Beyond ordinary [numbers](numbers.md), there are the structured values of mathematics:

- [complex numbers](https://en.wikipedia.org/wiki/Complex_number), whose real and imaginary
  parts may be of any suitably numeric type;
- [vectors](https://en.wikipedia.org/wiki/Euclidean_vector) and
  [matrices](https://en.wikipedia.org/wiki/Matrix_(mathematics)), whose dimensions are part of
  their types, so that a mismatch of dimensions does not compile;
- [permutations](https://en.wikipedia.org/wiki/Permutation), which are reversible
  rearrangements stored in a compact canonical form.

All three compose with the rest of the arithmetic. A vector of lengths dotted with a vector of
lengths gives an area, and a complex number of [quantities](quantities.md) keeps its units,
because the operations are defined through the same
[arithmetic typeclasses](numbers.md#overloading-the-operators) everywhere.

### On mathematical structure

A vector is not a list. A list of three numbers and a list of four concatenate happily. Vectors
of different dimensions cannot even be added, and a matrix product is defined only when the
inner dimensions agree. A library which models these as arrays defers every such rule to
[runtime], where a mismatch of shapes becomes an exception, or, worse, silent nonsense, long
after the mistake.

Here the dimensions are in the types. A `Vector[Int, 3]` and a `Vector[Int, 2]` are different
types. A `Matrix[…, 2, 3]` multiplies a `Matrix[…, 3, 2]`, and nothing else. A mismatched
multiplication is an [impossible state](../philosophy/impossible-states.md) of linear algebra.
The element type is generic over anything with the right arithmetic, which is how units flow
through a calculation.

Everything comes from the `soundness` package. Arithmetic between plain numbers and these types
relies on the compiler's *into* conversions, so that language feature is enabled too, and the
last import lets the samples print their results:

```scala
import soundness.*
import scala.language.experimental.into
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Complex numbers

A `Complex` pairs a real component with an imaginary one, and supports the usual arithmetic:

```scala fuel
run
##
Out.println(Complex(1, 3).show)
Out.println((Complex(1, 3)*Complex(2, 4)).show)
Out.println((-Complex(10, 7)).show)
```

A complex number can also be written by adding to a multiple of `i`, the imaginary unit, or
given in polar form, as a modulus and an `Angle`:

```scala
val sum = Complex(0.8, 0.0) + i*1.8
val polar = Complex(12*Kilo(Gram), 0.3845.rad)
```

`modulus` and `argument` give the polar form back, with the argument as a typed angle. The
prefix operator `~` gives the complex conjugate.

The components may be of any type which has the arithmetic an operation needs. That includes
quantities, so a complex impedance keeps its units. A quantity is written out through a
`Decimalizer`, which says how many significant figures to show:

```scala
given decimalizer: Decimalizer = Decimalizer(3)

val real = 1.0*Metre/Second
val imaginary = 9.0*Metre/Second
```

```scala fuel
run
##
Out.println(Complex(real, imaginary).show)
```

### Vectors

A `Vector` is a fixed number of components, with the dimension in its type. It is in effect a
hybrid of a `Tuple`, whose size is known statically, and a collection, whose elements share a
type. That is what a Euclidean vector is, and it is why neither of the two ordinary choices
quite fits it.

Constructing a vector infers both the element type and the size:

```scala
val v: Vector[Int, 3] = Vector(1, 2, 3)
```

A value of the wrong size does not compile:

```scala fuel
compile
errors show
##
val w: Vector[Int, 2] = Vector(1, 3, 4)
```

Vectors of one dimension add and subtract. A vector negates, and multiplies and divides
component by component, either by a scalar or by another vector of the same dimension,
whenever its components do:

```scala fuel
run
##
Out.println((Vector(1, 2, 3) + Vector(3, 4, 5)).inspect)
Out.println((-Vector(1, -2, 3)).inspect)
Out.println((Vector(1, 2, 3)*2).inspect)
Out.println((Vector(1, 2, 3)*Vector(4, 5, 6)).inspect)
```

Adding vectors of different dimensions does not compile:

```scala fuel
compile
errors show
##
Vector(1, 2, 3) + Vector(1, 2)
```

`dot` computes the scalar product and, in three dimensions, `cross` computes the vector
product:

```scala fuel
run
##
Out.println(Vector(1, 2, 3).dot(Vector(4, 3, 7)).inspect)
Out.println(Vector(1, 2, 3).cross(Vector(4, 3, 7)).inspect)
```

### Matrices

A `Matrix` has its numbers of rows and columns in its type. They are written as type
arguments, and the matrix is filled row by row:

```scala
val m1 = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
val m2 = Matrix[3, 2]((7, 8), (9, 10), (11, 12))
```

A matrix multiplies another matrix, a vector or a scalar, and negates:

```scala fuel
run
##
Out.println((m1*m2).inspect)
Out.println((m1*Vector(7, 8, 9)).inspect)
Out.println((m1*10).inspect)
Out.println((-m1).inspect)
```

A product whose dimensions do not agree is a compile error, not a check of shapes at runtime:

```scala fuel
compile
errors show
##
m1*m1
```

Beyond its arithmetic, a matrix has these operations. Those of a square matrix exist only for
a square matrix:

```tabular
column  Method
column  Of
column  Gives
row  `transpose`  any matrix  the transpose
row  `rank`  any matrix  the rank
row  `frobeniusNorm`  any matrix  the square root of the sum of the squares of its entries
row  `determinant`  a square matrix  the determinant
row  `trace`  a square matrix  the sum of the diagonal
row  `inverse`  a square matrix  the inverse, or `Unset` for a singular matrix
row  `solve`  a square matrix  the solution of a linear system, as an `Optional` vector
row  `adjugate`  a square matrix  the transpose of the cofactor matrix
row  `eigenvalues`  a square matrix of `Double`s  the eigenvalues, as an `Optional` vector
row  `eigenvectors`  a square matrix of `Double`s  the eigenvectors, as an `Optional` matrix
```

```scala fuel
run
##
Out.println(Matrix[3, 3]((2, -3, 1), (2, 0, -1), (1, 4, 5)).determinant.inspect)
Out.println(Matrix[2, 2]((1.0, 2.0), (3.0, 4.0)).inverse.let(_.inspect).or(t"singular"))
Out.println(Matrix[2, 2]((1.0, 2.0), (2.0, 4.0)).inverse.let(_.inspect).or(t"singular"))
```

`inverse` gives an [optional](optional-values.md) matrix, since a singular matrix has none.

The `adjugate` is available in its own right. It is defined over any ring, and not only where
division is possible, so a matrix of integers has an adjugate where it has no inverse:

```scala fuel
run
##
Out.println(Matrix[2, 2]((1, 2), (3, 4)).adjugate.inspect)
```

`frobeniusNorm` gives a matrix's magnitude. `eigenvalues` gives the values for which it has an
eigenvector, as an `Optional`, since not every matrix has real eigenvalues:

```scala fuel
run
##
Out.println(Matrix[2, 2]((3.0, 0.0), (0.0, 4.0)).frobeniusNorm.inspect)
Out.println(Matrix[2, 2]((2.0, 1.0), (1.0, 2.0)).eigenvalues.let(_.inspect).or(t"none"))
```

### Elements that are not numbers

Nothing above requires the entries to be `Double`s. Vectors and matrices are generic in their
element type, and their operations are found through the arithmetic typeclasses, so anything
with the right structure works. That includes typed [quantities](quantities.md), whose
dimensions compose as the arithmetic does:

```scala
val v1 = Vector(5*Inch, 2*Inch, Inch)
val v2 = Vector(2*Inch, 3*Inch, 6*Inch)
```

```scala fuel
run
##
Out.println(v1.dot(v2).inspect)
```

A dot product of lengths is an area, and the type says so. The same holds for complex entries,
for exact rationals, and for any other element type with the operations a computation needs.

### Permutations

A `Permutation` is a reversible rearrangement. It is built from the reordered indexes, and is
applied to any list of at least that length:

```scala
import strategies.throwUnsafely

val shuffle = Permutation(Sequence(3, 1, 4, 2, 0, 5))
val items = List(t"zero", t"one", t"two", t"three", t"four", t"five")
```

```scala fuel
run
##
Out.println(shuffle(items).inspect)
Out.println((shuffle.inverse(shuffle(items)) == items).inspect)
```

A sequence of indexes which is not a permutation, because it has a duplicate or a gap, is a
typed error:

```scala fuel
run
errors show
##
Permutation(Sequence(3, 1, 1))
```

Internally, a permutation is stored as its
[Lehmer code](https://en.wikipedia.org/wiki/Lehmer_code) in factorial-base form, which is a
single integer that uniquely identifies it. That makes permutations compact to store and to
enumerate: `Permutation.bySize(n)` gives all `n!` of them, as a lazy
[chain](collections.md#lazy-chains). A permutation of *n* elements occupies
O(*n* log *n*) space, which is the least possible, since there are *n*! of them to tell apart.

The number *is* the permutation, so it can be given directly, and both representations can be
read back:

```scala fuel
run
##
Out.println((Permutation(Factoradic(45)) == Permutation(Sequence(1, 4, 2, 3, 0))).inspect)
Out.println(shuffle.lehmer.inspect)
Out.println(shuffle.expansion.inspect)
```

A permutation leaves alone every position outside its own range. Applying one to a longer
sequence is therefore not an error: it rearranges the positions it covers, and leaves the rest
where they were.

### Related

- [Mathematical markup](mathematical-markup.md) typesets what is computed here, and [randomness](randomness.md) samples its distributions.
