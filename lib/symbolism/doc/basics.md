
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
        :   List[Result] =
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
        :   sum.Result
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
