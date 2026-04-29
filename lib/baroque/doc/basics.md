All terms and types are defined in the `baroque` package, which can be imported
with:
```scala
import baroque.*
```

### Constructing complex numbers

There are several ways to construct complex numbers. The factory method of
specifying real and imaginary part is the most straightforward, for example,
```scala
val complex = Complex(3.2, -1.25)
```
represents the complex number 3.2 - 1.25i.

This is an instance of `Complex[Double]`, since the real and imaginary parts of
the number are both `Double`s. With Quantitative, we could similarly create a
complex quantity, for example,
```scala
val complex2 = Complex(3.2*Metre, -1.25*Metre)
```
which would be a `Complex[Quantity[Metres[1]]]`.

But it is also possible construct a complex number by adding the real part to
the imaginary part, where the imaginary part is created by multiplying a real
number by _i_, the imaginary unit value, which is called `I` in Baroque:
```scala
val complex3 = 0.8 + 1.8*I
```

I further possibility is to specify the complex number in polar form, using the
`Complex.polar` constructor. This takes two parameter, _magnitude_ and
_argument_. The magnitude should have the same type a the real and imaginary
parts of the number, and the argument must be a `Double`, in radians. For
example,
```scala
val complex4 = Complex.polar(12*Kilo(Gram), 0.3845)
```

### Operations with Complex Numbers

Standard arithmetic operations between `Complex` instances, using the `+`, `-`,
`*` and `/` operators work intuitively. Additionally, the prefix `~` operator
can be used to find the complex conjugate of a number.

The methods `modulus` and `argument` on `Complex` values provide, predictably,
the modulus and argument. And `sqrt` will yield one of the two square roots of
a `Complex` number; the other will be its negation.

These operations are defined, in general, so long as the necessary operations
(such as addition, multiplication and square root) are defined on the
underlying type of the real and imaginary parts.


