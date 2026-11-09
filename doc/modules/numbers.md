## Numbers

### About

The numeric types are precise and regular, and each one's name says what it is:

```tabular
column  Family
column  Types
column  Is
column  Operators
row  `U*`  `U8`, `U16`, `U32`, `U64`  unsigned  arithmetic
row  `S*`  `S8`, `S16`, `S32`, `S64`  two's-complement signed  arithmetic
row  `F*`  `F32`, `F64`  floating-point  arithmetic
row  `B*`  `B8`, `B16`, `B32`, `B64`  a raw bit pattern  shifts, rotations and masks
```

The letter says how a number's bits are read, and the digits say how many bits there are, so
both are part of its type. Arithmetic can be checked for overflow and for division by zero. The
checking is chosen by import, and is paid for only where it is wanted.

A *bounded* type goes further, and carries a number's permitted range. A value outside `[0, 1]`
is then an error at [compiletime], where it is written, and arithmetic on bounded numbers works
out the range of the result. A range in the type is the plainest way to make an
[impossible state](../philosophy/impossible-states.md) unrepresentable.

### On numbers

Scala inherits the JVM's numbers, which are a mixed bag:

- Every integer is signed.
- Addition overflows in silence, and wraps round to a negative number.
- Dividing an integer by zero throws, while dividing a floating-point number by zero gives
  `NaN` or an infinity.
- The bitwise operators sit on the same types as the arithmetic ones, so a shift and a multiply
  look alike.

The type says only how many bits a number has, and never how those bits are meant to be read.

Here the concerns are separated. A number is *either* arithmetic, a `U*`, `S*` or `F*` with `+`,
`-`, `*` and `/` and no bitwise operators, *or* a bit pattern, a `B*` with shifts and masks and
no arithmetic. Conversions between them are explicit, and free. The dangerous operations are
checked only when a checking [given] is in scope. Each type is an [opaque type] over a JVM
primitive, so none of this exists at [runtime].

Everything comes from the `soundness` package. Generic number literals are enabled, so that a
literal takes whichever numeric type is expected, and the last import lets the samples print
their results:

```scala
import soundness.*
import scala.language.experimental.genericNumberLiterals
import scala.language.experimental.into
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Numeric types

A literal takes the type it is assigned to. A conversion between types is a short method named
for the target type:

```scala
val count: U64 = 123
val small: U8 = 200
val signed: S32 = 42
val widened: S32 = small.s32
```

Converting a `U8` to an `S32` can lose nothing, since the wider type holds every value of the
narrower one. An unsigned value and a signed one are different types, so mixing them is always
a deliberate conversion, never an accident:

```scala fuel
compile
errors show
##
val mixed: S32 = small
```

The operators `+`, `-`, `*`, `/` and prefix `-` are not methods of these types. They are found
through the [typeclasses][typeclass] `Addable`, `Subtractable`, `Multiplicable`, `Divisible`
and `Negatable`, which [Overloading the operators](#overloading-the-operators) describes. Code
written against those typeclasses can therefore sum a `U32` or an `F64` alike, and a value of
one of these types can be combined with a `Double`, an `Int` or a narrower type, wherever a
conversion exists:

```scala
val ratio: F64 = 1.5
```

```scala fuel
run
##
Out.println((ratio*2).inspect)
Out.println((ratio + 0.25).inspect)
Out.println((-ratio).inspect)
```

The operand of such an operator is a type parameter, which a bare integer literal does not
determine. An integer literal operand is therefore written with its type, as in
`count + (1: U64)`.

### Checked arithmetic

Arithmetic on the integer types needs an overflow policy in scope, and division needs one of
its own. Each policy is imported from the package `arithmeticOptions`:

```tabular
column  Import
column  Governs
column  Behavior
row  `uncheckedOverflow`  `+`, `-` and `*`  the hardware's, wrapping on overflow
row  `checkedOverflow`  `+`, `-` and `*`  raises `Arithmetic.Error` on overflow
row  `uncheckedDivision`  `/`  the bare machine behavior
row  `checkedDivision`  `/`  raises `Arithmetic.Error` on division by zero
```

`checkedOverflow` changes the result type of `+`, `-` and `*` to one which can raise an
`Arithmetic.Error`. An overflow then becomes a failure to be [handled](errors.md), not a silent
wrap to a negative number:

```scala
import arithmeticOptions.checkedOverflow
import strategies.throwUnsafely

val big: S32 = 2000000000
```

```scala fuel
run
##
Out.println(safely(big + big).inspect)
```

```scala fuel
run
errors show
##
big + big
```

The two checks are independent. A program which must not wrap, but is content to trust its
divisors, imports only the first.

Neither costs anything where it is not chosen. After inlining, an unchecked `a + b` on an `S32`
is the single `iadd` instruction the hardware has for it, with nothing allocated, and a checked
one is the same instruction followed by a test.

Overflow detection is exact. It tells the cases where a signed addition really overflows from
those where it merely crosses zero. A sum of the most negative value with itself is caught,
while ordinary negative arithmetic is undisturbed.

### Bit manipulation

The `B*` types treat a number as a set of bits. They have the shifts, rotations and masks, and
are written out in the usual radices, but they have no arithmetic:

```scala
val pattern: B32 = 0xf0.bits
```

```scala fuel
run
##
Out.println(pattern.hex)
Out.println((pattern << 2).hex)
Out.println((pattern & 0x30.bits).hex)
```

Arithmetic on a bit pattern does not compile:

```scala fuel
compile
errors show
##
pattern + pattern
```

The operations on a bit pattern are:

```tabular
column  Operation
column  Does
row  `<<`, `>>`  shift left or right, filling with zeros
row  `<<<`, `>>>`  rotate left or right
row  `&`, `|`, `^`  and, or and exclusive or
row  `~`  invert every bit
row  `bit`  read one bit
row  `set`, `clear`, `flip`  write one bit, or every bit of an interval
row  `leadingZeros`, `trailingZeros`  count the zeros at either end
row  `reverse`  reverse the order of the bits
```

A bit is addressed by an [ordinal](#ordinals): `Prim` is the first and `Sec` the second. A bit's
position therefore cannot be confused with the zero-based or one-based convention of whichever
specification is being implemented:

```scala fuel
run
##
Out.println(B64(Data(0, 0, 0, 0, 0, 0, 0, 6)).bit(Sec).inspect)
```

A bit pattern is written out padded to its type's full width, without dropping leading zeros,
so the number of characters says which type produced it:

```scala fuel
run
##
Out.println((-1: Byte).hex)
Out.println((-1: Byte).binary)
```

[Bit flags](flags.md) names the bits of a word by the cases of an enumeration, and
[Binary data](binary-data.md) reads these types out of a sequence of bytes.

### Mathematics

The operations which Java scatters across `java.lang.Math` are extension methods, consistent
across the numeric types: `abs`, `ceiling`, `floor`, `round`, `signum` and the rest. `**` raises
to a power. `%%` is a floor-modulus which, unlike `%`, never gives a negative remainder:

```scala fuel
run
##
Out.println((7.0 %% 3.0).inspect)
Out.println((-7.0 %% 3.0).inspect)
Out.println((2.0 ** 10.0).inspect)
```

The trigonometric and logarithmic functions are plain functions: `cos`, `sin`, `exp`, `ln` and
`log10`. The constants `π`, `euler` and `φ` sit beside them.

`**` widens its result. Raising a `Short` to a power which exceeds a `Short`'s range gives the
right answer, not a wrapped one, and a fractional exponent gives a fractional result:

```scala fuel
run
##
Out.println(((200: Short) ** 2.0).inspect)
Out.println(((1000: Short) ** 1.5).inspect)
```

The statistics of a collection are extension methods too. `median` finds the middle value by a
selection algorithm, without sorting the whole collection, and averages the two middle values
where the count is even:

```scala fuel
run
##
Out.println(Iterable[Double](7, 25, 1, 24, 2, 3, 23, 4, 22, 5, 21).median.inspect)
```

[Mathematics](mathematics.md) builds complex numbers, vectors and matrices on these types.

### Comparisons

Comparisons chain as they do in mathematics, so a value can be tested against two bounds at
once:

```scala fuel
run
##
Out.println(List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 < _ < 1.4).inspect)
```

`min` and `max` compare two values, and
[`minimum` and `maximum`](collections.md#searching-and-measuring) reduce a collection.

### Arbitrary precision

Some values must be exact, and no fixed width will do: a monetary total, a coordinate in a
document, a figure read from a file which promised nothing about its size. A `Decimal` holds a
sign, a magnitude of any size, and a decimal scale:

```scala
val large = Decimal(1234567890123L)
val scaled = Decimal(-1234567, 4)
val parsed = Decimal.parse(t"-12.34e+2")
```

`text` writes a decimal exactly, with nothing lost:

```scala fuel
run
##
Out.println(large.text)
Out.println(scaled.text)
Out.println(parsed.text)
```

A decimal literal in source code becomes a `Decimal` directly, where one is expected.
`Decimal(0.1)` converts a `Double`, and raises a `Decimal.Error` if the double is not exact.

Values are canonical. Trailing zeros are absorbed into the scale as a value is constructed, and
zero has one representation, so two decimals which are numerically equal are the same value.
`BigDecimal`'s trap, in which `equals` and `compareTo` disagree, has no counterpart here.

Division cannot always be exact, so it must be told what is wanted: a scale and a rounding
mode, which are given and never assumed.

```scala fuel
run
##
Out.println(Decimal(1, 0).divide(Decimal(3, 0), scale = 10, Decimal.Rounding.HalfEven).text)
```

`Decimal` is written in pure Scala, and is not a wrapper over the JVM's `BigDecimal`, so it
works on every platform. Nothing in it depends on native 64-bit arithmetic, which matters where
`Long` is emulated. [Money](money.md) uses exact arithmetic for amounts in a currency.

### Overloading the operators

`+`, `-`, `*` and `/` are not built into any of the types above. They come from typeclasses,
and that is what lets arithmetic mean something for types whose operands and results differ.

Scala's own operators are methods, so a type can define `+` only where it knows both sides and
the result. That is enough for a number plus a number. It is not enough for most of the
interesting cases. Multiplying a length by a length gives an *area*. Adding a duration to an
instant gives an instant. Dividing one quantity by another gives something whose type neither
operand could have anticipated.

The operators are therefore defined over typeclasses, each of which names both the operand and
the result:

```tabular
column  Typeclass
column  Supplies
row  `Addable`  `+`
row  `Subtractable`  `-`
row  `Multiplicable`  `*`
row  `Divisible`  `/`
row  `Negatable`  prefix `-`
row  `Rootable`  `sqrt` and `cbrt`
row  `Zeroic`  `zero`, the additive identity
row  `Unital`  `one`, the multiplicative identity
```

An instance reads as a sentence:

```scala
given Double is Addable by Double to Double = Addable(_ + _)
```

A `Double` *is addable by* a `Double` *to* a `Double`. The result type belongs to the instance,
and is not a parameter of the operator. So `length*length` can produce `Quantity[Metres[2]]`
while `length*number` produces `Quantity[Metres[1]]`, and the same `*` finds both. The
identities, `zero` and `one`, are for an algorithm which needs a value to start from.

Three other parts of Soundness rest on these typeclasses:

- [Quantities](quantities.md), where checking the dimensions of a calculation is exactly the
  computing of the result type.
- [Complex numbers and matrices](mathematics.md), which are generic in an element type they
  only ever combine through these operators.
- [Derivation](derivation.md), which can produce arithmetic for a case class, field by field.

[Time](time.md) uses them too, for adding a duration to an instant.

### Ordinals

Off-by-one errors come from a single ambiguity: whether "1" means the first element, or the one
after the first. `Ordinal` removes the ambiguity by being a different type from `Int`. An `Int`
cannot be used where an `Ordinal` is expected, nor the reverse, and there is no zeroth ordinal.
An `Ordinal` is an opaque type over an `Int`, so it costs nothing. The first seven are named:

```tabular
column  Ordinal
column  Position
column  From
row  `Prim`  first  primary
row  `Sec`  second  secondary
row  `Ter`  third  tertiary
row  `Quat`  fourth  quaternary
row  `Quin`  fifth  quinary
row  `Sen`  sixth  senary
row  `Sept`  seventh  septenary
```

Crossing between an `Int` and an `Ordinal` is explicit, and says which convention the number
comes *from*. `z` reads a count from zero as an ordinal, and `u` reads a count from one. So
`0.z` and `1.u` are both `Prim`. Where a number came from, whether a wire protocol counting from
zero or a specification counting from one, is thus recorded where the number enters the
program, and need not be remembered afterwards.

Only the arithmetic which means something is available. Adding a number to an ordinal gives an
ordinal, and subtracting one ordinal from another gives the distance between them, an `Int`:

```scala fuel
run
##
Out.println((Ter + 3).inspect)
Out.println((Sept - Quin).inspect)
```

Multiplying two ordinals does not compile, because it does not mean anything:

```scala fuel
compile
errors show
##
Ter*Quin
```

The last elements of a sequence are named, not computed. `ult` is the last ("ultimate"), `pen`
the second from last, and `ant` the third from last. Each gives an `Optional`, since a sequence
may be too short to have one, and each is available on any type with a `Countable` instance.

A range of ordinals is an `Interval`. It is built with `thru` for an inclusive end, `till` for
an exclusive one, or `span` for a length:

```scala fuel
run
##
Out.println((Ter thru Sen).inspect)
Out.println((Ter till Sen).inspect)
Out.println((Ter span 3).inspect)
```

An interval knows its `start`, `end` and `size`. It iterates with `each` and folds with `fuse`,
as a [collection](collections.md) does. `extent` gives the whole of a countable value as an
interval, so that a traversal states its bounds in terms of the value it traverses, not in raw
integers.

An interval which comes from `extent` carries, in its type, the value it came from, and so do
the ordinals it hands out when iterated. Such an ordinal has been proved to lie within the
collection, so reading with it cannot fail. It gives an element, where an unproven index would
give an `Optional`. A counting loop over a collection is therefore written without a counter,
and without a bounds check on either side:

```scala
val array = Array(10, 20, 30)
var total = 0
array.extent.each { ordinal => total += array(ordinal) }
```

```scala fuel
run
##
Out.println(total.show)
```

`capped` narrows such an interval while keeping what it proves. `array.extent.capped(2)`
iterates the first two positions, and its ordinals can still be read without a check.

### Bounded numbers

A number can carry its permitted range in its type, written with `~`:

```scala fuel
compile
##
val portion: 0.0 ~ 1.0 = 0.2
```

A literal outside the range is a compiletime error, caught where it is written:

```scala fuel
compile
errors show
##
val tooMuch: 0.0 ~ 1.0 = 2.0
```

Arithmetic on bounded numbers computes the range of the result from the ranges of its operands.
Doubling a value in `[0, 1]` gives one which is known to lie in `[0, 2]`, and so certainly
within `[-2, 2]`:

```scala fuel
parse
##
val doubled: -2.0 ~ 2.0 = portion*2.0
```

The bound travels with the value through a calculation, and a step which could break it does
not compile. This is the discipline of [physical quantities](quantities.md), applied to plain ranges. The
inference is precise. It takes account of whether each operand is a bounded value, a literal,
or a `Double` about which nothing is known, and it composes through a whole expression:

```scala fuel
parse
##
val x: 0.0 ~ 1.0 = 0.2
val y: -1.0 ~ 1.0 = 0.2
val z: 1e3 ~ 1e8 = 10000

val combined: -2.0e8 ~ 3.0e8 = (x + y*3.0)*z
```

Ordinary unbounded `Double`s are everywhere, though, and a program must be able to bring one
into this world from outside it. `force` asserts a bound which the compiler cannot check. That
makes it the one operation here which can be wrong, and so the one to use deliberately.
