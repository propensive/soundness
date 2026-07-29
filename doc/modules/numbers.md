## Numbers

### About

Soundness offers a precise, regular set of numeric types. Each names what it is — `U8` to
`U64` for unsigned integers, `S8` to `S64` for signed, `F32` and `F64` for floating-point,
and `B8` to `B64` for raw bit patterns — so the width and the interpretation of a number are
part of its type. Arithmetic can be checked for overflow, division by zero, and `NaN`, and
the checking is opt-in, paid for only where it is wanted.

Bounded types go further: a number can carry its permitted range in its type, so a value
outside `[0, 1]` is a compile error where it is written, and arithmetic on bounded numbers
works out the range of the result. A range in the type is the plainest way to make an
[impossible state](../philosophy/impossible-states.md) unrepresentable.

### On numbers

Scala inherits the JVM's numbers, and they are a grab-bag. Every integer is signed; addition
overflows in silence and wraps to a negative; integer division by zero throws while floating
division yields `NaN`; and the bitwise operators sit on the same types as the arithmetic ones,
so a shift and a multiply look alike. The type says only how many bits a number has, never how
those bits are meant to be read.

Soundness separates the concerns. A number is *either* arithmetic — a `U*` or `S*`, with `+`,
`-`, `*`, `/`, and no bitwise operators — *or* a bit pattern, a `B*` with shifts and masks and
no arithmetic. Conversions between them are explicit and free. The dangerous operations are
checked only when a checking given is in scope, so safety costs nothing until asked for. The
types are opaque over the JVM primitives, so none of this survives to runtime. Everything comes
from the `soundness` package, with generic number literals enabled so a literal takes whichever
numeric type is expected:

```scala
import soundness.*
import language.experimental.genericNumberLiterals
```

### Numeric types

A literal takes the type it is assigned to, and conversions between types are written as short
methods:

```scala
val count: U64 = 123
val small: U8 = count.u8     // narrower, explicit
val signed: S32 = 42
```

The name carries the meaning: `U*` is unsigned, `S*` is two's-complement signed, `F*` is
floating-point, and the number is the bit width. An unsigned value and a signed one are
different types, and mixing them is a deliberate conversion rather than an accident.

### Checked arithmetic

By default arithmetic behaves as the hardware does, wrapping on overflow. Importing the
checked given changes the result type of `+`, `-` and `*` to one that can raise
`OverflowError`, so an overflow becomes a handled failure rather than a silent wrap:

```scala
import arithmeticOptions.overflow.checked
import strategies.throwUnsafely

val big: S32 = 2000000000
big + big   // raises OverflowError rather than wrapping negative
```

Division by zero is checked the same way, by importing `arithmeticOptions.division.checked`,
after which `/` may raise a `DivisionError`. Where the check is not imported, the operations
keep their bare machine behaviour and cost nothing.

The two checks are independent, so a program that must not wrap but is content to trust its
divisors imports only the first. Both are `inline`, and the unchecked forms compile to the same
instructions the hardware would have executed anyway — the choice costs nothing where it is not
taken.

Overflow detection is exact rather than approximate: it distinguishes the cases where a signed
addition genuinely overflows from those where it merely crosses zero, so a sum of the most
negative value with itself is caught while ordinary negative arithmetic is not disturbed.

### Bit manipulation

The `B*` types are for treating a number as a set of bits. They carry the shifts, rotations
and masks, and render in the usual radices, but no arithmetic:

```scala
val flags: B32 = 0xf0
flags << 2          // shift left
flags & 0x30        // mask
flags.hex           // t"000000f0"
```

`<<<` and `>>>` rotate rather than shift, `~` inverts, and individual bits are read and
written with `bit`, `set`, `clear` and `flip`. Bits are addressed by
[ordinal](https://en.wikipedia.org/wiki/Ordinal_number) — `Prim` is the first, `Sec` the second —
so a bit index cannot be confused with the off-by-one convention of whichever specification is
being implemented:

```scala
B64(Data(0, 0, 0, 0, 0, 0, 0, 6)).bit(Sec)   // true
```

Rendering a bit pattern pads to the type's width rather than dropping leading zeros, so the
number of characters says which type produced it:

```scala
(-1: Byte).hex       // t"ff", two characters
(-1: Byte).binary    // eight characters
```

### Mathematics

The mathematical operations that Java scatters across `java.lang.Math` are extension methods,
consistent across the numeric types: `abs`, `ceiling`, `floor`, `round`, `signum`, and the
rest. `**` raises to a power and `%%` is a floor-modulus that, unlike `%`, never returns a
negative remainder:

```scala
7.0 %% 3.0     // 1.0
-7.0 %% 3.0    // 2.0
2.0 ** 10.0    // 1024.0
```

The trigonometric and logarithmic functions are available as plain functions — `cos`, `sin`,
`exp`, `ln`, `log10` — alongside the constants `π`, `euler` and `φ`.

`**` widens rather than truncating: raising a `Short` to a power that exceeds a `Short`'s range
gives the right answer rather than a wrapped one, and a fractional exponent gives a fractional
result:

```scala
(200: Short) ** 2.0    // 40000.0, not a truncated Short
(1000: Short) ** 1.5
```

The collection statistics are extension methods too. `median` finds the middle value without
sorting the whole collection — a selection algorithm rather than a sort — and averages the two
middle values where the count is even:

```scala
Iterable[Double](7, 25, 1, 24, 2, 3, 23, 4, 22, 5, 21).median   // 7.0
```

### Comparisons

Comparisons chain as they do in mathematics, so a value can be tested against two bounds at
once, and `min`, `max` and the collection reductions `minimum` and `maximum` read directly:

```scala
List(1.1, 1.2, 1.3, 1.4, 1.5).filter(1.2 < _ < 1.4)   // List(1.3)
```

### Arbitrary precision

Where a value must be exact and no fixed width will do — a monetary total, a coordinate in a
document, a figure read from a file that promised nothing about its size — a `Decimal` holds a
sign, an arbitrary magnitude and a decimal scale:

```scala
Decimal(1234567890123L)
Decimal(-1234567, 4)      // -123.4567
Decimal(0.1)              // raises a DecimalError if the double is not exact
Decimal.parse(t"-12.34e+2")
```

A decimal literal written in source becomes a `Decimal` directly where one is expected, and
`text` renders it back, so the round trip is exact:

```scala
Decimal(-1234567, 4).text   // t"-123.4567"
```

Values are canonical: trailing zeros are absorbed into the scale as a value is constructed, and
zero has one representation, so two numerically equal decimals are the same value. There is no
counterpart of `BigDecimal`'s trap where `equals` and `compareTo` disagree.

Division cannot always be exact, so it says what it wants: a scale and a rounding mode, given
rather than assumed.

```scala
left.divide(right, scale = 10, Decimal.Rounding.HalfEven)
```

The implementation is pure Scala rather than a wrapper over the JVM's `BigDecimal`, so decimals
work on every platform — and nothing in it depends on 64-bit-native arithmetic, which matters
where `Long` is emulated.

### Bounded numbers

A number can carry its permitted range in its type, written with `~`. A literal outside the
range is a compile error, caught where it is written:

```scala
val portion: 0.0 ~ 1.0 = 0.2
val tooMuch: 0.0 ~ 1.0 = 2.0   // does not compile: outside [0.0, 1.0]
```

Arithmetic on bounded numbers computes the range of the result from the ranges of its inputs,
so doubling a value in `[-1, 1]` yields one known to lie in `[-2, 2]`:

```scala
val doubled: -2.0 ~ 2.0 = portion*2.0
```

The bound travels with the value through a calculation, and a step that could break it does
not compile — the same discipline as [physical quantities](quantities.md), applied to plain
ranges.
