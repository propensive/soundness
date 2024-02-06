### Philosophy

_Hypotenuse_ adopts a philosophy of rigor and regularity to numerical
operations in Scala. This includes consistent naming of types, methods and
operators; accurate distinction in the typesystem between types with different
purposes; and, accurate representation of exceptional numeric operations.

These enhancements are all implemented with minimal impact on performance,
using opaque type aliases and inlining.

#### Consistent naming

In addition to 1-bit `Boolean`s, the JVM provides 8-bit, 16-bit, 32-bit and
64-bit integers, called `Byte`, `Short`, `Int` and `Long`. Hypotenuse provides
these types with new names: `I8`, `I16`, `I32` and `I64`.

Additionally, the floating-point types, `Float` and `Double`, are provided as,
`F32` and `F64`.

In itself, this provides only marginally better clarity, but it sets up a
naming scheme for other related types.

#### Distinct types

For each of the `I` types, representing a two's-complement signed integer, a
`U` type is also provided, representing an unsigned, non-negative integer with
a larger maximum value: `U8`, `U16`, `U32` and `U64`.

These types all define the standard arithmetic and related operations, but do
not define any bitwise operations like shifts, `AND` or `OR`. The collection of
bits in a number is either intended for arithmetic or interpretation as a raw
set of bits. So it is not logical for a single type to provide both bitwise
_and_ arithmetic operations. Thus, the types `B8`, `B16`, `B32` and `B64` are
provided for bitwise operations.

Conversions between these different types are trivially available with methods
such as `b8` and `u32` (which will compile to no-ops in bytecode, if possible).

#### Strict Exceptions

Many arithmetic operations on numeric types are inherently _unsafe_, yet
unlikely to be problematic for the majority of use cases.

For example, adding two positive 32-bit integers may result in overflow, where
each operand is small enough to be represented in 32 bits, but their sum is too
large. Unless detected, the result will be a negative number, which might cause
problems in algorithms which rely, for example, on the invariant that the sum
of two positive numbers is also positive.

But a 32-bit integer is so large that many applications would use only small
integers, and will not come remotely close to the limits where overflow can
occur. In these cases, any checks to detect overflow would be redundant.

Other operations, such as division by an integer, may fail if the divisor is
zero. But it may also be known, from context, that the divisor is nonzero, and
that the division operation is safe. On the JVM, a division by zero is an
unchecked exception, so the operation is _partial_.

Therefore, it is sometimes appropriate to explicitly handle exceptional cases,
and on other occasions, unnecessary and therefore not worth the effort.
Hypotenuse caters for both scenarios, with additional safety checks controlled
by an import. For example, a division by zero will raise a `DivisionError` if,
```scala
import arithmeticOptions.division.checked
```
is in scope, and the `DivisionError` must be handled (or explicitly ignored).

Enabling this and other checks can ensure that all partial or undesirable
arithmetic operations will be checked, and the programmer will be forced to
handle each exceptional case.

### Inequalities

Hypotenuse provides more elegant syntax for writing inequalities with both
upper and lower bounds.

Traditionally, a bounds predicate might be written,
```scala
lowerBound <= x && x < upperBound
```
but Hypotenuse makes it possible to write this more intuitively as:
```scala
lowerBound <= x < upperBound
```

This is semantically identical to the original, and compiles into it via a
straightforward rewrite.


