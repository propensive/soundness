All terms and types are defined in the `abacist` package,
```scala
import abacist.*
```
and build upon definitions in [Quantitative](https://github.com/propensive/quantitative/):
```scala
import quantitative.*
```

### Discrete Cascading Units

Prior to the adoption of the metric system for quantities of mass and length, other systems based
on non-decimal (but exact) multiples of other units were widely used. These are commonly called
"the Imperial System", particularly for lengths, and for masses, "Avoirdupois", but actually
represent several different systems (often with units of the same name representing different
physical amounts, confusingly) adopted in various jurisdictions with varying degrees of
officiality.

Abacist can accommodate all such systems through a single _type_ which defines a cascade of
units of the same dimension, in a tuple. For example, one variant of the Imperial System measuring
human heights could be defined as,
```scala
type ImperialHeight = (Feet[1], Inches[1])
```
or for distances,
```scala
type ImperialDistance = (Miles[1], Yards[1], Inches[1])
```
that is, a number of miles, yards and inches, represented as a `Tuple` of these units' types
(each raised to the power `1`). Another example for mass is,
```scala
type Avoirdupois = (Hundredweights[1], Stones[1], Pounds[1], Ounces[1], Drams[1])
```
or alternatively:
```scala
type SimpleAvoirdupois = (Pounds[1], Ounces[1])
```

Each type, a tuple of subtypes of `Measure`, statically represents a system of discrete cascading units,
provided that,
- each element of the tuple has the same dimensionality (i.e. represents the same sort of physical quantity)
- the elements are ordered by decreasing magnitude
and furthermore, for most useful operations, that contextual `Ratio` instances exist between each unit and
the principal unit for their common dimension.

With a valid definition, such as one of the above, we can represent values in its units, called a `Count`
(because it's a count of integer multiples of each of the units).

To construct a new `Count`, simply call its factory method with the appropriate tuple type, and as many
integer arguments as necessary. The rightmost `Int` argument will be interpreted as the multiple of the
rightmost unit in the tuple, and additional arguments will represent (right-to-left) multiples of units of
increasing magnitude. For example, `Count[ImperialDistance](180, 24)` represents, "180 yards and 24 inches",
while, `Count[ImperialDistance](1, 180, 24)` represents, "1 mile, 180 yards and 24 inches".

Individual units from a `Count` may be extracted.

`Count`s of identical units may be added and subtracted, and multiplied and divided by numbers (but not
other quantities). They may be converted to `Quantity`s with the `in` method, much as a `Quantity` can
be converted, or constructed from a `Quantity` by applying in to the factory method, e.g.
```scala
Count[Avoirdupois](18*Kilo(Gram))
```

### Representation

A `Count` is an opaque type alias for a `Long`, meaning that operations involving `Count`s do not involve
any heap objects. The bits of the `Long` are organized so that a fixed range of the 64 bits available in
a `Long` will exclusively represent any possible value for that unit. Since the number of different integer
values that can be represent by such a bit-range will always be a power of two, there may be some unused
values. These bit-ranges are organized right-to-left in order of increasing magnitude, much like the bits
in any other integer.

The leftmost bit is a sign bit. The unit of greatest magnitude (which appears first in the tuple) will use
all the remaining bits to represent the greatest possible range of values.

For example, an Imperial distance measurement in miles, yards and inches, represented by the type,
`(Miles[1], Yards[1], Inches[1])`, would use: 6 bits for the number of inches, since there are 36 inches in
a yard, and 64 (or 2⁶) is the smallest power of two large enough to represent every integer in the range
0-35; 11 bits for the number of yards, since there are 1760 yards in a mile, and 2¹¹ (which is 2048) can
accommodate any value in the range 0-1759; 1 bit for the sign, and 46 bits for the number of miles, since
this is the number that remains.

This allows distances of up to 7×10¹³ miles to be represented.

The approach of packing bits into a `Long` provides very fast access of each unit's value, since in may be
accessed with just a binary `AND` operation and a right-shift.


