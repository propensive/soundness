All terms and types are defined in the `abacist` package, and are exported to
the `soundness` package, and build upon
definitions in [Quantitative](https://github.com/propensive/quantitative/). You
can import from either:
```amok
syntax scala
transform
  before   selective imports
  after    universal soundness import
  replace  abacist.*, quantitative.*  soundness.*
##
import abacist.*, quantitative.*
```

### Discrete Cascading Units

Prior to the adoption of the metric system for quantities of mass and length, other systems based
on non-decimal (but exact) multiples of other units were widely used. These are commonly called
"the Imperial System", particularly for lengths, and for masses, "Avoirdupois", but actually
represent several different systems (often with units of the same name representing different
physical amounts, confusingly) adopted in various jurisdictions with varying degrees of
officiality.

Abacist can accommodate all such systems through a single _type_ which defines a cascade of
units (of the same dimension), in a tuple. The unit types are defined in
Quantitative, and Abacist makes it
possible to use them in discrete multiples.

For example, one variant of the Imperial System measuring
human heights could be defined as,
```amok
syntax scala
##
type ImperialHeight = (Feet[1], Inches[1])
```
or for longer distances,
```amok
syntax scala
##
type ImperialDistance = (Miles[1], Yards[1], Inches[1])
```
that is, a number of miles, yards and inches, represented as a `Tuple` of these units' types
(each raised to the power `1`). Another example for mass is,
```amok
syntax scala
##
type Avoirdupois = (Hundredweights[1], Stones[1], Pounds[1], Ounces[1], Drams[1])
```
or alternatively:
```amok
syntax scala
##
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

### Construction

To construct a new `Count`, simply call its factory method with the appropriate tuple type, and as many
integer arguments as necessary. The rightmost `Int` argument will be interpreted as the multiple of the
rightmost unit in the tuple, and additional arguments will represent (right-to-left) multiples of units of
increasing magnitude. For example, `Count[ImperialDistance](180, 24)` represents, "180 yards and 24 inches",
while, `Count[ImperialDistance](1, 180, 24)` represents, "1 mile, 180 yards and 24 inches".

### Extraction

Individual units from a `Count` may be extracted by applying the units value
to extract, without its dimension, to the `Count` value, like so:
```amok
syntax scala
##
type Height = (Feet[1], Inches[1])
val height: Count[Height] = Count(6, 4)
val feet = height[Feet]
val inches = height[Inches]
```

The value `feet` will be set to `6`, and the value `inches` will be `4`.

`Count`s of identical units may be added and subtracted, and multiplied and divided by numbers, but not
by other quantities. They may be converted to `Quantity`s with the `quantity` method, much as a `Quantity` can
be converted, or constructed from a `Quantity` by calling `count` on the
`Quantity`, e.g.
```amok
syntax scala
##
(18*Kilo(Gram)).count[Avoirdupois]
```

### Rounding

Note that in many cases, the discrete units of a `Count` will not be able to
precisely represent a `Quantity`. A `Quantity` will be rounded to the nearest
whole `Count` value. Converting back to a `Quantity` will include this
rounding error.

For example, note how the error changes when more precise `Count` units are
used:
```amok
syntax scala
transform
  before  Imprecise height
  after   Precise height
  replace  Inches[1]  Inches[1], Points[1]
  replace  0.00460  0.0000139
##
type Height = (Feet[1], Inches[1])
val height: Quantity[Metres[1]] = Quantity(1.3)
val error = height - height.count[Height].quantity

assert(error == 0.00460)
```


### Underlying Representation

A `Count` is an opaque type alias for a `Long`, meaning that operations involving `Count`s do not involve
any heap objects. The underlying value of a count represents an integer
multiple of the smallest unit in the cascade. For example, a length of
`1ft 3in` would be stored as `15`, being the sum of the 12 inches in one foot,
plus 3 inches.
