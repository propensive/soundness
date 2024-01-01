All Quantitative terms and types are defined in the `quantitative` package:
```scala
import quantitative.*
```

### `Quantity` types

Physical quantities can be represented by different `Quantity` types, with an appropriate parameter that encodes
the value's units. We can create a quantity by multiplying an existing `Double` (or any numeric type) by some
unit value, such as `Metre` or `Joule`—which are just `Quantity` values equal to `1.0` of the appropriate unit.
For example:
```amok
##
val distance = 58.3*Metre
```

The types of these values will be inferred. The value `distance` will get the type `Quantity[Metres[1]]`, since
its value is a number of metres (raised to the power `1`).

In general, types representing units are written in the plural (for example, `Metres`, `Feet`, `Candelas`), with
a bias for distinction when the singular name is often used in the plural; for example, the type is `Kelvins`
even though "Kelvins" and "Kelvin" are both commonly used for plural values. Unit instances are always named in
the singular.

We can compute an `area` value by squaring the distance,
```amok
##
val area = distance*distance
```
which should have units of square metres (`m²`). Quantitative represents this as the type, `Quantity[Metres[2]]`; the
`2` singleton literal value represents the metres being squared. Likewise, a volume would have the parameter
`Metres[3]`.

### Representation and displaying

Each quantity, regardless of its units, is represented in the JVM as a `Double` using an opaque type
alias.

The precise types, representing units, are known statically, but are erased by runtime. Hence, all
dimensionality checking takes place at compiletime, after which, operations on `Quantity`s will be
operations on `Double`s—and will achieve similar performance.

The raw `Double` value of a `Quantity` can always be obtained with `Quantity#value`

Due to this representation, the `toString` method on `Quantity`s is the same as `Double`s `toString`,
so the `toString` representations will show just the raw numerical value, without any units. In
general, `toString` should not be used. A `gossamer.Show` instance is provided to produce
human-readable `Text` values, so calling `show` on a `Quantity` will produce much better output.

### Derived units

We can also define:
```amok
##
val energy = Joule*28000
```

The type of the `energy` value _could_ have been defined as `Quantity[Joule[1]]`, but 1 J is equivalent to 1
kg⋅m²⋅s¯², and it's more useful for the type to reflect a product of thes more basic units (even though we
can still use the `Joule` value to construct it).

Metres, seconds and kilograms are all SI base units. Kilograms are a little different, since _nominally_, a
kilogram is one thousand grams (while a gram is _not_ an SI base unit), and this has a small implication on
the way we construct such units.

Quantitative provides general syntax for metric naming conventions, allowing prefixes such as `Nano` or `Mega`
to be applied to existing unit values to specify the appropriate scale to the value. Hence, a kilogram value
is written, `Kilo(Gram)`. But since the SI base unit is the kilogram, this and any other multiple of `Gram`,
such as `Micro(Gram)`, will use the type `Kilogram`, or more precisely, `Kilogram[1]`.

Therefore, the type of `energy` is `Quantity[Grams[1] & Metres[2] & Second[-2]]`, using a combination of three
base units raised to different powers. They are combined into an intersection type with the `&` type operator,
which provides the useful property that the order of the intersection is unimportant;
`Second[-2] & Metres[2] & Grams[1]` is an _identical_ type, much as kg m²s¯² and s¯²m²kg are identical
units.

Just as we could construct an area by multiplying two lengths, we can compute a new value with appropriate units
by combining, say, `area` and `energy`,
```amok
##
val volume = distance*distance*distance
val energyDensity = energy/volume
```
and its type will be inferred with the parameter `Kilogram[1] & Metres[-1] & Second[-2]`.

If we had instead calculated `energy/area`, whose units do not include metres, the type parameter would be just
`Kilogram[1] & Second[-2]`; the redundant `Metres[0]` would be automatically removed from the conjunction.

We can go further. For example, the "SUVAT" equations of motion can be safely implemented as methods, and
their dimensionality will be checked at compiletime. For example, the equation, `s = ut + ½at²` for
calculating a distance (`s`) from an initial velocity (`u`), acceleration (`a`) and time (`t`) can be
implemented using Quantitative `Quantity`s with:
```amok
##
def s(u: Quantity[Metres[1] & Seconds[-1]], t: Quantity[Seconds[1]], a: Quantity[Metres[1] & Seconds[-2]])
    : Quantity[Metres[1]] =
  u*t + 0.5*a*t*t
```

While the method arguments have more complex types, the expression, `u*t + 0.5*a*t*t`, is checked for
dimensional consistency. If we had written `t + 0.5*a*t*t` or `u*t + 0.5*a*a*t` instead, these would
have produced errors at compiletime.

### Combining mixed units

Kilograms, metres and seconds are units of in the mass, length and time dimensions, which are never
interchangeable. Yet we sometimes need to work with different units of the same dimension, such as
feet, metres, yards and miles as different (but interchangeable) units of length; or kilograms and
pounds, as units of mass.

Each type representing units, such as `Metres` or `Kilograms`, must be a subtype of the `Units` type,
which is parameterized with its power (with a singleton literal integer) and a _dimension_, i.e. another type
representing the nature of the measurement. For `Metres` the dimension is `Length`; for `Kilograms`'s it is
`Mass`; `Candela`'s is `Luminosity`.

`Metres[PowerType]` is a subtype of `Units[PowerType, Length]`, where `PowerType` must be a singleton
integer type. More specifically, `Metres[1]` would be a subtype of `Units[1, Length]`.

Note that there are no special dimensions for compound units, like energy, since the time, length and mass
components of the units of an energy quantity will be associated with the `Second`, `Metres` and `Kilogram`
types respectively.

Encoding the dimension in the type makes it possible to freely mix different units of the same dimension.

It is possible to create new length or mass units, such as `Inch` or `Pound`, which share the `Length` or `Mass`
dimensions. This allows them to be considered equivalent in some calculations, if a conversion coefficient is
available.

Quantitative defines a variety of imperial measurements, and will automatically convert units of the same
dimension to the same units in multiplications and divisions. For example,
```amok
##
val width = 0.3*Metre
val height = 5*Inch
val area2 = width*height
```
will infer the type `Quantity[Metres[2]]` for `area`.

However, the conversion of one of the units from inches to metres was necessary only to avoid a mixture of
`Inches` and `Metres` in the resultant type, but the expression, `height*height` would produce a value with the
units, `Inches[2]`, performing no unnecessary conversions.

### Conversions

#### Addition & subtraction

Addition and subtraction are possible between quantities which share the same dimension.

We can safely add an inch and a metre,
```amok
##
val length = 1*Inch + 1*Metre
```
but we can't subtract a second from a litre:
```amok
##
val nonsense = Litre - Second // will not compile
```

For the addition and subtraction of values with mixed units, the question arises of which units the result
should take. Quantitative will use the _principal unit_ for the dimension, which is determined by the presence
of a unique contextual `PrincipalUnit` instance, parameterized on `Dimension` and `Units` types.

In general, if the units for the same dimension don't match between the operands, then the principal unit
will be used for both. This may mean that adding a foot to a mile produces a result measured in metres,
but a new `PrincipalUnit[Length, Miles[1]]()` contextual value could always be provided in-scope,
which will take precedence over the `PrincipalUnit[Length, Metres[1]]` in scope.

Some additional contextual values may be required, though. See [below](#conversion-ratios) for more
information on conversions.

#### Inequality Comparisons

Likewise, we can compare units in like or mixed values with the four standard inequality operators
(`<`, `>`, `<=`, `>=`). These will return `true` or `false` if the operands have the same dimension,
even if they have different units, for example,
```amok
##
8*Foot < 4*Metre // returns true
```
while incompatible units will result in a compile error.

#### Equality

Equality between different `Quantity` values should be treated with care, since all such values are
represented as `Double`s at runtime, and the JVM's standard equality will not take units into
account. So, by default, `3*Foot == 3*Metre` will yield `true`, since `3.0 == 3.0`!

This is highly undesirable, but luckily there's a solution:
```amok
##
import language.strictEquality
```

This turns on Scala's strict-equality feature, which forbids comparisons between any two types unless
a corresponding `CanEqual[LeftOperandType, RightOperandType]` exists in scope for the appropriate
operand types. Quantitative provides just such an instance for `Quantity` instances with the same units.

The runtime equality check, however, is performed in exactly the same way: by comparing two `Double`s.
That is absolutely fine if we know the units are identical, but it does not allow equality comparisons
between `Quantity`s of the same dimension and different units.

For this, there are two possibilities:
- convert one of the `Quantity`s to the units of the other
- test `left <= right && left >= right`, which will only be true if `left` equals `right`

#### Conversion ratios

In order to automatically convert between two units, Quantitative needs to know the ratio between them.
This is provided with a contextual `Ratio` value for the appropriate pair of units: one with the
power `1` and the other with the power `-1`. The rate of conversion should be specified as a singleton
literal `Double` as the second parameter. The `given` may be `erased`, if using Scala's erased definitions.

For example,
```amok
##
erased given Ratio[Kilograms[1] & Tons[-1], 1016.0469088]
```
which specifies that there are about 1016 kilograms in a ton, and will be used if Quantitative ever needs
to convert between kilograms and tons.

By making the conversion rate a _type_ (a singleton literal, specifically), its value is available at
compiletime, even while the `given` is `erased`. This has the further advantage that any calculations on
`Quantity`s which need to use the conversion ratio in a calculation involving other constants will use
constant folding to automatically perform arithmetic operations on constants at compiletime, saving the
performance cost of doing these at runtime.

### Explicit Conversions

To convert a quantity to different units, we can use the `in` method, passing it an _unapplied_ units type
constructor, such as `Hour` or `Furlong`. The significance of the type being "unapplied" is that a units type
constructor is typically _applied_ to an integer singleton type, such as `Metres[2]` representing square
metres. Each dimension in a quantity must have the same units, no matter what its power, so it doesn't make
sense to specify that power when converting.

So, `(10*Metre).in[Yards]`, would create a value representing approximately 10.94 yards, while,
`(3*Foot * 1*Metre * 0.4*Centi(Metre)).in[Inches]`, would calculate a volume in cubic inches.

If a quantity includes units in multiple dimensions, these can be converted in steps, for example,
```amok
##
val distance2 = 100*Metre
val time = 9.8*Second
val speed = distance2/time
val mph = speed.in[Miles].in[Hours]
```

### SI definitions

There are seven SI base dimensions, with corresponding units, which are defined by Quantitative:
 - `Length` with units type, `Metres`, and unit value, `Metre`
 - `Mass` with units, `Kilograms`, and unit value, `Kilogram`
 - `Time` with units, `Seconds`, and unit value, `Second`
 - `Current` with units, `Amperes`, and unit value, `Ampere`
 - `Luminosity` with units, `Candelas`, and unit value, `Candela`
 - `AmountOfSubstance` with units, `Moles`, and unit value, `Mole`
 - `Temperature` with units, `Kelvins`, and unit value, `Kelvin`

As well as these, the following SI derived unit values are defined in terms of the base units:
 - `Hertz`, for measuring frequency, as one per second
 - `Newton`, for measuring force, as one metre-kilogram per square second
 - `Pascal`, for measuring pressure, as one Newton per square metre
 - `Joule`, for measuring energy, as one Newton-metre
 - `Watt`, for measuring power, as one Joule per second
 - `Coulomb`, for measuring electric charge, as one second-Ampere
 - `Volt`, for measuring electric potential, as one Watt per Ampere
 - `Farad`, for measuring electrical capacitance, as one Coulomb per Volt
 - `Ohm`, for measuring electrical resistance, as one Volt per Ampere
 - `Siemens`, for measuring electrical conductance, as one Ampere per Volt
 - `Weber`, for measuring magnetic flux, as one Volt-second
 - `Tesla`, for measuring magnetic flux density, as one Weber per square metre
 - `Henry`, for measuring electrical inductance, as one Weber per Ampere
 - `Lux`, for measuring illuminance, as one Candela per square metre
 - `Becquerel`, for measuring radioactivity, as one per second
 - `Gray`, for measuring ionizing radiation dose, as one Joule per kilogram
 - `Sievert`, for measuring stochastic health risk of ionizing radiation, as one Joule per kilogram
 - `Katal`, for measuring catalytic activity, as one mole per second

## Defining your own units

Quantitative provides implementations of a variety of useful (and some less useful) units from the
metric system, CGS and imperial. It's also very easy to define your own units.

Imagine we wanted to implement the FLOPS unit, for measuring the floating-point performance of a
CPU: floating-point instructions per second.

Trivially, we could create a value,
```amok
##
val SimpleFlop = 1.0/Second
```
and use it in equations such as, `1000000*SimpleFlop * Minute` to yield an absolute number representing
the number of floating-point instructions that could (theoretically) be calculated in one minute by
a one-megaFLOP CPU.

But this definition is just a value, not a unit. We can tweak the definition slightly to,
```amok
##
val Flop = MetricUnit(1.0/Second)
```
and it becomes possible to use metric prefixes on the value. So we could rewrite the above expression
as, `Mega(Flop) * Minute`.

### Introducing new dimensions

The result is just a `Double`, though, which is a little unsatisfactory, since it represents
something more specific: a number of instructions. To do better, we need to introduce a new
`Dimension`, distinct from length, mass and other dimensions, and representing a CPU's
performance,
```amok
##
trait CpuPerformance extends Dimension
```
and create a `Flops` type corresponding to this dimension:
```amok
##
import rudiments.*
trait Flops[PowerType <: Nat] extends Units[PowerType, CpuPerformance]
val Flop: MetricUnit[Flops[1]] = MetricUnit(1)
```

The type parameter, `PowerType`, is a necessary part of this definition, and must be constrained on
the `Nat` type defined in [Rudiments](https://github.com/propensive/rudiments/), which is just an
alias for `Int & Singleton`. If you are using Scala's erased definitions, both `CpuPerformance` and
`Flops` may be made `erased trait`s to reduce the bytecode size slightly.

With these definitions, we can now write `Mega(Flop) * Minute` to get a result with the dimensions
"FLOPS-seconds", represented by the type, `Quantity[Flops[1] & Seconds[1]]`.

If we want to show the FLOPS value as `Text`, a symbolic name is required. This can be specified
with a contextual instance of `UnitName[Flops[1]]`,
```amok
##
given UnitName[Flops[1]] = () => t"FLOPS"
```
which will allow `show` to be called on a quantity involving FLOPs.

### Describing physical quantities

English provides many names for physical quantities, including the familiar base dimensions of
_length_, _mass_, _time_ and so on, as well as combinations of these, such as _velocity_,
_acceleration_ and _electrical resistance_.

Definitions of names for many of these physical quantities are already defined, and will appear in
error messages when a mismatch occurs.
```
scala> Metre/Second + Metre/(Second*Second)

quantitative: the left operand represents velocity, but the right operand represents acceleration;
these are incompatible physical quantities
```
It is also possible to define your own, for example, here is the definition for "force":
```amok
##
erased given DimensionName[Units[1, Mass] & Units[1, Length] & Units[-2, Time], "force"] = erasedValue
```

The singleton type `"force"` is the provided name for any units corresponding to the dimensions,
mass×length×time¯².

### Substituting simplified units

While the SI base units can be used to describe the units of most physical quantities, there often
exist simpler forms of their units. For example, the Joule, `J`, is equal to `kg⋅m²⋅s¯²`, and is
much easier to write.

By default, Quantitative will use the latter form, but it is possible to define alternative
representations of units where these exist, and Quantitative will use these whenever a quantity is
displayed. A contextual value can be defined, such as the following,
```amok
##
import gossamer.t

given SubstituteUnits[Kilograms[1] & Metres[2] & Seconds[-2]](t"J")
```
and then a value such as, `2.8*Kilo(Joule)` will be rendered as `2800 J` instead of `2800 kg⋅m²⋅s¯²`.

Note that this only applies if the quantity's units exactly match the type parameter of
`SubstituteUnits`, and units such as Joule-seconds would still be displayed as `kg⋅m²⋅s¯¹`.

## Discrete Cascading Units

Prior to the adoption of the metric system for quantities of mass and length, other systems based
on non-decimal (but exact) multiples of other units were widely used. These are commonly called
"the Imperial System", particularly for lengths, and for masses, "Avoirdupois", but actually
represent several different systems (often with units of the same name representing different
physical amounts, confusingly) adopted in various jurisdictions with varying degrees of
officiality.

Quantitative can accommodate all such systems through a single _type_ which defines a cascade of
units of the same dimension, in a tuple. For example, one variant of the Imperial System measuring
human heights could be defined as,
```amok
##
type ImperialHeight = (Feet[1], Inches[1])
```
or for distances,
```amok
##
type ImperialDistance = (Miles[1], Yards[1], Inches[1])
```
that is, a number of miles, yards and inches, represented as a `Tuple` of these units' types
(each raised to the power `1`). Another example for mass is,
```amok
##
type Avoirdupois = (Hundredweights[1], Stones[1], Pounds[1], Ounces[1], Drams[1])
```
or alternatively:
```amok
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

To construct a new `Count`, simply call its factory method with the appropriate tuple type, and as many
integer arguments as necessary. The rightmost `Int` argument will be interpreted as the multiple of the
rightmost unit in the tuple, and additional arguments will represent (right-to-left) multiples of units of
increasing magnitude. For example, `Count[ImperialDistance](180, 24)` represents, "180 yards and 24 inches",
while, `Count[ImperialDistance](1, 180, 24)` represents, "1 mile, 180 yards and 24 inches".

Individual units from a `Count` may be extracted.

`Count`s of identical units may be added and subtracted, and multiplied and divided by numbers (but not
other quantities). They may be converted to `Quantity`s with the `in` method, much as a `Quantity` can
be converted, or constructed from a `Quantity` by applying in to the factory method, e.g.
```amok
##
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



