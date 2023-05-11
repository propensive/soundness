[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/quantify/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/quantify/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Quantify

____

When working with physical quantities, such as lengths, masses or temperatures,
it can be easy to mix up quantities with different units, especially if we
represent all quantities with `Double`sâ€”which is often necessary for
performance.

Quantify represents physical quantities with a generic `Quantity` type, an
opaque alias of `Double`, which statically encodes the value's units in its
type parameter. This provides all the desirable homogeneity constraints when
combining quantities, with the performance of `Double`s, and without
compromising on intuitive syntax for arithmetic operations.

Quantities can be multiplied and divided arbitrarily, with new units computed
by the compiler, and checked for consistency in additions and subtractions.

## Features

- statically checks that physical quantities have consistent units by making them distinct types
- `Quantity` values encode the (nonzero) power of each unit in their type
- all `Quantity`s are opaque aliases of `Double`, so are stored and processed efficiently
- enforces homogeneous units for all additions and subtractions
- calculates resultant units for multiplications and divisions
- unitless values are seamlessly represented by `Double`s
- distinguishes between _dimensions_ (such as length or mass) and _units_ (such as metres or feet)
- different units of the same dimension may be combined
- convertions between different units of the same dimension
- requires no new or special syntax
- fully extensible: new units, dimensions and conversions can be introduced
- provides implementations of base and most derived SI units

## Availability

Quantify has not yet been published as a binary. It is currently waiting for the
final release of Scala 3.3.

## Getting Started

### `Quantity` types

Physical quantities can be represented by different `Quantity` types, with an appropriate parameter that encodes
the value's units. We can create a quantity by multiplying an existing `Double` (or any numeric type) by some
unit value, such as `Metre` or `Joule`â€”which are just `Quantity` values equal to `1.0` of the appropriate unit.
For example:
```scala
val distance = 58.3*Metre
```

The types of these values will be inferred. The value `distance` will get the type `Quantity[Metres[1]]`, since
its value is a number of metres (raised to the power `1`).

In general, types representing units are written in the plural (for example, `Metres`, `Feet`, `Candelas`), with
a bias for distinction when the singular name is often used in the plural; for example, the type is `Kelvins`
even though both "Kelvins" and "Kelvin" are both commonly used. Unit values are alwas named in the singular.

We can compute an `area` value by squaring the distance,
```scala
val area = distance*distance
```
which should have units of square metres (`m
²`). Quantify represents this as the type, `Quantity[Metres[2]]`; the
`2` singleton literal value represents the metres being squared. Likewise, a volume would have the parameter
`Metres[3]`.

### Representation and displaying

Each quantity, regardless of its units, is represented in the JVM as a `Double` using an opaque type
alias.

The precise types, representing units, are known statically, but are erased by runtime. Hence, all
dimensionality checking takes place at compiletime, after which, operations on `Quantity`s will be
operations on `Double`sâ€”and will achieve similar performance.

The raw `Double` value of a `Quantity` can always be obtained with `Quantity#value`

Due to this representation, the `toString` method on `Quantity`s is the same as `Double`s `toString`,
so the `toString` representations will show just the raw numerical value, without any units. In
general, `toString` should not be used. A `gossamer.Show` instance is provided to produce
human-readable `Text` values, so calling `show` on a `Quantity` will produce much better output.

### Derived units

We can also define:
```scala
val energy = Joule*28000
```

The type of the `energy` value _could_ have been defined as `Quantity[Joule[1]]`, but 1 J is equivalent to 1
kgâ‹…m
²â‹…s
¯
², and it's more useful for the type to reflect a product of thes more basic units (even though we
can still use the `Joule` value to construct it).

Metres, seconds and kilograms are all SI base units. Kilograms are a little different, since _nominally_, a
kilogram is one thousand grams (while a gram is _not_ an SI base unit), and this has a small implication on
the way we construct such units.

Quantify provides general syntax for metric naming conventions, allowing prefixes such as `Nano` or `Mega`
to be applied to existing unit values to specify the appropriate scale to the value. Hence, a kilogram value
is written, `Kilo(Gram)`. But since the SI base unit is the kilogram, this and any other multiple of `Gram`,
such as `Micro(Gram)`, will use the type `Kilogram`, or more precisely, `Kilogram[1]`.

Therefore, the type of `energy` is `Quantity[Grams[1] & Metres[2] & Second[-2]]`, using a combination of three
base units raised to different powers. They are combined into an intersection type with the `&` type operator,
which provides the useful property that the order of the intersection is unimportant;
`Second[-2] & Metres[2] & Grams[1]` is an _identical_ type, much as kg m
²s
¯
² and s
¯
²m
²kg are identical
units.

Just as we could construct an area by multiplying two lengths, we can compute a new value with appropriate units
by combining, say, `area` and `energy`,
```scala
val volume = distance*distance*distance
val energyDensity = energy/volume
```
and its type will be inferred with the parameter `Kilogram[1] & Metres[-1] & Second[-2]`.

If we had instead calculated `energy/area`, whose units do not include metres, the type parameter would be just
`Kilogram[1] & Second[-2]`; the redundant `Metres[0]` would be automatically removed from the conjunction.

We can go further. For example, the "SUVAT" equations of motion can be safely implemented as methods, and
their dimensionality will be checked at compiletime. For example, the equation, `s = ut + 
½at
²` for
calculating a distance (`s`) from an initial velocity (`u`), acceleration (`a`) and time (`t`) can be
implemented using Quantify `Quantity`s with:
```scala
def s(u: Quantity[Metres[1] & Seconds[-2]], t: Quantity[Seconds[1]], a: Quantity[Metres[1] & Seconds[-2]])
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

Quantify defines a variety of imperial measurements, and will automatically convert units of the same
dimension to the same units in multiplications and divisions. For example,
```scala
val width = 0.3*Metre
val height = 5*Inch
val area = width*height
```
will infer the type `Quantity[Metres[2]]` for `area`.

However, the conversion of one of the units from inches to metres was necessary only to avoid a mixture of
`Inches` and `Metres` in the resultant type, but the expression, `height*height` would produce a value with the
units, `Inches[2]`, performing no unnecessary conversions.

### Conversions

#### Addition & subtraction

Addition and subtraction are possible between quantities which share the same dimension.

We can safely add an inch and a metre,
```scala
val length = 1*Inch + 1*Metre
```
but we can't subtract a second from a litre:
```scala
val nonsense = Litre - Second // will not compile
```

For the addition and subtraction of values with mixed units, the question arises of which units the result
should take. Quantify will use the _principal unit_ for the dimension, which is determined by the presence
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
```scala
8*Foot < 4*Metre // returns true
```
while incompatible units will result in a compile error.

#### Equality

Equality between different `Quantity` values should be treated with care, since all such values are
represented as `Double`s at runtime, and the JVM's standard equality will not take units into
account. So, by default, `3*Foot == 3*Metre` will yield `true`, since `3.0 == 3.0`!

This is highly undesirable, but luckily there's a solution:
```scala
import language.strictEquality
```

This turns on Scala's strict-equality feature, which forbids comparisons between any two types unless
a corresponding `CanEqual[LeftOperandType, RightOperandType]` exists in scope for the appropriate
operand types. Quantify provides just such an instance for `Quantity` instances with the same units.

The runtime equality check, however, is performed in exactly the same way: by comparing two `Double`s.
That is absolutely fine if we know the units are identical, but it does not allow equality comparisons
between `Quantity`s of the same dimension and different units.

For this, there are two possibilities:
- convert one of the `Quantity`s to the units of the other
- test `left <= right && left >= right`, which will only be true if `left` equals `right`

#### Conversion ratios

In order to automatically convert between two units, Quantify needs to know the ratio between them.
This is provided with a contextual `Ratio` value for the appropriate pair of units: one with the
power `1` and the other with the power `-1`.

For example,
```scala
given Ratio[Kilograms[1] & Tons[-1]](1016.0469088)
```
which specifies that there are about 1016 kilograms in a ton, and will be used if Quantify ever needs
to convert between kilograms and tons.

### Explicit Conversions

To convert a quantity to different units, we can use the `in` method, passing it an _unapplied_ units type
constructor, such as `Hour` or `Furlong`. The significance of the type being "unapplied" is that a units type
constructor is typically _applied_ to an integer singleton type, such as `Metres[2]` representing square
metres. Each dimension in a quantity must have the same units, no matter what its power, so it doesn't make
sense to specify that power when converting.

So, `(10*Metre).in[Yards]`, would create a value representing approximately 10.94 yards, while,
`(3*Foot * 1*Metre * 0.4*Centi(Metre)).in[Inches]`, would calculate a volume in cubic inches.

If a quantity includes units in multiple dimensions, these can be converted in steps, for example,
```scala
val distance = 100*Metre
val time = 9.8*Second
val speed = distance/time
val mph = speed.in[Miles].in[Hours]
```

### SI definitions

There are seven SI base dimensions, with corresponding units, which are defined by Quantify:
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

Quantify provides implementations of a variety of useful (and some less useful) units from the
metric system, CGS and imperial. It's also very easy to define your own units.

Imagine we wanted to implement the FLOPS unit, for measuring the floating-point performance of a
CPU: floating-point instructions per second.

Trivially, we could create a value,
```scala
val Flop = 1.0/Second
```
and use it in equations such as, `1000000*Flop * Minute` to yield an absolute number representing
the number of floating-point instructions that could (theoretically) be calculated in one minute by
a one-megaFLOP CPU.

But this definition is just a value, not a unit. We can tweak the definition slightly to,
```scala
val Flop = SiUnit(1.0/Second)
```
and it becomes possible to use SI prefixes on the value. So we could rewrite the above expression as,
`Mega(Flop) * Minute`.

### Introducing new dimensions

The result is just a `Double`, though, which is a little unsatisfactory, since it represents
something more specific: a number of instructions. To do better, we need to introduce a new
`Dimension` representing the size of code,
```scala
trait CpuPerformance extends Dimension
```
and create a `Flops` type corresponding to this dimension:
```scala
import rudiments.*
trait Flops[PowerType <: Nat] extends Units[PowerType, CpuPerformance]
val Flop: SiUnit[Flops[1]] = SiUnit(1)
```

The type parameter, `PowerType`, is a necessary part of this definition, and must be constrained on
the `Nat` type defined in [Rudiments](https://github.com/propensive/rudiments/), which is just an
alias for `Int & Singleton`. If you are using Scala's erased definitions, both `CpuPerformance` and
`Flops` may be made `erased trait`s to reduce the bytecode size slightly.

With these definitions, we can now write `Mega(Flop) * Minute` to get a result with the dimensions
"FLOPS-seconds", represented by the type, `Quantity[Flops[1] & Seconds[1]]`.

If we want to show the FLOPS value as `Text`, a symbolic name is required. This can be specified
with a contextual instance of `UnitName[Flops[1]]`,
```scala
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

quantify: the left operand represents velocity, but the right operand represents acceleration;
these are incompatible physical quantities
```
It is also possible to define your own, for example, here is the definition for "force":
```scala
erased given DimensionName[Units[1, Mass] & Units[1, Length] & Units[-2, Time], "force"] = erasedValue
```

The singleton type `"force"` is the provided name for any units corresponding to the dimensions,
massÃ—lengthÃ—time
¯
².

### Substituting simplified units

While the SI base units can be used to describe the units of most physical quantities, there often
exist simpler forms of their units. For example, the Joule, `J`, is equal to `kgâ‹…m
²â‹…s
¯
²`, and is
much easier to write.

By default, Quantify will use the latter form, but it is possible to define alternative
representations of units where these exist, and Quantify will use these whenever a quantity is
displayed. A contextual value can be defined, such as the following,
```scala
given SubstituteUnits[Kilograms[1] & Metres[2] & Seconds[-2]](t"J")
```
and then a value such as, `2.8*Kilo(Joule)` will be rendered as `2800 J` instead of `2800 kgâ‹…m
²â‹…s
¯
²`.

Note that this only applies if the quantity's units exactly match the type parameter of
`SubstituteUnits`, and units such as Joule-seconds would still be displayed as `kgâ‹…m
²â‹…s
¯
¹`.

## Status

Quantify is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Quantify is designed to be _small_. Its entire source code currently consists
of 851 lines of code.

## Building

Quantify can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Quantify are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/quantify/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Quantify easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Quantify was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

To _quantify_ is "to qualify with respect to quantity". This is exactly Quantify's remit.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meaningsâ€”since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## License

Quantify is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
