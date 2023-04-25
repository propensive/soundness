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

The types of these values will be inferred. The value `distance` will get the type `Quantity[Metre[1]]`, since
its value is a number of metres (raised to the power `1`).

We can compute an `area` value by squaring the distance,
```scala
val area = distance*distance
```
which should have units of square metres (`m
²`). Quantify represents this as the type, `Quantity[Metre[2]]`; the
`2` singleton literal value represents the metres being squared. Likewise, a volume would have the parameter
`Metre[3]`.

We can also define:
```scala
val energy = Joule*28000
```

The type of the `energy` value _could_ have been defined as `Quantity[Joule[1]]`, but 1 J is equivalent to 1 kg
m
² s
¯
², and it's more useful for the type to reflect a product of simpler units (even though we can use the
`Joule` value to construct it).

So the type of `energy` is `Quantity[Kilogram[1] & Metre[2] & Second[-2]]`, using a combination of three SI base
units raised to different powers. They are combined into an intersection type with the `&` type operator, which
provides the useful property that the order of the intersection is unimportant;
`Second[-2] & Metre[2] & Kilogram[1]` is an _identical_ type, much as kg m
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
and its type will be inferred with the parameter `Kilogram[1] & Metre[-1] & Second[-2]`.

If we had instead calculated `energy/area`, whose units do not include metres, the type parameter would be just
`Kilogram[1] & Second[-2]`; the redundant `Metre[0]` would be automatically removed from the conjunction.

### Different Units

Kilograms, metres and seconds are units of three different dimensions which are never interchangeable, we often
need to work with different units of the same dimension, such as feet, metres, yards and miles as different (but
interchangeable) units of length, or kilograms and pounds, as units of mass.

Each type representing units, such as `Metre` or `Kilogram`, must be a subtype of the `Units` type,
which is parameterized on its power (with a singleton literal integer) and a _dimension_, that is, another type
representing the nature of the measurement. For `Metre` the dimension is `Length`; for `Kilogram`'s it is
`Mass`; `Candela`'s is `Luminosity`.

It is possible to create new length or mass units, such as `Inch` or `Pound`, which share the `Length` or `Mass`
dimensions. This allows them to be considered equivalent in some calculations, if a conversion coefficient is
available.

Quantify will automatically convert units of the same dimension to the same units in multiplications and
divisions. For example,
```scala
val width = 0.3*Metre
val height = 5*Inch
val area = width*height
```
will infer the type `Quantity[Metre[2]]` for `area`.

However, the conversion of one of the units from inches to metres was necessary only to avoid a mixture of
`Inch` and `Metre` in the resultant type, but the expression, `height*height` would produce a value with the
units, `Inch[2]`, performing no unnecessary conversions.



## Status

Quantify is classified as __fledgling__. For reference, Scala One projects are
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
of 588 lines of code.

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
