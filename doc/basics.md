### `Quantity` types

Physical quantities can be represented by different `Quantity` types, with an appropriate parameter that encodes
the value's units. We can create a quantity by multiplying an existing `Double` (or any numeric type) by some
unit value, such as `Metre` or `Joule`—which are just `Quantity` values equal to `1.0` of the appropriate unit.
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
which should have units of square metres (`m²`). Quantify represents this as the type, `Quantity[Metre[2]]`; the
`2` singleton literal value represents the metres being squared. Likewise, a volume would have the parameter
`Metre[3]`.

We can also define:
```scala
val energy = Joule*28000
```

The type of the `energy` value _could_ have been defined as `Quantity[Joule[1]]`, but 1 J is equivalent to 1 kg
m² s¯², and it's more useful for the type to reflect a product of simpler units (even though we can use the
`Joule` value to construct it).

Quantify provides general syntax for metric naming, allowing prefixes such as `Nano` or `Mega` to be applied
to existing unit values to apply the appropriate exponential multiplier to the value.

Metres, seconds and kilograms are all SI base units. But unlike the other base units, a kilogram already
includes the metric `kilo` prefix, so it is _nominally_ interpreted as one thousand times a metric gram, even
though the gram itself is not an SI base unit.

The distinction between SI base units and nominal metric units introduces a little extra complexity, and for
this reason, the `Gram` type appears in metric quantities and the raw value (a `Double`) will correspond to
a grams-based amount, quantities will nevertheless be shown in kilograms by default.

So the type of `energy` is `Quantity[Gram[1] & Metre[2] & Second[-2]]`, using a combination of three base
units raised to different powers. They are combined into an intersection type with the `&` type operator, which
provides the useful property that the order of the intersection is unimportant;
`Second[-2] & Metre[2] & Gram[1]` is an _identical_ type, much as kg m²s¯² and s¯²m²kg are identical
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

Kilograms, metres and seconds are units of three different dimensions which are never interchangeable, yet we
often need to work with different units of the same dimension, such as feet, metres, yards and miles as
different (but interchangeable) units of length, or kilograms and pounds, as units of mass.

Each type representing units, such as `Metre` or `Kilogram`, must be a subtype of the `Units` type,
which is parameterized on its power (with a singleton literal integer) and a _dimension_, that is, another type
representing the nature of the measurement. For `Metre` the dimension is `Length`; for `Kilogram`'s it is
`Mass`; `Candela`'s is `Luminosity`.

Note that there are no special dimensions for compound units, like energy, since the time, length and mass
components of the units of an energy quantity will be associated with the `Second`, `Metre` and `Kilogram`
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
will infer the type `Quantity[Metre[2]]` for `area`.

However, the conversion of one of the units from inches to metres was necessary only to avoid a mixture of
`Inch` and `Metre` in the resultant type, but the expression, `height*height` would produce a value with the
units, `Inch[2]`, performing no unnecessary conversions.

### Addition and subtraction

Addition and subtraction are possible between quantities which share the same dimensions. We can add an inch
and a metre,
```scala
val length = 1*Inch + 1*Metre
```
but we can't subtract a second from a litre:
```scala
val nonsense = Litre - Second // will not compile
```

For the addition and subtraction of values with mixed units, the question arises of which units the result
should take. Quantify will use the _principal unit_ for the dimension, which is determined by the presence
of a unique contextual `PrincipalUnit` instance, parameterized on `Dimension` and `Units` types, as well as
a parameter which exists to deal with the special case of kilograms (see above), where the _kilogram_ should
be preferred over other units, despite the type `Gram` being used as the base.

### Conversions

To convert a quantity to different units, we can use the `in` method, passing it an _unapplied_ units type
constructor, such as `Hour` or `Furlong`. The significance of the type being "unapplied" is that a units type
constructor is typically _applied_ to an integer singleton type, such as `Metre[2]` representing square
metres. Each dimension in a quantity must have the same units, no matter what its power, so it doesn't make
sense to specify that power when converting.

So, `(10*Metre).in[Yards]`, would create a value representing approximately 10.94 yards, while,
`(3*Foot * 1*Metre * 0.4*Centi(Metre)).in[Inches]`, would calculate a volume in cubic inches.

If a quantity includes units in multiple dimensions, these can be converted independently, for example,
```scala
val distance = 100*Metre
val time = 9.8*Second
val speed = distance/time
val mph = speed.in[Miles].in[Hours]
```
