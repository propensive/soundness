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

So the type of `energy` is `Quantity[Kilogram[1] & Metre[2] & Second[-2]]`, using a combination of three SI base
units raised to different powers. They are combined into an intersection type with the `&` type operator, which
provides the useful property that the order of the intersection is unimportant;
`Second[-2] & Metre[2] & Kilogram[1]` is an _identical_ type, much as `kg m² s¯²` and `s¯² m² kg` are identical
units.

Just as we could construct an area by multiplying two lengths, we can compute a new value with appropriate units
by combining, say, `area` and `energy`,
```scala
val volume = distance*distance*distance
val energyDensity = energy/volume
```
and its type will be inferred with the parameter `Kilogram[1] & Metre[-1] & Second[-2]`.

If we had instead calculated `energy/area`, whose units do not include metres, the type would be just
`Kilogram[1] & Second[-2]`; the redundant `Metre[0]` would be automatically elided.

