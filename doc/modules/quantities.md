## Quantities

### About

Soundness represents physical quantities — a distance, a mass, a speed — as values
that carry their [units](https://en.wikipedia.org/wiki/Units_of_measurement) in
their type. Adding a distance to a time is a compile error; multiplying a distance
by a force gives a quantity whose units are worked out for you; and a quantity
costs nothing at runtime beyond the `Double` it wraps.

### On quantities

A bare number carries no units: the figure `5` says nothing about whether it counts
metres, feet, seconds or kilograms. Code that loses track of the answer has caused
real disasters, the
[loss of a Mars orbiter](https://en.wikipedia.org/wiki/Mars_Climate_Orbiter#Cause_of_failure)
among them, when one component worked in newtons and another in pound-force. The
usual defences — naming conventions, comments, a wrapper class per unit — rely on
discipline and catch nothing automatically.

Soundness records the units of every quantity in its type and performs
[dimensional analysis](https://en.wikipedia.org/wiki/Dimensional_analysis) as the
code compiles. A `Quantity` is parameterized by its units, so a length and a
duration are different types, and the compiler refuses to add them. Arithmetic
combines units correctly — dividing a length by a duration yields a speed — and
quantities of compatible units convert into one another automatically. None of
this survives to runtime: a `Quantity` is an opaque `Double`, and its operations
inline away, so the safety is paid for entirely at compiletime.

Units, prefixes and the quantity operations all come from the `soundness`
package:

```scala
import soundness.*
```

### Quantities and units

A quantity is a number multiplied by a unit. The SI base units — `Metre`,
`Second`, `Gram`, `Kelvin`, `Ampere`, `Candela`, `Mole` — are values, as are many
others:

```scala
val distance = 5*Metre
val duration = 2*Second
```

The unit lives in the type. `5*Metre` is a `Quantity[Metres[1]]`, where the `1` is
the power to which the unit is raised; an area would be `Metres[2]` and a frequency
`Seconds[-1]`. These types are inferred, so they rarely need to be written, but
they are what the compiler checks.

### Arithmetic and dimensions

Quantities of the same unit add and subtract:

```scala
Metre + Metre*2.0   // 3 metres
```

Multiplication and division combine units, producing a quantity whose type records
the new dimensions:

```scala
2*Second * 3*Metre        // 6 metre-seconds
(2*Metre/Second).invert   // half a second per metre
```

Adding quantities whose dimensions disagree is a compile error, and the message
names the mismatch in plain words rather than in terms of types:

```scala
Metre + 2*Second
// does not compile:
// the left operand represents distance, but the right operand represents time;
// these are incompatible physical quantities
```

The check is by dimension, not merely by unit, so a length cannot be added to an
energy, and even two powers of the same dimension stay apart — a length is not an
area:

```scala
2*Metre + 2*Joule    // does not compile: distance versus energy
7*Metre >= 2*Metre*Metre   // does not compile: distance versus area
```

### Mixing units

Different units of the _same_ dimension combine freely, converting automatically.
The result takes the unit of the principal — here, SI — operand:

```scala
2*Metre + 2*Foot    // 2.6096 metres
2*Foot + 2*Metre    // 2.6096 metres
```

Conversion applies to multiplication and division too, so an expression may be
written in whatever units are natural and still produce a coherent result:

```scala
val width = 2*Metre
val depth = 3*Foot
width*depth         // 1.8288 square metres
```

### Converting

A quantity converts explicitly to another compatible unit with `in`, naming the
target unit family:

```scala
(3*Foot).in[Metres]   // 0.9144 metres
(3*Metre).in[Feet]    // 9.8425… feet
```

`normalize` does the same, written with the fully-powered unit type:

```scala
(2*Hour).normalize[Seconds[1]]   // 7200 seconds
(1*Inch).normalize[Metres[1]]    // 0.0254 metres
```

It acts only on the dimension of the unit it names. Components measured in that
dimension are converted to the target, and components of every other dimension are
left exactly as they were — so normalizing a speed to seconds rescales its time
part and leaves its distance part untouched. It follows that converting to a unit
of a dimension the quantity does not have is not an error; it simply does nothing,
because there is no matching component to rescale.

### Metric prefixes

A [metric prefix](https://en.wikipedia.org/wiki/Metric_prefix) scales a unit.
`Kilo`, `Mega`, `Giga`, `Milli`, `Micro`, `Nano` and the rest apply to a unit to
produce a larger or smaller one:

```scala
15*Kilo(Metre)    // 15000 metres
1.5*Milli(Metre)  // 0.0015 metres
```

The binary prefixes — `Kibi`, `Mebi`, `Gibi` — scale by powers of two instead of
ten, for quantities of information:

```scala
10*Kibi(Metre)    // 10240 metres
```

Mass is the one awkward case among the base units, because its SI base unit is
itself a prefixed unit: the kilogram, not the gram. Soundness still names the unit
`Gram`, for consistency with every other unit, and writes the base unit as the
prefixed `Kilo(Gram)`. But the dimension is `Kilograms`, and masses always
normalize and display in kilograms — so `Kilo(Gram)` is one whole `kg`, while a
bare `Gram` is a thousandth of it.

### Comparing

Quantities of compatible units compare with the usual operators, and the
comparison converts units as arithmetic does:

```scala
6*Foot < 2*Metre           // true
9*Foot*Foot < Metre*Metre  // true
```

Equality across different units of one dimension uses `===`, which asks whether two
quantities measure the same amount:

```scala
Mile === 1760*Yard   // true
Mile === Inch         // false
```

As with arithmetic, comparing incompatible dimensions does not compile.

Equality, whether through `===` or `==`, is exact. Two quantities count as equal
only when their underlying values agree to the last bit, so a conversion that
introduces a [rounding difference](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
can leave quantities that are equal on paper comparing unequal; for converted
values, `<` and `>` or an explicit tolerance are usually the better test.

Plain `==` is reserved for quantities that already share a unit type, where it
compares those underlying values directly. Two quantities of different units have no
`==` between them: the only equality Soundness provides is between matching unit
types, so under strict equality (`import language.strictEquality`) the comparison
does not compile — which is exactly the gap `===` fills.

### Derived units

The named SI units — `Newton`, `Joule`, `Watt`, `Pascal`, `Volt` and many more —
are defined in terms of the base units, so they interoperate with them. A force in
newtons and a length in metres multiply to an energy that displays in joules.
Every quantity can also name its own dimension, worked out from its units:

```scala
Metre.dimension          // t"distance"
(Metre/Second).dimension // t"velocity"
```

Even an unusual combination resolves to its physical name:

```scala
(Foot*Foot*Kilo(Gram)/(Second*Second*Mole)).dimension   // t"chemical potential"
```

### Displaying

A quantity renders through `show`. The rendering needs a `Decimalizer` in scope to
fix the number of significant figures:

```scala
given Decimalizer = Decimalizer(3)

(7.567*Metre).show      // t"7.57 m"
(1.4*Metre*Metre).show  // t"1.40 m²"
```

Compound units render with a middle dot and superscript powers, and very large or
small values switch to scientific notation:

```scala
(8.54*Metre/Second).show         // t"8.54 m·s¯¹"
(8.54*Kilo(Metre)/Second).show   // t"8.54×10³ m·s¯¹"
constants.SpeedOfLightInVacuum.show
// t"3.00×10⁸ m·s¯¹"
```

Rather than scientific notation, a quantity can scale itself to a metric prefix
for display. A `Prefixes` instance lists the prefixes to choose from, and `show`
picks the one that keeps the number readable:

```scala
sealed trait Information extends Dimension
sealed trait Bits[Power <: Nat] extends Units[Power, Information]
val Bit: MetricUnit[Bits[1]] = MetricUnit(1.0)
given Designation[Bits[1]] = () => t"bit"

given Prefixes on Bits[1] = Prefixes(List(Kilo, Mega, Giga, Tera))

(5000*Bit).show       // t"5.00 kbit"
(5_000_000*Bit).show  // t"5.00 Mbit"
```

That example also shows how a new dimension and unit are declared: a `Dimension`,
a `Units` type indexed by its power, a unit value, and a `Designation` giving its
symbol.

### Temperature

Temperature is measured from an arbitrary zero, not from nothing, so it behaves
unlike a length or a mass: thirty degrees plus thirty degrees is not sixty degrees
of anything meaningful. Soundness models it as a separate `Temperature` type — an
offset quantity — and keeps the offset scales distinct from the absolute Kelvin
quantity used for differences.

Four scales appear, and they fall into two kinds. Kelvin and Rankine measure from
absolute zero, so they are ordinary quantities as well as scales: `Kelvin` is a
unit, and a Rankine quantity is reached through its `Rankines` unit. Celsius and
Fahrenheit place their zero elsewhere — at the freezing point of water, and lower
still — so a reading on either is meaningful only as a `Temperature`, created with
`Celsius(…)` or `Fahrenheit(…)`. Multiplying or adding two such readings directly
would be meaningless, and the types do not allow it.

A temperature is created on a named scale, and displayed on whichever scale is in
scope:

```scala
import temperatureScales.celsiusScale
Celsius(30).show   // t"30.0 °C"
```

```scala
import temperatureScales.fahrenheitScale
Fahrenheit(30).show   // t"30.0 °F"
```

A `Temperature` adds and subtracts Kelvin (or Rankine) _quantities_ to give another
temperature, and the difference of two temperatures is a quantity that converts
like any other:

```scala
import temperatureScales.celsiusScale
(zero[Temperature] + 300*Kelvin).show   // t"26.9 °C"

(Fahrenheit(100) - zero[Temperature]).in[Rankines].show   // t"560 °R"
```

Reading the same temperature on a different scale is a matter of importing a
different one, with no conversion written by hand:

```scala
import temperatureScales.celsiusScale
Fahrenheit(100).show   // t"37.8 °C"
```

### Aggregating

A collection of quantities sharing a unit reduces to a quantity of that unit. The
total, mean, variance and standard deviation are each unit-aware — the variance of
a list of seconds is in seconds squared, and the standard deviation back in
seconds:

```scala
List(1*Second, 2*Second, 3*Second).total        // 6 seconds
List(1*Second, 2*Second, 3*Second).mean.vouch    // 2 seconds
List(1*Second, 2*Second, 3*Second).std.vouch     // √(2/3) seconds
```

### Quantifying your own types

A type that stands for a measurement can be taught to produce a quantity through a
`Quantifiable` instance, after which it takes part in the same dimensional
arithmetic. A typographic point, one seventy-second of an inch, becomes a genuine
length:

```scala
case class Pts(value: Double)
given Quantifiable[Pts, Inches[1]] = pts => (Inch*pts.value)/72.0

Pts(71).quantify < Inch   // true
Pts(73).quantify > Inch   // true
```

### Calculations

A formula written in quantities is checked as it is written. Each step must combine
its units coherently, and the units of the result are computed from the units of the
inputs, so the dimensions of a calculation are verified before it ever runs. The
[displacement of a body under constant acceleration](https://en.wikipedia.org/wiki/Equations_of_motion#Constant_translational_acceleration_in_a_straight_line),
`s = ut + ½at²`, reads directly:

```scala
def displacement
    ( initial: Quantity[Metres[1] & Seconds[-1]],
      time:    Quantity[Seconds[1]],
      accel:   Quantity[Metres[1] & Seconds[-2]] )
:   Quantity[Metres[1]] =
  initial*time + 0.5*accel*time*time
```

Both terms of the sum come out as a distance — a velocity times a time, and an
acceleration times a time squared — so the addition type-checks and the result is a
distance. Passing an argument in the wrong units, or returning the wrong dimension,
is a compile error rather than a number that is quietly wrong.

The same calculation can be written once for any consistent choice of units, rather
than fixed to metres and seconds. A `transparent inline def` resolves its units at
each call, so its parameters can be left abstract; the relationships the formula
relies on are then stated as the constraints that a velocity times a time and an
acceleration times a time squared can each be formed, and that the two results can
be added:

```scala
transparent inline def displacement
    [ velocity     <: Measure,
      time         <: Measure,
      acceleration <: Measure ]
    ( initial: Quantity[velocity],
      elapsed: Quantity[time],
      rate:    Quantity[acceleration] )
    ( using vt:  Quantity[velocity] is Multiplicable by Quantity[time],
            at:  Quantity[acceleration] is Multiplicable by Quantity[time] )
    ( using att: at.Result is Multiplicable by Quantity[time] )
    ( using add: vt.Result is Addable by att.Result )
=   initial*elapsed + 0.5*rate*elapsed*elapsed

displacement(3*Metre/Second, 2*Second, 9.8*Metre/(Second*Second))   // 25.6 metres
```

Those constraints are nothing more than the statement that the equation's dimensions
line up, expressed in types; a call whose units cannot satisfy them does not compile.
