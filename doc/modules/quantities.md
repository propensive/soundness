## Quantities

### About

Physical quantities, such as a distance, a mass or a speed, are values which carry their
[units](https://en.wikipedia.org/wiki/Units_of_measurement) in their type:

- Adding a distance to a time is a compile error.
- Multiplying a distance by a force gives a quantity whose units are worked out
  automatically.
- A quantity costs nothing at [runtime] beyond the `Double` it wraps.

That last property is a constraint on the design, not an optimization, as
[zero cost](../philosophy/zero-cost.md) explains.

### On quantities

A bare number has no units. The figure `5` says nothing about whether it counts meters, feet,
seconds or kilograms. Code which loses track of the answer has caused real disasters. The
[loss of a Mars orbiter](https://en.wikipedia.org/wiki/Mars_Climate_Orbiter#Cause_of_failure)
is one of them: one component worked in newtons, and another in pound-force. The usual
defenses are naming conventions, comments, and a wrapper class for each unit. They rely on
discipline, and catch nothing automatically.

Here the units of every quantity are recorded in its type, and
[dimensional analysis](https://en.wikipedia.org/wiki/Dimensional_analysis) is performed at
[compiletime]. A `Quantity` has a type parameter for its units, so a length and a duration are
different types, and the compiler refuses to add them. Arithmetic combines units correctly:
dividing a length by a duration gives a speed. Quantities of compatible units convert into
one another automatically.

None of this exists at runtime. A `Quantity` is an [opaque type] over a `Double`, and its
operations are inlined, so the safety is paid for entirely at compiletime.

The units, the prefixes and the operations on quantities all come from the `soundness`
package. Arithmetic between a plain number and a quantity relies on the compiler's *into*
conversions, so that language feature is enabled too. A `Decimalizer` in scope fixes how many
significant figures a quantity is shown with, and the last import lets the samples print
their results:

```scala
import soundness.*
import scala.language.experimental.into
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)
```

### Quantities and units

A quantity is a number multiplied by a unit. The SI base units are values: `Metre`, `Second`,
`Gram`, `Kelvin`, `Ampere`, `Candela` and `Mole`. So are many other units.

```scala
val distance = 5*Metre
val duration = 2*Second
```

`show` writes a quantity with its unit:

```scala fuel
run
##
Out.println(distance.show)
Out.println(duration.show)
```

The unit lives in the type. `5*Metre` is a `Quantity[Metres[1]]`, in which the `1` is the
power to which the unit is raised. An area would be `Metres[2]`, and a frequency
`Seconds[-1]`. These types are inferred, so they rarely need to be written, but they are what
the compiler checks.

### Arithmetic and dimensions

Quantities of the same unit add and subtract. Multiplication and division combine units, and
give a quantity whose type records the new dimensions:

```scala fuel
run
##
Out.println((Metre + Metre*2.0).show)
Out.println((2*Second * 3*Metre).show)
Out.println((distance/duration).show)
Out.println((2*Metre/Second).invert.show)
```

Adding quantities whose dimensions disagree is a compile error. The message names the
mismatch in plain words, not in terms of types:

```scala fuel
compile
errors show
##
Metre + 2*Second
```

The check is by dimension, not merely by unit, so a length cannot be added to an energy:

```scala fuel
compile
errors show
##
2*Metre + 2*Joule
```

Even two powers of the same dimension stay apart. A length is not an area:

```scala fuel
compile
errors show
##
7*Metre >= 2*Metre*Metre
```

### Mixing units

Different units of the *same* dimension combine freely, and are converted automatically. The
result takes the unit of the principal operand, which here is the SI one:

```scala fuel
run
##
Out.println((2*Metre + 2*Foot).show)
Out.println((2*Foot + 2*Metre).show)
```

Conversion applies to multiplication and division too. An expression may be written in
whatever units are natural, and still give a coherent result:

```scala
val width = 2*Metre
val depth = 3*Foot
```

```scala fuel
run
##
Out.println((width*depth).show)
```

### Converting

`convert` converts a quantity explicitly to another compatible unit, naming the family of the
target unit:

```scala fuel
run
##
Out.println((3*Foot).convert[Metres].show)
Out.println((3*Metre).convert[Feet].show)
```

`normalize` does the same, with the unit type written with its power. An hour is not one of
the predefined unit values, but a unit is only a quantity of one, so it is defined in one
line:

```scala
val Hour: Quantity[Hours[1]] = Quantity(1.0)
```

```scala fuel
run
##
Out.println((2*Hour).normalize[Seconds[1]].show)
Out.println((1*Inch).normalize[Metres[1]].show)
```

`normalize` acts only on the dimension of the unit which it names. Components which are
measured in that dimension are converted to the target, and components of every other
dimension are left exactly as they were. Normalizing a speed to seconds therefore rescales
its time, and leaves its distance untouched. It follows that converting to a unit of a
dimension which the quantity does not have is not an error. It simply does nothing, because
there is no matching component to rescale.

### Metric prefixes

A [metric prefix](https://en.wikipedia.org/wiki/Metric_prefix) scales a unit. `Kilo`, `Mega`,
`Giga`, `Milli`, `Micro`, `Nano` and the rest apply to a unit, to give a larger or a smaller
one. The binary prefixes, `Kibi`, `Mebi` and `Gibi`, scale by powers of two instead of ten,
for quantities of information:

```scala fuel
run
##
Out.println((15*Kilo(Metre)).show)
Out.println((1.5*Milli(Metre)).show)
Out.println((10*Kibi(Metre)).show)
```

The full set, decimal and binary:

```tabular
column  Prefix
column  Symbol
column  Factor
row  `Quetta`  Q  10³⁰
row  `Ronna`  R  10²⁷
row  `Yotta`  Y  10²⁴
row  `Zetta`  Z  10²¹
row  `Exa`  E  10¹⁸
row  `Peta`  P  10¹⁵
row  `Tera`  T  10¹²
row  `Giga`  G  10⁹
row  `Mega`  M  10⁶
row  `Kilo`  k  10³
row  `Hecto`  h  10²
row  `Deka`  da  10¹
row  `Deci`  d  10⁻¹
row  `Centi`  c  10⁻²
row  `Milli`  m  10⁻³
row  `Micro`  µ  10⁻⁶
row  `Nano`  n  10⁻⁹
row  `Pico`  p  10⁻¹²
row  `Femto`  f  10⁻¹⁵
row  `Atto`  a  10⁻¹⁸
row  `Zepto`  z  10⁻²¹
row  `Yocto`  y  10⁻²⁴
row  `Ronto`  r  10⁻²⁷
row  `Quecto`  q  10⁻³⁰
row  `Kibi`  Ki  2¹⁰
row  `Mebi`  Mi  2²⁰
row  `Gibi`  Gi  2³⁰
row  `Tebi`  Ti  2⁴⁰
row  `Pebi`  Pi  2⁵⁰
row  `Exbi`  Ei  2⁶⁰
row  `Zebi`  Zi  2⁷⁰
row  `Yobi`  Yi  2⁸⁰
```

Mass is the one awkward case among the base units, because its SI base unit is itself a
prefixed unit: the kilogram, not the gram. The unit is still named `Gram`, for consistency
with every other unit, and the base unit is written as the prefixed `Kilo(Gram)`. But the
dimension is `Kilograms`, and masses are always normalized and displayed in kilograms. So
`Kilo(Gram)` is one whole `kg`, while a bare `Gram` is a thousandth of it:

```scala fuel
run
##
Out.println((1*Kilo(Gram)).show)
Out.println((1*Gram).show)
```

### Comparing

Quantities of compatible units compare with the usual operators, and the comparison converts
units as arithmetic does:

```scala fuel
run
##
Out.println((6*Foot < 2*Metre).inspect)
Out.println((9*Foot*Foot < Metre*Metre).inspect)
```

Equality across different units of one dimension uses `===`, which asks whether two
quantities measure the same amount:

```scala fuel
run
##
Out.println((Mile === 1760*Yard).inspect)
Out.println((Mile === Inch).inspect)
```

As with arithmetic, comparing incompatible dimensions does not compile.

Equality is exact, whether through `===` or `==`. Two quantities count as equal only when
their underlying values agree to the last bit. A conversion which introduces a
[rounding difference](https://en.wikipedia.org/wiki/Floating-point_arithmetic) can therefore
leave quantities which are equal on paper comparing as unequal. For converted values, `<` and
`>`, or an explicit tolerance, are usually the better test.

Plain `==` is reserved for quantities which already share a unit type, where it compares
those underlying values directly. Two quantities of different units have no `==` between
them. Under strict equality, with `import language.strictEquality`, such a comparison does
not compile, which is exactly the gap that `===` fills.

### Derived units

The named SI units, such as `Newton`, `Joule`, `Watt`, `Pascal` and `Volt`, are defined in
terms of the base units, so they work together with them. A force in newtons and a length in
meters multiply to an energy. Every quantity can also name its own dimension, worked out
from its units, and even an unusual combination resolves to its physical name:

```scala fuel
run
##
Out.println(Metre.dimension)
Out.println((Metre/Second).dimension)
Out.println((Newton*Metre).dimension)
Out.println((Foot*Foot*Kilo(Gram)/(Second*Second*Mole)).dimension)
```

### The SI units

Seven base dimensions are defined, each with its type of units, and the value which names
one of them:

```tabular
column  Dimension
column  Units
column  Unit
row  `Distance`  `Metres`  `Metre`
row  `Mass`  `Kilograms`  `Gram`
row  `Time`  `Seconds`  `Second`
row  `Current`  `Amperes`  `Ampere`
row  `Luminosity`  `Candelas`  `Candela`
row  `AmountOfSubstance`  `Moles`  `Mole`
row  `Heat`  `Kelvins`  `Kelvin`
```

The named derived units are defined in terms of those, so each has its full dimensions, and
is not a distinct kind of thing. The last column is the name which its `dimension` reports:

```tabular
column  Unit
column  Definition
column  Dimension
row  `Hertz`  one per second  frequency
row  `Newton`  meter-kilogram per second squared  force
row  `Pascal`  newton per square meter  pressure
row  `Joule`  newton-meter  energy
row  `Watt`  joule per second  power
row  `Coulomb`  second-ampere  electric charge
row  `Volt`  watt per ampere  electric potential
row  `Farad`  coulomb per volt  capacitance
row  `Ohm`  volt per ampere  impedance
row  `Siemens`  ampere per volt  electric conductance
row  `Weber`  volt-second  magnetic flux
row  `Tesla`  weber per square meter  magnetic flux density
row  `Henry`  weber per ampere  inductance
row  `Lux`  candela per square meter  illuminance
row  `Becquerel`  one per second  frequency
row  `Gray`  joule per kilogram  specific energy
row  `Sievert`  joule per kilogram  specific energy
row  `Katal`  mole per second
```

The CGS and imperial systems are provided beside them: `Dyne`, `Erg`, `Gauss`, `Poise`,
`Foot`, `Furlong`, `Stone` and the rest.

### Displaying

`show` writes a quantity out, and needs a `Decimalizer` in scope to fix the number of
significant figures. Compound units are written with a middle dot and superscript powers,
and very large or very small values switch to scientific notation:

```scala fuel
run
##
Out.println((7.567*Metre).show)
Out.println((1.4*Metre*Metre).show)
Out.println((8.54*Metre/Second).show)
Out.println((8.54*Kilo(Metre)/Second).show)
Out.println(constants.SpeedOfLightInVacuum.show)
```

`show` is the only correct way to write a quantity out. A `Quantity` is an opaque type over
a `Double`, so its `toString` is the `Double`'s: the bare number, with no units at all. A
quantity which is substituted into a plain string therefore silently loses the very thing
which distinguishes it. [Text](text.md#rendering-numbers) describes the `Decimalizer`.

Instead of scientific notation, a quantity can scale itself to a metric prefix for display.
A `Prefixes` instance lists the prefixes to choose from, and `show` picks the one which keeps
the number readable:

```scala
sealed trait Information extends Dimension
sealed trait Bits[Power <: Nat] extends Units[Power, Information]
val Bit: MetricUnit[Bits[1]] = MetricUnit(1.0)
given bitDesignation: Designation[Bits[1]] = () => t"bit"

given bitPrefixes: (Prefixes on Bits[1]) = Prefixes(List(Kilo, Mega, Giga, Tera))
```

```scala fuel
run
##
Out.println((5000*Bit).show)
Out.println((5_000_000*Bit).show)
```

That sample also shows how a new dimension and unit are declared: a `Dimension`, a `Units`
type indexed by its power, a unit value, and a `Designation` which gives its symbol.

### Defining a unit of your own

A unit which measures something for which the SI has no dimension needs both a dimension and
a type of units. Take FLOPS, which is a processor's floating-point instructions per second.
The definition is built up in four steps:

```scala fuel
frame
  label A bare rate, in inverse seconds
  code
      val SimpleFlop = 1.0/Second
frame
  label A metric unit, so that the prefixes apply
  code
      val PrefixableFlop: MetricUnit[Seconds[-1]] = MetricUnit(1)

      Mega(PrefixableFlop)
frame
  label A dimension of its own
  code
      trait CpuPerformance extends Dimension
      trait Flops[power <: Nat] extends Units[power, CpuPerformance]

      val Flop: MetricUnit[Flops[1]] = MetricUnit(1)

      Mega(Flop)*Second
frame
  label With a symbol for display
  code
      trait CpuPerformance extends Dimension
      trait Flops[power <: Nat] extends Units[power, CpuPerformance]

      val Flop: MetricUnit[Flops[1]] = MetricUnit(1)

      given Designation[Flops[1]] = () => t"FLOPS"

      Mega(Flop)*Second
##
```

Written as a bare value, `1.0/Second` can be used in arithmetic, but says nothing about what
it counts. Wrapping it as a `MetricUnit` at least makes the metric prefixes apply, so
`Mega(Flop)` can be written, but the result is still just a number in units of inverse
seconds. Giving it a dimension of its own makes it distinct from every frequency. The
`power` parameter is required, and is bounded by `Nat`, which is an alias for
`Int & Singleton`. `Mega(Flop)*Second` then has the type
`Quantity[Flops[1] & Seconds[1]]`, and a `Designation` gives it a symbol for display.

Both traits may be `sealed`, since nothing implements them at runtime.

### Conversion ratios

Converting between two units needs the ratio between them. That comes from a contextual
`Ratio` for the pair, with one unit at the power `1` and the other at the power `-1`, and the
rate as a singleton literal `Double`:

```scala
inline given ratio: Ratio[Metres[1] & Furlongs[-1], 201.168] = !!
```

The rate is a *type*, not a value, and that is what makes a conversion cost nothing. Its
value is known at compiletime, so arithmetic on it is done as the code compiles, and the
given itself can be erased. A conversion between two units therefore contributes no work at
runtime beyond the multiplication which it actually denotes.

### Mixed and non-decimal bases

Not every measurement is a single number in a single unit. Before the metric system,
quantities were expressed as a *cascade* of units, in exact multiples which were not powers
of ten: feet and inches, or stones, pounds and ounces. Some still are. Hours, minutes and
seconds are exactly that, and so is a person's height in most of the English-speaking world.

A `Quanta` represents one such value: a whole number of each unit in a descending cascade,
with the carrying done automatically. The type states the cascade, with the smallest unit as
the type parameter, and the larger ones after `in`:

```scala
type Weight = Quanta[Ounces[1]] in (Pounds[1], Stones[1])
type Height = Quanta[Inches[1]] in (Feet[1])
```

A value is written with the largest unit first, as it is spoken, and each component is read
back by naming its unit. This weight is 1 stone, 3 pounds and 2 ounces:

```scala
val weight: Weight = Quanta(1, 3, 2)
```

```scala fuel
run
##
Out.println(weight[Stones].show)
Out.println(weight[Pounds].show)
Out.println(weight[Ounces].show)
```

Arithmetic carries between the units as the ratios require. Adding fifteen ounces to 3 pounds
2 ounces gives 4 pounds 1 ounce, not 3 pounds 17 ounces:

```scala
val heavier: Weight = weight + Quanta(15)
```

```scala fuel
run
##
Out.println(heavier[Pounds].show)
Out.println(heavier[Ounces].show)
```

The cascade is checked as it is written. Every unit in it must have the same dimension, so
mixing seconds into a cascade of lengths does not compile, and the base must be a unit, not
an arbitrary type. `TimeSeconds` and `TimeMinutes` are provided, for the two cascades which
everyone needs.

The units are real units, so a `Quanta` converts to and from an ordinary `Quantity`, which
is how a measurement crosses between the two worlds:

```scala
val length: Quantity[Metres[1]] = 5.9*Foot + 10.0*Inch
val height = length.quanta[Height]
```

```scala fuel
run
##
Out.println(height[Feet].show)
Out.println(height[Inches].show)
Out.println(weight.quantity.convert[Pounds].show)
```

Showing a `Quanta` uses each unit's symbol, as in `1st 3lb 2oz`. A `UnitsNames` given
overrides the symbols where a cascade is conventionally written differently, so that a
height can be shown as `5' 9"`.

### Temperature

Temperature is measured from an arbitrary zero, not from nothing, so it behaves unlike a
length or a mass. Thirty degrees plus thirty degrees is not sixty degrees of anything
meaningful. It is modeled as a separate type, `Temperature`, and the scales with an
arbitrary zero are kept distinct from the absolute Kelvin quantity which is used for
differences.

Four scales appear, and they fall into two kinds. Kelvin and Rankine measure from absolute
zero, so they are ordinary quantities as well as scales. Celsius and Fahrenheit place their
zero elsewhere, at the freezing point of water and lower still, so a reading on either is
meaningful only as a `Temperature`. Multiplying or adding two such readings directly would
be meaningless, and the types do not allow it.

A temperature is created on a named scale, and displayed on whichever scale is in scope:

```tabular
column  Scale
column  Zero
column  Reading
column  As a quantity
column  Display import
row  Kelvin  absolute zero  `Kelvin(…)`  the unit `Kelvin`  `temperatureScales.kelvinScale`
row  Rankine  absolute zero  `Rankine(…)`  the units `Rankines`  `temperatureScales.rankineScale`
row  Celsius  freezing point of water  `Celsius(…)`  none  `temperatureScales.celsiusScale`
row  Fahrenheit  below freezing  `Fahrenheit(…)`  none  `temperatureScales.fahrenheitScale`
```

```scala fuel
run
##
locally:
  import temperatureScales.celsiusScale
  Out.println(Celsius(30).show)
  Out.println(Fahrenheit(100).show)

locally:
  import temperatureScales.fahrenheitScale
  Out.println(Celsius(30).show)
  Out.println(Fahrenheit(100).show)
```

Reading the same temperature on a different scale is thus a matter of importing a different
one, with no conversion written by hand.

A `Temperature` adds and subtracts Kelvin or Rankine *quantities*, to give another
temperature. The difference of two temperatures is a quantity, which converts like any
other:

```scala fuel
run
##
locally:
  import temperatureScales.celsiusScale

  Out.println((zero[Temperature] + 300*Kelvin).show)
  Out.println((Fahrenheit(100) - zero[Temperature]).convert[Rankines].show)
```

### Aggregating

A collection of quantities which share a unit reduces to a quantity, and each reduction
understands units:

```tabular
column  Method
column  Result
column  Of an empty collection
row  `total`  the sum, in the same unit  zero
row  `mean`  the mean, in the same unit  `Unset`
row  `variance`  the variance, in the unit squared  `Unset`
row  `std`  the standard deviation, in the same unit  `Unset`
```

So the variance of a list of seconds is in seconds squared, and the standard deviation is
back in seconds:

```scala
val samples = List(1*Second, 2*Second, 3*Second)
```

```scala fuel
run
##
Out.println(samples.total.let(_.show).or(t"none"))
Out.println(samples.mean.let(_.show).or(t"none"))
Out.println(samples.std.let(_.show).or(t"none"))
```

[Mathematics](mathematics.md#elements-that-are-not-numbers) builds vectors and matrices of
quantities, whose products carry units too.

### Quantifying your own types

A type which stands for a measurement can be taught to give a quantity, through a
`Quantifiable` instance. It then takes part in the same dimensional arithmetic. A
typographic point, which is one seventy-second of an inch, becomes a real length:

```scala
case class Pts(value: Double)
given ptsQuantifiable: Quantifiable[Pts, Inches[1]] = pts => (Inch*pts.value)/72.0
```

```scala fuel
run
##
Out.println((Pts(71).quantify < Inch).inspect)
Out.println((Pts(73).quantify > Inch).inspect)
```

### Calculations

A formula written in quantities is checked as it is written. Each step must combine its
units coherently, and the units of the result are computed from the units of the inputs, so
the dimensions of a calculation are verified before it ever runs. The
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

```scala fuel
run
##
Out.println(displacement(3*Metre/Second, 2*Second, 9.8*Metre/(Second*Second)).show)
```

Both terms of the sum come out as a distance: a velocity times a time, and an acceleration
times a time squared. The addition therefore typechecks, and the result is a distance.
Passing an argument in the wrong units is a compile error, not a number which is quietly
wrong:

```scala fuel
compile
errors show
##
displacement(3*Metre, 2*Second, 9.8*Metre/(Second*Second))
```

The same calculation can be written once for any consistent choice of units, instead of
being fixed to meters and seconds. A `transparent inline def` resolves its units at each
call, so its parameters can be left abstract. The relationships which the formula relies on
are then stated as constraints: that a velocity times a time, and an acceleration times a
time squared, can each be formed, and that the two results can be added.

```scala fuel
parse
##
transparent inline def motion
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

val fallen = motion(3*Metre/Second, 2*Second, 9.8*Metre/(Second*Second))
```

Those constraints are nothing more than the statement that the equation's dimensions line
up, expressed in types. A call whose units cannot satisfy them does not compile.
[Numbers](numbers.md#overloading-the-operators) describes `Multiplicable` and `Addable`.

### What it costs at runtime

None of this survives to the bytecode. A `Quantity` is an opaque type over a `Double`, its
units exist only in the type, and every operation is `inline`. A dimensional calculation
therefore compiles to the same arithmetic instructions as the equivalent arithmetic on bare
`Double`s would, with no boxing, no wrapper objects, and no virtual dispatch to a typeclass.

That is a property worth checking, not merely asserting, and it is checked. The test suite
inspects the compiled bytecode of representative calculations, and fails if a typeclass
dispatch appears where inlined arithmetic should be. A guarantee about performance which is
only claimed tends to stop being true.

### Related

- [Numbers](numbers.md) describes the numeric types and the operators which quantities use.
- [Time](time.md#durations-and-timespans) measures a `Duration` as a quantity of seconds.
- [Mathematics](mathematics.md) carries units through vectors, matrices and complex numbers.
- [Money](money.md) applies the same idea, of a unit in the type, to currencies.
- [Geography](geography.md) and [Chemistry](chemistry.md) build on quantities.
