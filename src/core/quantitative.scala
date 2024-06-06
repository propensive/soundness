package quantitative

import language.experimental.captureChecking

import anticipation.*
import gossamer.*
import hypotenuse.*
import rudiments.*
import symbolism.*

val Galileo = MetricUnit(0.01*Metre/(Second*Second))
val Poise = MetricUnit(0.1*Pascal)
val Franklin = MetricUnit(3.34e-10*Coulomb)
val Biot = MetricUnit(10*Ampere)
val Debye = MetricUnit(3.335e-30*Coulomb*Metre)
val Erg = MetricUnit(10e-7*Joule)
val Dyne = MetricUnit(10e-5*Newton)
val Calorie = MetricUnit(4.184*Joule)
val Langley = MetricUnit(41840*Joule/(Metre*Metre))
val Phot = MetricUnit(10e4*Lux)
val Stokes = MetricUnit(10e-4*Metre*Metre/Second)
val Lambert = MetricUnit((10e4/π)*Candela/(Metre*Metre))
val Emu = MetricUnit(10e-3*Ampere/(Metre*Metre))
val Oersted = MetricUnit(79.577*Ampere/Metre)
val Maxwell = MetricUnit(10e-8*Weber)
val Gauss = MetricUnit(10e-4*Tesla)
val Gilbert = MetricUnit(0.796*Ampere)
val Darcy = MetricUnit(0.987e-12*Metre*Metre)
val Barye = MetricUnit(0.1*Pascal)
val Kayser = MetricUnit(100/Second)

val Hertz = MetricUnit(1.0/Second)
val Newton = MetricUnit(Metre*Kilo(Gram)/(Second*Second))
val Pascal = MetricUnit(Newton/(Metre*Metre))
val Joule = MetricUnit(Newton*Metre)
val Watt = MetricUnit(Joule/Second)
val Coulomb = MetricUnit(Second*Ampere)
val Volt = MetricUnit(Watt/Ampere)
val Farad = MetricUnit(Coulomb/Volt)
val Ohm = MetricUnit(Volt/Ampere)
val Siemens = MetricUnit(Ampere/Volt)
val Weber = MetricUnit(Volt*Second)
val Tesla = MetricUnit(Weber/(Metre*Metre))
val Henry = MetricUnit(Weber/Ampere)
val Lux = MetricUnit(Candela/(Metre*Metre))
val Becquerel = MetricUnit(1.0/Second)
val Gray = MetricUnit(Joule/Kilo(Gram))
val Sievert = MetricUnit(Joule/Kilo(Gram))
val Katal = MetricUnit(Mole/Second)

extension [UnitsType <: Measure](inline quantity: Quantity[UnitsType])
  @targetName("plus")
  transparent inline infix def + [UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${Quantitative.add[UnitsType, UnitsType2]('quantity, 'quantity2, '{false})}

  @targetName("minus")
  transparent inline infix def - [UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${Quantitative.add[UnitsType, UnitsType2]('quantity, 'quantity2, '{true})}

  transparent inline def invert: Any = Quantity[Measure](1.0)/quantity

  transparent inline def in[UnitsType2[power <: Nat] <: Units[power, ?]]: Any =
    ${Quantitative.norm[UnitsType, UnitsType2]('quantity)}

  @targetName("times2")
  transparent inline infix def * [UnitsType2 <: Measure](inline quantity2: Quantity[UnitsType2])
          : Any =

    ${Quantitative.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, false)}

  @targetName("times3")
  transparent inline infix def * [UnitsType2 <: Measure](inline double: Double): Any =
    quantity*Quantity(double)

  @targetName("divide2")
  transparent inline infix def / [UnitsType2 <: Measure](inline quantity2: Quantity[UnitsType2])
          : Any =

    ${Quantitative.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, true)}

  @targetName("divide3")
  transparent inline infix def / [UnitsType2 <: Measure](inline double: Double): Any =
    quantity/Quantity(double)

  inline def sqrt(using root: RootOperator[2, Quantity[UnitsType]]): root.Result =
    root.root(quantity)

  inline def cbrt(using root: RootOperator[3, Quantity[UnitsType]]): root.Result =
    root.root(quantity)

  inline def units: Map[Text, Int] = ${Quantitative.collectUnits[UnitsType]}
  inline def render(using Decimalizer): Text = t"${quantity.value} ${Quantity.renderUnits(units)}"
  inline def dimension: Text = ${Quantitative.describe[UnitsType]}

extension (value: Double)
  @targetName("times")
  infix def * [UnitsType <: Measure](quantity: Quantity[UnitsType]): Quantity[UnitsType] =
    quantity*value

  @targetName("divide")
  transparent inline infix def / [UnitsType <: Measure](quantity: Quantity[UnitsType]): Any =
    ((1.0/value)*quantity).invert

package constants:
  val SpeedOfLightInVacuum = 299792458*Metre/Second
  val MagneticConstant = 4*π*(10e-7)*Newton/(Ampere*Ampere)
  val ElectricConstant = 8.854187817e-12*Farad/Metre
  val CharacteristicImpedanceOfVacuum = 376.730313461*Ohm
  val PlanckConstant = 6.62607015e-34*Metre*Metre*Kilo(Gram)/Second
  val GravitationalConstant = 6.67430e-11*Newton/Metre*Metre/Kilo(Gram)/Kilo(Gram)
  val ElementaryCharge = 1.602176634e-19*Coulomb
  val AvogadroConstant = 6.02214076e23/Mole
  val BoltzmannConstant = 1.380649e-13*Joule/Kelvin

val Metre: MetricUnit[Metres[1]] = MetricUnit(1)
val Gram: MetricUnit[Kilograms[1]] = MetricUnit(0.001)
val Candela: MetricUnit[Candelas[1]] = MetricUnit(1)
val Mole: MetricUnit[Moles[1]] = MetricUnit(1)
val Ampere: MetricUnit[Amperes[1]] = MetricUnit(1)
val Kelvin: MetricUnit[Kelvins[1]] = MetricUnit(1)
val Second: MetricUnit[Seconds[1]] = MetricUnit(1)
val Radian: MetricUnit[Radians[1]] = MetricUnit(1)

val Inch = Quantity[Inches[1]](1.0)
val Foot = Quantity[Feet[1]](1.0)
val Yard = Quantity[Yards[1]](1.0)
val Mile = Quantity[Miles[1]](1.0)
val Lightyear = Quantity[Lightyears[1]](1.0)
val NauticalMile = Quantity[NauticalMiles[1]](1.0)
val Furlong = Quantity[Furlongs[1]](1.0)
val Chain = Quantity[Chains[1]](1.0)

val Grain = Quantity[Grains[1]](1.0)
val Ounce = Quantity[Ounces[1]](1.0)
val Pound = Quantity[Pounds[1]](1.0)
val Stone = Quantity[Stones[1]](1.0)
val Hundredweight = Quantity[Hundredweights[1]](1.0)
val Ton = Quantity[Tons[1]](1.0)

val Day = Quantity[Days[1]](1.0)
val Hour = Quantity[Hours[1]](1.0)
val Minute = Quantity[Minutes[1]](1.0)

// Units of Area

val Are = MetricUnit[Metres[2]](100.0)
val Acre = Furlong*Furlong/10.0

// Units of Volume

val Litre = MetricUnit[Metres[3]](0.001)
val FluidOunce = Milli(Litre)*28.4130625
val Pint = Milli(Litre)*568.26125
val Quart = Milli(Litre)*1136.5225
val Gallon = Milli(Litre)*4546.09
