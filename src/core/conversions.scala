package quantify

case class Ratio[UnitsType <: Units[?, ?]](value: Quantity[UnitsType])

// Units of length

trait Inches[Power <: Int & Singleton] extends Units[Power, Length]
object Inches:
  given inchesPerMetre: Ratio[Inches[1] & Metre[-1]] = Ratio(Quantity(39.3701))

trait Feet[Power <: Int & Singleton] extends Units[Power, Length]
object Feet:
  given feetPerMetre: Ratio[Feet[1] & Metre[-1]] = Ratio(Quantity(3.28084))

trait Yards[Power <: Int & Singleton] extends Units[Power, Length]
object Yards:
  given yardsPerMetre: Ratio[Yards[1] & Metre[-1]] = Ratio(Quantity(1.09361))

trait Miles[Power <: Int & Singleton] extends Units[Power, Length]
object Miles:
  given milesPerMetre: Ratio[Miles[1] & Metre[-1]] = Ratio(Quantity(0.000621371))

trait Lightyears[Power <: Int & Singleton] extends Units[Power, Length]
object Lightyears:
  given lightYearsPerMetre: Ratio[Lightyears[1] & Metre[-1]] = Ratio(Quantity(1.057e-16))

trait NauticalMiles[Power <: Int & Singleton] extends Units[Power, Length]
object NauticalMiles:
  given nauticalMilesPerMetre: Ratio[NauticalMiles[1] & Metre[-1]] = Ratio(Quantity(1.0/1852.0))

trait Furlongs[Power <: Int & Singleton] extends Units[Power, Length]
object Furlongs:
  given metresPerFurlong: Ratio[Metre[1] & Furlongs[-1]] = Ratio(Quantity(201.168))

trait Chains[Power <: Int & Singleton] extends Units[Power, Length]
object Chains:
  given metresPerChain: Ratio[Metre[1] & Chains[-1]] = Ratio(Quantity(20.1168))

val Inch = Quantity[Inches[1]](1.0)
val Foot = Quantity[Feet[1]](1.0)
val Yard = Quantity[Yards[1]](1.0)
val Mile = Quantity[Miles[1]](1.0)
val Lightyear = Quantity[Lightyears[1]](1.0)
val NauticalMile = Quantity[NauticalMiles[1]](1.0)
val Furlong = Quantity[Furlongs[1]](1.0)
val Chain = Quantity[Chains[1]](1.0)

// Units of Area

val Acre = Furlong*Furlong/10.0

// Units of Volume

val Litre = Quantity[Metre[3]](0.001)
val Millilitre = Litre/1000.0
val FluidOunce = Millilitre*28.4130625
val Pint = Millilitre*568.26125
val Quart = Millilitre*1136.5225
val Gallon = Millilitre*4546.09

// Units of Mass

trait Grains[Power <: Int & Singleton] extends Units[Power, Mass]
object Grains:
  given grainsPerGram: Ratio[Grains[1] & Gram[-1]] = Ratio(Quantity(15.4324))

trait Ounces[Power <: Int & Singleton] extends Units[Power, Mass]
object Ounces:
  given ouncesPerGram: Ratio[Ounces[1] & Gram[-1]] = Ratio(Quantity(0.035274))

trait Pounds[Power <: Int & Singleton] extends Units[Power, Mass]
object Pounds:
  given gramsPerPound: Ratio[Gram[1] & Pounds[-1]] = Ratio(Quantity(453.59237))

trait Stones[Power <: Int & Singleton] extends Units[Power, Mass]
object Stones:
  given gramsPerStone: Ratio[Gram[1] & Stones[-1]] = Ratio(Quantity(6350.29318))

trait Hundredweights[Power <: Int & Singleton] extends Units[Power, Mass]
object Hundredweights:
  given gramsPerHundredweight: Ratio[Gram[1] & Hundredweights[-1]] = Ratio(Quantity(50802.34544))

trait Tons[Power <: Int & Singleton] extends Units[Power, Mass]
object Tons:
  given gramsPerTon: Ratio[Gram[1] & Tons[1]] = Ratio(Quantity(1016046.9088))

val Grain = Quantity[Grains[1]](1.0)
val Ounce = Quantity[Ounces[1]](1.0)
val Pound = Quantity[Pounds[1]](1.0)
val Stone = Quantity[Stones[1]](1.0)
val Hundredweight = Quantity[Hundredweights[1]](1.0)
val Ton = Quantity[Tons[1]](1.0)