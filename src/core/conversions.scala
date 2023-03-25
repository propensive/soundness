package quantify

case class Ratio[UnitsType <: Units[?, ?]](value: Quantity[UnitsType])

trait Inches[Power <: Int & Singleton] extends Units[Power, Length]
object Inches:
  given inchesPerMetre: Ratio[Inches[1] & Metre[-1]] = Ratio(Quantity(39.3701))
  given Coefficient[Inches[1], Metre[1]](39.3701)

val Inch = Quantity[Inches[1]](1.0)

trait Feet[Power <: Int & Singleton] extends Units[Power, Length]
object Feet:
  given feetPerMetre: Ratio[Feet[1] & Metre[-1]] = Ratio(Quantity(3.28084))
  given Coefficient[Feet[1], Metre[1]](3.28084)

val Foot = Quantity[Feet[1]](1.0)

trait Yards[Power <: Int & Singleton] extends Units[Power, Length]
object Yards:
  given yardsPerMetre: Ratio[Yards[1] & Metre[-1]] = Ratio(Quantity(1.09361))
  given Coefficient[Yards[1], Metre[1]](1.09361)

val Yard = Quantity[Yards[1]](1.0)

trait Miles[Power <: Int & Singleton] extends Units[Power, Length]
object Miles:
  given milesPerMetre: Ratio[Miles[1] & Metre[-1]] = Ratio(Quantity(0.000621371))
  given Coefficient[Miles[1], Metre[1]](0.000621371)

val Mile = Quantity[Miles[1]](1.0)

trait Lightyears[Power <: Int & Singleton] extends Units[Power, Length]
object Lightyears:
  given lightYearsPerMetre: Ratio[Lightyears[1] & Metre[-1]] = Ratio(Quantity(1.057e-16))
  given Coefficient[Lightyears[1], Metre[1]](1.057e-16)

val Lightyear = Quantity[Lightyears[1]](1.0)