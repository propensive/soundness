package quantify

import scala.quoted.*
import annotation.targetName

trait Dimension

erased trait Length extends Dimension
erased trait Mass extends Dimension
erased trait TimeMeasurement extends Dimension
erased trait Current extends Dimension
erased trait Luminosity extends Dimension
erased trait Temperature extends Dimension
erased trait AmountOfSubstance extends Dimension

trait Units[PowerType <: Int & Singleton, DimensionType <: Dimension]

erased trait Metre[Power <: Int & Singleton] extends Units[Power, Length]
erased trait Kilogram[Power <: Int & Singleton] extends Units[Power, Mass]
erased trait Candela[Power <: Int & Singleton] extends Units[Power, Luminosity]
erased trait Mole[Power <: Int & Singleton] extends Units[Power, AmountOfSubstance]
erased trait Ampere[Power <: Int & Singleton] extends Units[Power, Current]
erased trait Kelvin[Power <: Int & Singleton] extends Units[Power, Temperature]
erased trait Second[Power <: Int & Singleton] extends Units[Power, TimeMeasurement]

object Metre extends Quantity[Metre[1]](1)
object Kilogram extends Quantity[Kilogram[1]](1)
object Candela extends Quantity[Candela[1]](1)
object Mole extends Quantity[Mole[1]](1)
object Ampere extends Quantity[Ampere[1]](1)
object Kelvin extends Quantity[Kelvin[1]](1)
object Second extends Quantity[Second[1]](1)

trait UnitName[-ValueType]:
  def name(): String

object UnitName:
  given UnitName[Metre[1]] = () => "m"
  given UnitName[Kilogram[1]] = () => "kg"
  given UnitName[Candela[1]] = () => "cd"
  given UnitName[Mole[1]] = () => "mol"
  given UnitName[Ampere[1]] = () => "A"
  given UnitName[Kelvin[1]] = () => "K"
  given UnitName[Second[1]] = () => "s"

object Coefficient:
  given Coefficient[Metre[1], Inch[1]](39.3701)
  given Coefficient[Inch[1], Metre[1]](1.0/39.3701)

trait Coefficient[FromType <: Units[1, ?], ToType <: Units[1, ?]](val value: Double)

trait PrincipalUnit[DimensionType <: Dimension, UnitType <: Units[1, DimensionType]]()
object PrincipalUnit:
  given PrincipalUnit[Length, Metre[1]]()
  given PrincipalUnit[Mass, Kilogram[1]]()
  given PrincipalUnit[TimeMeasurement, Second[1]]()
  given PrincipalUnit[Current, Ampere[1]]()
  given PrincipalUnit[Luminosity, Candela[1]]()
  given PrincipalUnit[Temperature, Kelvin[1]]()
  given PrincipalUnit[AmountOfSubstance, Mole[1]]()

object Quantity

case class Quantity[UnitsType <: Units[?, ?]](value: Double):
  @targetName("plus")
  def +(amount2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(value + amount2.value)
  
  @targetName("minus")
  def -(amount2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(value - amount2.value)
  
  @targetName("times")
  def *(value2: Double): Quantity[UnitsType] = Quantity(value*value2)
  
  @targetName("times")
  transparent inline def *[UnitsType2 <: Units[?, ?]](inline amount2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('this, 'amount2, false)}
  
  @targetName("divide")
  def /(value2: Double): Quantity[UnitsType] = Quantity(value/value2)

  @targetName("divide")
  transparent inline def /[UnitsType2 <: Units[?, ?]](inline amount2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('this, 'amount2, true)}

  inline def units: Map[String, Int] = ${QuantifyMacros.collectUnits[UnitsType]}
  
  inline def renderUnits: String =
    units.map: (unit, power) =>
      if power == 1 then unit
      else unit+power.toString.map:
        case '0' => "⁰"
        case '1' => "¹"
        case '2' => "²"
        case '3' => "³"
        case '4' => "⁴"
        case '5' => "⁵"
        case '6' => "⁶"
        case '7' => "⁷"
        case '8' => "⁸"
        case '9' => "⁹"
        case '-' => "⁻"
      .mkString("")
    .mkString("·")

extension (value: Double)
  @targetName("times")
  def *[UnitsType <: Units[?, ?]](quantity: Quantity[UnitsType]): Quantity[UnitsType] = quantity*value
  
  // @targetName("divide")
  // def /[UnitsType <: Units[?, ?]](quantity: Quantity[UnitsType]): Quantity[UnitsType] = quantity.invert*value
  