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
erased trait SubstanceAmount extends Dimension

trait Units[PowerType <: Int & Singleton, DimensionType <: Dimension]

erased trait Metre[Power <: Int & Singleton] extends Units[Power, Length]
erased trait Kilogram[Power <: Int & Singleton] extends Units[Power, Mass]
erased trait Candela[Power <: Int & Singleton] extends Units[Power, Luminosity]
erased trait Mole[Power <: Int & Singleton] extends Units[Power, SubstanceAmount]
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

trait UnitName[-ValueType <: Units[?, ?]]:
  def name(): String

object Show:
  given UnitName[Metre[?]] = () => "m"
  given UnitName[Kilogram[?]] = () => "kg"
  given UnitName[Candela[?]] = () => "cd"
  given UnitName[Mole[?]] = () => "mol"
  given UnitName[Ampere[?]] = () => "A"
  given UnitName[Kelvin[?]] = () => "K"
  given UnitName[Second[?]] = () => "s"

object Coefficient:
  given Coefficient[Metre[1], Inch[1]](39.3701)
  given Coefficient[Inch[1], Metre[1]](1.0/39.3701)

trait Coefficient[FromType <: Units[1, ?], ToType <: Units[1, ?]](val value: Double)

case class Quantity[UnitsType <: Units[?, ?]](value: Double):

  @targetName("plus")
  def +(amount2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(value + amount2.value)
  
  @targetName("minus")
  def -(amount2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(value - amount2.value)
  
  @targetName("times")
  def *(value2: Double): Quantity[UnitsType] = Quantity(value*value2)
  
  @targetName("times")
  transparent inline def *[UnitsType2 <: Units[?, ?]](inline amount2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('this, 'amount2)}
  
  @targetName("divide")
  def /(value2: Double): Quantity[UnitsType] = Quantity(value/value2)

  @targetName("divide")
  transparent inline def /[UnitsType2 <: Units[?, ?]](inline amount2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.divide[UnitsType, UnitsType2]('this, 'amount2)}

extension (value: Double)
  @targetName("times")
  def *[UnitsType <: Units[?, ?]](quantity: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(value)
  