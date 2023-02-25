package quantify

import gossamer.*
import rudiments.*

import scala.quoted.*
import annotation.targetName

trait Dimension

erased trait Length extends Dimension
erased trait Mass extends Dimension
erased trait TimeLength extends Dimension
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
erased trait Second[Power <: Int & Singleton] extends Units[Power, TimeLength]

object Metre extends Quantity[Metre[1]](1)
object Kilogram extends Quantity[Kilogram[1]](1)
object Candela extends Quantity[Candela[1]](1)
object Mole extends Quantity[Mole[1]](1)
object Ampere extends Quantity[Ampere[1]](1)
object Kelvin extends Quantity[Kelvin[1]](1)
object Second extends Quantity[Second[1]](1)

trait UnitName[-ValueType]:
  def name(): Text

object UnitName:
  given UnitName[Metre[1]] = () => t"m"
  given UnitName[Kilogram[1]] = () => t"kg"
  given UnitName[Candela[1]] = () => t"cd"
  given UnitName[Mole[1]] = () => t"mol"
  given UnitName[Ampere[1]] = () => t"A"
  given UnitName[Kelvin[1]] = () => t"K"
  given UnitName[Second[1]] = () => t"s"

object Coefficient:
  given Coefficient[Metre[1], Inch[1]](39.3701)
  given Coefficient[Inch[1], Metre[1]](1.0/39.3701)

trait Coefficient[FromType <: Units[1, ?], ToType <: Units[1, ?]](val value: Double)

trait PrincipalUnit[DimensionType <: Dimension, UnitType <: Units[1, DimensionType]]()
object PrincipalUnit:
  given PrincipalUnit[Length, Metre[1]]()
  given PrincipalUnit[Mass, Kilogram[1]]()
  given PrincipalUnit[TimeLength, Second[1]]()
  given PrincipalUnit[Current, Ampere[1]]()
  given PrincipalUnit[Luminosity, Candela[1]]()
  given PrincipalUnit[Temperature, Kelvin[1]]()
  given PrincipalUnit[AmountOfSubstance, Mole[1]]()

object Opaques:
  opaque type Quantity2[UnitsType <: Units[?, ?]] = Double

object Quantity:
  inline given [UnitsType <: Units[?, ?]](using DecimalFormat): Show[Quantity[UnitsType]] =
    new Show[Quantity[UnitsType]]:
      def show(value: Quantity[UnitsType]): Text = value.render

class Quantity[UnitsType <: Units[?, ?]](val value: Double):
  quantity =>
  
  @targetName("times")
  def *(value2: Double): Quantity[UnitsType] = Quantity(quantity.value*value2)
  
  @targetName("divide")
  def /(value2: Double): Quantity[UnitsType] = Quantity(quantity.value/value2)

  @targetName("plus")
  inline def +(quantity2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(quantity.value + quantity2.value)
  
  @targetName("minus")
  inline def -(quantity2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(quantity.value - quantity2.value)
  
  @targetName("times2")
  transparent inline def *[UnitsType2 <: Units[?, ?]](inline quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, false)}
  
  @targetName("divide2")
  transparent inline def /[UnitsType2 <: Units[?, ?]](inline quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, true)}

  inline def units: Map[Text, Int] = ${QuantifyMacros.collectUnits[UnitsType]}

  inline def render(using DecimalFormat): Text = t"${quantity.value}$renderUnits"

  inline def renderUnits: Text =
    units.to(List).map: (unit, power) =>
      if power == 1 then unit
      else 
        val exponent: Text =
          power.show.mapChars:
            case '0' => '⁰'
            case '1' => '¹'
            case '2' => '²'
            case '3' => '³'
            case '4' => '⁴'
            case '5' => '⁵'
            case '6' => '⁶'
            case '7' => '⁷'
            case '8' => '⁸'
            case '9' => '⁹'
            case '-' => '⁻'
            case _   => ' '
        
        t"$unit$exponent"
    .join(t"·")

extension (value: Double)
  @targetName("times")
  def *[UnitsType <: Units[?, ?]](quantity: Quantity[UnitsType]): Quantity[UnitsType] = quantity*value
  
  // @targetName("divide")
  // def /[UnitsType <: Units[?, ?]](quantity: Quantity[UnitsType]): Quantity[UnitsType] = quantity.invert*value
  