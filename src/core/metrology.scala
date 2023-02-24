package quantify

import scala.quoted.*
import annotation.targetName

trait Measurement

trait Length extends Measurement
trait Mass extends Measurement
trait TimeMeasurement extends Measurement
trait Current extends Measurement
trait Luminosity extends Measurement
trait Temperature extends Measurement
trait SubstanceAmount extends Measurement

trait Units[PowerType <: Int & Singleton, MeasurementType <: Measurement]

trait Inch[Power <: Int & Singleton] extends Units[Power, Length]
object Inch extends Quantity[Inch[1]](1)

trait Metre[Power <: Int & Singleton] extends Units[Power, Length]
trait Kilogram[Power <: Int & Singleton] extends Units[Power, Mass]
trait Candela[Power <: Int & Singleton] extends Units[Power, Luminosity]
trait Mole[Power <: Int & Singleton] extends Units[Power, SubstanceAmount]
trait Ampere[Power <: Int & Singleton] extends Units[Power, Current]
trait Kelvin[Power <: Int & Singleton] extends Units[Power, Temperature]
trait Second[Power <: Int & Singleton] extends Units[Power, TimeMeasurement]

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
  given Coefficient[Inch[1], Metre[1]](39.3701)

trait Coefficient[FromType <: Units[1, ?], ToType <: Units[1, ?]](val value: Double)

case class Quantity[UnitsType <: Units[?, ?]](value: Double):
  @targetName("plus")
  def +(amount2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(value + amount2.value)
  
  @targetName("minus")
  def -(amount2: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(value - amount2.value)
  
  @targetName("times")
  def *(value2: Double): Quantity[UnitsType] = Quantity(value*value2)
  
  @targetName("divide")
  def /(value2: Double): Quantity[UnitsType] = Quantity(value/value2)

extension (value: Double)
  def *[UnitsType <: Units[?, ?]](quantity: Quantity[UnitsType]): Quantity[UnitsType] = Quantity(value)

extension [UnitsType <: Units[?, ?]](inline amount: Quantity[UnitsType])
  @targetName("times")
  transparent inline def *[UnitsType2 <: Units[?, ?]](inline amount2: Quantity[UnitsType2]): Quantity[?] =
    ${Quantity.multiply[UnitsType, UnitsType2]('amount, 'amount2)}
  
  @targetName("divide")
  transparent inline def /[UnitsType2 <: Units[?, ?]](inline amount2: Quantity[UnitsType2]): Quantity[?] =
    ${Quantity.divide[UnitsType, UnitsType2]('amount, 'amount2)}

object Quantity:
  def multiply
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (left: Expr[Quantity[LeftType]], right: Expr[Quantity[RightType]])(using Quotes)
      : Expr[Quantity[?]] =
    operation[LeftType, RightType](left, right)(_ + _)('{ (a: Double, b: Double) => a*b })
  
  def divide
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (left: Expr[Quantity[LeftType]], right: Expr[Quantity[RightType]])(using Quotes)
      : Expr[Quantity[?]] =
    operation[LeftType, RightType](left, right)(_ - _)('{ (a: Double, b: Double) => a/b })

  private def operation
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (left: Expr[Quantity[LeftType]], right: Expr[Quantity[RightType]])
      (combine: (Int, Int) => Int)
      (ap: Expr[(Double, Double) => Double])(using Quotes)
      : Expr[Quantity[?]] =
    import quotes.*, reflect.*
    
    def deconstruct(typ: TypeRepr): Map[TypeRef, (TypeRef, Int)] = typ match
      case AndType(AppliedType(unit@TypeRef(_, _), List(ConstantType(IntConstant(power)))), remainder) =>
        unit.asType match
          case '[ Units[_, u] ] => TypeRepr.of[u] match
            case ref@TypeRef(_, _) => deconstruct(remainder).updated(ref, (unit, power))
      
      case AppliedType(unit@TypeRef(_, _), List(ConstantType(IntConstant(power)))) =>
        unit.asType match
          case '[ Units[p, u] ] => TypeRepr.of[u] match
            case ref@TypeRef(_, _) => Map(ref -> (unit, power))

    val rightMap: Map[TypeRef, (TypeRef, Int)] = deconstruct(TypeRepr.of[RightType])
    val leftMap: Map[TypeRef, (TypeRef, Int)] = deconstruct(TypeRepr.of[LeftType])

    def recur
        (map: Map[TypeRef, (TypeRef, Int)], todo: List[(TypeRef, (TypeRef, Int))], multiplier: Expr[Double])
        : (Map[TypeRef, (TypeRef, Int)], Expr[Double]) =
      todo match
        case Nil => (map, multiplier)
        case (measurement, (unit, power)) :: todo2 =>
          map.get(measurement) match
            case None     => recur(map.updated(measurement, (unit, combine(0, power))), todo2, multiplier)
            case Some((unit2, power2)) =>
              AppliedType(unit, List(ConstantType(IntConstant(1)))).asType match
                case '[ t1 ] => AppliedType(unit2, List(ConstantType(IntConstant(1)))).asType match
                  case '[ t2 ] => 
                    val multiplier2: Expr[Double] =
                      if unit == unit2 then multiplier else
                        val coefficient = Expr.summon[Coefficient[t1 & Units[1, ?], t2 & Units[1, ?]]].get
                    
                        '{$coefficient.value*$multiplier}
                    
                    val power3 = combine(power, power2)
                    recur(map.updated(measurement, (unit, power3)), todo2, multiplier2)
  
      
    val (map, multiplier) = recur(leftMap, rightMap.to(List), Expr(1.0))


    def construct(types: List[(TypeRef, Int)]): TypeRepr = types.filter(_(1) != 0) match
      case (ref, power) :: Nil => AppliedType(ref, List(ConstantType(IntConstant(power))))
      case (ref, power) :: more => AndType(AppliedType(ref, List(ConstantType(IntConstant(power)))), construct(more))

    construct(map.values.to(List)).asType match
      case '[ t ] =>
        '{Quantity[t & Units[?, ?]]($multiplier*$ap($left.value, $right.value))}
