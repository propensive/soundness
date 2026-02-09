                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package quantitative

import anticipation.*
import fulminate.*
import hypotenuse.*
import prepositional.*
import probably.*
import proscenium.*
import rudiments.*
import symbolism.*

import scala.quoted.*
import scala.compiletime.*

trait Quantitative2:
  private given realm: Realm = realm"quantitative"

  case class UnitPower(ref: UnitRef, power: Int)


  trait Temperature2:
    inline given orderable: Temperature is Commensurable:
      type Contrast = Temperature

      inline def compare
        ( inline left:    Temperature,
          inline right:   Temperature,
          inline strict:  Boolean,
          inline greater: Boolean )
      : Boolean =

          !strict && left.kelvin == right.kelvin || (left.kelvin < right.kelvin) ^ greater



  private object UnitsMap:
    def apply[measure <: Measure: Type](using Quotes): UnitsMap =
      import quotes.reflect.*

      def recur(repr: TypeRepr): Map[DimensionRef, UnitPower] =
        repr.asMatchable match
          case AndType(left, right) =>
            recur(left) ++ recur(right)

          case AppliedType(_, List(_)) =>
            val unitPower = readUnitPower(repr)
            Map(unitPower.ref.dimensionRef -> unitPower)

          case other =>
            Map()

      new UnitsMap(recur(quotes.reflect.TypeRepr.of[measure]))

  case class Dimensionality(map: Map[DimensionRef, Int]):
    def quantityName(using Quotes): Option[String] =
      import quotes.reflect.*
      def recur(todo: List[(DimensionRef, Int)], current: TypeRepr): TypeRepr = todo match
        case Nil =>
          current

        case (dimension, n) :: tail =>
          (current.asType, dimension.power(n).asType).absolve match
            case ('[current], '[next]) => recur(tail, TypeRepr.of[current & next])

      recur(map.to(List), TypeRepr.of[Units[?, ?]]).asType.absolve match
        case '[type units <: Units[?, ?]; units] =>
          Expr.summon[Amount[units, ?]].map: value =>
            value.absolve match
              case '{$name: dimension} => Type.of[dimension].absolve match
                case '[Amount[?, name]] =>
                  TypeRepr.of[name].asMatchable.absolve match
                    case ConstantType(StringConstant(name)) => name

  def readUnitPower(using Quotes)(typeRepr: quotes.reflect.TypeRepr): UnitPower =
    import quotes.reflect.*

    typeRepr.asMatchable.absolve match
      case AppliedType(unit, List(constantType)) => constantType.asMatchable.absolve match
        case ConstantType(constant) => constant.absolve match
          case IntConstant(power) => unit.asMatchable.absolve match
            case unit@TypeRef(_, _) =>
              UnitPower(UnitRef(unit.asType, unit.show), power)

  case class UnitsMap(map: Map[DimensionRef, UnitPower]):
    def repr(using Quotes): Option[quotes.reflect.TypeRepr] = construct(map.values.to(List))

    def inverseMap: Map[DimensionRef, UnitPower] =
      map.view.mapValues { case UnitPower(unit, power) => UnitPower(unit, -power) }.to(Map)

    def dimensionality: Dimensionality = Dimensionality(map.view.mapValues(_.power).to(Map))
    def dimensions: List[DimensionRef] = map.keys.to(List)
    def empty: Boolean = map.values.all(_.power == 0)

    @targetName("multiply")
    infix def * (that: UnitsMap): UnitsMap =
      new UnitsMap
           ((dimensions ++ that.dimensions).to(Set).to(List).map: dim =>
              val dimUnit = unit(dim).orElse(that.unit(dim)).get
              dim -> UnitPower(dimUnit, (unitPower(dim) + that.unitPower(dim)))

            . to(Map).filter(_(1).power != 0))

    @targetName("divide")
    infix def / (that: UnitsMap): UnitsMap =
      new UnitsMap
           ((dimensions ++ that.dimensions).to(Set).to(List).map: dim =>
              val dimUnit = unit(dim).orElse(that.unit(dim)).get
              dim -> UnitPower(dimUnit, (unitPower(dim) - that.unitPower(dim)))

            . to(Map).filter(_(1).power != 0))

    def construct(using Quotes)(types: List[UnitPower]): Option[quotes.reflect.TypeRepr] =
      import quotes.reflect.*

      types.filter(_.power != 0) match
        case Nil =>
          None

        case UnitPower(unit, power) :: Nil =>
          Some(AppliedType(unit.ref, List(ConstantType(IntConstant(power)))))

        case UnitPower(unit, power) :: more =>
          Some(AndType(AppliedType(unit.ref, List(ConstantType(IntConstant(power)))),
              construct(more).get))

    def sub(dimension: DimensionRef, unit: UnitRef, power: Int): UnitsMap =
      new UnitsMap(map.updated(dimension, UnitPower(unit, power)))

    def unit(dimension: DimensionRef): Option[UnitRef] = map.get(dimension).map(_.ref)
    def unitPower(dimension: DimensionRef): Int = map.get(dimension).map(_.power).getOrElse(0)

  class UnitRef(val unitType: Type[?], val name: String):
    def designation: Macro[Text] = power(1).asType.absolve match
      case '[unit]      => Expr.summon[Designation[unit]] match
        case None       => '{${Expr(name)}.tt}
        case Some(name) => '{$name.text}

    def ref(using Quotes): quotes.reflect.TypeRepr =
      unitType.absolve match { case '[ref] => quotes.reflect.TypeRepr.of[ref] }

    def dimensionRef(using Quotes): DimensionRef =
      import quotes.reflect.*

      unitType.absolve match
        case '[Units[power, unit]] => TypeRepr.of[unit].asMatchable.absolve match
          case ref@TypeRef(_, _) => DimensionRef(ref.asType, ref.show)

    def power(n: Int)(using Quotes): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      AppliedType(ref, List(ConstantType(IntConstant(n))))

    override def equals(that: Any): Boolean = that.asMatchable match
      case that: UnitRef => name == that.name
      case _             => false

    override def hashCode: Int = name.hashCode
    override def toString(): String = name

  class DimensionRef(val dimensionType: Type[?], val name: String):
    def ref(using Quotes): quotes.reflect.TypeRepr =
      dimensionType.absolve match
        case '[ref] => quotes.reflect.TypeRepr.of[ref]

    def dimensionality(using Quotes): Dimensionality = Dimensionality(Map(this -> 1))

    def power(n: Int)(using Quotes): quotes.reflect.TypeRepr =
      import quotes.reflect.*

      (ConstantType(IntConstant(n)).asType, ref.asType).absolve match
        case ('[type power <: Nat; power], '[type dimension <: Dimension; dimension]) =>
          TypeRepr.of[Units[power, dimension]]

    def principal(using Quotes): UnitRef =
      import quotes.reflect.*

      dimensionType.absolve match
        case '[type dimension <: Dimension; dimension] =>
          Expr.summon[Principal[dimension, ?]].absolve match
            case None =>
              val dimensionName =
                dimensionality.quantityName.map: name =>
                  "the physical quantity "+name

                . getOrElse("the same quantity")

              halt:
                m"""the operands both represent $dimensionName, but there is no principal unit
                      specified for this dimension"""

            case Some('{$expr: principal}) => Type.of[principal].absolve match
              case '[Principal[dimension, units]] =>
                TypeRepr.of[units].asMatchable.absolve match
                  case TypeLambda(_, _, appliedType) => appliedType.asMatchable.absolve match
                    case AppliedType(typeRef, _) => typeRef.asMatchable.absolve match
                      case typeRef@TypeRef(_, _) => UnitRef(typeRef.asType, typeRef.show)

                  case other =>
                    halt(m"principal units had an unexpected type: ${other.show}")


    override def equals(that: Any): Boolean = that.asMatchable match
      case that: DimensionRef => name == that.name
      case _                      => false

    override def hashCode: Int = name.hashCode
    override def toString(): String = name

  def normalizable[source <: Measure: Type, result <: Measure: Type]
  : Macro[source is Normalizable to result] =

      import quotes.reflect.*

      val sourceUnits = UnitsMap[source]
      val resultUnits = UnitsMap[result]

      if sourceUnits.dimensionality != resultUnits.dimensionality
      then incompatibleTypes(sourceUnits, resultUnits)

      val ratio = normalize(resultUnits, sourceUnits, Expr(1.0))(1)
      val ratio2 = normalize(sourceUnits, resultUnits, Expr(1.0))(1)

      '{() => $ratio/$ratio2}


  def ratio
    ( from: UnitRef, to: UnitRef, power: Int, retry: Boolean = true, viaPrincipal: Boolean = true )
  : Macro[Double] =

      import quotes.reflect.*

      val principal = from.dimensionRef.principal
      if from == to then Expr(1.0)
      else (from.power(-1).asType, to.power(1).asType).absolve match
        case ('[type from <: Measure; from], '[type to <: Measure; to]) =>
          Expr.summon[Ratio[from & to, ?]].absolve match
            case None =>
              if retry then ratio(to, from, -power, false)
              else if viaPrincipal && from != principal && to != principal then
                val numerator = ratio(from, principal, power, true, false)
                val denominator = ratio(to, principal, power, true, false)
                '{$numerator/$denominator}
              else
                val quantityName = from.dimensionRef.dimensionality.quantityName

                val dimensionName = quantityName.map("the physical quantity "+_).getOrElse:
                    "the same physical quantity"

                halt:
                  m"""both operands represent $dimensionName, but the coversion ratio between them
                        is not known

                        To provide the conversion ratio, please provide a contextual instance in
                        scope, with the type, `Ratio[${from.name}[1] & ${to.name}[-1]]`, or
                        `Ratio[${to.name}[1] & ${from.name}[-1]]`."""

            case Some('{$ratio: ratio}) => Type.of[ratio].absolve match
              case '[Ratio[?, double]] => TypeRepr.of[double].asMatchable.absolve match
                case ConstantType(constant) => constant.absolve match
                  case DoubleConstant(double) => Expr(double**power)


  private def normalize(using Quotes)
    ( units: UnitsMap, other: UnitsMap, init: Expr[Double], force: Boolean = false )
  : (UnitsMap, Expr[Double]) =

      def recur(dimensions: List[DimensionRef], target: UnitsMap, expr: Expr[Double])
      : (UnitsMap, Expr[Double]) =

        dimensions match
          case Nil =>
            (target, expr)

          case dimension :: dimensions =>
            if other.unitPower(dimension) == 0 || units.unit(dimension) == other.unit(dimension)
            then recur(dimensions, target, expr)
            else
              val unit = target.unit(dimension).get
              val power = target.unitPower(dimension)

              val unit2 =
                if force then other.unit(dimension).orElse(target.unit(dimension)).get
                else dimension.principal

              val value = '{$expr*${ratio(unit, unit2, power)}}
              recur(dimensions, target.sub(dimension, unit2, power), value)

      recur(units.dimensions, units, init)

  def collectUnits[units <: Measure: Type]: Macro[Map[Text, Int]] =
    def recur(expr: Expr[Map[Text, Int]], todo: List[UnitPower]): Expr[Map[Text, Int]] =
      todo match
        case Nil =>
          expr

        case UnitPower(unit, power) :: todo2 =>
          unit.power(1).asType.absolve match
            case '[ref] =>
              val designation = Expr.summon[Designation[ref]].get
              recur('{$expr.updated($designation.text, ${Expr(power)})}, todo2)

    Expr.summon[Redesignation[units]].absolve match
      case Some('{$redesignation: Redesignation[?]}) =>
        '{Map[Text, Int](($redesignation.name -> 1))}

      case None =>
        recur('{Map[Text, Int]()}, UnitsMap[units].map.values.to(List))


  def multiply[left <: Measure: Type, right <: Measure: Type]
    ( leftExpr: Expr[Quantity[left]], rightExpr: Expr[Quantity[right]], division: Boolean )
  : Macro[Any] =

      val left: UnitsMap = UnitsMap[left]
      val right: UnitsMap = UnitsMap[right]

      val (left2, leftValue) = normalize(left, right, '{$leftExpr.underlying})
      val (right2, rightValue) = normalize(right, left, '{$rightExpr.underlying})

      val resultUnits = if division then left2/right2 else left2*right2
      val resultValue = if division then '{$leftValue/$rightValue} else '{$leftValue*$rightValue}

      resultUnits.repr.map(_.asType).absolve match
        case Some('[type units <: Measure; units]) => '{Quantity[units]($resultValue)}
        case _                                     => resultValue

  def amount[units <: Measure: Type]: Macro[Text] =
    val amount = UnitsMap[units].dimensionality.quantityName.getOrElse:
      halt(m"the name of this amount is not known")

    '{${Expr(amount)}.tt}

  private def incompatibleTypes(left: UnitsMap, right: UnitsMap)(using Quotes): Nothing =
    (left.dimensionality.quantityName, right.dimensionality.quantityName) match
      case (Some(leftName), Some(rightName)) =>
        halt:
          m"""the left operand represents $leftName, but the right operand represents $rightName;
              these are incompatible physical quantities"""

      case _ =>
        halt(m"the operands represent different physical quantities")

  private def incompatibleTypeText(left: UnitsMap, right: UnitsMap)(using Quotes): String =
    (left.dimensionality.quantityName, right.dimensionality.quantityName) match
      case (Some(leftName), Some(rightName)) =>
        "quantitative: the left operand represents "+leftName+", but the right operand represents "+
        rightName+"; these are incompatible physical quantities"

      case _ =>
        "quantitative: the operands represent different physical quantities"


  def mulTypeclass
    [ left         <: Measure:         Type,
      multiplicand <: Quantity[left]:  Type,
      right        <: Measure:         Type,
      multiplier   <: Quantity[right]: Type ]
  : Macro[multiplicand is Multiplicable by multiplier] =

      val left = UnitsMap[left]
      val right = UnitsMap[right]

      val (leftNorm, _) = normalize(left, right, '{1.0})
      val (rightNorm, _) = normalize(right, left, '{1.0})

      (leftNorm*rightNorm).repr.map(_.asType).absolve match
        case Some('[type result <: Measure; result]) =>
          ' {
              Multiplicable[multiplicand, multiplier, Quantity[result]]:
                (left, right) =>
                  ${Quantitative.multiply('left, 'right, false).asExprOf[Quantity[result]]}
            }

        case None =>
          ' {
              Multiplicable[multiplicand, multiplier, Double]: (left, right) =>
                ${Quantitative.multiply('left, 'right, false).asExprOf[Double]}
            }


  def divTypeclass
    [ left     <: Measure:         Type,
      dividend <: Quantity[left]:  Type,
      right    <: Measure:         Type,
      divisor  <: Quantity[right]: Type ]
  : Macro[dividend is Divisible by divisor] =

      val left = UnitsMap[left]
      val right = UnitsMap[right]

      val (leftNorm, _) = normalize(left, right, '{1.0})
      val (rightNorm, _) = normalize(right, left, '{1.0})

      (leftNorm/rightNorm).repr.map(_.asType).absolve match
        case Some('[type result <: Measure; result]) =>
          ' {
              Divisible[dividend, divisor, Quantity[result]]:
                (left, right) =>
                  ${Quantitative.multiply('left, 'right, true).asExprOf[Quantity[result]]}
            }

        case None =>
          ' {
              Divisible[dividend, divisor, Double]: (left, right) =>
                ${Quantitative.multiply('left, 'right, true).asExprOf[Double]}
            }


  def divTypeclass2[right <: Measure: Type, divisor <: Quantity[right]: Type]
  : Macro[Double is Divisible by divisor] =

      val left = UnitsMap(Map())
      val right = UnitsMap[right]

      val (leftNorm, _) = normalize(left, right, '{1.0})
      val (rightNorm, _) = normalize(right, left, '{1.0})

      (leftNorm/rightNorm).repr.map(_.asType).absolve match
        case Some('[type result <: Measure; result]) =>
          ' {
              Divisible[Double, divisor, Quantity[result]]:
                (left, right) =>
                  $ {
                      Quantitative.multiply
                        ('{Quantity(left)}, 'right, true).asExprOf[Quantity[result]]
                    }
            }

        case None =>
          ' {
              Divisible[Double, divisor, Double]: (left, right) =>
                ${Quantitative.multiply('{Quantity(left)}, 'right, true).asExprOf[Double]}
            }


  def divTypeclass3[right <: Measure: Type, divisor <: Quantity[right]: Type]
  : Macro[Int is Divisible by divisor] =

      val left = UnitsMap(Map())
      val right = UnitsMap[right]

      val (leftNorm, _) = normalize(left, right, '{1.0})
      val (rightNorm, _) = normalize(right, left, '{1.0})

      (leftNorm/rightNorm).repr.map(_.asType).absolve match
        case Some('[type result <: Measure; result]) =>
          ' {
              Divisible[Int, divisor, Quantity[result]]:
                (left, right) =>
                  $ {
                      Quantitative.multiply
                       ('{Quantity(left.toDouble)}, 'right, true).asExprOf[Quantity[result]]
                    }
            }

        case None =>
          ' {
              Divisible[Int, divisor, Double]: (left, right) =>
                ${Quantitative.multiply('{Quantity(left.toDouble)}, 'right, true).asExprOf[Double]}
            }


  def sqrtTypeclass[value <: Measure: Type]: Macro[Quantity[value] is Rootable[2]] =
    val units = UnitsMap[value]

    if !units.map.values.all(_.power%2 == 0)
    then halt(m"only quantities with units in even powers can have square roots calculated")
    else
      val unitsType =
        UnitsMap:
          units.map.view.mapValues { case UnitPower(unit, power) => UnitPower(unit, power/2) }.toMap

        . repr.get.asType

      unitsType.absolve match
        case '[type result <: Measure; result] =>
          val sqrt = '{ (value: Quantity[value]) => Quantity[result](math.sqrt(value.value)) }
          val cast = sqrt.asExprOf[Quantity[value] => Quantity[result]]

          '{Rootable[2, Quantity[value], Quantity[result]]($cast(_))}

  def cbrtTypeclass[value <: Measure: Type](using Quotes): Expr[Quantity[value] is Rootable[3]] =

    val units = UnitsMap[value]

    if !units.map.values.all(_.power%3 == 0)
    then halt:
      m"only quantities with units whose powers are multiples of 3 can have cube roots calculated"
    else
      val unitsType =
        UnitsMap:
          units.map.view.mapValues { case UnitPower(unit, power) => UnitPower(unit, power/3) }.toMap

        . repr.get.asType

      unitsType.absolve match
        case '[type result <: Measure; result] =>
          val cbrt = '{ (value: Quantity[value]) => Quantity(math.cbrt(value.value)) }
          val cast = cbrt.asExprOf[Quantity[value] => Quantity[result]]

          '{Rootable[3, Quantity[value], Quantity[result]]($cast(_))}


  def greaterThan[left <: Measure: Type, right <: Measure: Type]
    ( leftExpr:  Expr[Quantity[left]],
      rightExpr: Expr[Quantity[right]],
      strict:    Expr[Boolean],
      invert:    Expr[Boolean] )
    ( using Quotes )
  : Expr[Boolean] =

      val left: UnitsMap = UnitsMap[left]
      val right: UnitsMap = UnitsMap[right]
      val closed = !strict.valueOrAbort

      val (left2, leftValue) = normalize(left, right, '{$leftExpr.underlying})
      val (right2, rightValue) = normalize(right, left, '{$rightExpr.underlying})

      if left2 != right2 then '{compiletime.error(${Expr(incompatibleTypeText(left, right))})} else
        if !invert.valueOrAbort
        then if closed then '{$leftValue <= $rightValue} else '{$leftValue < $rightValue}
        else if closed then '{$leftValue >= $rightValue} else '{$leftValue > $rightValue}


  def add[left <: Measure: Type, right <: Measure: Type]
    ( leftExpr: Expr[Quantity[left]], rightExpr: Expr[Quantity[right]] )
    ( using Quotes )
  : Expr[Any] =

      val left: UnitsMap = UnitsMap[left]
      val right: UnitsMap = UnitsMap[right]

      val (left2, leftValue) = normalize(left, right, '{$leftExpr.underlying})
      val (right2, rightValue) = normalize(right, left, '{$rightExpr.underlying})

      if left2 != right2 then '{compiletime.error(${Expr(incompatibleTypeText(left, right))})} else
        val resultValue = '{$leftValue + $rightValue}

        left2.repr.map(_.asType).absolve match
          case Some('[type unitsType <: Measure; unitsType]) => '{Quantity[unitsType]($resultValue)}
          case _                                             => resultValue


  def check[left <: Measure: Type, right <: Measure: Type]
    ( leftExpr: Expr[Quantity[left]], rightExpr: Expr[Quantity[right]] )
    ( using Quotes )
  : Expr[Boolean] =

      val left: UnitsMap = UnitsMap[left]
      val right: UnitsMap = UnitsMap[right]

      val (left2, leftValue) = normalize(left, right, '{$leftExpr.underlying})
      val (right2, rightValue) = normalize(right, left, '{$rightExpr.underlying})

      if left2 != right2 then '{compiletime.error(${Expr(incompatibleTypeText(left, right))})}
      else '{$leftValue == $rightValue}


  def sub[left <: Measure: Type, right <: Measure: Type]
    ( leftExpr: Expr[Quantity[left]], rightExpr: Expr[Quantity[right]] )
    ( using Quotes )
  : Expr[Any] =

      val left: UnitsMap = UnitsMap[left]
      val right: UnitsMap = UnitsMap[right]

      val (left2, leftValue) = normalize(left, right, '{$leftExpr.underlying})
      val (right2, rightValue) = normalize(right, left, '{$rightExpr.underlying})

      if left2 != right2 then '{compiletime.error(${Expr(incompatibleTypeText(left, right))})} else
        val resultValue = '{$leftValue - $rightValue}

        left2.repr.map(_.asType).absolve match
          case Some('[type unitsType <: Measure; unitsType]) => '{Quantity[unitsType]($resultValue)}
          case _                                             => resultValue



  def subTypeclass
    [ left      <: Measure:         Type,
      quantity  <: Quantity[left]:  Type,
      right     <: Measure:         Type,
      quantity2 <: Quantity[right]: Type ]
    ( using Quotes )
  : Expr[quantity is Subtractable by quantity2] =

      val (units, _) = normalize(UnitsMap[left], UnitsMap[right], '{0.0})

      units.repr.map(_.asType).absolve match
        case Some('[type measure <: Measure; measure]) =>
          ' {
              Subtractable[quantity, quantity2, Quantity[measure]]:
                (left, right) =>
                  ${Quantitative.sub('left, 'right).asExprOf[Quantity[measure]]}
            }


  def addTypeclass
    [ left      <: Measure:         Type,
      quantity  <: Quantity[left]:  Type,
      right     <: Measure:         Type,
      quantity2 <: Quantity[right]: Type ]
    ( using Quotes )
  : Expr[quantity is Addable by quantity2] =

      val (units, other) = normalize(UnitsMap[left], UnitsMap[right], '{0.0})

      units.repr.map(_.asType).absolve match
        case Some('[type result <: Measure; result]) =>
          ' {
              Addable[quantity, quantity2, Quantity[result]]: (left, right) =>
                ${Quantitative.add('left, 'right).asExprOf[Quantity[result]]}
            }

  def checkable
    [ left      <: Measure:         Type,
      quantity  <: Quantity[left]:  Type,
      right     <: Measure:         Type,
      quantity2 <: Quantity[right]: Type ]
    ( using Quotes )
  : Expr[quantity is Checkable against quantity2] =

      val (units, other) = normalize(UnitsMap[left], UnitsMap[right], '{0.0})

      units.repr.map(_.asType).absolve match
        case Some('[type result <: Measure; result]) =>
          ' {
              Checkable[quantity, quantity2]: (left, right) =>
                ${Quantitative.check('left, 'right)}
            }


  def norm[units <: Measure: Type, norm[power <: Nat] <: Units[power, ?]: Type]
    ( expr: Expr[Quantity[units]] )
    ( using Quotes )
  : Expr[Any] =

      val units: UnitsMap = UnitsMap[units]
      val norm: UnitsMap = UnitsMap[norm[1]]
      val (units2, value) = normalize(units, norm, '{$expr.underlying}, true)

      units2.repr.map(_.asType).absolve match
        case Some('[type unitsType <: Measure; unitsType]) => '{Quantity[unitsType]($value)}
        case None                                          => value


  def describe[units <: Measure: Type](using Quotes): Expr[Text] =
    UnitsMap[units].dimensionality.quantityName match
      case Some(name) => '{${Expr(name)}.tt}
      case None       => halt(m"there is no descriptive name for this physical quantity")
