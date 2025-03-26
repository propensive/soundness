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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package abacist

import scala.collection.immutable.*
import scala.quoted.*

import anticipation.*
import fulminate.*
import gossamer.{where as _, *}
import hieroglyph.*
import quantitative.*
import proscenium.*
import rudiments.*
import vacuous.*

given realm: Realm = realm"abacist"

object Abacist:
  import Quantitative.*

  def make[units <: Tuple: Type](values: Expr[Seq[Int]])(using Quotes): Expr[Count[units]] =
    val inputs: List[Expr[Int]] = values.absolve match
      case Varargs(values) => values.to(List).reverse

    def recur(multipliers: List[Multiplier], values: List[Expr[Int]], expr: Expr[Long])
    :     Expr[Long] =
      values match
        case Nil =>
          expr

        case unitValue :: valuesTail => multipliers match
          case Multiplier(unitPower, subdivision, max) :: tail =>
            unitValue.value match
              case Some(unitValue) =>
                if unitValue < 0
                then halt:
                  m"the value for the ${unitPower.ref.name} unit ($unitValue) cannot be negative"
                else if unitValue >= max
                then halt:
                  m"the value for the ${unitPower.ref.name} unit $unitValue must be less than $max"

                recur(tail, valuesTail, '{$expr + (${Expr(unitValue.toLong)}*${Expr(subdivision)})})

              case None =>
                recur(tail, valuesTail, '{$expr + ($unitValue.toLong*${Expr(subdivision)})})

          case Nil => halt:
            m"""${inputs.length} unit values were provided, but this Count only has
                ${multipliers.length} units"""

    '{Count.fromLong[units](${recur(multipliers[units].reverse, inputs, '{0L})})}

  def describeCount[CountUnits <: Tuple: Type](count: Expr[Count[CountUnits]])(using Quotes)
  :     Expr[ListMap[Text, Long]] =

    def recur(slices: List[Multiplier], expr: Expr[ListMap[Text, Long]])
    :     Expr[ListMap[Text, Long]] =
      slices match
        case Nil =>
          expr

        case (slice@Multiplier(unitPower, subdivision, max)) :: tail =>
          val power = if unitPower.power == 1 then "".tt else
            unitPower.power.toString.tt.mapChars(_.superscript.or(' '))

          val value = '{(($count.asInstanceOf[Long]/${Expr(subdivision)})%(${Expr(max)}))}
          recur(tail, '{$expr.updated(${unitPower.ref.designation}+${Expr(power)}, $value)})

    recur(multipliers[CountUnits], '{ListMap()})

  def multiplyCount[countUnits <: Tuple: Type]
     (count: Expr[Count[countUnits]], multiplier: Expr[Double], division: Boolean)(using Quotes)
  :     Expr[Any] =

    if division then '{Count.fromLong[countUnits](($count.longValue/$multiplier + 0.5).toLong)}
    else '{Count.fromLong[countUnits](($count.longValue*$multiplier + 0.5).toLong)}

  def toQuantity[countUnits <: Tuple: Type](count: Expr[Count[countUnits]])(using Quotes)
  :     Expr[Any] =
    val lastUnit = multipliers[countUnits].last
    val quantityUnit = lastUnit.unitPower.ref.dimensionRef.principal
    val ratioExpr = ratio(lastUnit.unitPower.ref, quantityUnit, lastUnit.unitPower.power)

    quantityUnit.power(1).asType.absolve match
      case '[type quantity <: Measure; quantity] =>
        '{Quantity[quantity]($count.longValue*$ratioExpr)}

  def fromQuantity[quantity <: Measure: Type, countUnits <: Tuple: Type]
     (quantity: Expr[Quantity[quantity]])
     (using Quotes)
  :     Expr[Count[countUnits]] =

    import quotes.reflect.*

    val lastUnit = multipliers[countUnits].last.unitPower
    val quantityUnit = readUnitPower(TypeRepr.of[quantity].dealias)
    val ratioExpr = ratio(quantityUnit.ref, lastUnit.ref, lastUnit.power)

    '{($quantity.value*$ratioExpr + 0.5).toLong.asInstanceOf[Count[countUnits]]}

  def get[units <: Tuple: Type, unit <: Units[1, ? <: Dimension]: Type](value: Expr[Count[units]])
     (using Quotes)
  :     Expr[Int] =

    import quotes.reflect.*

    val lookupUnit = readUnitPower(TypeRepr.of[unit])

    val multiplier: Multiplier = multipliers[units].where(_.unitPower == lookupUnit).or:
      halt(m"the Count does not include this unit")

    '{(($value.longValue/${Expr(multiplier.subdivision)})%${Expr(multiplier.max)}).toInt}

  private case class Multiplier(unitPower: UnitPower, subdivision: Int, max: Int)

  private def multipliers[units: Type](using Quotes): List[Multiplier] =
    import quotes.reflect.*

    def untuple[tuple: Type](dimension: Optional[DimensionRef], result: List[UnitPower])
    :     List[UnitPower] =
      TypeRepr.of[tuple].dealias.asType match
        case '[head *: tail] =>
          val unitPower = readUnitPower(TypeRepr.of[head])

          dimension.let: current =>
            if unitPower.ref.dimensionRef != current
            then halt:
              m"""the Count type incorrectly mixes units of ${unitPower.ref.dimensionRef.name} and
                  ${current.name}"""

          untuple[tail](unitPower.ref.dimensionRef, unitPower :: result)

        case _ =>
          result

    val cascade: List[UnitPower] = untuple[units](Unset, Nil)

    def recur(todo: List[UnitPower], units: List[Multiplier] = Nil): List[Multiplier] = todo match
      case Nil =>
        units

      case head :: tail =>
        val value = ratio(head.ref, cascade.head.ref, head.power).valueOrAbort
        val value2 = tail.prim.let(_.ref).let(ratio(_, head.ref, head.power).valueOrAbort + 0.5)
        recur
         (tail,
          Multiplier(head, (value + 0.5).toInt, value2.let(_.toInt).or(Int.MaxValue)) :: units)

    recur(cascade)
