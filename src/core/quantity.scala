/*
    Abacist, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package abacist

import quantitative.*
import anticipation.*
import fulminate.*
import vacuous.*

import scala.collection.immutable.*
import scala.quoted.*

object Abacist:

  import QuantitativeMacros.*

  def make[UnitsType <: Tuple: Type](values: Expr[Seq[Int]])(using Quotes): Expr[Count[UnitsType]] =
    val inputs: List[Expr[Int]] = (values: @unchecked) match
      case Varargs(values) => values.to(List).reverse

    def recur(multipliers: List[Multiplier], values: List[Expr[Int]], expr: Expr[Long]): Expr[Long] =
      values match
        case Nil => expr
        
        case unitValue :: valuesTail => multipliers match
          case Multiplier(unitPower, subdivision, max) :: tail =>
            unitValue.value match
              case Some(unitValue) =>
                if unitValue < 0
                then fail(msg"the value for the ${unitPower.ref.name} unit ($unitValue) cannot be negative")
                else if unitValue >= max
                then fail(msg"the value for the ${unitPower.ref.name} unit $unitValue must be less than $max")

                recur(tail, valuesTail, '{$expr + (${Expr(unitValue.toLong)}*${Expr(subdivision)})})
              
              case None =>
                recur(tail, valuesTail, '{$expr + ($unitValue.toLong*${Expr(subdivision)})})
          
          case Nil =>
            fail(msg"""
              ${inputs.length} unit values were provided, but this Count only has ${multipliers.length} units
            """)
    
    '{Count.fromLong[UnitsType](${recur(multipliers[UnitsType].reverse, inputs, '{0L})})}

  def addCount
      [CountUnitsType <: Tuple: Type]
      (left: Expr[Count[CountUnitsType]], right: Expr[Count[CountUnitsType]])
      (using Quotes)
      : Expr[Count[CountUnitsType]] =
    '{($left.asInstanceOf[Long] + $right.asInstanceOf[Long]).asInstanceOf[Count[CountUnitsType]]}

  def describeCount
      [CountUnits <: Tuple: Type]
      (count: Expr[Count[CountUnits]])
      (using Quotes)
      : Expr[ListMap[Text, Long]] =
    def recur(slices: List[Multiplier], expr: Expr[ListMap[Text, Long]]): Expr[ListMap[Text, Long]] =
      slices match
        case Nil =>
          expr
        
        case (slice@Multiplier(unitPower, subdivision, max)) :: tail =>
          val value = '{(($count.asInstanceOf[Long]/${Expr(subdivision)})%(${Expr(max)}))}
          recur(tail, '{$expr.updated(${unitPower.ref.unitName}, $value)})

    recur(multipliers[CountUnits], '{ListMap()})

  def multiplyCount
      [CountUnitsType <: Tuple: Type]
      (count: Expr[Count[CountUnitsType]], multiplier: Expr[Double], division: Boolean)
      (using Quotes)
      : Expr[Any] =
    '{Count.fromLong[CountUnitsType](($count.longValue*$multiplier + 0.5).toLong)}

  def toQuantity[CountUnitsType <: Tuple: Type](count: Expr[Count[CountUnitsType]])(using Quotes): Expr[Any] =
    val lastUnit = multipliers[CountUnitsType].last
    val quantityUnit = lastUnit.unitPower.ref.dimensionRef.principal
    val ratioExpr = ratio(lastUnit.unitPower.ref, quantityUnit, lastUnit.unitPower.power)

    (quantityUnit.power(1).asType: @unchecked) match
      case '[type quantityType <: Measure; quantityType] =>
        '{Quantity[quantityType]($count.longValue*$ratioExpr)}

  def fromQuantity
      [QuantityType <: Measure: Type, CountUnitsType <: Tuple: Type]
      (quantity: Expr[Quantity[QuantityType]])
      (using Quotes)
      : Expr[Count[CountUnitsType]] =
    import quotes.reflect.*
    
    val lastUnit = multipliers[CountUnitsType].last.unitPower
    val quantityUnit = readUnitPower(TypeRepr.of[QuantityType].dealias)
    val ratioExpr = ratio(quantityUnit.ref, lastUnit.ref, lastUnit.power)

    '{($quantity.value*$ratioExpr + 0.5).toLong.asInstanceOf[Count[CountUnitsType]]}
    
  def get
      [UnitsType <: Tuple: Type, UnitType <: Units[1, ? <: Dimension]: Type]
      (value: Expr[Count[UnitsType]])
      (using Quotes)
      : Expr[Int] =
    import quotes.reflect.*
    
    val lookupUnit = readUnitPower(TypeRepr.of[UnitType])
    
    val multiplier: Multiplier = multipliers[UnitsType].find(_.unitPower == lookupUnit).getOrElse:
      fail(msg"the Count does not include this unit")
    
    '{(($value.longValue/${Expr(multiplier.subdivision)})%${Expr(multiplier.max)}).toInt}
  
  private case class Multiplier(unitPower: UnitPower, subdivision: Int, max: Int)

  private def multipliers[UnitsType: Type](using Quotes): List[Multiplier] =
    import quotes.reflect.*
    
    def untuple[TupleType: Type](dimension: Optional[DimensionRef], result: List[UnitPower]): List[UnitPower] =
      TypeRepr.of[TupleType].dealias.asType match
        case '[head *: tail] =>
          val unitPower = readUnitPower(TypeRepr.of[head])
          
          dimension.let: current =>
            if unitPower.ref.dimensionRef != current
            then fail(msg"""
              the Count type incorrectly mixes units of ${unitPower.ref.dimensionRef.name} and
              ${current.name}
            """)
          
          untuple[tail](unitPower.ref.dimensionRef, unitPower :: result)
        
        case _ =>
          result

    val cascade: List[UnitPower] = untuple[UnitsType](Unset, Nil)
    
    def recur(todo: List[UnitPower], units: List[Multiplier] = Nil): List[Multiplier] = todo match
      case Nil =>
        units
      case head :: tail =>
        val value = ratio(head.ref, cascade.head.ref, head.power).valueOrAbort
        val value2 = tail.headOption.map(_.ref).map(ratio(_, head.ref, head.power).valueOrAbort + 0.5)
        recur(tail, Multiplier(head, (value + 0.5).toInt, value2.map(_.toInt).getOrElse(Int.MaxValue)) :: units)

    recur(cascade)