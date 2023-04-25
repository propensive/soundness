/*
    Quantify, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package quantify

import rudiments.*
import gossamer.*

import scala.quoted.*

object QuantifyMacros:

  type UnitsMap = Map[DimensionTypeRef, (UnitTypeRef, Int)]

  object UnitsTypeRepr:
    def apply[UnitsTypeType <: Units[?, ?]: Type](using Quotes): UnitsTypeRepr =
      import quotes.reflect.*
      
      def recur(repr: TypeRepr): UnitsMap =
        repr match
          case AndType(left, right) =>
            recur(left) ++ recur(right)
          
          case AppliedType(unit@TypeRef(_, _), List(ConstantType(IntConstant(power)))) =>
            unit.asType match
              case '[ Units[power, unitType] ] => TypeRepr.of[unitType] match
                case ref@TypeRef(_, _) =>
                  val unitTypeRef = UnitTypeRef(unit.asType, unit.show)
                  Map(DimensionTypeRef(ref.asType, ref.show) -> (unitTypeRef, power))
                case _ =>
                  throw Mistake("Should never match")
              case _ =>
                throw Mistake("Should never match")
          
          case other =>
            Map()
      
      new UnitsTypeRepr(recur(quotes.reflect.TypeRepr.of[UnitsTypeType]))

  class UnitsTypeRepr(val map: UnitsMap):
    def repr(using Quotes): Option[quotes.reflect.TypeRepr] = construct(map.values.to(List))
    def inverseMap: UnitsMap = map.mapValues { case (unit, power) => (unit, -power) }.to(Map)
    def dimensionality: Map[DimensionTypeRef, Int] = map.mapValues(_(1)).to(Map)
    def dimensions: List[DimensionTypeRef] = map.keys.to(List)
    def empty: Boolean = map.values.forall(_(1) == 0)

    def *(that: UnitsTypeRepr): UnitsTypeRepr = new UnitsTypeRepr(
      (dimensions ++ that.dimensions).to(Set).to(List).map: dim =>
        dim -> (unit(dim).orElse(that.unit(dim)).get, (unitPower(dim) + that.unitPower(dim)))
      .to(Map).filter(_(1)(1) != 0)
    )
    
    def /(that: UnitsTypeRepr): UnitsTypeRepr = new UnitsTypeRepr(
      (dimensions ++ that.dimensions).to(Set).to(List).map: dim =>
        dim -> (unit(dim).orElse(that.unit(dim)).get, (unitPower(dim) - that.unitPower(dim)))
      .to(Map).filter(_(1)(1) != 0)
    )

    def construct(using Quotes)(types: List[(UnitTypeRef, Int)]): Option[quotes.reflect.TypeRepr] =
      import quotes.reflect.*

      types.filter(_(1) != 0) match
        case Nil =>
          None
        
        case (ref, power) :: Nil =>
          Some(AppliedType(ref.ref, List(ConstantType(IntConstant(power)))))
        
        case (ref, power) :: more =>
          Some(AndType(AppliedType(ref.ref, List(ConstantType(IntConstant(power)))),
              construct(more).get))
    
    def sub(dimension: DimensionTypeRef, unit: UnitTypeRef, power: Int): UnitsTypeRepr =
      new UnitsTypeRepr(map.updated(dimension, (unit, power)))
    
    def unit(dimension: DimensionTypeRef): Option[UnitTypeRef] = map.get(dimension).map(_(0))
    def unitPower(dimension: DimensionTypeRef): Int = map.get(dimension).map(_(1)).getOrElse(0)
  
  class UnitTypeRef(val typ: Type[?], val name: String):
    def ref(using Quotes): quotes.reflect.TypeRepr =
      typ match { case '[ref] => quotes.reflect.TypeRepr.of[ref] }

    def power(n: Int)(using Quotes): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      AppliedType(ref, List(ConstantType(IntConstant(n))))
    
    override def equals(that: Any): Boolean = that match
      case that: UnitTypeRef => name == that.name
      case _                 => false
    
    override def hashCode: Int = name.hashCode
    override def toString(): String = name

  class DimensionTypeRef(val typ: Type[?], val name: String):
    def ref(using Quotes): quotes.reflect.TypeRepr =
      typ match { case '[ref] => quotes.reflect.TypeRepr.of[ref] }
    
    def principal(using Quotes): UnitTypeRef =
      import quotes.reflect.*
      typ match
        case '[dim] => Expr.summon[PrincipalUnit[dim & Dimension, ?, ?]] match
          case None =>
            fail(s"there is no principal unit specified for the dimension $name")
          case Some('{ $expr: principalUnit }) => Type.of[principalUnit] match
            case '[ PrincipalUnit[dim, units, power] ] => TypeRepr.of[units] match
              case AppliedType(ref@TypeRef(_, _), List(ConstantType(IntConstant(power)))) =>
                UnitTypeRef(ref.asType, ref.show)
              case other =>
                fail(s"principal units had unexpected type: $other")
        
    
    override def equals(that: Any): Boolean = that match
      case that: DimensionTypeRef => name == that.name
      case _                      => false
    
    override def hashCode: Int = name.hashCode
    override def toString(): String = name

  def collectUnits[UnitsType <: Units[?, ?]: Type](using Quotes): Expr[Map[Text, Int]] =
    import quotes.reflect.*
    
    def mkMap(expr: Expr[Map[Text, Int]], todo: List[(UnitTypeRef, Int)]): Expr[Map[Text, Int]] =
      todo match
        case Nil =>
          expr
        
        case (ref, power) :: todo2 =>
          AppliedType(ref.ref, List(ConstantType(IntConstant(1)))).asType match
            case '[ refType ] =>
              val unitName = Expr.summon[UnitName[refType]].get
              mkMap('{$expr.updated($unitName.name(), ${Expr(power)})}, todo2)
          
            case _ =>
              throw Mistake("Should never match")
    
    mkMap('{Map[Text, Int]()}, UnitsTypeRepr[UnitsType].map.values.to(List))

  def ratio(using Quotes)(from: UnitTypeRef, to: UnitTypeRef, power: Int, retry: Boolean = true): Expr[Double] =
    if from == to then Expr(1.0) else (from.power(-1).asType, to.power(1).asType) match
      case ('[from], '[to]) =>
        Expr.summon[Ratio[from & to & Units[?, ?]]] match
          case None =>
            if retry then ratio(to, from, -power, false)
            else fail(s"can't convert from ${from.name} to ${to.name}")
          case Some(ratio) =>
            if power == 1 then '{$ratio.value.value}
            else '{math.pow($ratio.value.value, ${Expr(power)})}

  def normalize
      (using Quotes)(units: UnitsTypeRepr, other: UnitsTypeRepr, init: Expr[Double])
      : (UnitsTypeRepr, Expr[Double]) =
    def recur
        (dimensions: List[DimensionTypeRef], target: UnitsTypeRepr, expr: Expr[Double])
        : (UnitsTypeRepr, Expr[Double]) =
      dimensions match
        case Nil =>
          (target, expr)
        
        case dimension :: dimensions =>
          if other.unitPower(dimension) == 0 || units.unit(dimension) == other.unit(dimension)
          then recur(dimensions, target, expr)
          else
            val unit = target.unit(dimension).get
            val power = target.unitPower(dimension)
            val unit2 = dimension.principal
            val value = '{$expr*${ratio(unit, unit2, power)}}
            recur(dimensions, target.sub(dimension, unit2, power), value)
    
    recur(units.dimensions, units, init)
  
  def multiply
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (leftExpr: Expr[Quantity[LeftType]], rightExpr: Expr[Quantity[RightType]], division: Boolean)
      (using Quotes)
      : Expr[Any] =
    import quotes.reflect.*

    val left: UnitsTypeRepr = UnitsTypeRepr[LeftType]
    val right: UnitsTypeRepr = UnitsTypeRepr[RightType]
    
    val (left2, leftValue) = normalize(left, right, '{$leftExpr.value})
    val (right2, rightValue) = normalize(right, left, '{$rightExpr.value})

    val resultUnits = if division then left2/right2 else left2*right2
    val resultValue = if division then '{$leftValue/$rightValue} else '{$leftValue*$rightValue}
    
    resultUnits.repr.map(_.asType) match
      case Some('[units]) => '{Quantity[units & Units[?, ?]]($resultValue)}
      case None           => resultValue

  def add
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (leftExpr: Expr[Quantity[LeftType]], rightExpr: Expr[Quantity[RightType]], sub: Boolean)
      (using Quotes)
      : Expr[Any] =
    import quotes.reflect.*

    val left: UnitsTypeRepr = UnitsTypeRepr[LeftType]
    val right: UnitsTypeRepr = UnitsTypeRepr[RightType]

    val (left2, leftValue) = normalize(left, right, '{$leftExpr.value})
    val (right2, rightValue) = normalize(right, left, '{$rightExpr.value})

    if left2.map != right2.map then fail("the operands have incompatible types")
    val resultValue = if sub then '{$leftValue - $rightValue} else '{$leftValue + $rightValue}
    
    left2.repr.map(_.asType) match
      case Some('[units]) => '{Quantity[units & Units[?, ?]]($resultValue)}
      case None           => resultValue
  