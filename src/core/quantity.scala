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

  object UnitsTypeRepr:
    def of[UnitsTypeType: Type](using quotes: Quotes): UnitsTypeRepr[quotes.type] =
      UnitsTypeRepr(quotes.reflect.TypeRepr.of[UnitsTypeType])

  class UnitsTypeRepr[QuotesType <: Quotes](using val quotes: QuotesType)(val repr: quotes.reflect.TypeRepr):
    lazy val map: Map[quotes.reflect.TypeRef, (UnitTypeRef[QuotesType], Int)] =
      import quotes.reflect.*
      
      def recur(repr: TypeRepr): Map[TypeRef, (UnitTypeRef[QuotesType], Int)] = repr match
        case AndType(left, right) =>
          recur(left) ++ recur(right)
        
        case AppliedType(unit@TypeRef(_, _), List(ConstantType(IntConstant(power)))) =>
          unit.asType match
            case '[ Units[p, u] ] => TypeRepr.of[u] match
              case ref@TypeRef(_, _) => Map(ref -> (UnitTypeRef[QuotesType](unit), power))
              case _ => throw Mistake("Should never match")
            case _ => throw Mistake("Should never match")
        
        case other =>
          Map()
      
      recur(repr)
    
    def inverseMap: Map[quotes.reflect.TypeRef, (UnitTypeRef[QuotesType], Int)] =
      map.map { case (key, (unit, power)) => (key, (unit, -power)) }
  
    def power(n: Int): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      AppliedType(repr, List(ConstantType(IntConstant(n))))
    
    def unitType(dimension: quotes.reflect.TypeRef): UnitTypeRef[QuotesType] = map(dimension)(0)
    def unitPower(dimension: quotes.reflect.TypeRef): Int = map(dimension)(1)
  
  class UnitTypeRef[QuotesType <: Quotes](using val quotes: QuotesType)(val ref: quotes.reflect.TypeRef):
    def name: String = ref match { case quotes.reflect.TypeRef(_, name) => name }

  private def deconjunct
      (using Quotes)(typ: quotes.reflect.TypeRepr, division: Boolean)
      : Map[quotes.reflect.TypeRef, (quotes.reflect.TypeRef, Int)] =
    import quotes.reflect.*
    
    typ match
      case AndType(left, right) =>
        deconjunct(left, division) ++ deconjunct(right, division)
      
      case AppliedType(unit@TypeRef(_, _), List(ConstantType(IntConstant(power)))) =>
        unit.asType match
          case '[ Units[p, u] ] => TypeRepr.of[u] match
            case ref@TypeRef(_, _) => Map(ref -> (unit, power))
            case _ => throw Mistake("Should never match")
          case _ => throw Mistake("Should never match")
      
      case other =>
        Map()
  
  def collectUnits[UnitsType <: Units[?, ?]: Type](using quotes: Quotes): Expr[Map[Text, Int]] =
    import quotes.reflect.*
    
    def mkMap(expr: Expr[Map[Text, Int]], todo: List[(UnitTypeRef[quotes.type], Int)]): Expr[Map[Text, Int]] =
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
    
    mkMap('{Map[Text, Int]()}, UnitsTypeRepr.of[UnitsType].map.values.to(List))

  def powerType(using quotes: Quotes)(repr: quotes.reflect.TypeRepr, n: Int): Type[?] =
    import quotes.reflect.*
    AppliedType(repr, List(ConstantType(IntConstant(n)))).asType
    
  def construct
      (using quotes: Quotes)(types: List[(UnitTypeRef[quotes.type], Int)])
      : Option[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    types.filter(_(1) != 0) match
      case Nil =>
        None
      
      case (ref, power) :: Nil =>
        Some(AppliedType(ref.ref, List(ConstantType(IntConstant(power)))))
      
      case (ref, power) :: more =>
        Some(AndType(AppliedType(ref.ref, List(ConstantType(IntConstant(power)))), construct(more).get))

  private def ratio
      (using quotes: Quotes)(from: quotes.reflect.TypeRef, to: quotes.reflect.TypeRef)
      : Expr[Double] =
    import quotes.reflect.*

    (powerType(from, -1), powerType(to, 1)) match
      case ('[fromType], '[toType]) =>
        Expr.summon[Ratio[fromType & toType & Units[1, ?]]] match
          case None => fail("can't convert type")
          case Some(ratio) => '{$ratio.value.value}
  
  private def convert
      (using quotes: Quotes)
      (expr: Expr[Double], from: Map[quotes.reflect.TypeRef, (quotes.reflect.TypeRef, Int)],
          to: Map[quotes.reflect.TypeRef, (quotes.reflect.TypeRef, Int)])
      : Expr[Double] =
    import quotes.reflect.*
    
    def recur(expr: Expr[Double], dimensions: List[TypeRef]): Expr[Double] = dimensions match
      case Nil =>
        expr
      
      case dimension :: tail =>
        val (fromUnits, fromPower) = from(dimension)
        val (toUnits, toPower) = to(dimension)
        if fromPower != toPower then fail("the units do not match")
        
        if fromUnits == toUnits then recur(expr, tail) else
          if fromPower == 1 then recur('{$expr*${ratio(using quotes)(fromUnits, toUnits)}}, tail)
          else recur('{$expr*math.pow(${ratio(using quotes)(fromUnits, toUnits)}, ${Expr(fromPower)})}, tail)
    
    if to.size != from.size then fail("the units do not match")
    
    recur(Expr(1.0), from.keys.to(List))

  private def preferred
      (using quotes: Quotes)(dim: quotes.reflect.TypeRef, units: quotes.reflect.TypeRef): Boolean =
    import quotes.reflect.*
    
    (dim.asType, units.asType) match
      case ('[d], '[t]) =>
        Expr.summon[PrincipalUnit[d & Dimension, t & Units[1, d & Dimension], 1]].isDefined
      case _ =>
        false
  
  def typeName(using quotes: Quotes)(repr: quotes.reflect.TypeRef): String = repr match
    case quotes.reflect.TypeRef(_, name) => name

  def multiply
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (left: Expr[Quantity[LeftType]], right: Expr[Quantity[RightType]], division: Boolean)
      (using Quotes)
      : Expr[Any] =
    import quotes.reflect.*

    val rightUnits = UnitsTypeRepr.of[RightType]
    val leftUnits = UnitsTypeRepr.of[LeftType]
    
    val rightMap: Map[TypeRef, (TypeRef, Int)] = deconjunct(TypeRepr.of[RightType], division)
    val leftMap: Map[TypeRef, (TypeRef, Int)] = deconjunct(TypeRepr.of[LeftType], false)

    def recur
        (map: Map[TypeRef, (UnitTypeRef[quotes.type], Int)], todo: List[TypeRef], multiplier: Expr[Double])
        : (Map[TypeRef, (UnitTypeRef[quotes.type], Int)], Expr[Double]) =
      todo match
        case Nil =>
          (map, multiplier)
        
        case dimension :: todo2 =>
          val rightUnit = rightUnits.unitType(dimension)
          val rightPower = rightUnits.unitPower(dimension)

          leftUnits.map.get(dimension) match
            case None =>
              recur(map.updated(dimension, (rightUnit, if division then -rightPower else
                  rightPower)), todo2, multiplier)
            
            case Some((leftUnit, leftPower)) =>
              
              (dimension.asType, powerType(leftUnit.ref, 1), powerType(leftUnit.ref, -1),
                  powerType(rightUnit.ref, 1), powerType(rightUnit.ref, -1)) match
                case ('[dimension], '[left], '[invLeft], '[right], '[invRight]) =>
                  type dim = dimension & Dimension
                  type lUnits = left & Units[1, dim]
                  type rUnits = right & Units[1, dim]
                    
                  val preferRight = Expr.summon[PrincipalUnit[dim, rUnits, ?]].isDefined

                  val multiplier2: Expr[Double] =
                    if leftUnit.ref.typeSymbol == rightUnit.ref.typeSymbol then multiplier else
                      def coefficient = Expr.summon[Ratio[rUnits & invLeft]]
                      def coefficient2 = Expr.summon[Ratio[lUnits & invRight]]

                      val power: Expr[Double] = Expr:
                        if division then if preferRight then leftPower else rightPower
                        else if preferRight then leftPower else -rightPower
                      
                      val coefficientExpr =
                        coefficient.map { tc => '{math.pow($tc.value.value, ${power})} }.orElse:
                          coefficient2.map { tc => '{math.pow($tc.value.value, -${power})} }
                        .getOrElse:
                          fail(txt"""
                            the left and right operand use incompatible units for the
                            ${typeName(dimension)} dimension
                                
                            The left operand uses ${typeName(leftUnit.ref)}. The right operand uses
                            ${typeName(rightUnit.ref)}.
                                   
                            This can be resolved by defining a contextual
                            Ratio[${typeName(leftUnit.ref)}[1], ${typeName(rightUnit.ref)}[1]].
                          """.s)
                  
                      '{$coefficientExpr*$multiplier}
                  
                  val typeUnits = if preferRight then rightUnit else leftUnit
                  val typePower = leftPower + (if division then -rightPower else rightPower)
                  recur(map.updated(dimension, (typeUnits, typePower)), todo2, multiplier2)
                
                case _ => fail("could not interpret unit types")
  
      
    val (map, multiplier) = recur(leftUnits.map, rightMap.keys.to(List), Expr(1.0))

    val number = if division then '{$left.value/$right.value} else '{$left.value*$right.value}
    
    construct(map.values.to(List)) match
      case None => '{$multiplier*$number}
      case Some(typeRepr) => typeRepr.asType match
        case '[ t ] => '{Quantity[t & Units[?, ?]]($multiplier*$number)}
        case _      => ???
