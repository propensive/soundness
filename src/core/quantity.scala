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
import scala.compiletime.*

import language.experimental.captureChecking

object QuantifyMacros:

  private case class UnitPower(ref: UnitRef, power: Int)
  
  private object UnitsMap:
    def apply[UnitsTypeType <: Measure: Type](using Quotes): UnitsMap =
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
      
      new UnitsMap(recur(quotes.reflect.TypeRepr.of[UnitsTypeType]))

  private case class Dimensionality(map: Map[DimensionRef, Int]):
    def quantityName(using Quotes): Option[String] =
      import quotes.reflect.*
      def recur(todo: List[(DimensionRef, Int)], current: TypeRepr): TypeRepr = todo match
        case Nil              => current
        case (dim, n) :: tail => ((current.asType, dim.power(n).asType): @unchecked) match
          case ('[current], '[next]) => recur(tail, TypeRepr.of[current & next])
      
      (recur(map.to(List), TypeRepr.of[Units[?, ?]]).asType: @unchecked) match
        case '[units] => Expr.summon[DimensionName[Units[?, ?] & units, ?]].map: value =>
          (value: @unchecked) match
            case '{$name: dimType} => (Type.of[dimType]: @unchecked) match
              case '[DimensionName[?, name]] => (TypeRepr.of[name].asMatchable: @unchecked) match
                case ConstantType(StringConstant(name)) => name
    
  private def readUnitPower(using quotes: Quotes)(typeRepr: quotes.reflect.TypeRepr): UnitPower =
    import quotes.reflect.*
    (typeRepr.asMatchable: @unchecked) match
      case AppliedType(unit, List(constantType)) => (constantType.asMatchable: @unchecked) match
        case ConstantType(constant) => (constant: @unchecked) match
          case IntConstant(power) => (unit.asMatchable: @unchecked) match
            case unit@TypeRef(_, _) =>
              UnitPower(UnitRef(unit.asType, unit.show), power)

  private case class UnitsMap(map: Map[DimensionRef, UnitPower]):
    def repr(using Quotes): Option[quotes.reflect.TypeRepr] = construct(map.values.to(List))
    
    def inverseMap: Map[DimensionRef, UnitPower] =
      map.view.mapValues { case UnitPower(unit, power) => UnitPower(unit, -power) }.to(Map)

    def dimensionality: Dimensionality = Dimensionality(map.view.mapValues(_.power).to(Map))
    def dimensions: List[DimensionRef] = map.keys.to(List)
    def empty: Boolean = map.values.forall(_.power == 0)

    @targetName("multiply")
    def *(that: UnitsMap): UnitsMap = new UnitsMap(
      (dimensions ++ that.dimensions).to(Set).to(List).map: dim =>
        val dimUnit = unit(dim).orElse(that.unit(dim)).get
        dim -> UnitPower(dimUnit, (unitPower(dim) + that.unitPower(dim)))
      .to(Map).filter(_(1).power != 0)
    )
    
    @targetName("divide")
    def /(that: UnitsMap): UnitsMap = new UnitsMap(
      (dimensions ++ that.dimensions).to(Set).to(List).map: dim =>
        val dimUnit = unit(dim).orElse(that.unit(dim)).get
        dim -> UnitPower(dimUnit, (unitPower(dim) - that.unitPower(dim)))
      .to(Map).filter(_(1).power != 0)
    )

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

  private class UnitRef(val unitType: Type[?], val name: String):
    def ref(using Quotes): quotes.reflect.TypeRepr =
      (unitType: @unchecked) match { case '[ref] => quotes.reflect.TypeRepr.of[ref] }
    
    def dimensionRef(using Quotes): DimensionRef =
      import quotes.reflect.*
      
      (unitType: @unchecked) match
        case '[Units[power, unitType]] => (TypeRepr.of[unitType].asMatchable: @unchecked) match
          case ref@TypeRef(_, _) => DimensionRef(ref.asType, ref.show)

    def power(n: Int)(using Quotes): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      AppliedType(ref, List(ConstantType(IntConstant(n))))
    
    override def equals(that: Any): Boolean = that.asMatchable match
      case that: UnitRef => name == that.name
      case _                 => false
    
    override def hashCode: Int = name.hashCode
    override def toString(): String = name

  private class DimensionRef(val dimensionType: Type[?], val name: String):
    def ref(using Quotes): quotes.reflect.TypeRepr =
      (dimensionType: @unchecked) match { case '[ref] => quotes.reflect.TypeRepr.of[ref] }
    
    def dimensionality(using Quotes): Dimensionality = Dimensionality(Map(this -> 1))
    
    def power(n: Int)(using Quotes): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      
      ((ConstantType(IntConstant(n)).asType, ref.asType): @unchecked) match
        case ('[power], '[dimension]) => TypeRepr.of[Units[power & Nat, dimension & Dimension]]
    
    def principal(using Quotes): UnitRef =
      import quotes.reflect.*
      
      (dimensionType: @unchecked) match
        case '[dim] => (Expr.summon[PrincipalUnit[dim & Dimension, ?]]: @unchecked) match
          case None =>
            val dimensionName =
              dimensionality.quantityName.map: name =>
                "the physical quantity "+name
              .getOrElse("the same quantity")

            fail(txt"""
              the operands both represent ${dimensionName}, but there is no principal unit specified
              for this dimension
            """.s)
          case Some('{$expr: principalUnit}) => (Type.of[principalUnit]: @unchecked) match
            case '[PrincipalUnit[dim, units]] => (TypeRepr.of[units].asMatchable: @unchecked) match
              case TypeLambda(_, _, appliedType) => (appliedType.asMatchable: @unchecked) match
                case AppliedType(typeRef, _) => (typeRef.asMatchable: @unchecked) match
                  case typeRef@TypeRef(_, _) => UnitRef(typeRef.asType, typeRef.show)
              
              case other =>
                fail(s"principal units had an unexpected type: $other")
        
    
    override def equals(that: Any): Boolean = that.asMatchable match
      case that: DimensionRef => name == that.name
      case _                      => false
    
    override def hashCode: Int = name.hashCode
    override def toString(): String = name



  private def ratio
      (using Quotes)
      (from: UnitRef, to: UnitRef, power: Int, retry: Boolean = true)
      : Expr[Double] =
    import quotes.reflect.*

    if from == to then Expr(1.0)
    else ((from.power(-1).asType, to.power(1).asType): @unchecked) match
      case ('[from], '[to]) => (Expr.summon[Ratio[from & to & Measure, ?]]: @unchecked) match
        case None =>
          if retry then ratio(to, from, -power, false)
          else
            val quantityName = from.dimensionRef.dimensionality.quantityName
            
            val dimensionName = quantityName.map("the physical quantity "+_).getOrElse:
                "the same physical quantity"
            
            fail(txt"""
              both operands represent $dimensionName, but the coversion ratio between them is not
              known

              To provide the conversion ratio, please provide a contextual instance in scope, with
              the type, `Ratio[${from.name}[1] & ${to.name}[-1]]`, or `Ratio[${to.name}[1] &
              ${from.name}[-1]]`.
            """.s)
        case Some('{$ratio: ratioType}) => (Type.of[ratioType]: @unchecked) match
          case '[Ratio[?, double]] => (TypeRepr.of[double].asMatchable: @unchecked) match
            case ConstantType(constant) => (constant: @unchecked) match
              case DoubleConstant(double) => Expr(math.pow(double, power))

  private def normalize
      (using Quotes)
      (units: UnitsMap, other: UnitsMap, init: Expr[Double], force: Boolean = false)
      : (UnitsMap, Expr[Double]) =
    def recur
        (dimensions: List[DimensionRef], target: UnitsMap, expr: Expr[Double])
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
  
  def collectUnits[UnitsType <: Measure: Type](using Quotes): Expr[Map[Text, Int]] =
    import quotes.reflect.*
    
    def recur(expr: Expr[Map[Text, Int]], todo: List[UnitPower]): Expr[Map[Text, Int]] =
      todo match
        case Nil =>
          expr
        
        case UnitPower(unit, power) :: todo2 =>
          (AppliedType(unit.ref, List(ConstantType(IntConstant(1)))).asType: @unchecked) match
            case '[refType] =>
              val unitName = Expr.summon[UnitName[refType]].get
              recur('{$expr.updated($unitName.text, ${Expr(power)})}, todo2)
    
    (Expr.summon[SubstituteUnits[UnitsType]]: @unchecked) match
      case Some('{$substitute: SubstituteUnits[?]}) =>
        '{Map[Text, Int](($substitute.name -> 1))}
      case None =>
        recur('{Map[Text, Int]()}, UnitsMap[UnitsType].map.values.to(List))

  def multiply
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (leftExpr: Expr[Quantity[LeftType]], rightExpr: Expr[Quantity[RightType]], division: Boolean)
      (using Quotes)
      : Expr[Any] =
    import quotes.reflect.*

    val left: UnitsMap = UnitsMap[LeftType]
    val right: UnitsMap = UnitsMap[RightType]
    
    val (left2, leftValue) = normalize(left, right, '{$leftExpr.value})
    val (right2, rightValue) = normalize(right, left, '{$rightExpr.value})

    val resultUnits = if division then left2/right2 else left2*right2
    val resultValue = if division then '{$leftValue/$rightValue} else '{$leftValue*$rightValue}
    
    (resultUnits.repr.map(_.asType): @unchecked) match
      case Some('[units]) => '{Quantity[units & Measure]($resultValue)}
      case None           => resultValue

  private def incompatibleTypes(left: UnitsMap, right: UnitsMap)(using Quotes): Nothing =
    (left.dimensionality.quantityName, right.dimensionality.quantityName) match
      case (Some(leftName), Some(rightName)) =>
        fail(txt"""
          the left operand represents $leftName, but the right operand represents $rightName; these
          are incompatible physical quantities
        """.s)
      
      case _ =>
        fail("the operands represent different physical quantities")

  def greaterThan
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (leftExpr: Expr[Quantity[LeftType]], rightExpr: Expr[Quantity[RightType]], closed: Boolean)
      (using Quotes)
      : Expr[Boolean] =
    import quotes.reflect.*

    val left: UnitsMap = UnitsMap[LeftType]
    val right: UnitsMap = UnitsMap[RightType]

    val (left2, leftValue) = normalize(left, right, '{$leftExpr.value})
    val (right2, rightValue) = normalize(right, left, '{$rightExpr.value})

    if left2 != right2 then incompatibleTypes(left, right)

    if closed then '{$leftValue >= $rightValue} else '{$leftValue > $rightValue}

  def add
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (leftExpr: Expr[Quantity[LeftType]], rightExpr: Expr[Quantity[RightType]], sub: Boolean)
      (using Quotes)
      : Expr[Any] =
    import quotes.reflect.*

    val left: UnitsMap = UnitsMap[LeftType]
    val right: UnitsMap = UnitsMap[RightType]

    val (left2, leftValue) = normalize(left, right, '{$leftExpr.value})
    val (right2, rightValue) = normalize(right, left, '{$rightExpr.value})

    if left2 != right2 then incompatibleTypes(left, right)
    
    val resultValue = if sub then '{$leftValue - $rightValue} else '{$leftValue + $rightValue}
    
    (left2.repr.map(_.asType): @unchecked) match
      case Some('[units]) => '{Quantity[units & Measure]($resultValue)}
      case None           => resultValue

  def norm
      [UnitsType <: Measure: Type, NormType[power <: Nat] <: Units[power, ?]: Type]
      (expr: Expr[Quantity[UnitsType]])(using Quotes)
      : Expr[Any] =
    import quotes.reflect.*
    val units: UnitsMap = UnitsMap[UnitsType]
    val norm: UnitsMap = UnitsMap[NormType[1]]
    val (units2, value) = normalize(units, norm, '{$expr.value}, true)

    (units2.repr.map(_.asType): @unchecked) match
      case Some('[units]) => '{Quantity[units & Measure]($value)}
      case None           => value
  
  def describe[UnitsType <: Measure: Type](using Quotes): Expr[Text] =
    UnitsMap[UnitsType].dimensionality.quantityName match
      case Some(name) => '{Text(${Expr(name)})}
      case None       => fail("there is no descriptive name for this physical quantity")

  private case class BitSlice(unitPower: UnitPower, max: Double, width: Int, shift: Int):
    def ones: Long = -1L >>> (64 - width)

  def get
      [UnitsType <: Tuple: Type, UnitType <: Units[1, ? <: Dimension]: Type]
      (value: Expr[Tally[UnitsType]])
      (using Quotes)
      : Expr[Int] =
    import quotes.reflect.*
  
    def decompose[TupleElemType: Type](dimension: Maybe[DimensionRef] = Unset, result: List[UnitPower] = Nil): List[UnitPower] =
      Type.of[TupleElemType] match
        case '[head *: tail] =>
          val unitPower = readUnitPower(TypeRepr.of[head])
          
          dimension.mm: current =>
            if unitPower.ref.dimensionRef != current
            then fail(txt"""
              the Tally type incorrectly mixes units of ${unitPower.ref.dimensionRef.name} and
              ${current.name}
            """.s)
          
          decompose[tail](unitPower.ref.dimensionRef, unitPower :: result)
        
        case _ =>
          result

    val cascade: List[UnitPower] = decompose[UnitsType]()
    val principalUnit = cascade.head.ref.dimensionRef.principal
    
    def width(value: Double, n: Int = 1): Int = if (1 << n) >= value then n else width(value, n + 1)
    
    def recur(unitPowers: List[UnitPower], next: UnitPower, factor: Double = 1.0, shift: Int = -1): List[BitSlice] =
      unitPowers match
        case Nil =>
          List(BitSlice(cascade.last, Double.MaxValue, 64 - shift, shift))
        
        case head :: tail =>
          val value = ratio(head.ref, principalUnit, head.power).valueOrAbort
          val max = value/factor
          BitSlice(next, max, width(max), shift) :: recur(tail, head, value, shift + width(max))
    
    val bitSlices = recur(cascade, cascade.head).tail

    val lookupUnit = readUnitPower(TypeRepr.of[UnitType])
    val bitSlice: BitSlice = bitSlices.reverse.find(_.unitPower == lookupUnit).get

    '{(($value.asInstanceOf[Long] >>> ${Expr(bitSlice.shift)}) & ${Expr(bitSlice.ones)}).toInt}
  