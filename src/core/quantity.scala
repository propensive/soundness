/*
    Quantitative, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package quantitative

import rudiments.*
import fulminate.*
import symbolism.*
import hypotenuse.*
import anticipation.*

import scala.quoted.*
import scala.compiletime.*

//import language.experimental.captureChecking

given Realm = realm"quantitative"
trait Quantitative2:
  case class UnitPower(ref: UnitRef, power: Int)
  
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

  case class Dimensionality(map: Map[DimensionRef, Int]):
    def quantityName(using Quotes): Option[String] =
      import quotes.reflect.*
      def recur(todo: List[(DimensionRef, Int)], current: TypeRepr): TypeRepr = todo match
        case Nil =>
          current
        
        case (dimension, n) :: tail =>
          ((current.asType, dimension.power(n).asType): @unchecked) match
            case ('[current], '[next]) => recur(tail, TypeRepr.of[current & next])
      
      (recur(map.to(List), TypeRepr.of[Units[?, ?]]).asType: @unchecked) match
        case '[type units <: Units[?, ?]; units] =>
          Expr.summon[PhysicalQuantity[units, ?]].map: value =>
            (value: @unchecked) match
              case '{$name: dimensionType} => (Type.of[dimensionType]: @unchecked) match
                case '[PhysicalQuantity[?, name]] =>
                  (TypeRepr.of[name].asMatchable: @unchecked) match
                    case ConstantType(StringConstant(name)) => name
    
  def readUnitPower(using Quotes)(typeRepr: quotes.reflect.TypeRepr): UnitPower =
    import quotes.reflect.*
    
    (typeRepr.asMatchable: @unchecked) match
      case AppliedType(unit, List(constantType)) => (constantType.asMatchable: @unchecked) match
        case ConstantType(constant) => (constant: @unchecked) match
          case IntConstant(power) => (unit.asMatchable: @unchecked) match
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
    infix def * (that: UnitsMap): UnitsMap = new UnitsMap(
      (dimensions ++ that.dimensions).to(Set).to(List).map: dim =>
        val dimUnit = unit(dim).orElse(that.unit(dim)).get
        dim -> UnitPower(dimUnit, (unitPower(dim) + that.unitPower(dim)))
      .to(Map).filter(_(1).power != 0)
    )
    
    @targetName("divide")
    infix def / (that: UnitsMap): UnitsMap = new UnitsMap(
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

  class UnitRef(val unitType: Type[?], val name: String):
    def unitName(using Quotes): Expr[Text] = (power(1).asType: @unchecked) match
      case '[unitType] => Expr.summon[UnitName[unitType]] match
        case None       => '{Text(${Expr(name)})}
        case Some(name) => '{$name.text}

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
      case _             => false
    
    override def hashCode: Int = name.hashCode
    override def toString(): String = name

  class DimensionRef(val dimensionType: Type[?], val name: String):
    def ref(using Quotes): quotes.reflect.TypeRepr =
      (dimensionType: @unchecked) match { case '[ref] => quotes.reflect.TypeRepr.of[ref] }
    
    def dimensionality(using Quotes): Dimensionality = Dimensionality(Map(this -> 1))
    
    def power(n: Int)(using Quotes): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      
      ((ConstantType(IntConstant(n)).asType, ref.asType): @unchecked) match
        case ('[type power <: Nat; power], '[type dimension <: Dimension; dimension]) =>
          TypeRepr.of[Units[power, dimension]]
    
    def principal(using Quotes): UnitRef =
      import quotes.reflect.*
      
      (dimensionType: @unchecked) match
        case '[type dimensionType <: Dimension; dimensionType] =>
          (Expr.summon[PrincipalUnit[dimensionType, ?]]: @unchecked) match
            case None =>
              val dimensionName =
                dimensionality.quantityName.map: name =>
                  "the physical quantity "+name
                .getOrElse("the same quantity")
  
              fail(msg"""
                the operands both represent ${dimensionName}, but there is no principal unit
                specified for this dimension
              """)

            case Some('{$expr: principalUnit}) => (Type.of[principalUnit]: @unchecked) match
              case '[PrincipalUnit[dimensionType, units]] =>
                (TypeRepr.of[units].asMatchable: @unchecked) match
                  case TypeLambda(_, _, appliedType) => (appliedType.asMatchable: @unchecked) match
                    case AppliedType(typeRef, _) => (typeRef.asMatchable: @unchecked) match
                      case typeRef@TypeRef(_, _) => UnitRef(typeRef.asType, typeRef.show)
                  
                  case other =>
                    fail(msg"principal units had an unexpected type: ${other.show}")
        
    
    override def equals(that: Any): Boolean = that.asMatchable match
      case that: DimensionRef => name == that.name
      case _                      => false
    
    override def hashCode: Int = name.hashCode
    override def toString(): String = name

  def ratio
      (using Quotes)
      (from: UnitRef, to: UnitRef, power: Int, retry: Boolean = true, viaPrincipal: Boolean = true)
      : Expr[Double] =
    import quotes.reflect.*

    val principalUnit = from.dimensionRef.principal
    if from == to then Expr(1.0)
    else ((from.power(-1).asType, to.power(1).asType): @unchecked) match
      case ('[type fromType <: Measure; fromType], '[type toType <: Measure; toType]) =>
        (Expr.summon[Ratio[fromType & toType, ?]]: @unchecked) match
          case None =>
            if retry then ratio(to, from, -power, false)
            else if viaPrincipal && from != principalUnit && to != principalUnit then
              val numerator = ratio(from, principalUnit, power, true, false)
              val denominator = ratio(to, principalUnit, power, true, false)
              '{$numerator/$denominator}
            else
              val quantityName = from.dimensionRef.dimensionality.quantityName
              
              val dimensionName = quantityName.map("the physical quantity "+_).getOrElse:
                  "the same physical quantity"
              
              fail(msg"""
                both operands represent $dimensionName, but the coversion ratio between them is not
                known
  
                To provide the conversion ratio, please provide a contextual instance in scope, with
                the type, `Ratio[${from.name}[1] & ${to.name}[-1]]`, or `Ratio[${to.name}[1] &
                ${from.name}[-1]]`.
              """)

          case Some('{$ratio: ratioType}) => (Type.of[ratioType]: @unchecked) match
            case '[Ratio[?, double]] => (TypeRepr.of[double].asMatchable: @unchecked) match
              case ConstantType(constant) => (constant: @unchecked) match
                case DoubleConstant(double) => Expr(double**power)

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
    def recur(expr: Expr[Map[Text, Int]], todo: List[UnitPower]): Expr[Map[Text, Int]] =
      todo match
        case Nil =>
          expr
        
        case UnitPower(unit, power) :: todo2 =>
          (unit.power(1).asType: @unchecked) match
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
    val left: UnitsMap = UnitsMap[LeftType]
    val right: UnitsMap = UnitsMap[RightType]
    
    val (left2, leftValue) = normalize(left, right, '{$leftExpr.underlying})
    val (right2, rightValue) = normalize(right, left, '{$rightExpr.underlying})

    val resultUnits = if division then left2/right2 else left2*right2
    val resultValue = if division then '{$leftValue/$rightValue} else '{$leftValue*$rightValue}
    
    (resultUnits.repr.map(_.asType): @unchecked) match
      case Some('[type units <: Measure; units]) =>
        '{Quantity[units]($resultValue)}
      
      case _ =>
        resultValue

  private def incompatibleTypes(left: UnitsMap, right: UnitsMap)(using Quotes): Nothing =
    (left.dimensionality.quantityName, right.dimensionality.quantityName) match
      case (Some(leftName), Some(rightName)) =>
        fail(msg"""
          the left operand represents $leftName, but the right operand represents $rightName; these
          are incompatible physical quantities
        """)
      
      case _ =>
        fail(msg"the operands represent different physical quantities")

  def mulTypeclass
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (using Quotes)
      : Expr[MulOperator[Quantity[LeftType], Quantity[RightType]]] =
    val left = UnitsMap[LeftType]
    val right = UnitsMap[RightType]

    val (leftNorm, _) = normalize(left, right, '{1.0})
    val (rightNorm, _) = normalize(right, left, '{1.0})

    ((leftNorm*rightNorm).repr.map(_.asType): @unchecked) match
      case Some('[type resultType <: Measure; resultType]) =>
        '{
          new MulOperator[Quantity[LeftType], Quantity[RightType]]:
            type Result = Quantity[resultType]
            def mul(left: Quantity[LeftType], right: Quantity[RightType]): Quantity[resultType] =
              ${Quantitative.multiply[LeftType, RightType]('left, 'right, false)}
                .asInstanceOf[Quantity[resultType]]
        }
      
      case None =>
        '{
          new MulOperator[Quantity[LeftType], Quantity[RightType]]:
            type Result = Double
            def div(left: Quantity[LeftType], right: Quantity[RightType]): Double =
              ${Quantitative.multiply[LeftType, RightType]('left, 'right, false)}.asInstanceOf[Double]
        }


  def divTypeclass
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (using Quotes)
      : Expr[DivOperator[Quantity[LeftType], Quantity[RightType]]] =
    val left = UnitsMap[LeftType]
    val right = UnitsMap[RightType]

    val (leftNorm, _) = normalize(left, right, '{1.0})
    val (rightNorm, _) = normalize(right, left, '{1.0})

    ((leftNorm/rightNorm).repr.map(_.asType): @unchecked) match
      case Some('[type resultType <: Measure; resultType]) =>
        '{
          new DivOperator[Quantity[LeftType], Quantity[RightType]]:
            type Result = Quantity[resultType]
            def div(left: Quantity[LeftType], right: Quantity[RightType]): Quantity[resultType] =
              ${Quantitative.multiply[LeftType, RightType]('left, 'right, true)}
                .asInstanceOf[Quantity[resultType]]
        }
      
      case None =>
        '{
          new DivOperator[Quantity[LeftType], Quantity[RightType]]:
            type Result = Double
            def div(left: Quantity[LeftType], right: Quantity[RightType]): Double =
              ${Quantitative.multiply[LeftType, RightType]('left, 'right, true)}.asInstanceOf[Double]
        }
  
  def sqrtTypeclass[ValueType <: Measure: Type](using Quotes): Expr[SquareRoot[Quantity[ValueType]]] =
    val units = UnitsMap[ValueType]
    if !units.map.values.all(_.power%2 == 0)
    then fail(msg"only quantities with units in even powers can have square roots calculated")
    else
      (UnitsMap(units.map.view.mapValues { case UnitPower(unit, power) => UnitPower(unit, power/2) }.toMap).repr.get.asType: @unchecked) match
        case '[type resultType <: Measure; resultType] =>
          '{
            new SquareRoot[Quantity[ValueType]]:
              type Result = Quantity[resultType]
              
              def squareRoot(value: Quantity[ValueType]): Quantity[resultType] =
                math.sqrt(value.value).asInstanceOf[Quantity[resultType]]
          }
      
  def cbrtTypeclass[ValueType <: Measure: Type](using Quotes): Expr[CubeRoot[Quantity[ValueType]]] =
    val units = UnitsMap[ValueType]
    if !units.map.values.all(_.power%3 == 0)
    then fail(msg"only quantities with units in powers which are multiples of 3 can have cube roots calculated")
    else
      (UnitsMap(units.map.view.mapValues { case UnitPower(unit, power) => UnitPower(unit, power/3) }.toMap).repr.get.asType: @unchecked) match
        case '[type resultType <: Measure; resultType] =>
          '{
            new CubeRoot[Quantity[ValueType]]:
              type Result = Quantity[resultType]
              
              def cubeRoot(value: Quantity[ValueType]): Quantity[resultType] =
                math.cbrt(value.value).asInstanceOf[Quantity[resultType]]
          }

  def greaterThan
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (leftExpr: Expr[Quantity[LeftType]], rightExpr: Expr[Quantity[RightType]],
          strict: Expr[Boolean], invert: Expr[Boolean])
      (using Quotes)
      : Expr[Boolean] =
    import quotes.reflect.*

    val left: UnitsMap = UnitsMap[LeftType]
    val right: UnitsMap = UnitsMap[RightType]
    val closed = !strict.valueOrAbort

    val (left2, leftValue) = normalize(left, right, '{$leftExpr.underlying})
    val (right2, rightValue) = normalize(right, left, '{$rightExpr.underlying})

    if left2 != right2 then incompatibleTypes(left, right)

    if !invert.valueOrAbort then
      if closed then '{$leftValue <= $rightValue} else '{$leftValue < $rightValue}
    else if closed then '{$leftValue >= $rightValue} else '{$leftValue > $rightValue}

  def add
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (leftExpr: Expr[Quantity[LeftType]], rightExpr: Expr[Quantity[RightType]], sub: Expr[Boolean])
      (using Quotes)
      : Expr[Any] =
    import quotes.reflect.*

    val left: UnitsMap = UnitsMap[LeftType]
    val right: UnitsMap = UnitsMap[RightType]

    val (left2, leftValue) = normalize(left, right, '{$leftExpr.underlying})
    val (right2, rightValue) = normalize(right, left, '{$rightExpr.underlying})

    if left2 != right2 then incompatibleTypes(left, right)
    
    val resultValue = sub.value match
      case Some(sub) => if sub then '{$leftValue - $rightValue} else '{$leftValue + $rightValue}
      case None      => '{if $sub then $leftValue - $rightValue else $leftValue + $rightValue}
    
    (left2.repr.map(_.asType): @unchecked) match
      case Some('[type unitsType <: Measure; unitsType]) =>
        '{Quantity[unitsType]($resultValue)}
      
      case _ =>
        resultValue

  def subTypeclass
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (using Quotes)
      : Expr[SubOperator[Quantity[LeftType], Quantity[RightType]]] =
    val (units, _) = normalize(UnitsMap[LeftType], UnitsMap[RightType], '{0.0})

    (units.repr.map(_.asType): @unchecked) match
      case Some('[type resultType <: Measure; resultType]) =>
        '{
          new SubOperator[Quantity[LeftType], Quantity[RightType]]:
            type Result = Quantity[resultType]
            def sub
                (left: Quantity[LeftType], right: Quantity[RightType])
                : Quantity[resultType] =
              ${Quantitative.add[LeftType, RightType]('left, 'right, '{true})}
                  .asInstanceOf[Quantity[resultType]]
        }

  def addTypeclass
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (using Quotes)
      : Expr[AddOperator[Quantity[LeftType], Quantity[RightType]]] =
    val (units, _) = normalize(UnitsMap[LeftType], UnitsMap[RightType], '{0.0})

    (units.repr.map(_.asType): @unchecked) match
      case Some('[type resultType <: Measure; resultType]) =>
        '{
          new AddOperator[Quantity[LeftType], Quantity[RightType]]:
            type Result = Quantity[resultType]
            def add
                (left: Quantity[LeftType], right: Quantity[RightType])
                : Quantity[resultType] =
              ${Quantitative.add[LeftType, RightType]('left, 'right, '{false})}
                  .asInstanceOf[Quantity[resultType]]
        }

  def norm
      [UnitsType <: Measure: Type, NormType[power <: Nat] <: Units[power, ?]: Type]
      (expr: Expr[Quantity[UnitsType]])(using Quotes)
      : Expr[Any] =
    val units: UnitsMap = UnitsMap[UnitsType]
    val norm: UnitsMap = UnitsMap[NormType[1]]
    val (units2, value) = normalize(units, norm, '{$expr.underlying}, true)

    (units2.repr.map(_.asType): @unchecked) match
      case Some('[type unitsType <: Measure; unitsType]) =>
        '{Quantity[unitsType]($value)}
      
      case None =>
        value
  
  def describe[UnitsType <: Measure: Type](using Quotes): Expr[Text] =
    UnitsMap[UnitsType].dimensionality.quantityName match
      case Some(name) => '{Text(${Expr(name)})}
      case None       => fail(msg"there is no descriptive name for this physical quantity")
