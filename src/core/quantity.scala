/*
    Quantitative, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import anticipation.*

import scala.quoted.*
import scala.compiletime.*

//import language.experimental.captureChecking

object QuantitativeMacros:

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
    
  private def readUnitPower(using Quotes)(typeRepr: quotes.reflect.TypeRepr): UnitPower =
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

  private class DimensionRef(val dimensionType: Type[?], val name: String):
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

  private def ratio
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

  def multiplyTypeclass
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (using Quotes)
      : Expr[Operator["*", Quantity[LeftType], Quantity[RightType]]] =
    val left = UnitsMap[LeftType]
    val right = UnitsMap[RightType]

    val (leftNorm, _) = normalize(left, right, '{1.0})
    val (rightNorm, _) = normalize(right, left, '{1.0})

    ((leftNorm*rightNorm).repr.map(_.asType): @unchecked) match
      case Some('[type resultType <: Measure; resultType]) =>
        '{
          new Operator["*", Quantity[LeftType], Quantity[RightType]]:
            type Result = Quantity[resultType]
            def apply(left: Quantity[LeftType], right: Quantity[RightType]): Quantity[resultType] =
              ${QuantitativeMacros.multiply[LeftType, RightType]('left, 'right, false)}
                .asInstanceOf[Quantity[resultType]]
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

  def subtractTypeclass
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (using Quotes)
      : Expr[Operator["-", Quantity[LeftType], Quantity[RightType]]] =
    val (units, _) = normalize(UnitsMap[LeftType], UnitsMap[RightType], '{0.0})

    (units.repr.map(_.asType): @unchecked) match
      case Some('[type resultType <: Measure; resultType]) =>
        '{
          new Operator["-", Quantity[LeftType], Quantity[RightType]]:
            type Result = Quantity[resultType]
            def apply
                (left: Quantity[LeftType], right: Quantity[RightType])
                : Quantity[resultType] =
              ${QuantitativeMacros.add[LeftType, RightType]('left, 'right, '{true})}
                  .asInstanceOf[Quantity[resultType]]
        }

  def addTypeclass
      [LeftType <: Measure: Type, RightType <: Measure: Type]
      (using Quotes)
      : Expr[Operator["+", Quantity[LeftType], Quantity[RightType]]] =
    val (units, _) = normalize(UnitsMap[LeftType], UnitsMap[RightType], '{0.0})

    (units.repr.map(_.asType): @unchecked) match
      case Some('[type resultType <: Measure; resultType]) =>
        '{
          new Operator["+", Quantity[LeftType], Quantity[RightType]]:
            type Result = Quantity[resultType]
            def apply
                (left: Quantity[LeftType], right: Quantity[RightType])
                : Quantity[resultType] =
              ${QuantitativeMacros.add[LeftType, RightType]('left, 'right, '{false})}
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

  private case class BitSlice(unitPower: UnitPower, max: Int, width: Int, shift: Int):
    def ones: Long = -1L >>> (64 - width)

  private def bitSlices[UnitsType: Type](using Quotes): List[BitSlice] =
    import quotes.reflect.*
    
    def untuple
        [TupleType: Type]
        (dimension: Maybe[DimensionRef], result: List[UnitPower])
        : List[UnitPower] =
      Type.of[TupleType] match
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
    val principalUnit = cascade.head.ref.dimensionRef.principal
    
    def width(value: Double, n: Int = 1): Int = if (1 << n) >= value then n else width(value, n + 1)

    def recur(unit: List[UnitPower], next: UnitPower, factor: Double, shift: Int): List[BitSlice] =
      unit match
        case Nil =>
          List(BitSlice(cascade.last, Int.MaxValue, 64 - shift, shift))
        
        case head :: tail =>
          val value = ratio(head.ref, principalUnit, head.power).valueOrAbort
          val max = value/factor
          val bitSlice = BitSlice(next, (max + 0.5).toInt, width(max), shift)
          
          bitSlice :: recur(tail, head, value, shift + width(max))
    
    recur(cascade, cascade.head, 1.0, -1).tail.reverse

  def make[UnitsType <: Tuple: Type](values: Expr[Seq[Int]])(using Quotes): Expr[Count[UnitsType]] =
    val inputs: List[Expr[Int]] = (values: @unchecked) match
      case Varargs(values) => values.to(List).reverse
    
    def recur(slices: List[BitSlice], values: List[Expr[Int]], expr: Expr[Long]): Expr[Long] =
      values match
        case Nil =>
          expr
        
        case unitValue :: valuesTail => slices match
          case BitSlice(unitPower, max, width, shift) :: tail =>
            unitValue.value match
              case Some(unitValue) =>
                if unitValue < 0 then fail(msg"""
                  the value for the ${unitPower.ref.name} unit ($unitValue) cannot be a negative
                  number
                """)
                else if unitValue >= max then fail(msg"""
                  the value for the ${unitPower.ref.name} unit ${unitValue} must be less than ${max}
                """)
                recur(tail, valuesTail, '{$expr + (${Expr(unitValue.toLong)} << ${Expr(shift)})})
              
              case None =>
                recur(tail, valuesTail, '{$expr + ($unitValue.toLong << ${Expr(shift)})})
          
          case Nil =>
            fail(msg"""
              ${inputs.length} unit values were provided, but this Count only has ${slices.length}
              units
            """)
    
    '{Count.fromLong[UnitsType](${recur(bitSlices[UnitsType].reverse, inputs, '{0L})})}

  def addCount
      [CountUnitsType <: Tuple: Type]
      (left: Expr[Count[CountUnitsType]], right: Expr[Count[CountUnitsType]])
      (using Quotes)
      : Expr[Count[CountUnitsType]] =
    import quotes.reflect.*

    val slices = bitSlices[CountUnitsType]

    '{
      var total: Long = 0
      var part: Long = 0
      
      ${
        def recur(slices: List[BitSlice], statements: Expr[Unit]): Expr[Unit] = slices match
          case Nil =>
            statements
        
          case (slice@BitSlice(unitPower, max, width, shift)) :: tail =>
            recur(tail, '{
              $statements
              part += ($left.asInstanceOf[Long] >>> ${Expr(shift)}) & ${Expr(slice.ones)}
              part += ($right.asInstanceOf[Long] >>> ${Expr(shift)}) & ${Expr(slice.ones)}
              
              if part < ${Expr(max)} then
                total += part << ${Expr(shift)}
                part = 0
              else
                total += (part - ${Expr(max)}) << ${Expr(shift)}
                part = 1
            })
      
        recur(slices.reverse, '{()})
      }
      
      Count.fromLong(total)
    }

  def describeCount
      [CountUnits <: Tuple: Type]
      (count: Expr[Count[CountUnits]])
      (using Quotes)
      : Expr[ListMap[Text, Long]] =
    def recur(slices: List[BitSlice], expr: Expr[ListMap[Text, Long]]): Expr[ListMap[Text, Long]] =
      slices match
        case Nil =>
          expr
        
        case (slice@BitSlice(unitPower, max, width, shift)) :: tail =>
          val value = '{($count.asInstanceOf[Long] >>> ${Expr(shift)}) & ${Expr(slice.ones)}}
          recur(tail, '{$expr.updated(${unitPower.ref.unitName}, $value)})

    recur(bitSlices[CountUnits], '{ListMap()})

  def multiplyCount
      [CountUnitsType <: Tuple: Type]
      (count: Expr[Count[CountUnitsType]], multiplier: Expr[Double], division: Boolean)
      (using Quotes)
      : Expr[Any] =
    val principal = bitSlices[CountUnitsType].head.unitPower.ref.dimensionRef.principal
    
    (principal.power(1).asType: @unchecked) match
      case '[type unitType <: Measure; unitType] =>
        val quantityExpr = toQuantity[CountUnitsType](count).asExprOf[Quantity[unitType]]
        val multiplier2 = if division then '{1.0/$multiplier} else multiplier
        val multiplied = multiply('{Quantity($multiplier2)}, quantityExpr, false)
        val quantity2 = multiplied.asExprOf[Quantity[unitType]]
        
        fromQuantity[unitType, CountUnitsType](quantity2)

  def toQuantity
      [CountUnitsType <: Tuple: Type]
      (count: Expr[Count[CountUnitsType]])
      (using Quotes)
      : Expr[Any] =
    val slices = bitSlices[CountUnitsType]
    val quantityUnit = slices.head.unitPower.ref.dimensionRef.principal

    def recur(slices: List[BitSlice], expr: Expr[Double]): Expr[Double] = slices match
      case Nil =>
        expr
      
      case (slice@BitSlice(unitPower, max, width, shift)) :: tail =>
        val factor = ratio(unitPower.ref, quantityUnit, unitPower.power)
        recur(tail, '{$expr + $factor*(($count.longValue >>> ${Expr(shift)}) &
            ${Expr(slice.ones)})})
    
    (quantityUnit.power(1).asType: @unchecked) match
      case '[type quantityType <: Measure; quantityType] =>
        '{Quantity[quantityType](${recur(slices, '{0.0})})}

  def fromQuantity
      [QuantityType <: Measure: Type, CountUnitsType <: Tuple: Type]
      (quantity: Expr[Quantity[QuantityType]])
      (using Quotes)
      : Expr[Count[CountUnitsType]] =
    import quotes.reflect.*
    
    val slices = bitSlices[CountUnitsType]
    val quantityUnit = readUnitPower(TypeRepr.of[QuantityType].dealias)
    val rounding = ratio(slices.last.unitPower.ref, quantityUnit.ref, slices.last.unitPower.power)
    
    '{
      var result: Long = 0L
      var current: Double = $quantity.underlying + $rounding/2
      var part: Int = 0
      ${
        def recur(bitSlices: List[BitSlice], statements: Expr[Unit]): Expr[Unit] =
          bitSlices match
            case Nil =>
              statements
            
            case (bitSlice@BitSlice(unitPower, max, width, shift)) :: tail =>
              val ratioExpr = ratio(unitPower.ref, quantityUnit.ref, unitPower.power)
              
              val expr = '{
                $statements
                part = (current/$ratioExpr).toInt
                result = result + (part << ${Expr(shift)})
                current = current - part*$ratioExpr
              }
              
              recur(tail, expr)
            
        recur(slices, '{})
      }
      
      Count.fromLong[CountUnitsType](result)
    }

  def get
      [UnitsType <: Tuple: Type, UnitType <: Units[1, ? <: Dimension]: Type]
      (value: Expr[Count[UnitsType]])
      (using Quotes)
      : Expr[Int] =
    import quotes.reflect.*
  
    val slices = bitSlices[UnitsType]
    val lookupUnit = readUnitPower(TypeRepr.of[UnitType])
    
    val bitSlice: BitSlice = slices.find(_.unitPower == lookupUnit).getOrElse:
      fail(msg"the Count does not include this unit")

    '{(($value.longValue >>> ${Expr(bitSlice.shift)}) & ${Expr(bitSlice.ones)}).toInt}
  
