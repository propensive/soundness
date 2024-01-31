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

  private case class BitSlice(unitPower: UnitPower, max: Int, width: Int, shift: Int):
    def ones: Long = -1L >>> (64 - width)

  private def bitSlices[UnitsType: Type](using Quotes): List[BitSlice] =
    import quotes.reflect.*
    
    def untuple
        [TupleType: Type]
        (dimension: Optional[DimensionRef], result: List[UnitPower])
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

