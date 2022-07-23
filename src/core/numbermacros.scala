package cardinality

import scala.quoted.*

object CardinalityMacro:
  def apply[D1 <: Double: Type, D2 <: Double: Type](digits: Expr[String])(using Quotes): Expr[D1 ~ D2] =
    import quotes.*, reflect.*
    digits.value match
      case Some(str) =>
        (TypeRepr.of[D1], TypeRepr.of[D2]) match
          case (ConstantType(lb), ConstantType(ub)) => (lb.value, ub.value) match
            case (lb: Double, ub: Double) =>
              val value = str.toDouble
              
              if value < lb
              then report.errorAndAbort(s"the value $str is less than the lower bound for this value, $lb")
              
              if value > ub
              then report.errorAndAbort(s"the value $str is greater than the upper bound for this value, $ub")
   
              '{${Expr(value)}.asInstanceOf[D1 ~ D2]}
      case None =>
        '{NumericRange($digits.toDouble)}
