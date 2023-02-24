package quantify

import scala.quoted.*
import scala.compiletime.ops.int

object QuantifyMacros:

  def deconstruct
      (using quotes: Quotes)(typ: quotes.reflect.TypeRepr, invert: Boolean)
      : Map[quotes.reflect.TypeRef, (quotes.reflect.TypeRef, Int)] =
    import quotes.*, reflect.*
    typ match
      case AndType(left, right) =>
        deconstruct(left, invert) ++ deconstruct(right, invert)
      
      case AppliedType(unit@TypeRef(_, _), List(ConstantType(IntConstant(power)))) =>
        unit.asType match
          case '[ Units[p, u] ] => TypeRepr.of[u] match
            case ref@TypeRef(_, _) => Map(ref -> (unit, power))
            case _ => ???
          case _ => ???
      
      case other => report.errorAndAbort(other.toString)
  
  def collectUnits[UnitsType <: Units[?, ?]: Type](using Quotes): Expr[Map[String, Int]] =
    import quotes.*, reflect.*
    
    def mkMap(expr: Expr[Map[String, Int]], todo: List[(TypeRef, Int)]): Expr[Map[String, Int]] = todo match
      case Nil =>
        expr
      
      case (ref, power) :: todo2 => AppliedType(ref, List(ConstantType(IntConstant(1)))).asType match
        case '[ refType ] =>
          val unitName = Expr.summon[UnitName[refType]].get
          mkMap('{$expr.updated($unitName.name(), ${Expr(power)})}, todo2)
    
    mkMap('{Map[String, Int]()}, deconstruct(TypeRepr.of[UnitsType], false).values.to(List))

  def multiply
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (left: Expr[Quantity[LeftType]], right: Expr[Quantity[RightType]], invert: Boolean)(using Quotes)
      : Expr[Any] =
    import quotes.*, reflect.*

    val rightMap: Map[TypeRef, (TypeRef, Int)] = deconstruct(TypeRepr.of[RightType], invert)
    val leftMap: Map[TypeRef, (TypeRef, Int)] = deconstruct(TypeRepr.of[LeftType], false)

    def recur
        (map: Map[TypeRef, (TypeRef, Int)], todo: List[(TypeRef, (TypeRef, Int))], multiplier: Expr[Double])
        : (Map[TypeRef, (TypeRef, Int)], Expr[Double]) =
      todo match
        case Nil => (map, multiplier)
        case (dimension, (rightUnit, rightPower)) :: todo2 =>
          map.get(dimension) match
            case None     => recur(map.updated(dimension, (rightUnit, if invert then -rightPower else rightPower)), todo2, multiplier)
            case Some((leftUnit, leftPower)) =>
              AppliedType(leftUnit, List(ConstantType(IntConstant(1)))).asType match
                case '[ left ] => AppliedType(rightUnit, List(ConstantType(IntConstant(1)))).asType match
                  case '[ right ] => 
                    val multiplier2: Expr[Double] =
                      if leftUnit == rightUnit then multiplier else
                        def coefficient = Expr.summon[Coefficient[right & Units[1, ?], left & Units[1, ?]]]
                        def coefficient2 = Expr.summon[Coefficient[left & Units[1, ?], right & Units[1, ?]]]

                        val coefficientExpr =
                          coefficient.map { tc => '{math.pow($tc.value, ${Expr(if invert then rightPower else -rightPower)})} }.orElse:
                            coefficient2.map { tc => '{math.pow($tc.value, ${Expr(if invert then rightPower else -rightPower)})} }
                          .getOrElse:
                            val dimName = dimension match { case TypeRef(_, name) => name }
                            val leftName = leftUnit match { case TypeRef(_, name) => name }
                            val rightName = rightUnit match { case TypeRef(_, name) => name }

                            report.errorAndAbort(s"quantify: the left and right operand use incompatible units for the $dimName dimension\n\nThe left operand uses $leftName.\nThe right operand uses $rightName.\n\nThis can be resolved by defining a contextual Coefficient[$leftName[1], $rightName[1]].")
                    
                        '{$coefficientExpr*$multiplier}
                    
                    recur(map.updated(dimension, (leftUnit, if invert then leftPower - rightPower else leftPower + rightPower)), todo2, multiplier2)
                  case _ => ???//throw Mistake("Should never match")
                case _ => ???//throw Mistake("Should never match")
  
      
    val (map, multiplier) = recur(leftMap, rightMap.to(List), Expr(1.0))

    def construct(types: List[(TypeRef, Int)]): Option[TypeRepr] = types.filter(_(1) != 0) match
      case Nil                  => None
      case (ref, power) :: Nil  => Some(AppliedType(ref, List(ConstantType(IntConstant(power)))))
      case (ref, power) :: more => Some(AndType(AppliedType(ref, List(ConstantType(IntConstant(power)))), construct(more).get))
    
    val number = if invert then '{$left.value/$right.value} else '{$left.value*$right.value}
    
    construct(map.values.to(List)) match
      case None => '{$multiplier*$number}
      case Some(typeRepr) => typeRepr.asType match
        case '[ t ] => '{Quantity[t & Units[?, ?]]($multiplier*$number)}
        case _      => ???
