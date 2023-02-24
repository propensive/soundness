package quantify

import scala.quoted.*

object QuantifyMacros:
  def multiply
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (left: Expr[Quantity[LeftType]], right: Expr[Quantity[RightType]])(using Quotes)
      : Expr[Any] =
    operation[LeftType, RightType](left, right)(_ + _)('{ (a: Double, b: Double) => a*b })
  
  def divide
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (left: Expr[Quantity[LeftType]], right: Expr[Quantity[RightType]])(using Quotes)
      : Expr[Any] =
    operation[LeftType, RightType](left, right)(_ - _)('{ (a: Double, b: Double) => a/b })

  private def operation
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (left: Expr[Quantity[LeftType]], right: Expr[Quantity[RightType]])
      (combine: (Int, Int) => Int)
      (ap: Expr[(Double, Double) => Double])(using Quotes)
      : Expr[Any] =
    import quotes.*, reflect.*
    
    def deconstruct(typ: TypeRepr): Map[TypeRef, (TypeRef, Int)] = typ match
      case AndType(left, right) =>
        deconstruct(left) ++ deconstruct(right)
      
      case AppliedType(unit@TypeRef(_, _), List(ConstantType(IntConstant(power)))) =>
        unit.asType match
          case '[ Units[p, u] ] => TypeRepr.of[u] match
            case ref@TypeRef(_, _) => Map(ref -> (unit, power))
            case _ => ???
          case _ => ???
      
      case other => ???

    val rightMap: Map[TypeRef, (TypeRef, Int)] = deconstruct(TypeRepr.of[RightType])
    val leftMap: Map[TypeRef, (TypeRef, Int)] = deconstruct(TypeRepr.of[LeftType])

    def recur
        (map: Map[TypeRef, (TypeRef, Int)], todo: List[(TypeRef, (TypeRef, Int))], multiplier: Expr[Double])
        : (Map[TypeRef, (TypeRef, Int)], Expr[Double]) =
      todo match
        case Nil => (map, multiplier)
        case (dimension, (unit, power)) :: todo2 =>
          map.get(dimension) match
            case None     => recur(map.updated(dimension, (unit, combine(0, power))), todo2, multiplier)
            case Some((unit2, power2)) =>
              AppliedType(unit, List(ConstantType(IntConstant(1)))).asType match
                case '[ t1 ] => AppliedType(unit2, List(ConstantType(IntConstant(1)))).asType match
                  case '[ t2 ] => 
                    val multiplier2: Expr[Double] =
                      if unit == unit2 then multiplier else
                        def coefficient = Expr.summon[Coefficient[t1 & Units[1, ?], t2 & Units[1, ?]]]
                        def coefficient2 = Expr.summon[Coefficient[t2 & Units[1, ?], t1 & Units[1, ?]]]

                        val coefficientExpr =
                          coefficient.map { tc => '{math.pow($tc.value, ${Expr(power)})} }.orElse:
                            coefficient2.map { tc => '{math.pow(1.0/$tc.value, ${Expr(power2)})} }
                          .getOrElse:
                            val dimName = dimension match
                              case TypeRef(_, name) => name
                            
                            val leftName = unit match
                              case TypeRef(_, name) => name
                            
                            val rightName = unit2 match
                              case TypeRef(_, name) => name

                            report.errorAndAbort(s"quantify: the left and right operand use incompatible units for the $dimName dimension\n\nThe left operand uses $leftName.\nThe right operand uses $rightName.\n\nThis can be resolved by defining a contextual Coefficient[$leftName[1], $rightName[1]].")
                    
                        '{$coefficientExpr*$multiplier}
                    
                    val power3 = combine(power, power2)
                    recur(map.updated(dimension, (unit, power3)), todo2, multiplier2)
                  case _ => ???//throw Mistake("Should never match")
                case _ => ???//throw Mistake("Should never match")
  
      
    val (map, multiplier) = recur(leftMap, rightMap.to(List), Expr(1.0))


    def construct(types: List[(TypeRef, Int)]): Option[TypeRepr] = types.filter(_(1) != 0) match
      case Nil                  => None
      case (ref, power) :: Nil  => Some(AppliedType(ref, List(ConstantType(IntConstant(power)))))
      case (ref, power) :: more => Some(AndType(AppliedType(ref, List(ConstantType(IntConstant(power)))), construct(more).get))

    construct(map.values.to(List)) match
      case None => '{$multiplier*$ap($left.value, $right.value)}
      case Some(typeRepr) => typeRepr.asType match
        case '[ t ] => '{Quantity[t & Units[?, ?]]($multiplier*$ap($left.value, $right.value))}
        case _      => ???
