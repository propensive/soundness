package quantify

import rudiments.*

import scala.quoted.*
import scala.annotation.nowarn

object QuantifyMacros:
  private def deconjunct
      (using quotes: Quotes)(typ: quotes.reflect.TypeRepr, division: Boolean)
      : Map[quotes.reflect.TypeRef, (quotes.reflect.TypeRef, Int)] =
    import quotes.*, reflect.*
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
  
  def collectUnits[UnitsType <: Units[?, ?]: Type](using Quotes): Expr[Map[Text, Int]] =
    import quotes.*, reflect.*
    
    def mkMap(expr: Expr[Map[Text, Int]], todo: List[(TypeRef, Int)]): Expr[Map[Text, Int]] =
      todo match
        case Nil =>
          expr
        
        case (ref, power) :: todo2 =>
          AppliedType(ref, List(ConstantType(IntConstant(1)))).asType match
            case '[ refType ] =>
              val unitName = Expr.summon[UnitName[refType]].get
              mkMap('{$expr.updated($unitName.name(), ${Expr(power)})}, todo2)
          
            case _ =>
              throw Mistake("Should never match")
    
    mkMap('{Map[Text, Int]()}, deconjunct(TypeRepr.of[UnitsType], false).values.to(List))

  def multiply
      [LeftType <: Units[?, ?]: Type, RightType <: Units[?, ?]: Type]
      (left: Expr[Quantity[LeftType]], right: Expr[Quantity[RightType]], division: Boolean)
      (using Quotes)
      : Expr[Any] =
    import quotes.reflect.*

    val rightMap: Map[TypeRef, (TypeRef, Int)] = deconjunct(TypeRepr.of[RightType], division)
    val leftMap: Map[TypeRef, (TypeRef, Int)] = deconjunct(TypeRepr.of[LeftType], false)

    def powerType(repr: TypeRepr, n: Int): Type[?] =
      AppliedType(repr, List(ConstantType(IntConstant(n)))).asType
    
    def recur
        (map: Map[TypeRef, (TypeRef, Int)], todo: List[(TypeRef, (TypeRef, Int))],
            multiplier: Expr[Double])
        : (Map[TypeRef, (TypeRef, Int)], Expr[Double]) =
      todo match
        case Nil =>
          (map, multiplier)
        
        case (dimension, (rightUnit, rightPower)) :: todo2 =>
          map.get(dimension) match
            case None =>
              recur(map.updated(dimension, (rightUnit, if division then -rightPower else
                  rightPower)), todo2, multiplier)
            
            case Some((leftUnit, leftPower)) =>
              
              (dimension.asType, powerType(leftUnit, 1), powerType(leftUnit, -1),
                  powerType(rightUnit, 1), powerType(rightUnit, -1)) match
                case ('[dimension], '[left], '[invLeft], '[right], '[invRight]) =>
                  type dim = dimension & Dimension
                  type lUnits = left & Units[1, dim]
                  type rUnits = right & Units[1, dim]
                    
                  val preferRight = Expr.summon[PrincipalUnit[dim, rUnits]].isDefined
                    
                  val multiplier2: Expr[Double] =
                    if leftUnit.typeSymbol == rightUnit.typeSymbol then multiplier else
                      
                      def coefficient = Expr.summon[Ratio[rUnits & invLeft]]
                      def coefficient2 = Expr.summon[Ratio[lUnits & invRight]]

                      val power: Expr[Double] = Expr:
                        if division then if preferRight then leftPower else rightPower
                        else if preferRight then leftPower else -rightPower
                      
                      val coefficientExpr =
                        coefficient.map { tc => '{math.pow($tc.value.value, ${power})} }.orElse:
                          coefficient2.map { tc => '{math.pow($tc.value.value, -${power})} }
                        .getOrElse:
                          val dimName = dimension match { case TypeRef(_, name) => name }
                          val leftName = leftUnit match { case TypeRef(_, name) => name }
                          val rightName = rightUnit match { case TypeRef(_, name) => name }

                          fail(s"the left and right operand use incompatible units for the "+
                              s"$dimName dimension\n\nThe left operand uses $leftName.\nThe right "+
                              s"operand uses $rightName.\n\nThis can be resolved by defining a "+
                              s"contextual Ratio[$leftName[1], $rightName[1]].")
                  
                      '{$coefficientExpr*$multiplier}
                  
                  val typeUnits = if preferRight then rightUnit else leftUnit
                  val typePower = leftPower + (if division then -rightPower else rightPower)
                  recur(map.updated(dimension, (typeUnits, typePower)), todo2, multiplier2)
                
                case _ => fail("could not interpret unit types")
  
      
    val (map, multiplier) = recur(leftMap, rightMap.to(List), Expr(1.0))

    def construct(types: List[(TypeRef, Int)]): Option[TypeRepr] = types.filter(_(1) != 0) match
      case Nil =>
        None
      
      case (ref, power) :: Nil =>
        Some(AppliedType(ref, List(ConstantType(IntConstant(power)))))
      
      case (ref, power) :: more =>
        Some(AndType(AppliedType(ref, List(ConstantType(IntConstant(power)))), construct(more).get))
    
    val number = if division then '{$left.value/$right.value} else '{$left.value*$right.value}
    
    construct(map.values.to(List)) match
      case None => '{$multiplier*$number}
      case Some(typeRepr) => typeRepr.asType match
        case '[ t ] => '{Quantity[t & Units[?, ?]]($multiplier*$number)}
        case _      => ???
