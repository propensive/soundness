package nomenclature

import anticipation.*
import fulminate.*
import rudiments.*
import contingency.*
import spectacular.*

import scala.quoted.*
import scala.compiletime.*

object Nomenclature2:
  def disintersection[IntersectionType: Type](using Quotes): Expr[Tuple] =
    import quotes.reflect.*

    def decompose(repr: TypeRepr): Set[TypeRepr] = repr.dealias.asMatchable match
      case AndType(left, right) => decompose(left) ++ decompose(right)
      case other                => Set(other)
    
    def build(todo: List[TypeRepr]): TypeRepr = todo match
      case Nil          => TypeRepr.of[EmptyTuple]
      case next :: todo => (next.asType: @unchecked) match
        case '[next] => (build(todo).asType: @unchecked) match
          case '[type tupleType <: Tuple; tupleType] => TypeRepr.of[next *: tupleType]
        
    (build(decompose(TypeRepr.of[IntersectionType]).to(List)).asType: @unchecked) match
      case '[type tupleType <: Tuple; tupleType] => '{null.asInstanceOf[tupleType]}

transparent inline def disintersect[IntersectionType] =
  ${Nomenclature2.disintersection[IntersectionType]}