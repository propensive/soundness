/*
    Contingency, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package contingency

import fulminate.*
import rudiments.*
import anticipation.*

import scala.quoted.*
import scala.compiletime.*

given realm: Realm = realm"contingency"

object Contingency:
  def mitigate[ErrorType <: Error: Type, ResultType: Type]
      (context: Expr[Tended[ErrorType, ResultType]],
       handler: Expr[PartialFunction[ErrorType, ? <: Error]])
      (using Quotes)
           : Expr[Any] = '{???}
  
  def remedy[ErrorType <: Error: Type, ResultType: Type]
      (context: Expr[Tended[ErrorType, ResultType]],
       handler: Expr[PartialFunction[ErrorType, ResultType]])
      (using Quotes)
           : Expr[Any] =

    import quotes.reflect.*

    val action: Expr[Errant[ErrorType] ?=> ResultType] = context.asTerm match
      case Inlined(None, Nil, Apply(_, List(action))) =>
        action.asExprOf[Errant[ErrorType] ?=> ResultType]

    def exhaustive(pattern: Tree, patternType: TypeRepr): Boolean = pattern match
      case Wildcard()          => true
      case Typed(_, matchType) => patternType <:< matchType.tpe
      case Bind(_, pattern)    => exhaustive(pattern, patternType)

      case TypedOrTest(Unapply(Select(target, method), _, params), _) =>
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || fail(msg"bad pattern")

      case Unapply(Select(target, method), _, params) =>
        // TODO: Check that extractor is exhaustive
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || fail(msg"bad pattern")

      case other =>
        fail(msg"bad pattern")

    def patternType(pattern: Tree): List[TypeRepr] = pattern match
      case Typed(_, matchType)   => List(matchType.tpe)
      case Bind(_, pattern)      => patternType(pattern)
      case Alternatives(patters) => patters.flatMap(patternType)
      case Wildcard()            => fail(msg"wildcard")
      
      case TypedOrTest(Unapply(Select(target, method), _, _), typeTree) =>
        if exhaustive(pattern, typeTree.tpe) then List(typeTree.tpe) else Nil
      
      case other =>
        fail(msg"too general: ${other.toString}")

    val caseDefs: List[CaseDef] = handler.asTerm match
      case Inlined(None, Nil, Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _)) =>
        cases.flatMap:
          case caseDef@CaseDef(pattern, None, rhs) => List(caseDef)
          case _                                   => Nil
      
      case _ =>
        fail(msg"unexpected lambda")

    val handledTypes: List[TypeRepr] = caseDefs.flatMap:
      case CaseDef(pattern, _, _) => patternType(pattern)
      
    val resolutionErrorTypes: List[TypeRepr] = caseDefs.flatMap:
      case CaseDef(_, _, rhs) => List(rhs.tpe)

    def unpack(repr: TypeRepr): Set[TypeRepr] = repr.asMatchable match
      case OrType(left, right) => unpack(left) ++ unpack(right)
      case other               => Set(other)
    
    def pack(reprs: List[TypeRepr]): TypeRepr = reprs.foldLeft(TypeRepr.of[Error])(OrType(_, _))
    
    val requiredHandlers = unpack(TypeRepr.of[ErrorType])
    val unhandledErrorTypes: List[TypeRepr] = (requiredHandlers -- handledTypes).to(List)

    val errantTypes: List[TypeRepr] = unhandledErrorTypes.map(_.asType).map:
      case '[type errorType <: Error; errorType] => TypeRepr.of[Errant[errorType]]

    def typeLambda[InsideType[_]: Type](errorTypes: List[TypeRepr])
        (makeExpr: Expr[PartialFunction[ErrorType, Nothing] => ResultType] => Expr[InsideType[ResultType]])
            : Term =

      errorTypes match
        case Nil =>
          makeExpr
           ('{
              (pf: PartialFunction[ErrorType, Nothing]) =>
                boundary: label ?=>
                  $action(using new RemedyErrant[ResultType, ErrorType]($handler.andThen(boundary.break(_)).orElse(pf), label))
            }).asTerm
        
        case errorType :: more => errorType.asType match
          case '[type errorType <: ErrorType; errorType] =>
            typeLambda[[ParamType] =>> Errant[errorType] ?=> InsideType[ParamType]](more):
              (makeResult: Expr[PartialFunction[ErrorType, Nothing] => ResultType]) =>
                '{ (errant: Errant[errorType]) ?=>
                  val makeResult2: PartialFunction[ErrorType, Nothing] => ResultType =
                    pf => $makeResult(pf.orElse { case error: `errorType` => errant.abort(error) })
                  ${makeExpr('makeResult2)}
                }
    
    val result = typeLambda[[ParamType] =>> ParamType](unhandledErrorTypes): fn =>
      '{$fn(PartialFunction.empty[ErrorType, Nothing])}
    
    result.asExpr

class RemedyErrant[ResultType, ErrorType <: Error](pf: PartialFunction[ErrorType, Nothing], label: boundary.Label[ResultType])
extends Errant[ErrorType]:
  def record(error: ErrorType): Unit = abort(error)
  def abort(error: ErrorType): Nothing = pf(error)
   