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
  private def action[ErrorType <: Error: Type, ResultType: Type](using Quotes)
      (context: Expr[Tended[ErrorType, ResultType]])
          : Expr[Errant[ErrorType] ?=> ResultType] =
    import quotes.reflect.*

    context.asTerm match
      case Inlined(None, Nil, Apply(_, List(action))) =>
        action.asExprOf[Errant[ErrorType] ?=> ResultType]
      
      case _ =>
        throw Panic(msg"the tended action was not in the expected form")

  private def caseDefs[ErrorType <: Error: Type, ResultType: Type](using Quotes)
      (handler: Expr[PartialFunction[ErrorType, ResultType]])
          : List[quotes.reflect.CaseDef] =
    import quotes.reflect.*
    
    handler.asTerm match
      case Inlined(None, Nil, Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _)) =>
        cases.flatMap:
          case caseDef@CaseDef(pattern, None, rhs) => List(caseDef)
          case _                                   => Nil
      
      case _ =>
        fail(msg"unexpected lambda")


  
  private def unhandledErrorTypes[ErrorType <: Error: Type, ResultType: Type](using Quotes)
      (handler: Expr[PartialFunction[ErrorType, ResultType]])
          : List[quotes.reflect.Symbol] =

    import quotes.reflect.*

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

    def unpack(repr: TypeRepr): Set[TypeRepr] = repr.asMatchable match
      case OrType(left, right) => unpack(left) ++ unpack(right)
      case other               => Set(other)
    
    val requiredHandlers = unpack(TypeRepr.of[ErrorType]).map(_.typeSymbol)
    
    def patternType(pattern: Tree): List[TypeRepr] = pattern match
      case Typed(_, matchType)   => List(matchType.tpe)
      case Bind(_, pattern)      => patternType(pattern)
      case Alternatives(patters) => patters.flatMap(patternType)
      case Wildcard()            => fail(msg"wildcard")
      
      case Unapply(select, _, _) =>
        if exhaustive(pattern, TypeRepr.of[ErrorType]) then List(TypeRepr.of[ErrorType])
        else fail(msg"Unapply ${select.symbol.declaredType.toString}")
      
      case TypedOrTest(Unapply(Select(target, method), _, _), typeTree) =>
        if exhaustive(pattern, typeTree.tpe) then List(typeTree.tpe) else Nil
      
      case other =>
        fail(msg"this pattern could not be recognized as a distinct `Error` type")

    val handledTypes: List[Symbol] =
      caseDefs(handler).flatMap:
        case CaseDef(pattern, _, _) => patternType(pattern)
      .map(_.typeSymbol)
      
    (requiredHandlers -- handledTypes).to(List)


  def mitigate[ErrorType <: Error: Type, ResultType: Type, ErrorType2 <: Error: Type]
      (context: Expr[Tended[ErrorType, ResultType]],
       handler: Expr[PartialFunction[ErrorType, ErrorType2]])
      (using Quotes)
           : Expr[Any] =
    
    '{???}

    
  def remedy[ErrorType <: Error: Type, ResultType: Type]
      (context: Expr[Tended[ErrorType, ResultType]],
       handler: Expr[PartialFunction[ErrorType, ResultType]])
      (using Quotes)
           : Expr[Any] =
    
    import quotes.reflect.*

    def wrap[InsideType[_]: Type](errorTypes: List[TypeRepr])
        (makeExpr: Expr[PartialFunction[ErrorType, Nothing] => ResultType] =>
                     Expr[InsideType[ResultType]])
            : Term =

      errorTypes match
        case Nil =>
          makeExpr
           ('{(partialFunction0: PartialFunction[ErrorType, Nothing]) =>
                boundary: label ?=>
                  val partialFunction = $handler.andThen(boundary.break(_)).orElse(partialFunction0)
                  val errant = new RemedyErrant[ResultType, ErrorType](partialFunction)
                  ${action(context)}(using errant)
            }).asTerm
        
        case errorType :: more => errorType.asType match
          case '[type errorType <: ErrorType; errorType] =>
            wrap[[ParamType] =>> Errant[errorType] ?=> InsideType[ParamType]](more):
              (makeResult: Expr[PartialFunction[ErrorType, Nothing] => ResultType]) =>
                '{ (errant: Errant[errorType]) ?=>
                  val makeResult2: PartialFunction[ErrorType, Nothing] => ResultType =
                    partialFunction =>
                      $makeResult:
                        partialFunction.orElse { case error: `errorType` => errant.abort(error) }
                  ${makeExpr('makeResult2)}
                }
            
          case _ =>
            fail(msg"Match error ${errorType.show} type was not a subtype of ErrorType")
    
    wrap[[ParamType] =>> ParamType](unhandledErrorTypes(handler).map(_.typeRef)): function =>
      '{$function(PartialFunction.empty[ErrorType, Nothing])}
    .asExpr

class RemedyErrant[ResultType, ErrorType <: Error]
    (partialFunction: PartialFunction[ErrorType, Nothing])
extends Errant[ErrorType]:
  def record(error: ErrorType): Unit = abort(error)
  def abort(error: ErrorType): Nothing = partialFunction(error)
   