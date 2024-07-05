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

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import fulminate.*
import rudiments.*

object Contingency:
  private def caseDefs[ErrorType <: Exception: Type, ResultType: Type](using Quotes)
      (handler: Expr[PartialFunction[ErrorType, ResultType]])
          : List[quotes.reflect.CaseDef] =
    import quotes.reflect.*

    handler.asTerm match
      case Inlined(None, Nil, Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _)) =>
        cases.flatMap:
          case caseDef@CaseDef(pattern, None, rhs) => List(caseDef)
          case _                                   => Nil

      case other =>
        abandon(m"unexpected AST: ${other.toString}")

  private def mapping[ErrorType <: Exception: Type, ResultType: Type](using Quotes)
      (handler: Expr[PartialFunction[ErrorType, ResultType]])
          : Map[quotes.reflect.Symbol, quotes.reflect.Symbol] =

    import quotes.reflect.*

    def exhaustive(pattern: Tree, patternType: TypeRepr): Boolean = pattern match
      case Wildcard()          => true
      case Typed(_, matchType) => patternType <:< matchType.tpe
      case Bind(_, pattern)    => exhaustive(pattern, patternType)

      case TypedOrTest(Unapply(Select(target, method), _, params), _) =>
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || abandon(m"bad pattern")

      case Unapply(Select(target, method), _, params) =>
        // TODO: Check that extractor is exhaustive
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || abandon(m"bad pattern")

      case other =>
        abandon(m"bad pattern")

    def unpack(repr: TypeRepr): Set[TypeRepr] = repr.asMatchable match
      case OrType(left, right) => unpack(left) ++ unpack(right)
      case other               => Set(other)

    val requiredHandlers = unpack(TypeRepr.of[ErrorType]).map(_.typeSymbol)

    def patternType(pattern: Tree): List[TypeRepr] = pattern match
      case Typed(_, matchType)   => List(matchType.tpe)
      case Bind(_, pattern)      => patternType(pattern)
      case Alternatives(patters) => patters.flatMap(patternType)
      case Wildcard()            => abandon(m"wildcard")

      case Unapply(select, _, _) =>
        if exhaustive(pattern, TypeRepr.of[ErrorType]) then List(TypeRepr.of[ErrorType])
        else abandon(m"Unapply ${select.symbol.declaredType.toString}")

      case TypedOrTest(Unapply(Select(target, method), _, _), typeTree) =>
        if exhaustive(pattern, typeTree.tpe) then List(typeTree.tpe) else Nil

      case other =>
        abandon(m"this pattern could not be recognized as a distinct `Error` type")

    caseDefs(handler).flatMap:
      case CaseDef(pattern, _, rhs) => (rhs.asExpr: @unchecked) match
        case '{$rhs: rhsType} =>
          patternType(pattern).map(_.typeSymbol -> TypeRepr.of[rhsType].typeSymbol)

    .to(Map)

  def unpack(using Quotes)(repr: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    repr.asMatchable match
      case quotes.reflect.OrType(left, right) => unpack(left) ++ unpack(right)
      case other                              => List(other)

  private def unhandledErrorTypes[ErrorType <: Exception: Type, ResultType: Type](using Quotes)
      (handler: Expr[PartialFunction[ErrorType, ResultType]])
          : List[quotes.reflect.Symbol] =

    import quotes.reflect.*

    def exhaustive(pattern: Tree, patternType: TypeRepr): Boolean = pattern match
      case Wildcard()          => true
      case Typed(_, matchType) => patternType <:< matchType.tpe
      case Bind(_, pattern)    => exhaustive(pattern, patternType)

      case TypedOrTest(Unapply(Select(target, method), _, params), _) =>
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || abandon(m"bad pattern")

      case Unapply(Select(target, method), _, params) =>
        // TODO: Check that extractor is exhaustive
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || abandon(m"bad pattern")

      case other =>
        abandon(m"bad pattern")

    val requiredHandlers = unpack(TypeRepr.of[ErrorType]).map(_.typeSymbol)

    def patternType(pattern: Tree): List[TypeRepr] = pattern match
      case Typed(_, matchType)   => List(matchType.tpe)
      case Bind(_, pattern)      => patternType(pattern)
      case Alternatives(patters) => patters.flatMap(patternType)
      case Wildcard()            => abandon(m"wildcard")

      case Unapply(select, _, _) =>
        if exhaustive(pattern, TypeRepr.of[ErrorType]) then List(TypeRepr.of[ErrorType])
        else abandon(m"Unapply ${select.symbol.declaredType.toString}")

      case TypedOrTest(Unapply(Select(target, method), _, _), typeTree) =>
        if exhaustive(pattern, typeTree.tpe) then List(typeTree.tpe) else Nil

      case other =>
        abandon(m"this pattern could not be recognized as a distinct `Error` type")

    val handledTypes: List[Symbol] =
      caseDefs(handler).flatMap:
        case CaseDef(pattern, _, _) => patternType(pattern)
      .map(_.typeSymbol)

    (requiredHandlers.to(Set) -- handledTypes).to(List)

  def quell[ErrorTypes <: Exception: Type](handler: Expr[PartialFunction[Exception, ErrorTypes]])
      (using Quotes)
          : Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler)
    val errants = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Errant].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("ResultType"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(errants :+ typeLambda.param(0)))

    (typeLambda.asType: @unchecked) match
      case '[type typeLambda[_]; typeLambda] => '{Quell[typeLambda]($handler)}

  def quellWithin[ContextType[_]: Type, ResultType: Type]
      (quell: Expr[Quell[ContextType]], lambda: Expr[ContextType[ResultType]])
      (using Quotes)
          : Expr[ResultType] =
    import quotes.reflect.*

    val errants = quell.asTerm match
      case Inlined(_, _, Inlined(_, _, Inlined(_, _, Apply(_, List(Inlined(_, _, matches)))))) =>
        val partialFunction = matches.asExprOf[PartialFunction[Exception, Exception]]

        mapping(partialFunction).values.map: errorType =>
          (errorType.typeRef.asType: @unchecked) match
            case '[type errorType <: Exception; errorType] =>
              Expr.summon[Errant[errorType]] match
                case Some(errorErrant) =>
                  '{  $errorErrant.contramap($partialFunction(_).asInstanceOf[errorType])  }.asTerm

                case None =>
                  abandon(m"There is no available handler for ${TypeRepr.of[errorType].show}")
      case _ =>
        abandon(m"argument to `quell` should be a partial function implemented as match cases")

    val method = TypeRepr.of[ContextType[ResultType]].typeSymbol.declaredMethod("apply").head
    lambda.asTerm.select(method).appliedToArgs(errants.to(List)).asExprOf[ResultType]

  def quash[ResultType: Type](handler: Expr[PartialFunction[Exception, ResultType]])(using Quotes)
          : Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler)
    val errants = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Errant].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("ResultType"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(errants :+ typeLambda.param(0)))

    (typeLambda.asType: @unchecked) match
      case '[type typeLambda[_]; typeLambda] => '{Quash[ResultType, typeLambda]($handler)}

  def quashWithin[ContextType[_]: Type, ResultType: Type]
      (quash: Expr[Quash[?, ContextType]], lambda: Expr[ContextType[ResultType]])
      (using Quotes)
          : Expr[ResultType] =
    import quotes.reflect.*

    type ContextResult = ContextType[ResultType]

    val partialFunction = quash.asTerm match
      case Inlined(_, _, Inlined(_, _, Inlined(_, _, Apply(_, List(Inlined(_, _, matches)))))) =>
        matches.asExprOf[PartialFunction[Exception, ResultType]]

      case _ =>
        abandon(m"argument to `quash` should be a partial function implemented as match cases")

    '{
        boundary[ResultType]: label ?=>
          val tactic: Errant[Break[ResultType]] = EscapeTactic(label)
          ${
              val errants = mapping(partialFunction).map: (_, _) =>
                '{
                    tactic.contramap: error =>
                      Break[ResultType]($partialFunction(error))
                }.asTerm

              val method = TypeRepr.of[ContextResult].typeSymbol.declaredMethod("apply").head
              lambda.asTerm.select(method).appliedToArgs(errants.to(List)).asExprOf[ResultType]  }
    }
