/*
    Contingency, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import java.util.concurrent.atomic as juca

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import fulminate.*
import rudiments.*
import vacuous.*

object Contingency:

  def unwrap(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case Inlined(_, _, ast)                                          => unwrap(ast)
      case Block(List(DefDef(_, _, _, Some(Inlined(_, _, block)))), _) => unwrap(block)
      case ast                                                         => ast

  private def caseDefs[ErrorType <: Exception: Type](using Quotes)(handler: quotes.reflect.Term)
  :     List[quotes.reflect.CaseDef] =
    import quotes.reflect.*

    unwrap(handler) match
      case Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _) =>
        cases.flatMap:
          case caseDef@CaseDef(pattern, None, rhs) => List(caseDef)
          case _                                   => Nil

      case other =>
        halt(m"unexpected AST: ${other.toString}")

  private def mapping[ErrorType <: Exception: Type](using Quotes)
     (handler: quotes.reflect.Term)
  :     Map[quotes.reflect.Symbol, quotes.reflect.Symbol] =

    import quotes.reflect.*

    def exhaustive(pattern: Tree, patternType: TypeRepr): Boolean = pattern match
      case Wildcard()          => true
      case Typed(_, matchType) => patternType <:< matchType.tpe
      case Bind(_, pattern)    => exhaustive(pattern, patternType)

      case TypedOrTest(Unapply(Select(target, method), _, params), _) =>
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || halt(m"bad pattern")

      case Unapply(Select(target, method), _, params) =>
        // TODO: Check that extractor is exhaustive
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || halt(m"bad pattern")

      case other =>
        halt(m"bad pattern")

    def unpack(repr: TypeRepr): Set[TypeRepr] = repr.asMatchable match
      case OrType(left, right) => unpack(left) ++ unpack(right)
      case other               => Set(other)

    val requiredHandlers = unpack(TypeRepr.of[ErrorType]).map(_.typeSymbol)

    def patternType(pattern: Tree): List[TypeRepr] = pattern match
      case Typed(_, matchType)   => List(matchType.tpe)
      case Bind(_, pattern)      => patternType(pattern)
      case Alternatives(patters) => patters.flatMap(patternType)
      case Wildcard()            => halt(m"wildcard")

      case Unapply(select, _, _) =>
        if exhaustive(pattern, TypeRepr.of[ErrorType]) then List(TypeRepr.of[ErrorType])
        else halt(m"Unapply ${select.symbol.declaredType.toString}")

      case TypedOrTest(Unapply(Select(target, method), _, _), typeTree) =>
        if exhaustive(pattern, typeTree.tpe) then List(typeTree.tpe) else Nil

      case other =>
        halt(m"this pattern could not be recognized as a distinct `Error` type")

    caseDefs(handler).flatMap:
      case CaseDef(pattern, _, rhs) => rhs.asExpr.absolve match
        case '{$rhs: rhsType} =>
          patternType(pattern).map(_.typeSymbol -> TypeRepr.of[rhsType].typeSymbol)

    . to(Map)

  def unpack(using Quotes)(repr: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    repr.asMatchable match
      case quotes.reflect.OrType(left, right) => unpack(left) ++ unpack(right)
      case other                              => List(other)


  def tend[ErrorTypes <: Exception: Type](handler: Expr[Exception ~> ErrorTypes])
     (using Quotes)
  :     Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("ResultType"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)))

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] => '{Tend[typeLambda]($handler)}

  def track[AccrualType <: Exception: Type, FocusType: Type]
     (accrual: Expr[AccrualType],
      handler: Expr[(Optional[FocusType], AccrualType) ?=> Exception ~> AccrualType])
     (using Quotes)
  :     Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("ResultType"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)))

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] =>
        '{Tracking[AccrualType, typeLambda, FocusType]($accrual, (focus, accrual) ?=> $handler(using
            focus, accrual))}

  def accrue[AccrualType <: Exception: Type]
     (accrual: Expr[AccrualType],
      handler: Expr[AccrualType ?=> Exception ~> AccrualType])
     (using Quotes)
  :     Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("ResultType"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)))

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] =>
        '{Accrue[AccrualType, typeLambda]($accrual, accrual ?=> $handler(using accrual))}

  def mend[ResultType: Type](handler: Expr[Exception ~> ResultType])(using Quotes)
  :     Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("ResultType"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)))

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] => '{Mend[ResultType, typeLambda]($handler)}

  def tendWithin[ContextType[_]: Type, ResultType: Type]
        (tend: Expr[Tend[ContextType]], lambda: Expr[ContextType[ResultType]])
        (using Quotes)
    :     Expr[ResultType] =
      import quotes.reflect.*

      val tactics = unwrap(tend.asTerm) match
        case Apply(_, List(Inlined(_, _, matches))) =>
          val partialFunction = matches.asExprOf[Exception ~> Exception]

          mapping(partialFunction.asTerm).values.map: errorType =>
            errorType.typeRef.asType.absolve match
              case '[type errorType <: Exception; errorType] =>
                Expr.summon[Tactic[errorType]] match
                  case Some(errorTactic) =>
                    '{$errorTactic.contramap($partialFunction(_).asInstanceOf[errorType])}.asTerm

                  case None =>
                    halt(m"There is no available handler for ${TypeRepr.of[errorType].show}")
        case _ =>
          halt(m"argument to `tend` should be a partial function implemented as match cases")

      val method = TypeRepr.of[ContextType[ResultType]].typeSymbol.declaredMethod("apply").head
      lambda.asTerm.select(method).appliedToArgs(tactics.to(List)).asExprOf[ResultType]

  def mendWithin[ContextType[_]: Type, ResultType: Type]
     (mend: Expr[Mend[?, ContextType]], lambda: Expr[ContextType[ResultType]])
     (using Quotes)
  :     Expr[ResultType] =

    type ContextResult = ContextType[ResultType]

    '{
        boundary[ResultType]: label ?=>
          val tactic: Tactic[Break[ResultType]] = EscapeTactic(label)
          ${
              import quotes.reflect.*
              val partialFunction = unwrap(mend.asTerm) match
                case Apply(_, List(Inlined(_, _, matches))) => matches

                case _ =>
                  halt:
                    m"argument to `mend` should be a partial function implemented as match cases"

              val pfExpr = partialFunction.asExprOf[Exception ~> ResultType]

              val tactics = mapping(partialFunction).map: (_, _) =>
                '{
                    tactic.contramap: error =>
                      Break[ResultType]($pfExpr(error))
                }.asTerm

              val method = TypeRepr.of[ContextResult].typeSymbol.declaredMethod("apply").head
              lambda.asTerm.select(method).appliedToArgs(tactics.to(List)).asExprOf[ResultType]  }
    }

  def accrueWithin[AccrualType <: Exception: Type, ContextType[_]: Type, ResultType: Type]
     (accrue:     Expr[Accrue[AccrualType, ContextType]],
      lambda:     Expr[ContextType[ResultType]],
      tactic:     Expr[Tactic[AccrualType]],
      diagnostics: Expr[Diagnostics])
     (using Quotes)
  :     Expr[ResultType] =

    '{  val ref: juca.AtomicReference[AccrualType] = juca.AtomicReference(null)
        val result = boundary[Option[ResultType]]: label ?=>
          ${  import quotes.reflect.*

              val cases = unwrap(accrue.asTerm) match
                case Apply(_, List(_, Block(List(DefDef(_, _, _, Some(block))), _))) =>
                  mapping(unwrap(block))

                case other => halt:
                  m"argument to `accrue` should be a partial function implemented as match cases"

              val tactics = cases.map: (_, _) =>
                '{AccrueTactic(label, ref, $accrue.initial)($accrue.lambda)(using $diagnostics)}
                . asTerm

              val contextTypeRepr = TypeRepr.of[ContextType[ResultType]]
              val method = contextTypeRepr.typeSymbol.declaredMethod("apply").head
              val term = lambda.asTerm.select(method).appliedToArgs(tactics.to(List))
              val expr = term.asExprOf[ResultType]

              '{Some($expr)}  }

        result match
          case None        => $tactic.abort:
            ref.get() match
              case null  => $accrue.initial
              case error => error
          case Some(value) => ref.get() match
            case null        => value
            case error       => $tactic.abort(error)
    }

  def trackWithin
     [AccrualType <: Exception: Type, ContextType[_]: Type, ResultType: Type, FocusType: Type]
     (track:      Expr[Tracking[AccrualType, ContextType, FocusType]],
      lambda:     Expr[Foci[FocusType] ?=> ContextType[ResultType]],
      tactic:     Expr[Tactic[AccrualType]],
      diagnostics: Expr[Diagnostics])
     (using Quotes)
  :     Expr[ResultType] =

    '{  val foci: Foci[FocusType] = TrackFoci()

        val result: Option[ResultType] = boundary[Option[ResultType]]: label ?=>
          ${  import quotes.reflect.*

              val cases = unwrap(track.asTerm) match
                case Apply(_, List(_, Block(List(DefDef(_, _, _, Some(block))), _))) =>
                  mapping(unwrap(block))

                case other => halt:
                  m"argument to `track` should be a partial function implemented as match cases"

              val tactics = cases.map: (_, _) =>
                '{TrackTactic(label, $track.initial, foci)(using $diagnostics)}.asTerm

              val contextTypeRepr = TypeRepr.of[ContextType[ResultType]]
              val method = contextTypeRepr.typeSymbol.declaredMethod("apply").head

              val term =
                '{$lambda(using foci)}.asTerm.select(method).appliedToArgs(tactics.to(List))

              val expr = term.asExprOf[ResultType]

              '{Some($expr)}  }

        result match
          case None =>
            $tactic.abort(foci.fold[AccrualType]($track.initial)($track.lambda(using _, _)))

          case Some(value) =>
            if foci.success then value
            else $tactic.abort(foci.fold[AccrualType]($track.initial)($track.lambda(using _, _)))

    }
