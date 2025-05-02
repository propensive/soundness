                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package contingency

import java.util.concurrent.atomic as juca

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import fulminate.*
import proscenium.*
import rudiments.*
import vacuous.*

object Contingency:
  def unwrap(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*
    term match
      case Inlined(_, _, ast)                                          => unwrap(ast)
      case Block(List(DefDef(_, _, _, Some(Inlined(_, _, block)))), _) => unwrap(block)
      case ast                                                         => ast

  private def caseDefs[error <: Exception: Type](using Quotes)(handler: quotes.reflect.Term)
  :     List[quotes.reflect.CaseDef] =
    import quotes.reflect.*

    unwrap(handler) match
      case Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _) =>
        cases.flatMap:
          case caseDef@CaseDef(pattern, None, rhs) => List(caseDef)
          case _                                   => Nil

      case other =>
        halt(m"unexpected AST: ${other.toString}")

  private def mapping[error <: Exception: Type](using Quotes)(handler: quotes.reflect.Term)
  :     Map[quotes.reflect.Symbol, quotes.reflect.Symbol] =

    import quotes.reflect.*

    object RepeatedParam:
      def unapply(using quotes: Quotes)(param: quotes.reflect.Symbol)
      :     Option[quotes.reflect.TypeRepr] =
        import quotes.reflect.*
        param.info match
          case AnnotatedType(repr, Apply(Select(New(annotation), _), _))
          if annotation.symbol == defn.RepeatedAnnot                  => Some(repr)
          case _                                                      => None

    def exhaustive(pattern: Tree, patternType: TypeRepr): Boolean = pattern match
      case Wildcard()          => true
      case Typed(_, matchType) => patternType <:< matchType.tpe
      case Bind(_, pattern)    => exhaustive(pattern, patternType)

      case TypedOrTest(Unapply(Select(target, method), _, params), _) =>
        val types = patternType.typeSymbol.caseFields

        val repetition = types.exists:
          case RepeatedParam(_) => true
          case _                => false

        val isExhaustive = params.zip(types).all:
          case (param, pattern@RepeatedParam(patternType)) => param match
            case Bind(_, Typed(Ident("_*"), bound)) => true
            case other                              => false

          case (param, pattern) =>
            exhaustive(param, pattern.info.typeSymbol.typeRef)

        if isExhaustive then true else
          if repetition
          then halt(m"the pattern (which involves repeated parameters) is not exhaustive")
          else halt(m"the pattern is not exhaustive")

      case Unapply(Select(target, method), _, params) =>
        // TODO: Check that extractor is exhaustive
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).all(exhaustive) || halt(m"this type of pattern is not recognized")

      case other =>
        halt(m"bad pattern")

    def unpack(repr: TypeRepr): Set[TypeRepr] = repr.asMatchable match
      case OrType(left, right) => unpack(left) ++ unpack(right)
      case other               => Set(other)

    val requiredHandlers = unpack(TypeRepr.of[error]).map(_.typeSymbol)

    def patternType(pattern: Tree): List[TypeRepr] = pattern match
      case Typed(_, matchType)   => List(matchType.tpe)
      case Bind(_, pattern)      => patternType(pattern)
      case Alternatives(patters) => patters.flatMap(patternType)
      case Wildcard()            => halt(m"wildcard")

      case Unapply(select, _, _) =>
        if exhaustive(pattern, TypeRepr.of[error]) then List(TypeRepr.of[error])
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


  def mitigate[errors <: Exception: Type](handler: Expr[Exception ~> errors])(using Quotes)
  :     Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("result"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)))

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] => '{Mitigation[typeLambda]($handler)}

  def track[accrual <: Exception: Type, focus: Type]
       (accrual: Expr[accrual], handler: Expr[(Optional[focus], accrual) ?=> Exception ~> accrual])
       (using Quotes)
  :     Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("result"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)))

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] =>
        '{Tracking[accrual, typeLambda, focus]($accrual, (focus, accrual) ?=> $handler(using
            focus, accrual))}

  def validate[accrual: Type, focus: Type]
       (accrual: Expr[accrual], handler: Expr[(Optional[focus], accrual) ?=> Exception ~> accrual])
       (using Quotes)
  :     Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("result"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)))

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] =>
        '{Validate[accrual, typeLambda, focus]($accrual, (focus, accrual) ?=> $handler(using
            focus, accrual))}

  def accrue[accrual <: Exception: Type]
       (accrual: Expr[accrual], handler: Expr[accrual ?=> Exception ~> accrual])
       (using Quotes)
  :     Expr[Any] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("result"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)))

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] =>
        '{Accrue[accrual, typeLambda]($accrual, accrual ?=> $handler(using accrual))}

  def recover[result: Type](handler: Expr[Exception ~> result])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.keys.to(List).map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
       (List("result"),
        _ => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)))

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] => '{Recovery[result, typeLambda]($handler)}

  def mitigateWithin[context[_]: Type, result: Type]
       (mitigate: Expr[Mitigation[context]], lambda: Expr[context[result]])
       (using Quotes)
    :     Expr[result] =
      import quotes.reflect.*

      val tactics = unwrap(mitigate.asTerm) match
        case Apply(_, List(Inlined(_, _, matches))) =>
          val partialFunction = matches.asExprOf[Exception ~> Exception]

          mapping(partialFunction.asTerm).values.map: errorType =>
            errorType.typeRef.asType.absolve match
              case '[type errorType <: Exception; errorType] =>
                Expr.summon[Tactic[errorType]] match
                  case Some(errorTactic) =>
                    '{$errorTactic.contramap($partialFunction(_).asInstanceOf[errorType])}.asTerm

                  case None =>
                    halt(m"There is no available handler for ${TypeRepr.of[errorType]}")
        case _ =>
          halt(m"argument to `mitigate` should be a partial function implemented as match cases")

      val method = TypeRepr.of[context[result]].typeSymbol.declaredMethod("apply").head
      lambda.asTerm.select(method).appliedToArgs(tactics.to(List)).asExprOf[result]

  def recoverWithin[context[_]: Type, result: Type]
       (recovery: Expr[Recovery[?, context]], lambda: Expr[context[result]])
       (using Quotes)
  :     Expr[result] =

    type ContextResult = context[result]

    '{
        boundary[result]: label ?=>
          val tactic: Tactic[Break[result]] = EscapeTactic(label)
          ${
              import quotes.reflect.*
              val partialFunction = unwrap(recovery.asTerm) match
                case Apply(_, List(Inlined(_, _, matches))) => matches

                case _ =>
                  halt:
                    m"argument to `recover` should be a partial function implemented as match cases"

              val pfExpr = partialFunction.asExprOf[Exception ~> result]

              val tactics = mapping(partialFunction).map: (_, _) =>
                '{
                    tactic.contramap: error =>
                      Break[result]($pfExpr(error))
                }.asTerm

              val method = TypeRepr.of[ContextResult].typeSymbol.declaredMethod("apply").head
              lambda.asTerm.select(method).appliedToArgs(tactics.to(List)).asExprOf[result]  }
    }

  def accrueWithin[accrual <: Exception: Type, context[_]: Type, result: Type]
       (accrue:      Expr[Accrue[accrual, context]],
        lambda:      Expr[context[result]],
        tactic:      Expr[Tactic[accrual]],
        diagnostics: Expr[Diagnostics])
     (using Quotes)
  :     Expr[result] =

    '{  val ref: juca.AtomicReference[accrual] = juca.AtomicReference(null)
        val result = boundary[Option[result]]: label ?=>
          ${  import quotes.reflect.*

              val cases = unwrap(accrue.asTerm) match
                case Apply(_, List(_, Block(List(DefDef(_, _, _, Some(block))), _))) =>
                  mapping(unwrap(block))

                case other => halt:
                  m"argument to `accrue` should be a partial function implemented as match cases"

              val tactics = cases.map: (_, _) =>
                '{AccrueTactic(label, ref, $accrue.initial)($accrue.lambda)(using $diagnostics)}
                . asTerm

              val contextTypeRepr = TypeRepr.of[context[result]]
              val method = contextTypeRepr.typeSymbol.declaredMethod("apply").head
              val term = lambda.asTerm.select(method).appliedToArgs(tactics.to(List))
              val expr = term.asExprOf[result]

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

  def trackWithin[accrual <: Exception: Type, context[_]: Type, result: Type, focus: Type]
       (track:       Expr[Tracking[accrual, context, focus]],
        lambda:      Expr[Foci[focus] ?=> context[result]],
        tactic:      Expr[Tactic[accrual]],
        diagnostics: Expr[Diagnostics])
       (using Quotes)
  :     Expr[result] =

    '{  val foci: Foci[focus] = TrackFoci()

        val result: Option[result] = boundary[Option[result]]: label ?=>
          ${  import quotes.reflect.*

              val cases = unwrap(track.asTerm) match
                case Apply(_, List(_, Block(List(DefDef(_, _, _, Some(block))), _))) =>
                  mapping(unwrap(block))

                case other => halt:
                  m"argument to `track` should be a partial function implemented as match cases"

              val tactics = cases.map: (_, _) =>
                '{TrackTactic(label, $track.initial, foci)(using $diagnostics)}.asTerm

              val contextTypeRepr = TypeRepr.of[context[result]]
              val method = contextTypeRepr.typeSymbol.declaredMethod("apply").head

              val term =
                '{$lambda(using foci)}.asTerm.select(method).appliedToArgs(tactics.to(List))

              val expr = term.asExprOf[result]

              '{Some($expr)}  }

        result match
          case None =>
            $tactic.abort(foci.fold[accrual]($track.initial)($track.lambda(using _, _)))

          case Some(value) =>
            if foci.success then value
            else $tactic.abort(foci.fold[accrual]($track.initial)($track.lambda(using _, _)))

    }

  def validateWithin[accrual <: Exception: Type, context[_]: Type, focus: Type]
       (validate:    Expr[Validate[accrual, context, focus]],
        lambda:      Expr[Foci[focus] ?=> context[Any]],
        diagnostics: Expr[Diagnostics])
       (using Quotes)
  :     Expr[accrual] =

    '{  val foci: Foci[focus] = TrackFoci()

        boundary[Any]: label ?=>
          ${  import quotes.reflect.*

              val cases = unwrap(validate.asTerm) match
                case Apply(_, List(_, Block(List(DefDef(_, _, _, Some(block))), _))) =>
                  mapping(unwrap(block))

                case other => halt:
                  m"argument to `validate` should be a partial function implemented as match cases"

              val tactics = cases.map: (_, _) =>
                '{TrackTactic(label, $validate.initial, foci)(using $diagnostics)}.asTerm

              val contextTypeRepr = TypeRepr.of[context[Any]]
              val method = contextTypeRepr.typeSymbol.declaredMethod("apply").head

              val term =
                '{$lambda(using foci)}.asTerm.select(method).appliedToArgs(tactics.to(List))

              term.asExpr  }

        foci.fold[accrual]($validate.initial)($validate.lambda(using _, _))

    }
