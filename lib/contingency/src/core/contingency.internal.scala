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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import scala.collection.immutable.`::`
import scala.collection.immutable.List
import scala.collection.immutable.Nil
import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import gigantism.*
import rudiments.*
import vacuous.*

object internal:
  def unwrap(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*

    term match
      case Inlined(_, _, ast)                                          => unwrap(ast)
      case Block(List(DefDef(_, _, _, Some(Inlined(_, _, block)))), _) => unwrap(block)
      case ast                                                         => ast


  private def caseDefs[error <: Exception: Type](using Quotes)(handler: quotes.reflect.Term)
  :   List[quotes.reflect.CaseDef] =

    import quotes.reflect.*

    unwrap(handler) match
      case Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _) =>
        cases.flatMap:
          case caseDef@CaseDef(pattern, None, rhs) => List(caseDef)
          case _                                   => Nil

      case other =>
        halt(44, m"unexpected AST: ${other.toString}")


  private def mapping[error <: Exception: Type](using Quotes)(handler: quotes.reflect.Term)
  :   Map[quotes.reflect.Symbol, quotes.reflect.Symbol] =

    import quotes.reflect.*

    object RepeatedParam:
      def unapply(using quotes: Quotes)(param: quotes.reflect.Symbol)
      :   Option[quotes.reflect.TypeRepr] =

        import quotes.reflect.*

        param.info match
          case AnnotatedType(repr, Apply(Select(New(annotation), _), _))
          if annotation.symbol == defn.RepeatedAnnot =>
            Some(repr)

          case _ => None

    def exhaustive(pattern: Tree, patternType: TypeRepr): Boolean = pattern match
      case Wildcard()          => true
      case Typed(_, matchType) => patternType <:< matchType.tpe
      case Bind(_, pattern)    => exhaustive(pattern, patternType)

      case TypedOrTest(Unapply(Select(target, method), _, params), _) =>
        val types = patternType.typeSymbol.caseFields

        val repetition = types.exists:
          case RepeatedParam(_) => true
          case _                => false

        val isExhaustive = params.zip(types).forall:
          case (param, pattern@RepeatedParam(patternType)) => param match
            case Bind(_, Typed(Ident("_*"), bound)) => true
            case other                              => false

          case (param, pattern) =>
            exhaustive(param, pattern.info.typeSymbol.typeRef)

        if isExhaustive then true else
          if repetition
          then halt(304, m"the pattern (which involves repeated parameters) is not exhaustive")
          else halt(470, m"the pattern is not exhaustive")

      case Unapply(Select(target, method), _, params) =>
        // TODO: Check that extractor is exhaustive
        val types = patternType.typeSymbol.caseFields.map(_.info.typeSymbol.typeRef)
        params.zip(types).forall(exhaustive) || halt(423, m"this type of pattern is not recognized")

      case other =>
        halt(454, m"bad pattern")

    def unpack(repr: TypeRepr): Set[TypeRepr] = repr.asMatchable match
      case OrType(left, right) => unpack(left) ++ unpack(right)
      case other               => Set(other)

    val requiredHandlers = unpack(TypeRepr.of[error]).map(_.typeSymbol)

    def patternType(pattern: Tree): List[TypeRepr] = pattern match
      case Typed(_, matchType)   => List(matchType.tpe)
      case Bind(_, pattern)      => patternType(pattern)
      case Alternatives(patters) => patters.flatMap(patternType)
      case Wildcard()            => halt(686, m"wildcard")

      case Unapply(select, _, _) =>
        if exhaustive(pattern, TypeRepr.of[error]) then List(TypeRepr.of[error])
        else halt(716, m"Unapply ${select.symbol.declaredType.toString}")

      case TypedOrTest(Unapply(Select(target, method), _, _), typeTree) =>
        if exhaustive(pattern, typeTree.tpe) then List(typeTree.tpe) else Nil

      case other =>
        halt(514, m"this pattern could not be recognized as a distinct `Error` type")

    caseDefs(handler).flatMap:
      case CaseDef(pattern, _, rhs) => rhs.asExpr.absolve match
        case '{$rhs: rhsType} =>
          patternType(pattern).map(_.typeSymbol -> TypeRepr.of[rhsType].typeSymbol)

    .to(Map)


  def unpack(using Quotes)(repr: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    repr.asMatchable match
      case quotes.reflect.OrType(left, right) => unpack(left) ++ unpack(right)
      case other                              => List(other)


  def track[accrual <: Exception: Type, focus: Type]
    ( accrual: Expr[accrual],
      handler: Expr[(Optional[focus] aka "prior", accrual aka "accrual") ?=> Exception ~> accrual] )
  :   Macro[Tracking[accrual, ?, focus]] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.scala.keys.toList.map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
        ( List("result"),
          void => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
          typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)) )

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] =>
        ' {
            Tracking[accrual, typeLambda, focus]
              ( $accrual, (priorFocus, acc) ?=> $handler(using priorFocus, acc) )
          }


  def validate[accrual: Type, focus: Type]
    ( accrual: Expr[accrual],
      handler: Expr[(Optional[focus] aka "prior", accrual aka "accrual") ?=> Exception ~> accrual] )
  :   Macro[Any] =

    import quotes.reflect.*

    val errors = mapping(handler.asTerm)
    val tactics = errors.scala.keys.toList.map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
        ( List("result"),
          void => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
          typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)) )

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] =>
        ' {
            Validate[accrual, typeLambda, focus]
              ( $accrual, (priorFocus, acc) ?=> $handler(using priorFocus, acc) )
          }


  def trackWithin[accrual <: Exception: Type, context[_]: Type, result: Type, focus: Type]
    ( track:       Expr[Tracking[accrual, context, focus]],
      lambda:      Expr[Foci[focus] ?=> context[result]],
      tactic:      Expr[Tactic[accrual]],
      diagnostics: Expr[Diagnostics] )
  :   Macro[result] =

    ' {
        val foci: Foci[focus] = TrackFoci()

        val result: Option[result] = boundary[Option[result]]: label ?=>
          $ {
              import quotes.reflect.*

              val cases = unwrap(track.asTerm) match
                case Apply(_, List(_, Block(List(DefDef(_, _, _, Some(block))), _))) =>
                  mapping(unwrap(block))

                case other =>
                  halt:
                    m"argument to `track` should be a partial function implemented as match cases"

              val tactics = cases.scala.map: (_, _) =>
                '{TrackTactic(label, $track.initial, foci)(using $diagnostics)}.asTerm

              val contextTypeRepr = TypeRepr.of[context[result]]
              val method = contextTypeRepr.typeSymbol.declaredMethod("apply").head

              val term =
                '{$lambda(using foci)}.asTerm.select(method).appliedToArgs(tactics.toList)

              val expr = term.asExprOf[result]

              '{Some($expr)}
            }

        result match
          case None =>
            $tactic.abort:
              foci.fold[accrual]($track.initial): (priorFocus, acc) =>
                $track.lambda(using priorFocus.aka["prior"], acc.aka["accrual"])

          case Some(value) =>
            if foci.success then value
            else
              $tactic.abort:
                foci.fold[accrual]($track.initial): (priorFocus, acc) =>
                  $track.lambda(using priorFocus.aka["prior"], acc.aka["accrual"])
      }


  def validateWithin[accrual <: Exception: Type, context[_]: Type, focus: Type]
    ( validate:    Expr[Validate[accrual, context, focus]],
      lambda:      Expr[Foci[focus] ?=> context[Any]],
      diagnostics: Expr[Diagnostics] )
  :   Macro[accrual] =

    ' {
        val foci: Foci[focus] = TrackFoci()

        boundary[Any]: label ?=>
          $ {
              import quotes.reflect.*

              val cases = unwrap(validate.asTerm) match
                case Apply(_, List(_, Block(List(DefDef(_, _, _, Some(block))), _))) =>
                  mapping(unwrap(block))

                case other =>
                  halt:
                    m"""
                      argument to `validate` should be a partial function implemented as match cases
                    """

              val tactics = cases.scala.map: (_, _) =>
                '{TrackTactic(label, $validate.initial, foci)(using $diagnostics)}.asTerm

              val contextTypeRepr = TypeRepr.of[context[Any]]
              val method = contextTypeRepr.typeSymbol.declaredMethod("apply").head

              val term =
                '{$lambda(using foci)}.asTerm.select(method).appliedToArgs(tactics.toList)

              term.asExpr
            }

        foci.fold[accrual]($validate.initial): (priorFocus, acc) =>
          $validate.lambda(using priorFocus.aka["prior"], acc.aka["accrual"])

      }


  /** Macro for `whereas { case … }` — analyses the handler and emits a
   *  `Whereas[lambda]` wrapping the partial function. */
  def whereas(handler: Expr[PartialFunction[Exception, Any]])
    ( using Quotes )
  :   Expr[Whereas[?]] =

    import quotes.reflect.*

    val errors = mapping[Exception](handler.asTerm)
    val tactics = errors.scala.keys.toList.map(_.typeRef).map(TypeRepr.of[Tactic].appliedTo(_))
    val functionType = defn.FunctionClass(errors.size, true).typeRef

    val typeLambda =
      TypeLambda
        ( List("result"),
          void => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
          typeLambda => functionType.appliedTo(tactics :+ typeLambda.param(0)) )

    typeLambda.asType.absolve match
      case '[type typeLambda[_]; typeLambda] =>
        '{Whereas[typeLambda]($handler)}


  /** Macro for `whereas { … }.mitigate { body }`. Supplies a contramap'd
   *  `Tactic[E_i]` for each error type the handler covers, explicitly
   *  binding the body's per-error context parameters rather than relying on
   *  implicit search. */
  def mitigateBody[context[_]: Type, result: Type]
    ( w: Expr[Whereas[context]], body: Expr[context[result]] )
    ( using Quotes )
  :   Expr[result] =

    import quotes.reflect.*

    val tactics = unwrap(w.asTerm) match
      case Apply(_, List(Inlined(_, _, matches))) =>
        val partialFunction = matches.asExprOf[PartialFunction[Exception, Any]]

        mapping[Exception](matches).values.scala.map: errorType =>
          errorType.typeRef.asType.absolve match
            case '[type errorType <: Exception; errorType] =>
              Expr.summon[Tactic[errorType]] match
                case Some(errorTactic) =>
                  '{$errorTactic.contramap($partialFunction(_).asInstanceOf[errorType])}.asTerm

                case None =>
                  halt
                    ( 602,
                      m"There is no available handler for ${TypeRepr.of[errorType].show}" )

      case _ =>
        halt(648, m"argument to `whereas` should be a partial function implemented as match cases")

    val method = TypeRepr.of[context[result]].typeSymbol.declaredMethod("apply").head
    body.asTerm.select(method).appliedToArgs(tactics.toList).asExprOf[result]


  /** Macro for `whereas { … }.recover { body }`. Wraps `body` in a
   *  `boundary` of the result type. Each per-error tactic supplied to body
   *  is a `Whereas.EscapeTactic` that, on record, contramaps the raised error
   *  through the handler to obtain the recovered result, then breaks the
   *  boundary with it. */
  def recoverBody[context[_]: Type, result: Type]
    ( w: Expr[Whereas[context]], body: Expr[context[result]] )
    ( using Quotes )
  :   Expr[result] =

    type ContextResult = context[result]

    ' {
        scala.util.boundary[result]: label ?=>
          val tactic: Tactic[Whereas.Escape[result]] = Whereas.EscapeTactic(label)

          $ {
              import quotes.reflect.*

              val partialFunction = unwrap(w.asTerm) match
                case Apply(_, List(Inlined(_, _, matches))) => matches

                case _ =>
                  halt
                    ( 29,
                      m"""
                        argument to `whereas` should be a partial function implemented as match
                        cases
                      """ )

              val pfExpr = partialFunction.asExprOf[PartialFunction[Exception, Any]]

              val tactics = mapping[Exception](partialFunction).scala.map: (_, _) =>
                ' {
                    tactic.contramap: error =>
                      Whereas.Escape[result]($pfExpr(error).asInstanceOf[result])
                  }

                . asTerm

              val method = TypeRepr.of[ContextResult].typeSymbol.declaredMethod("apply").head
              body.asTerm.select(method).appliedToArgs(tactics.toList).asExprOf[result]
            }
      }


  /** Macro for `whereas { … }.accrue(initial)(combine) { body }`. Each
   *  per-error tactic supplied to body is the same Whereas.AccrueTactic; it
   *  records raised errors via `combine`. When the body completes (or throws
   *  after accumulation), accumulated errors are forwarded to the outer
   *  tactic via `record`. */
  def accrueBody[accrual <: Exception: Type, context[_]: Type, result: Type]
    ( w:           Expr[Whereas[context]],
      initial:     Expr[accrual],
      combine:     Expr[(accrual, Exception) => accrual],
      body:        Expr[context[result]],
      outer:       Expr[Tactic[accrual]],
      diagnostics: Expr[Diagnostics] )
    ( using Quotes )
  :   Expr[result] =

    import quotes.reflect.*

    val cases = unwrap(w.asTerm) match
      case Apply(_, List(Inlined(_, _, matches))) =>
        mapping[Exception](matches)

      case _ =>
        halt(114, m"argument to `whereas` should be a partial function implemented as match cases")

    ' {
        val acc: Whereas.AccrueTactic[Exception, accrual] =
          Whereas.AccrueTactic[Exception, accrual]($initial, $combine)(using $diagnostics)

        val outcome: Either[accrual, result] =
          try Right($ {
            val tactics = cases.scala.map: (_, _) =>
              '{acc}.asTerm

            val contextTypeRepr = TypeRepr.of[context[result]]
            val method = contextTypeRepr.typeSymbol.declaredMethod("apply").head
            body.asTerm.select(method).appliedToArgs(tactics.toList).asExprOf[result]
          })
          catch case _: Exception => Left(acc.accumulated)

        outcome match
          case Right(value) if !acc.changed => value

          case _ =>
            $outer.record((_: Diagnostics) ?=> acc.accumulated)
            import scala.unsafeExceptions.canThrowAny
            throw acc.accumulated
      }
