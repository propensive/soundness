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
package contextual

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import fulminate.*
import gigantism.*
import proscenium.*
import rudiments.*
import vacuous.*

trait Interpolator[input, state, result]:
  given canThrow: CanThrow[InterpolationError] = !!
  private given realm: Realm = realm"cn"

  protected def initial: state
  protected def parse(state: state, next: Text): state
  protected def skip(state: state): state
  protected def substitute(state: state, value: Text): state = parse(state, value)
  protected def insert(state: state, value: input): state
  protected def complete(value: state): result

  case class PositionalError(positionalMessage: Message, start: Int, end: Int)(using Diagnostics)
  extends Error(realm"cn", 2, 0)(m"error $positionalMessage at position $start")

  def expand(context: Expr[StringContext], sequence: Expr[Seq[Any]])
    ( using thisType: Type[this.type] )
    ( using Type[input], Type[state], Type[result] )
  :   Macro[result] =

    expansion(context, sequence)(1)


  def expansion
    ( context: Expr[StringContext], sequence: Expr[Seq[Any]] )
    ( using thisType: Type[this.type] )
    ( using Quotes, Type[input], Type[state], Type[result] )
  :   (state, Expr[result]) =

    import quotes.reflect.*

    val ref = Ref(TypeRepr.of(using thisType).typeSymbol.companionModule)
    val target = ref.asExprOf[Interpolator[input, state, result]]

    def rethrow[success](block: => success, start: Int, end: Int): success =
      try block catch case error: InterpolationError => error match
        case InterpolationError(msg, offset, length) =>
          inline given canThrow: CanThrow[PositionalError] = unsafeExceptions.canThrowAny
          given diagnostics: Diagnostics = Diagnostics.omit

          throw PositionalError
            ( msg,
              start + offset.or(0),
              start + offset.or(0) + length.or(end - start - offset.or(0)) )


    def recur
      ( sequence:  Seq[Expr[Any]],
        parts:     Seq[String],
        positions: Seq[Position],
        state:     state,
        expr:      Expr[state] )
    :   (state, Expr[result]) throws PositionalError =

      sequence match
        case '{$head: head} +: tail =>
          def notFound: Nothing =
            val typeName: String = TypeRepr.of[head].widen.show

            halt
              ( 3,
                m"can't substitute ${typeName} to this interpolated string",
                head.asTerm.pos )

          val (newState, typeclass) = Expr.summon[Insertion[input, head]].fold(notFound):
            _.absolve match
              case '{$typeclass: Substitution[input, head, sub]} =>
                val substitution: String = TypeRepr.of[sub].asMatchable.absolve match
                  case ConstantType(StringConstant(string)) =>
                    string

                ( rethrow
                    ( parse
                        ( rethrow
                            ( substitute(state, substitution.tt),
                              expr.asTerm.pos.start,
                              expr.asTerm.pos.end ),
                              parts.head.tt ),
                          positions.head.start,
                          positions.head.end ),
                          typeclass )

              case typeclass =>
                ( rethrow
                    ( parse
                        ( rethrow(skip(state), expr.asTerm.pos.start, expr.asTerm.pos.end),
                          parts.head.tt ),
                      positions.head.start,
                      positions.head.end ),
                typeclass )

          val next =
            '{$target.parse($target.insert($expr, $typeclass.embed($head)), ${Expr(parts.head)}.tt)}

          recur(tail, parts.tail, positions.tail, newState, next)

        case _ =>
          rethrow(complete(state), Position.ofMacroExpansion.start, Position.ofMacroExpansion.end)
          (state, '{$target.complete($expr)})


    val exprs: Seq[Expr[Any]] = sequence match
      case Varargs(exprs) => exprs
      case _              => Nil

    val parts = context.value.getOrElse:
      halt(4, m"the StringContext extension method parameter does not appear to be inline")

    . parts

    val positions: Seq[Position] = context.absolve match
      case '{(${sc}: StringContext.type).apply(($parts: Seq[String])*)} =>
        parts.absolve match case Varargs(stringExprs) => stringExprs.to(List).map(_.asTerm.pos)

    try recur
      ( exprs,
        parts.tail,
        positions.tail,
        rethrow(parse(initial, parts.head.tt), positions.head.start, positions.head.end),
        '{$target.parse($target.initial, ${Expr(parts.head)}.tt)} )

    catch
      case error: PositionalError =>
        error match
          case error@PositionalError(_, start, end) =>
            halt(2, error.message, Position(Position.ofMacroExpansion.sourceFile, start, end))

      case error: InterpolationError =>
        error match
          case error@InterpolationError(_, _, _) =>
            halt(1, error.message, Position.ofMacroExpansion)
