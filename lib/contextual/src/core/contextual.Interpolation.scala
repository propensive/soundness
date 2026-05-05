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

import language.dynamics

import scala.quoted.*

import gigantism.*
import prepositional.*
import proscenium.*

object Interpolation:
  def apply[topic: Type](context: Expr[StringContext]): Macro[Interpolation of topic] =
    import quotes.reflect.*

    val parts: List[String] = context.valueOrAbort.parts.to(List)

    // Walk the StringContext.apply(...) Term to recover each part's source-file
    // (start, end) range. Falls back to (0, 0) for parts whose Term we can't
    // locate — the typeclass macros treat (0, 0) as "no source position".
    def stripWrappers(term: Term): Term = term match
      case Inlined(_, _, body) => stripWrappers(body)
      case Typed(body, _)      => stripWrappers(body)
      case other               => other

    val partOrigins: List[(Int, Int)] = stripWrappers(context.asTerm) match
      case Apply(_, List(arg)) => stripWrappers(arg) match
        case Repeated(lits, _) =>
          lits.map: lit =>
            stripWrappers(lit) match
              case lit2 @ Literal(StringConstant(s)) =>
                (lit2.pos.start, lit2.pos.end)

              case _ => (0, 0)

        case _ =>
          List.fill(parts.length)((0, 0))

      case _ =>
        List.fill(parts.length)((0, 0))

    def transportType
      (parts: List[String], repr: TypeRepr = TypeRepr.of[EmptyTuple.type])
    :   TypeRepr =

      parts match
        case head :: tail =>
          ConstantType(StringConstant(head)).asType.absolve match
            case '[label] => repr.asType.absolve match
              case '[type tuple <: Tuple; tuple] => transportType(tail, TypeRepr.of[label *: tuple])

        case Nil =>
          repr

    def originsType
      (origins: List[(Int, Int)], repr: TypeRepr = TypeRepr.of[EmptyTuple.type])
    :   TypeRepr =

      origins match
        case (start, end) :: tail =>
          ConstantType(IntConstant(start)).asType.absolve match
            case '[type s <: Int; s] => ConstantType(IntConstant(end)).asType.absolve match
              case '[type e <: Int; e] => repr.asType.absolve match
                case '[type tuple <: Tuple; tuple] =>
                  originsType(tail, TypeRepr.of[(s, e) *: tuple])

        case Nil =>
          repr

    transportType(parts).asType.absolve match
      case '[type transport <: Tuple; transport] =>
        originsType(partOrigins).asType.absolve match
          case '[type origins <: Tuple; origins] =>
            '{
                new Interpolation():
                  type Topic = topic
                  type Transport = transport
                  type Origins = origins
              }


trait Interpolation:
  type Topic
  type Transport <: Tuple
  type Origins <: Tuple

  transparent inline def apply(inline insertions: Any*)(using Topic is Interpolable): Topic =
    summon[Topic is Interpolable].interpolate[Transport, Origins](insertions*)

  transparent inline def unapply(using extrapolable: Topic is Extrapolable)(scrutinee: Topic): Any =
    extrapolable.extrapolate[Transport, Origins](scrutinee)
