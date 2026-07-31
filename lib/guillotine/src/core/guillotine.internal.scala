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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package guillotine

import scala.collection.immutable.Seq

import scala.language.experimental.pureFunctions

import scala.collection.immutable.{List, Nil, ::}
import scala.quoted.*

import anticipation.*
import contextual.*
import fulminate.*

object internal:
  def sh(context: Expr[StringContext], insertions: Expr[Seq[Any]])(using Quotes)
  :   Expr[Any] =

    import quotes.reflect.*

    val parts: List[String] =
      context.value.getOrElse:
        halt(m"the StringContext extension method parameter does not appear to be inline")

      . parts.toList

    val insertionExprs: List[Expr[Any]] = insertions.absolve match
      case Varargs(exprs) => exprs.toList

    def rethrow[result](block: => result): result =
      try block catch case error: Sh.ShError => halt(error.detail)

    val checkState = Sh.Runtime.initial
    rethrow(Sh.Runtime.parse(checkState, parts.head.tt))

    var runtimeExpr: Expr[Sh.State] =
      '{Sh.Runtime.parse(Sh.Runtime.initial, ${Expr(parts.head)}.tt)}

    var i = 0

    while i < insertionExprs.length do
      val head = insertionExprs(i)
      val nextPart = parts(i + 1)

      head.absolve match
        case '{$value: tpe} =>
          val typeclassExpr: Expr[Insertion[Sh.Parameters, tpe]] =
            Expr.summon[Insertion[Sh.Parameters, tpe]].getOrElse:
              halt(m"can't substitute ${TypeRepr.of[tpe].show} into a sh-interpolator")

          rethrow(Sh.Runtime.skip(checkState))
          rethrow(Sh.Runtime.parse(checkState, nextPart.tt))

          val current = runtimeExpr

          runtimeExpr =
            ' {
                Sh.Runtime.parse
                  ( Sh.Runtime.insert($current, $typeclassExpr.embed($value)),
                    ${Expr(nextPart)}.tt )
              }

      i += 1

    rethrow(Sh.Runtime.complete(checkState))

    val execType =
      ConstantType(StringConstant(parts.head.split(" ").nn.head.nn))

    val bounds = TypeBounds(execType, execType)

    Refinement(TypeRepr.of[Command], "Exec", bounds).asType.absolve match
      case '[type commandType <: Command; commandType] =>
        '{Sh.Runtime.complete($runtimeExpr).asInstanceOf[commandType]}
