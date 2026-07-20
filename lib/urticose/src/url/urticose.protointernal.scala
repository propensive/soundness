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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package urticose

import scala.collection.immutable.Seq

import scala.collection.immutable.{List, Nil, ::}
import scala.quoted.*

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import spectacular.*
import symbolism.*

object protointernal:
  case class UrlInterpolatorError(detail: Message)
  extends Exception(s"urticose: ${detail.text.s}")

  object Runtime:
    import unsafeExceptions.canThrowAny

    def initial: Text = t""

    def parse(state: Text, next: Text): Text = state+next

    def skip(state: Text): Text = state+t"1"

    def substitute(state: Text, sub: Text): Text = state+sub

    def insert(state: Text, value: UrlFragment): Text = value match
      case UrlFragment.Integral(port) =>
        if !state.ends(t":")
        then throw UrlInterpolatorError(m"a port number must be specified after a colon")

        try throwErrors((state+port.show).as[HttpUrl]) catch
          case error: UrlError => throw UrlInterpolatorError(Message(error.message.text))

        state+port.show

      case UrlFragment.Textual(text) =>
        try throwErrors((state+text.urlEncode).as[HttpUrl]) catch
          case error: UrlError => throw UrlInterpolatorError(Message(error.message.text))

        state+text.urlEncode

      case UrlFragment.RawTextual(text) =>
        try throwErrors((state+text.urlEncode).as[HttpUrl]) catch
          case error: UrlError => throw UrlInterpolatorError(Message(error.message.text))

        state+text

    def complete(value: Text): Url[Label] =
      try throwErrors(value.as[Url[Label]]) catch
        case error: UrlError => throw UrlInterpolatorError(error.message)


  def refined(context: Expr[StringContext], insertions: Expr[Seq[Any]])(using Quotes)
  :   Expr[Url[Label]] =

    import quotes.reflect.*

    val parts: List[String] =
      context.value.getOrElse:
        halt(m"the StringContext extension method parameter does not appear to be inline")

      . parts.toList

    val insertionExprs: List[Expr[Any]] = insertions.absolve match
      case Varargs(exprs) => exprs.toList

    def rethrow[result](block: => result): result =
      try block catch case error: UrlInterpolatorError => halt(error.detail)

    var checkState: Text = rethrow(Runtime.parse(Runtime.initial, parts.head.tt))

    var runtimeExpr: Expr[Text] =
      '{Runtime.parse(Runtime.initial, ${Expr(parts.head)}.tt)}

    var i = 0

    while i < insertionExprs.length do
      val head = insertionExprs(i)
      val nextPart = parts(i + 1)

      head.absolve match
        case '{$value: tpe} =>
          val typeclassExpr: Expr[Insertion[UrlFragment, tpe]] =
            Expr.summon[Insertion[UrlFragment, tpe]].getOrElse:
              halt(m"can't substitute ${TypeRepr.of[tpe].show} into a url-interpolator")

          typeclassExpr.absolve match
            case '{$_ : Substitution[UrlFragment, tpe, sub]} =>
              val label: String = TypeRepr.of[sub] match
                case ConstantType(StringConstant(s)) => s

                case _ =>
                  halt(m"expected a literal string label for the substitution")

              checkState = rethrow(Runtime.substitute(checkState, label.tt))

            case _ =>
              checkState = rethrow(Runtime.skip(checkState))

          checkState = rethrow(Runtime.parse(checkState, nextPart.tt))

          val current = runtimeExpr

          runtimeExpr =
            ' {
                Runtime.parse
                  ( Runtime.insert($current, $typeclassExpr.embed($value)),
                    ${Expr(nextPart)}.tt )
              }

      i += 1

    rethrow(Runtime.complete(checkState))

    val schemeConstant = parts.head.split(":").nn.head.nn

    ConstantType(StringConstant(schemeConstant)).asType.absolve match
      case '[type label <: Label; label] =>
        '{Runtime.complete($runtimeExpr).asInstanceOf[Url[label]]}
