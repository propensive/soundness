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
┃    Soundness, version 0.44.0.                                                                    ┃
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

import anticipation.*
import contextual.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import proscenium.*
import spectacular.*
import symbolism.*

import scala.quoted.*

import errorDiagnostics.empty

object UrlInterpolator extends contextual.Interpolator[UrlFragment, Text, Url[Label]]:

  def refined(context: Expr[StringContext], parts: Expr[Seq[Any]]): Macro[Url[Label]] =
    import quotes.reflect.*

    val constant = context.value.get.parts.head.split(":").nn.head.nn

    ConstantType(StringConstant(constant)).asType.absolve match
      case '[type label <: Label; label] => '{${expand(context, parts)}.asInstanceOf[Url[label]]}

  def complete(value: Text): Url[Label] =
    try throwErrors(value.decode[Url[Label]]) catch
      case error: UrlError      => throw InterpolationError(error.message)

  def initial: Text = t""

  def insert(state: Text, value: UrlFragment): Text = value match
    case UrlFragment.Integral(port) =>
      if !state.ends(t":")
      then throw InterpolationError(m"a port number must be specified after a colon")

      try throwErrors((state+port.show).decode[HttpUrl]) catch
        case err: UrlError      => throw InterpolationError(Message(err.message.text))

      state+port.show

    case UrlFragment.Textual(text) =>
      // if !state.ends(t"/")
      // then throw InterpolationError(m"a substitution may only be made after a slash")

      try throwErrors((state+text.urlEncode).decode[HttpUrl]) catch
        case err: UrlError      => throw InterpolationError(Message(err.message.text))

      state+text.urlEncode

    case UrlFragment.RawTextual(text) =>
      // if !state.ends(t"/")
      // then throw InterpolationError(m"a substitution may only be made after a slash")

      try throwErrors((state+text.urlEncode).decode[HttpUrl]) catch
        case err: UrlError      => throw InterpolationError(Message(err.message.text))

      state+text

  override def substitute(state: Text, sub: Text): Text = state+sub

  def parse(state: Text, next: Text): Text =
    //if !state.empty && !(next.starts(t"/") || next.empty)
    //then throw InterpolationError(m"a substitution must be followed by a slash")

    state+next

  def skip(state: Text): Text = state+t"1"
