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
package digression

import scala.caps

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

object Fqcn:
  // Decoding a `Fqcn` from `Text`, in `Fqcn`'s own companion rather than distillate's
  // `Decodable`, so that distillate need not depend on digression.
  given decodable: (tactic: Tactic[Fqcn.Error]^)
  =>  ( (Fqcn is Decodable in Text)^{tactic, caps.any} ) =
    Fqcn(_)

  def valid(char: Char): Boolean =
    char >= 'A' && char <= 'Z' || char >= 'a' && char <= 'z' || char >= '0' && char <= '9' ||
      char == '_' || char == '$'

  def apply(name: Text): Fqcn raises Fqcn.Error =
    val parts = List.from(name.s.split("\\.").nn.iterator.map(_.nn.tt))

    parts.each: part =>
      if part.s.length == 0 then raise(Fqcn.Error(name, Fqcn.Error.Reason.EmptyName))

      if digression.internal.javaKeywords.has(part.s)
      then raise(Fqcn.Error(name, Fqcn.Error.Reason.JavaKeyword(part)))

      part.s.foreach: char =>
        if !valid(char) then raise(Fqcn.Error(name, Fqcn.Error.Reason.InvalidChar(char)))

      if part.s.head >= '0' && part.s.head <= '9'
      then raise(Fqcn.Error(name, Fqcn.Error.Reason.InvalidStart(part.s.head)))

    // `split` yields at least one part, so this only guards the type; the `Populated` proof it
    // records is what makes `className` and `packageName` total.
    new Fqcn(parts.occupied.or(abort(Fqcn.Error(name, Fqcn.Error.Reason.EmptyName))))

  private[digression] def join(parts: List[Text]): Text =
    parts.fold(StringBuilder()): (builder, part) =>
      (if builder.isEmpty then builder else builder.append('.')).append(part.s)

    . toString.tt

  given encodable: Fqcn is Encodable in Text = _.text

  // FqcnError → Fqcn.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case InvalidChar(char: Char)    extends Reason(1)
      case InvalidStart(char: Char)   extends Reason(2)
      case EmptyName                  extends Reason(3)
      case JavaKeyword(keyword: Text) extends Reason(4)

    given communicable: Reason is Communicable =
      case Reason.InvalidChar(char)    => m"a package name may not contain the character $char"
      case Reason.InvalidStart(char)   => m"a package name may not start with the character $char"
      case Reason.EmptyName            => m"a package name cannot be empty"
      case Reason.JavaKeyword(keyword) => m"a package name cannot be the Java keyword, $keyword"

  case class Error(name: Text, reason: Fqcn.Error.Reason)(using Diagnostics)
  extends fulminate.Error(17, reason.number)(m"the class name $name is not valid because $reason")

// A class name has at least one part, and the type says so; the linear-size gate is open
// because a name's parts are a handful.
class Fqcn(val parts: List[Text] & Populated):
  import denominative.dysasymptotics.linearSize

  def text: Text = Fqcn.join(parts)
  def className: Text = parts.last
  def packageName: Text = Fqcn.join(parts.lead)
