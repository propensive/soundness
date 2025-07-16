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
┃    Soundness, version 0.39.0.                                                                    ┃
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
import denominative.*
import fulminate.*
import gossamer.*
import hypotenuse.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.quoted.*

import HostnameError.Reason.*

object Hostname:
  given realm: Realm = realm"urticose"

  given showable: Hostname is Showable = _.dnsLabels.map(_.show).join(t".")

  def expand(context: Expr[StringContext]): Macro[Hostname] = abortive:
    Expr(Hostname.parse(context.valueOrAbort.parts.head.tt))

  given toExpr: ToExpr[Hostname]:
    def apply(hostname: Hostname)(using Quotes): Expr[Hostname] =
      val labels = Varargs:
        hostname.dnsLabels.map: label =>
          '{DnsLabel(${Expr(label.text)})}

      '{Hostname($labels*)}

  def parse(text: Text): Hostname raises HostnameError =
    val builder: TextBuilder = TextBuilder()

    def recur(index: Ordinal, dnsLabels: List[DnsLabel]): Hostname = text.at(index) match
      case '.' | Unset =>
        val label = builder()
        if label.empty then raise(HostnameError(text, EmptyDnsLabel(dnsLabels.length)))
        if label.length > 63 then raise(HostnameError(text, LongDnsLabel(label)))
        if label.starts(t"-") then raise(HostnameError(text, InitialDash(label)))
        val dnsLabels2 = DnsLabel(label) :: dnsLabels
        builder.clear()

        if index <= Ult.of(text) then recur(index + 1, dnsLabels2) else
          if dnsLabels2.map(_.text.length + 1).sum > 254
          then raise(HostnameError(text, LongHostname))

          Hostname(dnsLabels2.reverse*)

      case char: Char =>
        if char == '-' || ('A' <= char <= 'Z') || ('a' <= char <= 'z') || char.isDigit
        then builder.append(char.toString.tt)
        else raise(HostnameError(text, InvalidChar(char)))
        recur(index + 1, dnsLabels)

    recur(Prim, Nil)

case class Hostname(dnsLabels: DnsLabel*)
