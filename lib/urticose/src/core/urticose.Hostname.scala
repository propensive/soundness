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
package urticose

import scala.quoted.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import Hostname.Error.Reason.*

object Hostname:
  given showable: Hostname is Showable = _.dnsLabels.map(_.show).join(t".")
  given inspectable: [hostname <: Hostname] => hostname is Inspectable = showable.text(_)
  given decodable: (tactic: Tactic[Hostname.Error])
  =>  ((Hostname is Decodable in Text)^{tactic}) =
    parse(_)
  given encodable: Hostname is Encodable in Text = showable.text(_)

  given toExpr: ToExpr[Hostname]:
    def apply(hostname: Hostname)(using Quotes): Expr[Hostname] =
      val labels = Varargs:
        hostname.dnsLabels.map: label => '{DnsLabel(${Expr(label.text)})}

      '{Hostname($labels*)}

  private[urticose] def parse(text: Text): Hostname raises Hostname.Error =
    val builder: TextBuilder = TextBuilder()

    def recur(index: Ordinal, dnsLabels: List[DnsLabel]): Hostname = text(index) match
      case char: Char if char != '.' =>
        if char == '-' || ('A' <= char <= 'Z') || ('a' <= char <= 'z') || char.isDigit
        then builder.append(char.toString.tt)
        else raise(Hostname.Error(text, InvalidChar(char)))

        recur(index + 1, dnsLabels)

      case _ =>
        val label = builder()
        if label.nil then raise(Hostname.Error(text, EmptyDnsLabel(dnsLabels.stdlib.length)))
        if label.length > 63 then raise(Hostname.Error(text, LongDnsLabel(label)))
        if label.starts(t"-") then raise(Hostname.Error(text, InitialDash(label)))
        val dnsLabels2 = DnsLabel(label) :: dnsLabels
        builder.clear()

        if index < text.limit then recur(index + 1, dnsLabels2) else
          if dnsLabels2.map(_.text.length + 1).total > 254
          then raise(Hostname.Error(text, LongHostname))

          Hostname(dnsLabels2.reverse*)

    recur(Prim, Nil)

  // HostnameError → Hostname.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case LongDnsLabel(label) => m"the DNS label $label is longer than 63 characters"
        case LongHostname        => m"the hostname is longer than 253 characters"
        case InvalidChar(char)   => m"the character $char is not allowed in a hostname"
        case EmptyDnsLabel(n)    => m"a DNS label cannot be empty"
        case InitialDash(label)  => m"the DNS label $label begins with a dash which is not allowed"

    enum Reason(val number: Int) extends Clarification:
      case LongDnsLabel(label: Text) extends Reason(1)
      case LongHostname              extends Reason(2)
      case InvalidChar(char: Char)   extends Reason(3)
      case EmptyDnsLabel(n: Int)     extends Reason(4)
      case InitialDash(label: Text)  extends Reason(5)

  case class Error(text: Text, reason: Hostname.Error.Reason)(using Diagnostics)
  extends fulminate.Error(892, reason.number)(m"the hostname is not valid because $reason")

case class Hostname(dnsLabels: DnsLabel*)
