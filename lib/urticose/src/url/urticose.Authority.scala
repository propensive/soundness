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

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Authority:
  given showable: Authority is Showable = auth =>
    t"${auth.userInfo.lay(t"")(_+t"@")}${auth.host.show}${auth.port.let(_.show).lay(t"")(t":"+_)}"

  given decodable: (hostnameTactic: Tactic[HostnameError])
  =>  (ipTactic: Tactic[IpAddressError])
  =>  (urlTactic: Tactic[UrlError])
  =>  ((Authority is Decodable in Text)^{hostnameTactic, ipTactic, urlTactic}) =
    parse(_)

  // The error capabilities are taken as an eager `using` clause rather than a chained `raises`
  // result, so the nested `parsePort`/`parseHostPort` helpers may capture them; a `raises` chain
  // makes each a context-function result that an enclosing helper literal cannot capture under CC.
  private def parse(value: Text)
      (using Tactic[HostnameError], Tactic[IpAddressError], Tactic[UrlError])
  :   Authority =

    import UrlError.{Expectation, Reason}, Expectation.*, Reason.*

    def parsePort(portText: Text, offset: Ordinal): Int =
      safely(portText.s.toInt).match
        case port: Int if port >= 0 && port <= 65535 => port

        case port: Int =>
          abort(UrlError(value, offset, Expected(PortRange)))

        case _ =>
          abort(UrlError(value, offset, Expected(Number)))

    def parseHostPort(hostPort: Text, base: Ordinal, userInfo: Optional[Text]): Authority =
      if hostPort(Prim) == '[' then
        safely(hostPort.where(_ == ']')).asMatchable match
          case Zerary(close) =>
            val ipv6 = hostPort.segment(Sec till close).as[Ipv6]
            val afterClose: Ordinal = close + 1

            if afterClose.n0 >= hostPort.limit.n0 then Authority(ipv6, userInfo)
            else if hostPort(afterClose) == ':' then
              val portOffset: Ordinal = base + (afterClose.n0 + 1)
              val port = parsePort(hostPort.after(afterClose), portOffset)
              Authority(ipv6, userInfo, port)
            else
              val errorOffset: Ordinal = base + afterClose.n0

              abort(UrlError(value, errorOffset, Expected(More)))

          case _ =>
            val errorOffset: Ordinal = base + (hostPort.limit.n0 - 1).max(0)

            abort(UrlError(value, errorOffset, Expected(More)))
      else
        safely(hostPort.where(_ == ':')).asMatchable match
          case Zerary(colon) =>
            val portOffset: Ordinal = base + (colon.n0 + 1)
            val port = parsePort(hostPort.after(colon), portOffset)
            Authority(hostPort.before(colon).as[Host], userInfo, port)

          case _ =>
            Authority(hostPort.as[Host], userInfo)

    safely(value.where(_ == '@')).asMatchable match
      case Zerary(arobase) =>
        val hostStart: Ordinal = arobase + 1
        parseHostPort(value.after(arobase), hostStart, value.keep(arobase.n0))

      case _ =>
        parseHostPort(value, Prim, Unset)

case class Authority
  ( host: Host,
    userInfo: Optional[Text] = Unset,
    port: Optional[Int] = Unset )
