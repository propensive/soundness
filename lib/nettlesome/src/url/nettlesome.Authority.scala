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
┃    Soundness, version 0.38.0.                                                                    ┃
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
package nettlesome

import scala.compiletime.*

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Authority:
  given showable: Authority is Showable = auth =>
    t"${auth.userInfo.lay(t"")(_+t"@")}${auth.host}${auth.port.let(_.show).lay(t"")(t":"+_)}"

  def parse(value: Text): Authority raises HostnameError raises UrlError =
    import UrlError.{Expectation, Reason}, Expectation.*, Reason.*

    safely(value.where(_ == '@')).asMatchable match
      case Zerary(arobase) => safely(value.where(_ == ':', arobase + 1)).asMatchable match
        case Zerary(colon) =>
          safely(value.after(colon).s.toInt).match
            case port: Int if port >= 0 && port <= 65535 => port

            case port: Int =>
              raise(UrlError(value, colon + 1, Expected(PortRange))) yet 0

            case _ =>
              raise(UrlError(value, colon + 1, Expected(Number))) yet 0

          . pipe:
            Authority
             (Hostname.parse(value.segment((arobase + 1) ~ (colon - 1))), value.keep(arobase.n0), _)

        case _ =>
          Authority(Hostname.parse(value.after(arobase)), value.before(arobase))

      case _ => value.where(_ == ':').asMatchable match
        case Zerary(colon) =>
          safely(value.after(colon).s.toInt).match
            case port: Int if port >= 0 && port <= 65535 => port

            case port: Int =>
              raise(UrlError(value, colon + 1, Expected(PortRange))) yet 0

            case _ =>
              raise(UrlError(value, colon + 1, Expected(Number))) yet 0

          . pipe(Authority(Hostname.parse(value.before(colon)), Unset, _))

        case _ =>
          Authority(Hostname.parse(value))

case class Authority(host: Hostname, userInfo: Optional[Text] = Unset, port: Optional[Int] = Unset)
