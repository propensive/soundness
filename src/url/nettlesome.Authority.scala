/*
    Nettlesome, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nettlesome

import gossamer.*
import rudiments.*
import vacuous.*
import fulminate.*
import contingency.*
import denominative.*
import anticipation.*
import contextual.*
import spectacular.*

object Authority:
  given Authority is Showable = auth =>
    t"${auth.userInfo.lay(t"")(_+t"@")}${auth.host}${auth.port.let(_.show).lay(t"")(t":"+_)}"

  def parse(value: Text)(using Tactic[UrlError]): Authority raises HostnameError =
    import UrlError.Expectation.*

    safely(value.where(_ == '@')) match
      case Unset => value.where(_ == ':') match
        case Unset =>
          Authority(Hostname.parse(value))

        case colon: Ordinal =>
          safely(value.after(colon).s.toInt).match
            case port: Int if port >= 0 && port <= 65535 => port

            case port: Int =>
              raise(UrlError(value, colon + 1, PortRange), 0)

            case Unset =>
              raise(UrlError(value, colon + 1, Number), 0)
          .pipe(Authority(Hostname.parse(value.before(colon)), Unset, _))

      case arobase: Ordinal => safely(value.where(_ == ':', arobase + 1)) match
        case Unset =>
          Authority(Hostname.parse(value.after(arobase)), value.before(arobase))

        case colon: Ordinal =>
          safely(value.after(colon).s.toInt).match
            case port: Int if port >= 0 && port <= 65535 => port

            case port: Int =>
              raise(UrlError(value, colon + 1, PortRange), 0)

            case Unset =>
              raise(UrlError(value, colon + 1, Number), 0)
          .pipe(Authority(Hostname.parse(value.slice((arobase + 1) ~ (colon - 1))), value.keep(arobase.n0), _))

case class Authority(host: Hostname, userInfo: Optional[Text] = Unset, port: Optional[Int] = Unset)
