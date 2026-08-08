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
import serpentine.*
import spectacular.*
import symbolism.*
import vacuous.*

object Url:
  given abstractable: HttpUrl is Abstractable across Urls to Text = _.show

  given showable: [scheme <: Label] => Url[scheme] is Showable = url =>
    val auth = url.authority.lay(t"")(t"//"+_.show)
    val rest = t"${url.query.lay(t"")(t"?"+_)}${url.fragment.lay(t"")(t"#"+_)}"
    t"${url.scheme}:$auth${url.location}$rest"

  given encodable: [scheme <: Label] => Url[scheme] is Encodable in Text = _.show

  given decodable: [scheme <: Label] => (tactic: Tactic[UrlError])
  =>  ((Url[scheme] is Decodable in Text)^{tactic}) =
    value =>
      import UrlError.Expectation.*

      safely(value.pinpoint(_ == ':')).asMatchable match
        case Zerary(colon) =>
          val text = value.before(colon)
          val scheme = Scheme(text)

          val (pathStart, auth) =
            if value.after(colon).keep(2) == t"//" then
              mitigate:
                case error@HostnameError(hostname, reason) =>
                  import error.diagnostics
                  UrlError(value, colon + 3, UrlError.Reason.BadHostname(hostname, reason))

                case error@IpAddressError(reason) =>
                  import error.diagnostics
                  UrlError(value, colon + 3, UrlError.Reason.BadIpv6(reason))

              . protect:
                  val authEnd = safely:
                    value.pinpoint(c => c == '/' || c == '?' || c == '#', colon + 3)

                  . or(value.limit)
                  val hostname = value.segment((colon + 3) till authEnd)
                  (authEnd, hostname.as[Authority])

            else
              (colon + 1, Unset)

          safely(value.pinpoint(_ == '?', pathStart)).asMatchable match
            case Zerary(qmark) =>
              safely(value.pinpoint(_ == '#', qmark + 1)).asMatchable match
                case Zerary(hash) =>
                  Url
                    ( Origin(scheme, auth),
                      value.segment(pathStart till qmark),
                      value.segment((qmark + 1) till hash),
                      value.after(hash) )

                case _ =>
                  Url
                    ( Origin(scheme, auth),
                      value.segment(pathStart till qmark),
                      value.after(qmark),
                      Unset )

            case _ => safely(value.pinpoint(_ == '#', pathStart)).asMatchable match
              case Zerary(hash) =>
                Url
                  ( Origin(scheme, auth),
                    value.segment(pathStart till hash),
                    Unset,
                    value.after(hash) )

              case _ =>
                Url(Origin(scheme, auth), value.from(pathStart), Unset, Unset)

        case _ =>
          abort(UrlError(value, value.limit - 1, UrlError.Reason.Expected(Colon)))

  given instantiable: (tactic: Tactic[UrlError])
  =>  ((HttpUrl is Instantiable across Urls from Text)^{tactic}) =
    _.as[HttpUrl]

class Url[+scheme <: Label]
  ( val origin:   Origin[scheme],
    val location: Text,
    val query:    Optional[Text] = Unset,
    val fragment: Optional[Text] = Unset )
extends Root(t"${origin.scheme}:${origin.authority.lay(t"")(t"//"+_.show)}$location"):

  type Plane = Www
  type Topic = Zero

  def scheme: Scheme[scheme] = origin.scheme
  def authority: Optional[Authority] = origin.authority
  def requestTarget: Text = location+query.lay(t"")(t"?"+_)
  def host: Optional[Host] = authority.let(_.host)
  // `Www`'s `Radical` always succeeds, so decoding the path cannot fail.
  def path: Path on Www = unsafely(location.as[Path on Www])
