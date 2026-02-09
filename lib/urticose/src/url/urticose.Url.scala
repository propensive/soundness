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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import escapade.*
import fulminate.*
import gossamer.*
import nomenclature.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import vacuous.*

class Url[+scheme <: Label]
  ( val origin:    Origin[scheme],
        val location:  Text,
        val query:     Optional[Text] = Unset,
        val fragment:  Optional[Text] = Unset)
extends Root(t"${origin.scheme}:${origin.authority.lay(t"")(t"//"+_.show)}$location"):

  type Plane = Www
  type Topic = Zero

  def scheme: Scheme[scheme] = origin.scheme
  def authority: Optional[Authority] = origin.authority
  def requestTarget: Text = location+query.lay(t"")(t"?"+_)
  def host: Optional[Hostname] = authority.let(_.host)
  def path: Path on Www = location.decode[Path on Www]

object Url:

  given abstractable: HttpUrl is Abstractable across Urls to Text = _.show

  given showable: [scheme <: Label] => Url[scheme] is Showable = url =>
    val auth = url.authority.lay(t"")(t"//"+_.show)
    val rest = t"${url.query.lay(t"")(t"?"+_)}${url.fragment.lay(t"")(t"#"+_)}"
    t"${url.scheme}:$auth${url.location}$rest"

  given encodable: [scheme <: Label] => Url[scheme] is Encodable in Text = _.show

  given teletype: [scheme <: Label] => Url[scheme] is Teletypeable =
    url => e"$Underline(${Fg(0x00bfff)}(${url.show}))"

  given decodable: [scheme <: Label] => Tactic[UrlError] => Url[scheme] is Decodable in Text =
    value =>
      import UrlError.Expectation.*

      safely(value.where(_ == ':')).asMatchable match
        case Zerary(colon) =>
          val text = value.before(colon)
          val scheme = Scheme(text)

          val (pathStart, auth) =
            if value.after(colon).keep(2) == t"//" then
              mitigate:
                case error@HostnameError(hostname, reason) =>
                  import error.diagnostics
                  UrlError(value, colon + 3, UrlError.Reason.BadHostname(hostname, reason))

              . within:
                  val authEnd = safely(value.where(_ == '/', colon + 3)).or(value.limit - 1)
                  val hostname = value.segment((colon + 3) till authEnd)
                  (authEnd, hostname.decode[Authority])

            else (colon + 1, Unset)

          safely(value.where(_ == '?', pathStart)).asMatchable match
            case Zerary(qmark) =>
              safely(value.where(_ == '#', qmark + 1)).asMatchable match
                case Zerary(hash) =>
                  Url
                   (Origin(scheme, auth),
                    value.segment(pathStart till qmark),
                    value.segment((qmark + 1) till hash),
                    value.after(hash))

                case _ =>
                  Url
                   (Origin(scheme, auth),
                    value.segment(pathStart till qmark),
                    value.after(qmark),
                    Unset)

            case _ => safely(value.where(_ == '#', pathStart)).asMatchable match
              case Zerary(hash) =>
                Url
                 (Origin(scheme, auth),
                  value.segment(pathStart till hash),
                  Unset,
                  value.after(hash))

              case _ =>
                Url(Origin(scheme, auth), value.from(pathStart), Unset, Unset)

        case _ =>
          abort(UrlError(value, value.limit - 1, UrlError.Reason.Expected(Colon)))

  given instantiable: (Tactic[UrlError]) => HttpUrl is Instantiable across Urls from Text =
    _.decode[HttpUrl]
