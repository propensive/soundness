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
┃    Soundness, version 0.27.0.                                                                    ┃
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
import vacuous.*

case class Url[+SchemeType <: Label]
   (origin:    Origin[SchemeType],
    pathText:  Text,
    query:     Optional[Text]      = Unset,
    fragment:  Optional[Text]      = Unset)
extends Root
         (t"${origin.scheme.name}://${origin.authority.lay(t"")(_.show)}$pathText", t"/",
          Case.Sensitive):
  type Platform = HttpUrl

  def scheme: Scheme[SchemeType] = origin.scheme
  def authority: Optional[Authority] = origin.authority
  def requestTarget: Text = pathText+query.lay(t"")(t"?"+_)
  def host: Optional[Hostname] = authority.let(_.host)

object Url:
  type Rules = MustMatch["[A-Za-z0-9_.~-]*"]

  given radical: (Tactic[UrlError], Tactic[NameError])
  =>    HttpUrl is Radical from HttpUrl = new Radical:
    type Self = HttpUrl
    type Source = HttpUrl

    def root(path: Text): HttpUrl = Url.parse(path.keep(rootLength(path)))
    def rootLength(path: Text): Int = path.where(_ == '/', 7.z).let(_.n0).or(path.length)
    def rootText(url: HttpUrl): Text = url.show

  given navigable: (Tactic[UrlError], Tactic[NameError])
  =>    HttpUrl is Navigable by Name[HttpUrl] under Rules = new Navigable:

    type Operand = Name[HttpUrl]
    type Self = HttpUrl
    type Constraint = Rules

    val separator: Text = t"/"
    val parentElement: Text = t".."
    val selfText: Text = t"."

    def element(element: Text): Name[HttpUrl] = Name(element)
    def elementText(element: Name[HttpUrl]): Text = element.text
    def caseSensitivity: Case = Case.Sensitive

  given HttpUrl is Abstractable across Urls into Text = _.show

  given (Tactic[UrlError])
  =>    HttpUrl is Instantiable across Urls from Text =
    Url.parse(_)

  given showable: [SchemeType <: Label] => Url[SchemeType] is Showable = url =>
    val auth = url.authority.lay(t"")(t"//"+_.show)
    val rest = t"${url.query.lay(t"")(t"?"+_)}${url.fragment.lay(t"")(t"#"+_)}"
    t"${url.scheme}:$auth${url.pathText}$rest"

  given [SchemeType <: Label] => Tactic[UrlError] => Url[SchemeType] is Decodable in Text =

    parse(_)

  given [SchemeType <: Label] => Url[SchemeType] is Encodable in Text = _.show

  given teletype: [SchemeType <: Label] => Url[SchemeType] is Teletypeable =
    url => e"$Underline(${Fg(0x00bfff)}(${url.show}))"

  given communicable: [SchemeType <: Label] => Url[SchemeType] is Communicable =
    url => Message(url.show)

  given action: [SchemeType <: Label] => ("action" is GenericHtmlAttribute[Url[SchemeType]]):
    def name: Text = t"action"
    def serialize(url: Url[SchemeType]): Text = url.show

  given codebale: [SchemeType <: Label] => ("codebase" is GenericHtmlAttribute[Url[SchemeType]]):
    def name: Text = t"codebase"
    def serialize(url: Url[SchemeType]): Text = url.show

  given cite: [SchemeType <: Label] => ("cite" is GenericHtmlAttribute[Url[SchemeType]]):
    def name: Text = t"cite"
    def serialize(url: Url[SchemeType]): Text = url.show

  given data: [SchemeType <: Label] => ("data" is GenericHtmlAttribute[Url[SchemeType]]):
    def name: Text = t"data"
    def serialize(url: Url[SchemeType]): Text = url.show

  given formaction: [SchemeType <: Label]
  =>    ("formaction" is GenericHtmlAttribute[Url[SchemeType]]):
    def name: Text = t"formaction"
    def serialize(url: Url[SchemeType]): Text = url.show

  given poster: [SchemeType <: Label] => ("poster" is GenericHtmlAttribute[Url[SchemeType]]):
    def name: Text = t"poster"
    def serialize(url: Url[SchemeType]): Text = url.show

  given src: [SchemeType <: Label] => ("src" is GenericHtmlAttribute[Url[SchemeType]]):
    def name: Text = t"src"
    def serialize(url: Url[SchemeType]): Text = url.show

  given href: [SchemeType <: Label] => ("href" is GenericHtmlAttribute[Url[SchemeType]]):
    def name: Text = t"href"
    def serialize(url: Url[SchemeType]): Text = url.show

  given manifest: [SchemeType <: Label] => ("manifest" is GenericHtmlAttribute[Url[SchemeType]]):
    def name: Text = t"manifest"
    def serialize(url: Url[SchemeType]): Text = url.show

  def parse[SchemeType <: Label](value: Text)
  :     Url[SchemeType] raises UrlError =
    import UrlError.Expectation.*

    safely(value.where(_ == ':')).asMatchable match
      case Zerary(colon) =>
        val text = value.before(colon)
        val scheme = Scheme(text)

        val (pathStart, auth) =
          if value.after(colon).keep(2) == t"//" then
            tend:
              case error@HostnameError(hostname, reason) =>
                import error.diagnostics
                UrlError(value, colon + 3, UrlError.Reason.BadHostname(hostname, reason))

            . within:
                val authEnd = safely(value.where(_ == '/', colon + 3)).or(Ult.of(value))
                val hostname = value.segment((colon + 3) ~ authEnd.previous)
                (authEnd, Authority.parse(hostname))

          else (colon + 1, Unset)

        safely(value.where(_ == '?', pathStart)).asMatchable match
          case Zerary(qmark) =>
            safely(value.where(_ == '#', qmark + 1)).asMatchable match
              case Zerary(hash) =>
                Url
                 (Origin(scheme, auth),
                  value.segment(pathStart ~ qmark.previous),
                  value.segment((qmark + 1) ~ hash.previous),
                  value.after(hash))

              case _ =>
                Url
                 (Origin(scheme, auth),
                  value.segment(pathStart ~ qmark.previous),
                  value.after(qmark),
                  Unset)

          case _ => safely(value.where(_ == '#', pathStart)).asMatchable match
            case Zerary(hash) =>
              Url
               (Origin(scheme, auth),
                value.segment(pathStart ~ hash.previous),
                Unset,
                value.after(hash))

            case _ =>
              Url(Origin(scheme, auth), value.from(pathStart), Unset, Unset)

      case _ =>
        abort(UrlError(value, Ult.of(value), UrlError.Reason.Expected(Colon)))
