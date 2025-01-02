/*
    Nettlesome, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.compiletime.*

import anticipation.*
import contextual.*
import contingency.*
import denominative.*
import escapade.*
import fulminate.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import vacuous.*

case class Url[+SchemeType <: Label]
   (scheme:    Scheme[SchemeType],
    authority: Optional[Authority],
    pathText:  Text,
    query:     Optional[Text]      = Unset,
    fragment:  Optional[Text]      = Unset)
extends Root(t"${scheme.name}://${authority.lay(t"")(_.show)}$pathText", t"/", Case.Sensitive):
  type Platform = HttpUrl

  def requestTarget: Text = pathText+query.lay(t"")(t"?"+_)

object Url:
  type Rules = MustMatch["[A-Za-z0-9_.~-]*"]

  given (using Tactic[UrlError], Tactic[HostnameError], Tactic[NameError])
      => HttpUrl is Radical from HttpUrl as radical = new Radical:
    type Self = HttpUrl
    type Source = HttpUrl

    def root(path: Text): HttpUrl = Url.parse(path.keep(rootLength(path)))
    def rootLength(path: Text): Int = path.where(_ == '/', Oct).let(_.n0).or(path.length)
    def rootText(url: HttpUrl): Text = url.show

  given (using Tactic[UrlError], Tactic[HostnameError], Tactic[NameError])
      => HttpUrl is Navigable by Name[HttpUrl] under Rules as navigable = new Navigable:
    type Operand = Name[HttpUrl]
    type Self = HttpUrl
    type Constraint = Rules

    val separator: Text = t"/"
    val parentElement: Text = t".."
    val selfText: Text = t"."

    def element(element: Text): Name[HttpUrl] = Name(element)
    def elementText(element: Name[HttpUrl]): Text = element.text
    def caseSensitivity: Case = Case.Sensitive

  given HttpUrl is GenericUrl = _.show
  given (using Tactic[UrlError], Tactic[HostnameError]) => HttpUrl is SpecificUrl = Url.parse(_)

  given [SchemeType <: Label] => Url[SchemeType] is Showable as showable = url =>
    val auth = url.authority.lay(t"")(t"//"+_.show)
    val rest = t"${url.query.lay(t"")(t"?"+_)}${url.fragment.lay(t"")(t"#"+_)}"
    t"${url.scheme}:$auth${url.pathText}$rest"

  given [SchemeType <: Label] => ("location" is GenericHttpRequestParam[Url[SchemeType]]) = _.show

  given [SchemeType <: Label](using Tactic[UrlError], Tactic[HostnameError])
      => Decoder[Url[SchemeType]] =
    parse(_)

  given [SchemeType <: Label] => Url[SchemeType] is Encodable in Text = _.show

  given [SchemeType <: Label] => Url[SchemeType] is Teletypeable as teletype =
    url => e"$Underline(${Fg(0x00bfff)}(${url.show}))"

  given [SchemeType <: Label] => Url[SchemeType] is Communicable as communicable =
    url => Message(url.show)

  given [SchemeType <: Label] => ("action" is GenericHtmlAttribute[Url[SchemeType]]) as action:
    def name: Text = t"action"
    def serialize(url: Url[SchemeType]): Text = url.show

  given [SchemeType <: Label] => ("codebase" is GenericHtmlAttribute[Url[SchemeType]]) as codebase:
    def name: Text = t"codebase"
    def serialize(url: Url[SchemeType]): Text = url.show

  given [SchemeType <: Label] => ("cite" is GenericHtmlAttribute[Url[SchemeType]]) as cite:
    def name: Text = t"cite"
    def serialize(url: Url[SchemeType]): Text = url.show

  given [SchemeType <: Label] => ("data" is GenericHtmlAttribute[Url[SchemeType]]) as data:
    def name: Text = t"data"
    def serialize(url: Url[SchemeType]): Text = url.show

  given [SchemeType <: Label]
      => ("formaction" is GenericHtmlAttribute[Url[SchemeType]]) as formaction:
    def name: Text = t"formaction"
    def serialize(url: Url[SchemeType]): Text = url.show

  given [SchemeType <: Label] => ("poster" is GenericHtmlAttribute[Url[SchemeType]]) as poster:
    def name: Text = t"poster"
    def serialize(url: Url[SchemeType]): Text = url.show

  given [SchemeType <: Label] => ("src" is GenericHtmlAttribute[Url[SchemeType]]) as src:
    def name: Text = t"src"
    def serialize(url: Url[SchemeType]): Text = url.show

  given [SchemeType <: Label] => ("href" is GenericHtmlAttribute[Url[SchemeType]]) as href:
    def name: Text = t"href"
    def serialize(url: Url[SchemeType]): Text = url.show

  given [SchemeType <: Label] => ("manifest" is GenericHtmlAttribute[Url[SchemeType]]) as manifest:
    def name: Text = t"manifest"
    def serialize(url: Url[SchemeType]): Text = url.show

  def parse[SchemeType <: Label](value: Text)(using Tactic[UrlError], Tactic[HostnameError])
          : Url[SchemeType] =
    import UrlError.Expectation.*

    safely(value.where(_ == ':')).asMatchable match
      case Zerary(colon) =>
        val text = value.before(colon)
        val scheme = Scheme(text)

        val (pathStart, auth) =
          if value.after(colon).keep(2) == t"//" then
            val authEnd = safely(value.where(_ == '/', colon + 3)).or(Ult.of(value))
            (authEnd, Authority.parse(value.segment((colon + 3) ~ authEnd.previous)))
          else (colon + 1, Unset)

        safely(value.where(_ == '?', pathStart)).asMatchable match
          case Zerary(qmark) =>
            safely(value.where(_ == '#', qmark + 1)).asMatchable match
              case Zerary(hash) =>
                Url
                 (scheme,
                  auth,
                  value.segment(pathStart ~ qmark.previous),
                  value.segment((qmark + 1) ~ hash.previous),
                  value.after(hash))

              case _ =>
                Url
                 (scheme, auth, value.segment(pathStart ~ qmark.previous), value.after(qmark), Unset)

          case _ => safely(value.where(_ == '#', pathStart)).asMatchable match
            case Zerary(hash) =>
              Url(scheme, auth, value.segment(pathStart ~ hash.previous), Unset, value.after(hash))

            case _ =>
              Url(scheme, auth, value.from(pathStart), Unset, Unset)

      case _ =>
        abort(UrlError(value, Ult.of(value), Colon))
