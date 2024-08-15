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

import serpentine.{keep as _, *}
import gossamer.*
import rudiments.*
import vacuous.*
import fulminate.*
import contingency.*
import denominative.*
import escapade.*
import anticipation.*
import contextual.*
import spectacular.*

case class Url[+SchemeType <: Label]
    (scheme:    Scheme[SchemeType],
     authority: Optional[Authority],
     pathText:  Text,
     query:     Optional[Text]      = Unset,
     fragment:  Optional[Text]      = Unset):

  lazy val path: List[Name[""]] =
    // FIXME: This needs to be handled better
    import strategies.throwUnsafely
    pathText.tail.cut(t"/").reverse.map(_.urlDecode).map(Name(_))

  def requestTarget: Text = pathText+query.lay(t"")(t"?"+_)

object Url:
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

  given [SchemeType <: Label]: Encoder[Url[SchemeType]] = _.show

  given [SchemeType <: Label]
      => Url[SchemeType] is Navigable["", (Scheme[SchemeType], Optional[Authority])]:
    def separator(url: Url[SchemeType]): Text = t"/"
    def descent(url: Url[SchemeType]): List[Name[""]] = url.path

    def root(url: Url[SchemeType]): (Scheme[SchemeType], Optional[Authority]) =
      (url.scheme, url.authority)

    def prefix(root: (Scheme[SchemeType], Optional[Authority])): Text =
      t"${root(0).name}:${root(1).let(t"//"+_.show).or(t"")}"

  given [SchemeType <: Label]
      => PathCreator[Url[SchemeType], "", (Scheme[SchemeType], Optional[Authority])]:

    def path(ascent: (Scheme[SchemeType], Optional[Authority]), descent: List[Name[""]])
            : Url[SchemeType] =

      Url(ascent(0), ascent(1), descent.reverse.map(_.render).join(t"/"))

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

    safely(value.where(_ == ':')) match
      case Unset =>
        abort(UrlError(value, Ult.of(value), Colon))

      case colon: Ordinal =>
        val text = value.before(colon)
        val scheme = Scheme(text)

        val (pathStart, auth) =
          if value.after(colon).keep(2) == t"//" then
            val authEnd = safely(value.where(_ == '/', colon + 3)).or(Ult.of(value))
            (authEnd, Authority.parse(value.slice((colon + 3) ~ authEnd.previous)))
          else (colon + 1, Unset)

        safely(value.where(_ == '?', pathStart)) match
          case Unset => safely(value.where(_ == '#', pathStart)) match
            case Unset =>
              Url(scheme, auth, value.from(pathStart), Unset, Unset)

            case hash: Ordinal =>
              Url(scheme, auth, value.slice(pathStart ~ hash.previous), Unset, value.after(hash))

          case qmark: Ordinal =>
            safely(value.where(_ == '#', qmark + 1)) match
              case Unset =>
                Url(scheme, auth, value.slice(pathStart ~ qmark.previous), value.after(qmark), Unset)

              case hash: Ordinal =>
                Url
                 (scheme,
                  auth,
                  value.slice(pathStart ~ qmark.previous),
                  value.slice((qmark + 1) ~ hash.previous),
                  value.after(hash))
