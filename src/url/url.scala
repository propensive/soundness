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

import serpentine.*
import gossamer.*
import rudiments.*
import vacuous.*
import fulminate.*
import contingency.*
import escapade.*
import anticipation.*
import contextual.*
import spectacular.*

import scala.quoted.*

case class UrlError(text: Text, offset: Int, expected: UrlError.Expectation)
extends Error(msg"the URL $text is not valid: expected $expected at $offset")

object Scheme:
  given Show[Scheme[Label]] = _.name
  object Http extends Scheme["http"](t"http")
  object Https extends Scheme["https"](t"https")

case class Scheme[+SchemeType <: Label](name: Text)

case class Raw(text: Text)

object UrlInput:
  given Substitution[UrlInput, Text, "x"] = UrlInput.Textual(_)
  given Substitution[UrlInput, Raw, "x"] = raw => UrlInput.RawTextual(raw.text)
  given Substitution[UrlInput, Int, "80"] = UrlInput.Integral(_)

enum UrlInput:
  case Integral(value: Int)
  case Textual(value: Text)
  case RawTextual(value: Text)

object UrlInterpolator extends contextual.Interpolator[UrlInput, Text, Url[Label]]:

  def refined(context: Expr[StringContext], parts: Expr[Seq[Any]])(using Quotes): Expr[Url[Label]] =
    import quotes.reflect.*

    (ConstantType(StringConstant(context.value.get.parts.head.split(":").nn.head.nn)).asType: @unchecked) match
      case '[type labelType <: Label; labelType] => '{${expand(context, parts)}.asInstanceOf[Url[labelType]]}

  def complete(value: Text): Url[Label] =
    try throwErrors(Url.parse(value)) catch
      case err: UrlError      => throw InterpolationError(Message(err.message.text))
      case err: HostnameError => throw InterpolationError(Message(err.message.text))

  def initial: Text = t""

  def insert(state: Text, value: UrlInput): Text = value match
    case UrlInput.Integral(port) =>
      if !state.ends(t":") then throw InterpolationError(msg"a port number must be specified after a colon")

      try throwErrors(Url.parse(state+port.show)) catch
        case err: UrlError      => throw InterpolationError(Message(err.message.text))
        case err: HostnameError => throw InterpolationError(Message(err.message.text))

      state+port.show

    case UrlInput.Textual(text) =>
      if !state.ends(t"/") then throw InterpolationError(msg"a substitution may only be made after a slash")

      try throwErrors(Url.parse(state+text.urlEncode)) catch
        case err: UrlError      => throw InterpolationError(Message(err.message.text))
        case err: HostnameError => throw InterpolationError(Message(err.message.text))

      state+text.urlEncode

    case UrlInput.RawTextual(text) =>
      if !state.ends(t"/") then throw InterpolationError(msg"a substitution may only be made after a slash")

      try throwErrors(Url.parse(state+text.urlEncode)) catch
        case err: UrlError      => throw InterpolationError(Message(err.message.text))
        case err: HostnameError => throw InterpolationError(Message(err.message.text))

      state+text

  override def substitute(state: Text, sub: Text): Text = state+sub

  def parse(state: Text, next: Text): Text =
    if !state.empty && !(next.starts(t"/") || next.empty)
    then throw InterpolationError(msg"a substitution must be followed by a slash")

    state+next

  def skip(state: Text): Text = state+t"1"

object Url:
  given HttpUrl is GenericUrl = _.show
  given (using Errant[UrlError], Errant[HostnameError]) => HttpUrl is SpecificUrl = Url.parse(_)
  given [SchemeType <: Label] => ("location" is GenericHttpRequestParam[Url[SchemeType]]) = show.text(_)

  given [SchemeType <: Label](using Errant[UrlError], Errant[HostnameError]): Decoder[Url[SchemeType]] =
    parse(_)

  given [SchemeType <: Label]: Encoder[Url[SchemeType]] = _.show

  given [SchemeType <: Label] => Url[SchemeType] is Navigable["", (Scheme[SchemeType], Optional[Authority])]:
    def separator(url: Url[SchemeType]): Text = t"/"
    def descent(url: Url[SchemeType]): List[PathName[""]] = url.path
    def root(url: Url[SchemeType]): (Scheme[SchemeType], Optional[Authority]) = (url.scheme, url.authority)

    def prefix(root: (Scheme[SchemeType], Optional[Authority])): Text =
      t"${root(0).name}:${root(1).let(t"//"+_.show).or(t"")}"

  given [SchemeType <: Label]: PathCreator[Url[SchemeType], "", (Scheme[SchemeType], Optional[Authority])] with
    def path(ascent: (Scheme[SchemeType], Optional[Authority]), descent: List[PathName[""]]): Url[SchemeType] =
      Url(ascent(0), ascent(1), descent.reverse.map(_.render).join(t"/"))

  given show[SchemeType <: Label]: Show[Url[SchemeType]] = url =>
    val auth = url.authority.lay(t"")(t"//"+_.show)
    val rest = t"${url.query.lay(t"")(t"?"+_)}${url.fragment.lay(t"")(t"#"+_)}"
    t"${url.scheme}:$auth${url.pathText}$rest"

  given display[SchemeType <: Label]: Displayable[Url[SchemeType]] =
    url => e"$Underline(${Fg(0x00bfff)}(${show.text(url)}))"

  given [SchemeType <: Label] => Url[SchemeType] is Communicable as communicable =
    url => Message(show.text(url))

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

  given [SchemeType <: Label] => ("formaction" is GenericHtmlAttribute[Url[SchemeType]]) as formaction:
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

  def parse[SchemeType <: Label](value: Text)(using Errant[UrlError], Errant[HostnameError]): Url[SchemeType] =
    import UrlError.Expectation.*

    safely(value.where(_ == ':')) match
      case Unset =>
        abort(UrlError(value, value.length, Colon))

      case colon: Int =>
        val text = value.take(colon)
        val scheme = Scheme(text)

        val (pathStart, auth) =
          if value.slice(colon + 1, colon + 3) == t"//" then
            val authEnd = safely(value.where(_ == '/', colon + 3)).or(value.length)
            (authEnd, Authority.parse(value.slice(colon + 3, authEnd)))
          else (colon + 1, Unset)

        safely(value.where(_ == '?', pathStart)) match
          case Unset => safely(value.where(_ == '#', pathStart)) match
            case Unset     => Url(scheme, auth, value.drop(pathStart), Unset, Unset)
            case hash: Int => Url(scheme, auth, value.slice(pathStart, hash), Unset, value.drop(hash + 1))

          case qmark: Int =>
            safely(value.where(_ == '#', qmark + 1)) match
              case Unset     => Url(scheme, auth, value.slice(pathStart, qmark), value.drop(qmark + 1), Unset)
              case hash: Int => Url(scheme, auth, value.slice(pathStart, qmark), value.slice(qmark + 1, hash),
                                    value.drop(hash + 1))

object Authority:
  given Show[Authority] = auth =>
    t"${auth.userInfo.lay(t"")(_+t"@")}${auth.host}${auth.port.let(_.show).lay(t"")(t":"+_)}"

  def parse(value: Text)(using Errant[UrlError]): Authority raises HostnameError =
    import UrlError.Expectation.*

    safely(value.where(_ == '@')) match
      case Unset => safely(value.where(_ == ':')) match
        case Unset =>
          Authority(Hostname.parse(value))

        case colon: Int =>
          safely(value.drop(colon + 1).s.toInt).match
            case port: Int if port >= 0 && port <= 65535 => port
            case port: Int                               => raise(UrlError(value, colon + 1, PortRange))(0)
            case Unset                                   => raise(UrlError(value, colon + 1, Number))(0)
          .pipe(Authority(Hostname.parse(value.take(colon)), Unset, _))

      case arobase: Int => safely(value.where(_ == ':', arobase + 1)) match
        case Unset =>
          Authority(Hostname.parse(value.drop(arobase + 1)), value.take(arobase))

        case colon: Int =>
          safely(value.drop(colon + 1).s.toInt).match
            case port: Int if port >= 0 && port <= 65535 => port
            case port: Int                               => raise(UrlError(value, colon + 1, PortRange))(0)
            case Unset                                   => raise(UrlError(value, colon + 1, Number))(0)
          .pipe(Authority(Hostname.parse(value.slice(arobase + 1, colon)), value.take(arobase), _))

case class Authority(host: Hostname, userInfo: Optional[Text] = Unset, port: Optional[Int] = Unset)

object Weblink:
  given (using ValueOf[""]) => Weblink is Followable["", "..", "."]:
    def separators: Set[Char] = Set('/')
    def descent(weblink: Weblink): List[PathName[""]] = weblink.descent
    def separator(weblink: Weblink): Text = t"/"
    def ascent(weblink: Weblink): Int = weblink.ascent

  given PathCreator[Weblink, "", Int] with
    def path(ascent: Int, descent: List[PathName[""]]): Weblink = Weblink(ascent, descent)

case class Weblink(ascent: Int, descent: List[PathName[""]])

type HttpUrl = Url["https" | "http"]

case class Url[+SchemeType <: Label]
    (scheme:    Scheme[SchemeType],
     authority: Optional[Authority],
     pathText:  Text,
     query:     Optional[Text]      = Unset,
     fragment:  Optional[Text]      = Unset):

  lazy val path: List[PathName[""]] =
    // FIXME: This needs to be handled better
    import errorHandlers.throwUnsafely
    pathText.drop(1).cut(t"/").to(List).reverse.map(_.urlDecode).map(PathName(_))

  def requestTarget: Text = pathText+query.lay(t"")(t"?"+_)

object UrlError:
  enum Expectation:
    case Colon, More, LowerCaseLetter, PortRange, Number

  object Expectation:
    given Expectation is Communicable =
      case Colon           => msg"a colon"
      case More            => msg"more characters"
      case LowerCaseLetter => msg"a lowercase letter"
      case PortRange       => msg"a port range"
      case Number          => msg"a number"
