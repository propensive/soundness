/*
    Nettlesome, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import fulminate.*
import perforate.*
import escapade.*
import iridescence.*
import anticipation.*
import contextual.*
import spectacular.*

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
  def complete(value: Text): Url[Label] =
    try throwErrors(Url.parse(value)) catch
      case err: UrlError      => throw InterpolationError(Message(err.message.text))
      case err: HostnameError => throw InterpolationError(Message(err.message.text))
  
  def initial: Text = t""
  
  def insert(state: Text, value: UrlInput): Text =
    value match
      case UrlInput.Integral(port) =>
        if !state.ends(t":")
        then throw InterpolationError(msg"a port number must be specified after a colon")
        
        try throwErrors(Url.parse(state+port.show))
        catch
          case err: UrlError      => throw InterpolationError(Message(err.message.text))
          case err: HostnameError => throw InterpolationError(Message(err.message.text))
        
        state+port.show
      
      case UrlInput.Textual(txt) =>
        if !state.ends(t"/")
        then throw InterpolationError(msg"a substitution may only be made after a slash")
        
        try throwErrors(Url.parse(state+txt.urlEncode))
        catch
          case err: UrlError      => throw InterpolationError(Message(err.message.text))
          case err: HostnameError => throw InterpolationError(Message(err.message.text))
        
        state+txt.urlEncode
      
      case UrlInput.RawTextual(txt) =>
        if !state.ends(t"/")
        then throw InterpolationError(msg"a substitution may only be made after a slash")

        try throwErrors(Url.parse(state+txt.urlEncode))
        catch
          case err: UrlError      => throw InterpolationError(Message(err.message.text))
          case err: HostnameError => throw InterpolationError(Message(err.message.text))
        
        state+txt
  
  override def substitute(state: Text, sub: Text): Text = state+sub

  def parse(state: Text, next: Text): Text =
    if !state.empty && !(next.starts(t"/") || next.empty)
    then throw InterpolationError(msg"a substitution must be followed by a slash")
    
    state+next
  
  def skip(state: Text): Text = state+t"1"

object Url:
  given GenericUrl[HttpUrl] = _.show
  given (using Raises[UrlError], Raises[HostnameError]): SpecificUrl[HttpUrl] = Url.parse(_)
  given [SchemeType <: Label]: GenericHttpRequestParam["location", Url[SchemeType]] = show(_)
  given [SchemeType <: Label](using Raises[UrlError], Raises[HostnameError]): Decoder[Url[SchemeType]] = parse(_)
  given [SchemeType <: Label]: Encoder[Url[SchemeType]] = _.show

  given [SchemeType <: Label]: Reachable[Url[SchemeType], "", (Scheme[SchemeType], Maybe[Authority])] with
    def separator(url: Url[SchemeType]): Text = t"/"
    def descent(url: Url[SchemeType]): List[PathName[""]] = url.path
    def root(url: Url[SchemeType]): (Scheme[SchemeType], Maybe[Authority]) = (url.scheme, url.authority)
    
    def prefix(root: (Scheme[SchemeType], Maybe[Authority])): Text =
      t"${root(0).name}:${root(1).mm(t"//"+_.show).or(t"")}"
    
  given [SchemeType <: Label]: PathCreator[Url[SchemeType], "", (Scheme[SchemeType], Maybe[Authority])] with
    def path(ascent: (Scheme[SchemeType], Maybe[Authority]), descent: List[PathName[""]]): Url[SchemeType] =
      Url(ascent(0), ascent(1), descent.reverse.map(_.render).join(t"/"))
    
  given show[SchemeType <: Label]: Show[Url[SchemeType]] = url =>
    val auth = url.authority.fm(t"")(t"//"+_.show)
    val rest = t"${url.query.fm(t"")(t"?"+_)}${url.fragment.fm(t"")(t"#"+_)}"
    t"${url.scheme}:$auth${url.pathText}$rest"
  
  given display[SchemeType <: Label]: Display[Url[SchemeType]] = url => e"$Underline(${colors.DeepSkyBlue}(${show(url)}))"

  given communicable[SchemeType <: Label]: Communicable[Url[SchemeType]] = url => Message(show(url))

  given action[SchemeType <: Label]: GenericHtmlAttribute["action", Url[SchemeType]] with
    def name: Text = t"action"
    def serialize(url: Url[SchemeType]): Text = url.show
  
  given codebase[SchemeType <: Label]: GenericHtmlAttribute["codebase", Url[SchemeType]] with
    def name: Text = t"codebase"
    def serialize(url: Url[SchemeType]): Text = url.show
  
  given cite[SchemeType <: Label]: GenericHtmlAttribute["cite", Url[SchemeType]] with
    def name: Text = t"cite"
    def serialize(url: Url[SchemeType]): Text = url.show
  
  given data[SchemeType <: Label]: GenericHtmlAttribute["data", Url[SchemeType]] with
    def name: Text = t"data"
    def serialize(url: Url[SchemeType]): Text = url.show

  given formaction[SchemeType <: Label]: GenericHtmlAttribute["formaction", Url[SchemeType]] with
    def name: Text = t"formaction"
    def serialize(url: Url[SchemeType]): Text = url.show
 
  given poster[SchemeType <: Label]: GenericHtmlAttribute["poster", Url[SchemeType]] with
    def name: Text = t"poster"
    def serialize(url: Url[SchemeType]): Text = url.show

  given src[SchemeType <: Label]: GenericHtmlAttribute["src", Url[SchemeType]] with
    def name: Text = t"src"
    def serialize(url: Url[SchemeType]): Text = url.show
  
  given href[SchemeType <: Label]: GenericHtmlAttribute["href", Url[SchemeType]] with
    def name: Text = t"href"
    def serialize(url: Url[SchemeType]): Text = url.show
  
  given manifest[SchemeType <: Label]: GenericHtmlAttribute["manifest", Url[SchemeType]] with
    def name: Text = t"manifest"
    def serialize(url: Url[SchemeType]): Text = url.show

  def parse[SchemeType <: Label](value: Text)(using Raises[UrlError], Raises[HostnameError]): Url[SchemeType] =
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
            authEnd -> Authority.parse(value.slice(colon + 3, authEnd))
          else
            (colon + 1) -> Unset
        
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
    t"${auth.userInfo.fm(t"")(_+t"@")}${auth.host}${auth.port.mm(_.show).fm(t"")(t":"+_)}"

  def parse(value: Text)(using Raises[UrlError]): Authority raises HostnameError =
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

case class Authority(host: Hostname, userInfo: Maybe[Text] = Unset, port: Maybe[Int] = Unset)

object Weblink:
  given Followable[Weblink, "", "..", "."] with
    def separators: Set[Char] = Set('/')
    def descent(weblink: Weblink): List[PathName[""]] = weblink.descent
    def separator(weblink: Weblink): Text = t"/"
    def ascent(weblink: Weblink): Int = weblink.ascent
    
  given PathCreator[Weblink, "", Int] with
    def path(ascent: Int, descent: List[PathName[""]]): Weblink = Weblink(ascent, descent)

case class Weblink(ascent: Int, descent: List[PathName[""]])

type HttpUrl = Url["https" | "http"]

case class Url
    [+SchemeType <: Label]
    (scheme: Scheme[SchemeType], authority: Maybe[Authority], pathText: Text, query: Maybe[Text] = Unset,
        fragment: Maybe[Text] = Unset):
  
  lazy val path: List[PathName[""]] =
    // FIXME: This needs to be handled better
    import errorHandlers.throwUnsafely
    pathText.drop(1).cut(t"/").reverse.map(_.urlDecode).map(PathName(_))

object UrlError:
  enum Expectation:
    case Colon, More, LowerCaseLetter, PortRange, Number

  object Expectation:
    given Communicable[Expectation] =
      case Colon           => msg"a colon"
      case More            => msg"more characters"
      case LowerCaseLetter => msg"a lowercase letter"
      case PortRange       => msg"a port range"
      case Number          => msg"a number"

