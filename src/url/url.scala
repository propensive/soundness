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
import symbolism.*
import perforate.*
import escapade.*
import iridescence.*
import anticipation.*
import contextual.*
import spectacular.*

object Hostname:
  given Show[Hostname] = _.parts.join(t".")
  
  def parse(str: Text): Hostname =
    val parts: List[Text] = str.cut(t".").map(_.punycode)
    Hostname(parts*)

case class Hostname(parts: Text*) extends Shown[Hostname]

object Scheme:
  given Show[Scheme] = _.name
  object Http extends Scheme(t"http")
  object Https extends Scheme(t"https")

case class Scheme(name: Text)

case class Raw(text: Text)

object UrlInput:
  given Substitution[UrlInput, Text, "x"] = UrlInput.Textual(_)
  given Substitution[UrlInput, Raw, "x"] = raw => UrlInput.RawTextual(raw.text)
  given Substitution[UrlInput, Int, "80"] = UrlInput.Integral(_)

enum UrlInput:
  case Integral(value: Int)
  case Textual(value: Text)
  case RawTextual(value: Text)

object UrlInterpolator extends contextual.Interpolator[UrlInput, Text, Url]:
  def complete(value: Text): Url =
    try throwErrors(Url.parse(value)) catch case err: UrlError =>
      throw InterpolationError(msg"the URL ${err.text} is not valid: expected ${err.expected}", err.offset)
  
  def initial: Text = t""
  
  def insert(state: Text, value: UrlInput): Text =
    value match
      case UrlInput.Integral(port) =>
        if !state.ends(t":")
        then throw InterpolationError(msg"a port number must be specified after a colon")
        
        try throwErrors(Url.parse(state+port.show))
        catch case err: UrlError => throw InterpolationError(Message(err.message.text))
        
        state+port.show
      
      case UrlInput.Textual(txt) =>
        if !state.ends(t"/")
        then throw InterpolationError(msg"a substitution may only be made after a slash")
        
        try throwErrors(Url.parse(state+txt.urlEncode))
        catch case err: UrlError => throw InterpolationError(Message(err.message.text))
        
        state+txt.urlEncode
      
      case UrlInput.RawTextual(txt) =>
        if !state.ends(t"/")
        then throw InterpolationError(msg"a substitution may only be made after a slash")

        try throwErrors(Url.parse(state+txt.urlEncode))
        catch case err: UrlError => throw InterpolationError(Message(err.message.text))
        
        state+txt
  
  override def substitute(state: Text, sub: Text): Text = state+sub

  def parse(state: Text, next: Text): Text =
    if !state.empty && !(next.starts(t"/") || next.empty)
    then throw InterpolationError(msg"a substitution must be followed by a slash")
    
    state+next
  
  def skip(state: Text): Text = state+t"1"

object Url:
  given GenericUrl[Url] = _.show
  given (using Raises[UrlError]): SpecificUrl[Url] = Url.parse(_)
  given GenericHttpRequestParam["location", Url] = show(_)
  given (using Raises[UrlError]): Decoder[Url] = parse(_)
  given Encoder[Url] = _.show
  given Debug[Url] = _.show

  given Reachable[Url, "", (Scheme, Maybe[Authority])] with
    def separator(url: Url): Text = t"/"
    def descent(url: Url): List[PathName[""]] = url.path
    def root(url: Url): (Scheme, Maybe[Authority]) = (url.scheme, url.authority)
    
    def prefix(root: (Scheme, Maybe[Authority])): Text =
      t"${root(0).name}:${root(1).mm(t"//"+_.show).or(t"")}"
    
  given PathCreator[Url, "", (Scheme, Maybe[Authority])] with
    def path(ascent: (Scheme, Maybe[Authority]), descent: List[PathName[""]]): Url =
      Url(ascent(0), ascent(1), descent.reverse.map(_.render).join(t"/"))
    
  given show: Show[Url] = url =>
    val auth = url.authority.fm(t"")(t"//"+_.show)
    val rest = t"/${url.query.fm(t"")(t"?"+_)}${url.fragment.fm(t"")(t"#"+_)}"
    t"${url.scheme}:$auth${url.pathText}$rest"
  
  given ansiShow: Display[Url] = url => out"$Underline(${colors.DeepSkyBlue}(${show(url)}))"

  given asMessage: AsMessage[Url] = url => Message(show(url))

  given action: GenericHtmlAttribute["action", Url] with
    def name: Text = t"action"
    def serialize(url: Url): Text = url.show
  
  given codebase: GenericHtmlAttribute["codebase", Url] with
    def name: Text = t"codebase"
    def serialize(url: Url): Text = url.show
  
  given cite: GenericHtmlAttribute["cite", Url] with
    def name: Text = t"cite"
    def serialize(url: Url): Text = url.show
  
  given data: GenericHtmlAttribute["data", Url] with
    def name: Text = t"data"
    def serialize(url: Url): Text = url.show

  given formaction: GenericHtmlAttribute["formaction", Url] with
    def name: Text = t"formaction"
    def serialize(url: Url): Text = url.show
 
  given poster: GenericHtmlAttribute["poster", Url] with
    def name: Text = t"poster"
    def serialize(url: Url): Text = url.show

  given src: GenericHtmlAttribute["src", Url] with
    def name: Text = t"src"
    def serialize(url: Url): Text = url.show
  
  given href: GenericHtmlAttribute["href", Url] with
    def name: Text = t"href"
    def serialize(url: Url): Text = url.show
  
  given manifest: GenericHtmlAttribute["manifest", Url] with
    def name: Text = t"manifest"
    def serialize(url: Url): Text = url.show

  def parse(value: Text)(using Raises[UrlError]): Url =
    import UrlError.Expectation.*

    safely(value.where(_ == ':')) match
      case Unset =>
        raise(UrlError(value, value.length, Colon))(Url(Scheme.Https, Unset, t""))
      
      case colon: Int =>
        val scheme = Scheme(value.take(colon))
        
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

  def parse(value: Text)(using Raises[UrlError]): Authority =
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

case class Url
    (scheme: Scheme, authority: Maybe[Authority], pathText: Text, query: Maybe[Text] = Unset,
        fragment: Maybe[Text] = Unset):
  
  lazy val path: List[PathName[""]] =
    // FIXME: This needs to be handled better
    import errorHandlers.throwUnsafely
    pathText.drop(1).cut(t"/").reverse.map(_.urlDecode).map(PathName(_))

object UrlError:
  enum Expectation:
    case Colon, More, LowerCaseLetter, PortRange, Number

  object Expectation:
    given AsMessage[Expectation] =
      case Colon           => msg"a colon"
      case More            => msg"more characters"
      case LowerCaseLetter => msg"a lowercase letter"
      case PortRange       => msg"a port range"
      case Number          => msg"a number"

case class UrlError(text: Text, offset: Int, expected: UrlError.Expectation)
extends Error(msg"the URL $text is not valid: expected $expected at $offset")

enum LocalPart:
  case Quoted(text: Text)
  case Unquoted(text: Text)

object EmailAddress:
  def parse(text: Text): EmailAddress =
    val buffer: StringBuilder = StringBuilder()
    if text.empty then abort(EmailAddressError())
    
    def quoted(index: Int, escape: Boolean): (LocalPart, Int) =
      if index < text.length then text(index) match
        case '\"' =>
          if escape then buffer.append('\"') else
            if text.length > index && text(index + 1) == '@'
            then (LocalPart.Quoted(buffer.toString.tt), index + 2)
            else abort(EmailAddressError())
        
        case '\\' =>
          if escape then buffer.append('\\')
          quoted(index + 1, !escape)
        
        case char =>
          buffer.append(char)
          quoted(index + 1, false)

      else raise(EmailAddressError())(LocalPart.Quoted(buffer.toString.tt), index)
    
    def unquoted(index: Int, dot: Boolean): (LocalPart, Int) =
      if index < text.length then text(index) match
        case '@' =>
          if dot then raise(EmailAddressError())(())
          (LocalPart.Unquoted(buffer.toString.tt), index + 1)

        case '.'  =>
          if dot then raise(EmailAddressError())(())
          if index == 0 then raise(EmailAddressError())(())
          buffer.append('.')
          unquoted(index + 1, true)

        case char =>
          if 'A' <= char <= 'Z' || 'a' <= char <= 'z' || char.isDigit || t"!#$%&'*+-/=?^_`{|}~".contains(char)
          then buffer.append(char)
          else raise(EmailAddressError())(())
          unquoted(index + 1, false)
      
      else raise(EmailAddressError())(LocalPart.Unuoted(buffer.toString.tt), index)
    
    val (localPart, index) =
      if text.starts(t"\"") then quoted(1, false, false) else unquoted(0, false, false)

    val domain =
      if text.length < index + 1 then abort(EmailAddressError())
      else if text(index) == '[' then
        if text.last != ']' then abort(EmailAddressError())
        val ipAddress = text.slice(index + 1, text.length - 1)
        if ipAddress.starts(t"IPv6:") then Ipv6.parse(ipAddress.drop(5)) else Ipv4.parse(ipAddress)
      else Hostname.parse(text.drop(index))

    EmailAddress(localPart, domain)

case class EmailAddress(displayName: Maybe[Text], localPart: LocalPart, domain: Hostname | Ipv4 | Ipv6):


