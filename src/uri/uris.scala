/*
    Scintillate, version 0.4.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package scintillate

import slalom.*
import gossamer.*
import rudiments.*
import anticipation.*

object Host:
  given Show[Host] = _.parts.join(t".")
  
  def parse(str: Text): Host =
    val parts: List[Text] = str.cut(t".").map(_.punycode)
    Host(parts*)

case class Host(parts: Text*) extends Shown[Host]

object Scheme:
  given Show[Scheme] = _.name
  object Http extends Scheme(t"http")
  object Https extends Scheme(t"https")

case class Scheme(name: Text)

object Url:
  given UriConverter[Url] with
    def apply(url: Url): String = url.show.s
    def unapply(value: String): Option[Url] = safely(Url.parse(value.show)).option

  given show: Show[Url] = url =>
    val auth = url.authority.fold(t"")(t"//"+_.show)
    val rest = t"${url.query.fold(t"")(t"?"+_)}${url.fragment.fold(t"")(t"#"+_)}"
    t"${url.scheme}:$auth${url.path}$rest"

  def parse(value: Text): Url throws InvalidUrlError =
    import InvalidUrlError.Expectation.*

    safely(value.where(_ == ':')) match
      case Unset =>
        throw InvalidUrlError(value, value.length, Colon)
      
      case colon: Int =>
        val scheme = Scheme(value.take(colon))
        
        val (pathStart, auth) =
          if value.slice(colon + 1, colon + 3) == t"//" then
            val authEnd = safely(value.where(_ == '/', colon + 3)).otherwise(value.length)
            authEnd -> Some(Authority.parse(value.slice(colon + 3, authEnd)))
          else
            (colon + 1) -> None
        
        safely(value.where(_ == '?', pathStart)) match
          case Unset => safely(value.where(_ == '#', pathStart)) match
            case Unset =>
              Url(scheme, auth, value.drop(pathStart), None, None)
            
            case hash: Int =>
              Url(scheme, auth, value.slice(pathStart, hash), None, Some(value.drop(hash + 1)))

          case qmark: Int =>
            safely(value.where(_ == '#', qmark + 1)) match
              case Unset =>
                Url(scheme, auth, value.slice(pathStart, qmark), Some(value.drop(qmark + 1)), None)
              
              case hash: Int =>
                Url(scheme, auth, value.slice(pathStart, qmark), Some(value.slice(qmark + 1, hash)),
                    Some(value.drop(hash + 1)))

object Authority:
  given Show[Authority] = auth =>
    t"${auth.userInfo.fold(t"")(_+t"@")}${auth.host}${auth.port.map(_.show).fold(t"")(t":"+_)}"

  def parse(value: Text): Authority throws InvalidUrlError =
    import InvalidUrlError.Expectation.*
    
    safely(value.where(_ == '@')) match
      case Unset => safely(value.where(_ == ':')) match
        case Unset =>
          Authority(Host.parse(value))
        
        case colon: Int =>
          val port = safely(value.drop(colon + 1).s.toInt) match
            case port: Int if port >= 0 && port <= 65535 =>
              port
            
            case port: Int =>
              throw InvalidUrlError(value, colon + 1, PortRange)
            
            case Unset =>
              throw InvalidUrlError(value, colon + 1, Number)
          
          Authority(Host.parse(value.take(colon)), None, Some(port))
      
      case arobase: Int =>
        safely(value.where(_ == ':', arobase + 1)) match
          case Unset =>
            Authority(Host.parse(value.drop(arobase + 1)), Some(value.take(arobase)))

          case colon: Int =>
            val port = safely(value.drop(colon + 1).s.toInt) match
              case port: Int if port >= 0 && port <= 65535 =>
                port
              
              case port: Int =>
                throw InvalidUrlError(value, colon + 1, PortRange)
              
              case Unset =>
                throw InvalidUrlError(value, colon + 1, Number)
            
            Authority(Host.parse(value.slice(arobase + 1, colon)), Some(value.take(arobase)),
                Some(port))

case class Authority(host: Host, userInfo: Option[Text] = None, port: Option[Int] = None)

case class Url(scheme: Scheme, authority: Option[Authority], path: Text, query: Option[Text],
                   fragment: Option[Text])

object InvalidUrlError:
  enum Expectation:
    case Colon, More, LowerCaseLetter, PortRange, Number

case class InvalidUrlError(text: Text, offset: Int, expected: InvalidUrlError.Expectation)
extends Error((t"the URL ", text, t" is not valid: expected ", expected, t" at ", offset))
