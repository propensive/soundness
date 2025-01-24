/*
    Telekinesis, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

object HttpStatus:
  private lazy val all: Map[Int, HttpStatus] =
    values.immutable(using Unsafe).bi.map(_.code -> _).to(Map)

  def unapply(code: Int): Option[HttpStatus] = all.get(code)

  given HttpStatus is Communicable = status => m"${status.code} (${status.description})"

enum HttpStatus(val code: Int, val description: Text):
  case Continue extends HttpStatus(100, t"Continue"), FailureCase
  case SwitchingProtocols extends HttpStatus(101, t"Switching Protocols"), FailureCase
  case EarlyHints extends HttpStatus(103, t"Early Hints"), FailureCase
  case Ok extends HttpStatus(200, t"OK")
  case Created extends HttpStatus(201, t"Created")
  case Accepted extends HttpStatus(202, t"Accepted")
  case NonAuthoritativeInformation extends HttpStatus(203, t"Non-Authoritative Information")
  case NoContent extends HttpStatus(204, t"No Content")
  case ResetContent extends HttpStatus(205, t"Reset Content")
  case PartialContent extends HttpStatus(206, t"Partial Content")
  case MultipleChoices extends HttpStatus(300, t"Multiple Choices")
  case MovedPermanently extends HttpStatus(301, t"Moved Permanently"), FailureCase
  case Found extends HttpStatus(302, t"Found"), FailureCase
  case SeeOther extends HttpStatus(303, t"See Other"), FailureCase
  case NotModified extends HttpStatus(304, t"Not Modified"), FailureCase
  case TemporaryRedirect extends HttpStatus(307, t"Temporary Redirect"), FailureCase
  case PermanentRedirect extends HttpStatus(308, t"Permanent Redirect"), FailureCase
  case BadRequest extends HttpStatus(400, t"Bad Request"), FailureCase
  case Unauthorized extends HttpStatus(401, t"Unauthorized"), FailureCase
  case PaymentRequired extends HttpStatus(402, t"Payment Required"), FailureCase
  case Forbidden extends HttpStatus(403, t"Forbidden"), FailureCase
  case NotFound extends HttpStatus(404, t"Not Found"), FailureCase
  case MethodNotAllowed extends HttpStatus(405, t"Method Not Allowed"), FailureCase
  case NotAcceptable extends HttpStatus(406, t"Not Acceptable"), FailureCase

  case ProxyAuthenticationRequired
  extends HttpStatus(407, t"Proxy Authentication Required"), FailureCase

  case RequestTimeout extends HttpStatus(408, t"Request Timeout"), FailureCase
  case Conflict extends HttpStatus(409, t"Conflict"), FailureCase
  case Gone extends HttpStatus(410, t"Gone"), FailureCase
  case LengthRequired extends HttpStatus(411, t"Length Required"), FailureCase
  case PreconditionFailed extends HttpStatus(412, t"Precondition Failed"), FailureCase
  case PayloadTooLarge extends HttpStatus(413, t"Payload Too Large"), FailureCase
  case UriTooLong extends HttpStatus(414, t"URI Too Long"), FailureCase
  case UnsupportedMediaType extends HttpStatus(415, t"Unsupported Media Type"), FailureCase
  case RangeNotSatisfiable extends HttpStatus(416, t"Range Not Satisfiable"), FailureCase
  case ExpectationFailed extends HttpStatus(417, t"Expectation Failed"), FailureCase
  case UnprocessableEntity extends HttpStatus(422, t"Unprocessable Entity"), FailureCase
  case TooEarly extends HttpStatus(425, t"Too Early"), FailureCase
  case UpgradeRequired extends HttpStatus(426, t"Upgrade Required"), FailureCase
  case PreconditionRequired extends HttpStatus(428, t"Precondition Required"), FailureCase
  case TooManyRequests extends HttpStatus(429, t"Too Many Requests"), FailureCase

  case RequestHeaderFieldsTooLarge
  extends HttpStatus(431, t"Request Header Fields Too Large"), FailureCase

  case UnavailableForLegalReasons
  extends HttpStatus(451, t"Unavailable For Legal Reasons"), FailureCase

  case InternalServerError extends HttpStatus(500, t"Internal Server Error"), FailureCase
  case NotImplemented extends HttpStatus(501, t"Not Implemented"), FailureCase
  case BadGateway extends HttpStatus(502, t"Bad Gateway"), FailureCase
  case ServiceUnavailable extends HttpStatus(503, t"Service Unavailable"), FailureCase
  case GatewayTimeout extends HttpStatus(504, t"Gateway Timeout"), FailureCase
  case HttpVersionNotSupported extends HttpStatus(505, t"HTTP Version Not Supported"), FailureCase
  case VariantAlsoNegotiates extends HttpStatus(506, t"Variant Also Negotiates"), FailureCase
  case InsufficientStorage extends HttpStatus(507, t"Insufficient Storage"), FailureCase
  case LoopDetected extends HttpStatus(508, t"Loop Detected"), FailureCase
  case NotExtended extends HttpStatus(510, t"Not Extended"), FailureCase

  case NetworkAuthenticationRequired
  extends HttpStatus(511, t"Network Authentication Required"), FailureCase
