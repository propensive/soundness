/*
    Telekinesis, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

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

import gossamer.*
import rudiments.*
import deviation.*
import turbulence.*
import gesticulate.*
import wisteria.*
import eucalyptus.*
import iridescence.*
import escapade.*
import serpentine.*
import anticipation.*

import java.net.*
import java.io.*
import java.util as ju

import language.dynamics

private[telekinesis] given Realm(t"telekinesis")

enum HttpBody:
  case Empty
  case Chunked(stream: LazyList[IArray[Byte] throws StreamCutError])
  case Data(data: IArray[Byte])

  def as[T](using readable: HttpReadable[T]): T throws StreamCutError = readable.read(HttpStatus.Ok, this)

object QuerySerializer extends ProductDerivation[QuerySerializer]:
  def join[T](ctx: CaseClass[QuerySerializer, T]): QuerySerializer[T] = value =>
    ctx.params.map: param =>
      param.typeclass.params(param.deref(value)).prefix(Text(param.label))
    .reduce(_.append(_))

  given QuerySerializer[Text] = str => Params(List((t"", str)))
  given QuerySerializer[Int] = int => Params(List((t"", int.show)))
  given QuerySerializer[Params] = identity(_)
  given [M <: Map[Text, Text]]: QuerySerializer[M] = map => Params(map.to(List))

trait QuerySerializer[T]:
  def params(value: T): Params

trait FallbackPostable:
  given [T](using serializer: QuerySerializer[T]): Postable[T] =
    Postable(media"application/x-www-form-urlencoded", v =>
        LazyList(serializer.params(v).queryString.bytes(using characterEncodings.utf8)))

object Postable extends FallbackPostable:
  given (using enc: Encoding): Postable[Text] =
    Postable(media"text/plain", value => LazyList(IArray.from(value.bytes)))
  
  given (using enc: Encoding): Postable[LazyList[Text]] =
    Postable(media"application/octet-stream", _.map(_.bytes))
  
  given Postable[Unit] = Postable(media"text/plain", unit => LazyList())
  given Postable[Bytes] = Postable(media"application/octet-stream", LazyList(_))
  given Postable[LazyList[Bytes]] = Postable(media"application/octet-stream", _.map(identity(_)))
  given Postable[DataStream] = Postable(media"application/octet-stream", identity(_))
  
  given dataStream[T](using response: GenericHttpResponseStream[T]): Postable[T] =
    erased given CanThrow[InvalidMediaTypeError] = compiletime.erasedValue
    Postable(Media.parse(response.mediaType.show), response.content(_).map(identity))
  
class Postable[T](val contentType: MediaType,
                      val content: T => LazyList[Bytes throws StreamCutError]):
  def preview(value: T): Text = content(value).headOption.fold(t""): bytes =>
    try
      val sample = bytes.take(256)
      val str: Text = if sample.forall { b => b >= 32 && b <= 127 } then sample.uString else sample.hex
      if bytes.length > 128 then t"$str..." else str
    catch case err: StreamCutError => t"[broken stream]"

object HttpMethod:
  given formmethod: GenericHtmlAttribute["formmethod", HttpMethod] with
    def name: String = "formmethod"
    def serialize(method: HttpMethod): String = summon[AnsiShow[HttpMethod]].ansiShow(method).plain.s

  given AnsiShow[HttpMethod] = method => ansi"${colors.Crimson}[${Showable(method).show.upper}]"

enum HttpMethod:
  case Get, Head, Post, Put, Delete, Connect, Options, Trace, Patch

object HttpReadable:
  given HttpReadable[Text] with
    def read(status: HttpStatus, body: HttpBody): Text throws StreamCutError = body match
      case HttpBody.Empty         => t""
      case HttpBody.Data(body)    => body.uString
      case HttpBody.Chunked(body) => body.slurp().uString
  
  given HttpReadable[Bytes] with
    def read(status: HttpStatus, body: HttpBody): Bytes throws StreamCutError = body match
      case HttpBody.Empty         => IArray()
      case HttpBody.Data(body)    => body
      case HttpBody.Chunked(body) => body.slurp()

  given [T](using reader: GenericHttpReader[T]): HttpReadable[T] with
    def read(status: HttpStatus, body: HttpBody): T throws StreamCutError = body match
      case HttpBody.Empty         => reader.read("")
      case HttpBody.Data(data)    => reader.read(data.uString.s)
      case HttpBody.Chunked(data) => reader.read(data.slurp().uString.s)

  given HttpReadable[HttpStatus] with
    def read(status: HttpStatus, body: HttpBody) = status

trait HttpReadable[+T]:
  def read(status: HttpStatus, body: HttpBody): T throws StreamCutError

case class HttpResponse(status: HttpStatus, headers: Map[ResponseHeader, List[String]], body: HttpBody):
  def as[T](using readable: HttpReadable[T]): T throws HttpError | StreamCutError = status match
    case status: FailureCase => throw HttpError(status, body)
    case status              => readable.read(status, body)

object Locatable:
  given Locatable[Url] = identity(_)

trait Locatable[-T]:
  def location(value: T): Url

object Http:
  def post[T: Postable, L: Locatable]
          (uri: L, content: T = (), headers: RequestHeader.Value*)
          (using Internet, Log)
          : HttpResponse throws StreamCutError =
    request[T](summon[Locatable[L]].location(uri), content, HttpMethod.Post, headers)

  def put[T: Postable, L: Locatable]
         (uri: L, content: T = (), headers: RequestHeader.Value*)
         (using Internet, Log)
         : HttpResponse throws StreamCutError =
    request[T](summon[Locatable[L]].location(uri), content, HttpMethod.Put, headers)
  
  def get[L: Locatable](uri: L, headers: Seq[RequestHeader.Value] = Nil)(using Internet, Log)
         : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Get, headers)

  def options[L: Locatable](uri: L, headers: RequestHeader.Value*)(using Internet, Log)
             : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Options, headers)

  def head[L: Locatable](uri: L, headers: RequestHeader.Value*)(using Internet, Log)
          : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Head, headers)
  
  def delete[L: Locatable](uri: L, headers: RequestHeader.Value*)(using Internet, Log)
            : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Delete, headers)
  
  def connect[L: Locatable](uri: L, headers: RequestHeader.Value*)(using Internet, Log)
             : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Connect, headers)
  
  def trace[L: Locatable](uri: L, headers: RequestHeader.Value*)(using Internet, Log)
           : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Trace, headers)
  
  def patch[L: Locatable](uri: L, headers: RequestHeader.Value*)(using Internet, Log)
           : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Patch, headers)

  private def request[T: Postable]
                     (url: Url, content: T, method: HttpMethod,headers: Seq[RequestHeader.Value])
                     (using Internet, Log)
                     : HttpResponse throws StreamCutError =
    Log.info(ansi"Sending HTTP $method request to $url")
    headers.foreach(Log.fine(_))
    Log.fine(ansi"HTTP request body: ${summon[Postable[T]].preview(content)}")
    
    URI(url.show.s).toURL.nn.openConnection.nn match
      case conn: HttpURLConnection =>
        conn.setRequestMethod(Showable(method).show.upper.s)
        conn.setRequestProperty(RequestHeader.ContentType.header.s, summon[Postable[T]].contentType.show.s)
        conn.setRequestProperty("User-Agent", "Telekinesis/1.0.0")
        
        headers.foreach:
          case RequestHeader.Value(key, value) => conn.setRequestProperty(key.header.s, value.s)
        
        if method == HttpMethod.Post || method == HttpMethod.Put then
          conn.setDoOutput(true)
          val out = conn.getOutputStream().nn
          summon[Postable[T]].content(content).map(_.to(Array)).foreach(out.write(_))
          out.close()

        val buf = new Array[Byte](65536)

        def read(in: InputStream): HttpBody.Chunked =
          val len = in.read(buf, 0, buf.length)
          
          HttpBody.Chunked(if len < 0 then LazyList() else IArray(buf.slice(0, len)*) #:: read(in).stream)
       

        def body: HttpBody =
          try read(conn.getInputStream.nn) catch case _: Exception =>
            try read(conn.getErrorStream.nn) catch case _: Exception => HttpBody.Empty
        
        val HttpStatus(status) = conn.getResponseCode: @unchecked
        Log.info(ansi"Received response with HTTP status ${status.show}")

        val responseHeaders =
          val scalaMap: Map[String | Null, ju.List[String]] = conn.getHeaderFields.nn.asScala.toMap
          
          scalaMap.flatMap:
            case (null, v)              => Nil
            case (ResponseHeader(k), v) => List((k, v.asScala.to(List)))
            case _                      => throw Mistake("Previous cases are irrefutable")
          .to(Map)

        HttpResponse(status, responseHeaders, body)
      
      case conn: URLConnection =>
        throw Mistake("URL connection is not HTTP")
            
case class HttpError(status: HttpStatus & FailureCase, body: HttpBody)
extends Error(err"HTTP error $status"):
  def as[T](using readable: HttpReadable[T]): T throws StreamCutError = readable.read(status, body)

trait FailureCase

object HttpStatus:
  private lazy val all: Map[Int, HttpStatus] = values.immutable(using Unsafe).mtwin.map(_.code -> _).to(Map)
  def unapply(code: Int): Option[HttpStatus] = all.get(code)

  given Show[HttpStatus] = status => t"${status.code} (${status.description})"

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
  case ProxyAuthenticationRequired extends HttpStatus(407, t"Proxy Authentication Required"), FailureCase
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
  case RequestHeaderFieldsTooLarge extends HttpStatus(431, t"Request Header Fields Too Large"), FailureCase
  case UnavailableForLegalReasons extends HttpStatus(451, t"Unavailable For Legal Reasons"), FailureCase
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
  case NetworkAuthenticationRequired extends HttpStatus(511, t"Network Authentication Required"), FailureCase

case class Params(values: List[(Text, Text)]):
  def append(more: Params): Params = Params(values ++ more.values)
  def isEmpty: Boolean = values.isEmpty
  
  def prefix(str: Text): Params = Params:
    values.map { (k, v) => if k.length == 0 then str -> v else t"$str.$k" -> v }

  def queryString: Text = values.map: (k, v) =>
    if k.length == 0 then v.urlEncode else t"${k.urlEncode}=${v.urlEncode}"
  .join(t"&")


extension (url: Url)(using Internet, Log)
  def post[T: Postable](headers: RequestHeader.Value*)(body: T): HttpResponse throws StreamCutError =
    Http.post(url, body, headers*)
  
  def put[T: Postable](headers: RequestHeader.Value*)(body: T): HttpResponse throws StreamCutError =
    Http.put(url, body, headers*)
  
  def post[T: Postable](body: T): HttpResponse throws StreamCutError = Http.post(url, body)
  def put[T: Postable](body: T): HttpResponse throws StreamCutError = Http.put(url, body)
  def get(headers: RequestHeader.Value*): HttpResponse throws StreamCutError = Http.get(url, headers)
  def options(headers: RequestHeader.Value*): HttpResponse throws StreamCutError = Http.options(url, headers*)
  def trace(headers: RequestHeader.Value*): HttpResponse throws StreamCutError = Http.trace(url, headers*)
  def patch(headers: RequestHeader.Value*): HttpResponse throws StreamCutError = Http.patch(url, headers*)
  def head(headers: RequestHeader.Value*): HttpResponse throws StreamCutError = Http.head(url, headers*)
  def delete(headers: RequestHeader.Value*): HttpResponse throws StreamCutError = Http.delete(url, headers*)
  def connect(headers: RequestHeader.Value*): HttpResponse throws StreamCutError = Http.connect(url, headers*)
