/*
    Telekinesis, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import gossamer.{slice as _, *}
import rudiments.*
import hypotenuse.*
import vacuous.*
import fulminate.*
import hieroglyph.*
import contingency.*
import turbulence.*
import gesticulate.*
import wisteria.*
import spectacular.*
import anticipation.*
import nettlesome.*

import java.net.*
import java.io.*
import java.util as ju

import language.dynamics

given Realm = realm"telekinesis"

enum HttpBody:
  case Empty
  case Chunked(stream: LazyList[IArray[Byte]])
  case Data(data: IArray[Byte])

  def as[ResultType: HttpReadable as readable]: ResultType = readable.read(HttpStatus.Ok, this)

object QueryEncoder extends ProductDerivation[QueryEncoder]:
  inline def join[DerivationType <: Product: ProductReflection]: QueryEncoder[DerivationType] =
    fields(_):
      [FieldType] => field => context.params(field).prefix(label)
    .reduce(_.append(_))

  given text: QueryEncoder[Text] = string => Params(List((t"", string)))
  given int: QueryEncoder[Int] = int => Params(List((t"", int.show)))
  given params: QueryEncoder[Params] = identity(_)
  given map[MapType <: Map[Text, Text]]: QueryEncoder[MapType] = map => Params(map.to(List))

trait QueryEncoder[ValueType]:
  def params(value: ValueType): Params

trait FallbackPostable:
  given [QueryType](using serializer: QueryEncoder[QueryType]): Postable[QueryType] =
    Postable(media"application/x-www-form-urlencoded", value =>
        LazyList(serializer.params(value).queryString.bytes(using charEncoders.utf8)))

object Postable extends FallbackPostable:
  given text(using encoder: CharEncoder): Postable[Text] =
    Postable(media"text/plain", value => LazyList(IArray.from(value.bytes)))

  given textStream(using encoder: CharEncoder): Postable[LazyList[Text]] =
    Postable(media"application/octet-stream", _.map(_.bytes))

  given unit: Postable[Unit] = Postable(media"text/plain", unit => LazyList())
  given bytes: Postable[Bytes] = Postable(media"application/octet-stream", LazyList(_))
  given byteStream: Postable[LazyList[Bytes]] = Postable(media"application/octet-stream", _.map(identity(_)))

  given [ResponseType: GenericHttpResponseStream](using mediaType: Tactic[MediaTypeError])
      => Postable[ResponseType] as dataStream =

    // FIXME: Check if mapping `identity` is necessary
    Postable(Media.parse(ResponseType.mediaType.show), ResponseType.content(_).map(identity))

class Postable[PostType](val contentType: MediaType, val content: PostType => LazyList[Bytes]):
  def preview(value: PostType): Text = content(value).prim.lay(t""): bytes =>
    val sample = bytes.take(256)
    val string: Text = if sample.all(32.toByte <= _ <= 127.toByte) then sample.utf8 else sample.hex
    if bytes.length > 128 then t"$string..." else string

object HttpMethod:
  given ("formmethod" is GenericHtmlAttribute[HttpMethod]) as formmethod:
    def name: Text = t"formmethod"
    def serialize(method: HttpMethod): Text = method.show

  given HttpMethod is Communicable as communicable = method => Message(method.show.upper)

enum HttpMethod:
  case Get, Head, Post, Put, Delete, Connect, Options, Trace, Patch

case class HttpRequest
    (method:        HttpMethod,
     host:          Hostname,
     requestTarget: Text,
     headers:       List[RequestHeader.Value],
     body:          HttpBody):

  def serialize: LazyList[Bytes] =
    import charEncoders.ascii
    val buffer = StringBuffer()
    buffer.append(method.show.upper)
    buffer.append(t" ")
    buffer.append(requestTarget)
    buffer.append(t" HTTP/1.0\nhost: ")
    buffer.append(host.show)

    body match
      case HttpBody.Chunked(_) => ()
      case HttpBody.Empty      => buffer.append(t"\ncontent-length: 0")

      case HttpBody.Data(data) =>
        buffer.append(t"\ncontent-length: ")
        buffer.append(data.length.show)

    headers.map: parameter =>
      buffer.append(t"\n")
      buffer.append(parameter.header.header)
      buffer.append(t": ")
      buffer.append(parameter.value)

    buffer.append(t"\n\n")

    body match
      case HttpBody.Chunked(data) => buffer.toString.tt.bytes #:: data
      case HttpBody.Empty         => LazyList(buffer.toString.tt.bytes)
      case HttpBody.Data(data)    => LazyList(buffer.toString.tt.bytes, data)

object HttpReadable:
  given Text is HttpReadable as text = (status, body) => body match
    case HttpBody.Empty         => t""
    case HttpBody.Data(body)    => body.utf8
    case HttpBody.Chunked(body) => body.read[Bytes].utf8

  given Bytes is HttpReadable as bytes = (status, body) => body match
    case HttpBody.Empty         => IArray()
    case HttpBody.Data(body)    => body
    case HttpBody.Chunked(body) => body.read[Bytes]

  given LazyList[Bytes] is HttpReadable as byteStream = (status, body) => body match
    case HttpBody.Empty         => LazyList()
    case HttpBody.Data(body)    => LazyList(body)
    case HttpBody.Chunked(body) => body

  given [ContentType: GenericHttpReader as readable] => ContentType is HttpReadable as genericHttpReader =
    (status, body) => body match
      case HttpBody.Empty         => readable.read(t"")
      case HttpBody.Data(data)    => readable.read(data.utf8)
      case HttpBody.Chunked(data) => readable.read(data.read[Bytes].utf8)

  given HttpStatus is HttpReadable as httpStatus:
    def read(status: HttpStatus, body: HttpBody) = status

trait HttpReadable:
  type Self
  def read(status: HttpStatus, body: HttpBody): Self

case class HttpResponse
    (status: HttpStatus, headers: Map[ResponseHeader[?], List[Text]], body: HttpBody):

  def as[BodyType: HttpReadable as readable]: BodyType raises HttpError = (status: @unchecked) match
    case status: FailureCase => abort(HttpError(status, body))
    case status              => readable.read(status, body)

  def apply[ValueType](header: ResponseHeader[ValueType])(using decoder: HttpHeaderDecoder[ValueType])
          : List[ValueType] =

    headers.at(header).or(Nil).map(decoder.decode(_))

object Locatable:
  given httpUrl: Locatable[HttpUrl] = identity(_)

trait Locatable[-UrlType]:
  def location(value: UrlType): HttpUrl

object Http:
  def post[PostType: Postable, UrlType: Locatable]
      (url: UrlType, content: PostType = (), headers: RequestHeader.Value*)
      (using Online)
          : HttpResponse logs HttpEvent =

    request[PostType](summon[Locatable[UrlType]].location(url), content, HttpMethod.Post, headers)

  def put[PostType: Postable, UrlType: Locatable]
      (url: UrlType, content: PostType = (), headers: RequestHeader.Value*)
      (using Online)
          : HttpResponse logs HttpEvent =

    request[PostType](summon[Locatable[UrlType]].location(url), content, HttpMethod.Put, headers)

  def get[UrlType: Locatable](url: UrlType, headers: Seq[RequestHeader.Value] = Nil)(using Online)
          : HttpResponse logs HttpEvent =

    request(summon[Locatable[UrlType]].location(url), (), HttpMethod.Get, headers)

  def options[UrlType: Locatable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(summon[Locatable[UrlType]].location(url), (), HttpMethod.Options, headers)

  def head[UrlType: Locatable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(summon[Locatable[UrlType]].location(url), (), HttpMethod.Head, headers)

  def delete[UrlType: Locatable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(summon[Locatable[UrlType]].location(url), (), HttpMethod.Delete, headers)

  def connect[UrlType: Locatable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(summon[Locatable[UrlType]].location(url), (), HttpMethod.Connect, headers)

  def trace[UrlType: Locatable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(summon[Locatable[UrlType]].location(url), (), HttpMethod.Trace, headers)

  def patch[UrlType: Locatable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(summon[Locatable[UrlType]].location(url), (), HttpMethod.Patch, headers)

  private def request[PostType: Postable]
      (url: HttpUrl, content: PostType, method: HttpMethod,headers: Seq[RequestHeader.Value])
      (using Online)
          : HttpResponse logs HttpEvent =

    Log.info(HttpEvent.Send(method, url, headers))
    Log.fine(HttpEvent.Request(PostType.preview(content)))

    (URI(url.show.s).toURL.nn.openConnection.nn: @unchecked) match
      case conn: HttpURLConnection =>
        conn.setRequestMethod(method.toString.show.upper.s)
        conn.setRequestProperty(RequestHeader.ContentType.header.s, PostType.contentType.show.s)
        conn.setRequestProperty("User-Agent", "Telekinesis/1.0.0")

        headers.each:
          case RequestHeader.Value(key, value) => conn.setRequestProperty(key.header.s, value.s)

        if method == HttpMethod.Post || method == HttpMethod.Put then
          conn.setDoOutput(true)
          val out = conn.getOutputStream().nn
          PostType.content(content).map(_.to(Array)).each(out.write(_))
          out.close()

        val buf = new Array[Byte](65536)

        def read(in: InputStream): HttpBody.Chunked =
          val len = in.read(buf, 0, buf.length)

          HttpBody.Chunked(if len < 0 then LazyList() else IArray(buf.slice(0, len)*) #:: read(in).stream)


        def body: HttpBody =
          try read(conn.getInputStream.nn) catch case _: Exception =>
            try read(conn.getErrorStream.nn) catch case _: Exception => HttpBody.Empty

        val HttpStatus(status) = conn.getResponseCode: @unchecked
        Log.fine(HttpEvent.Response(status))

        val responseHeaders: Map[ResponseHeader[?], List[Text]] =
          val scalaMap: Map[String | Null, ju.List[String]] = conn.getHeaderFields.nn.asScala.toMap

          scalaMap.flatMap: value =>
            (value: @unchecked) match
              case (null, v)              => Nil
              case (ResponseHeader(k), v) => List((k, v.asScala.to(List).map(_.tt)))
          .to(Map)

        HttpResponse(status, responseHeaders, body)

case class HttpError(status: HttpStatus & FailureCase, body: HttpBody)
extends Error(m"HTTP error $status"):
  def as[BodyType: HttpReadable as readable]: BodyType = readable.read(status, body)

trait FailureCase

object HttpStatus:
  private lazy val all: Map[Int, HttpStatus] = values.immutable(using Unsafe).bi.map(_.code -> _).to(Map)
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

extension (url: HttpUrl)(using Online)
  def post[BodyType: Postable](headers: RequestHeader.Value*)(body: BodyType)
          : HttpResponse logs HttpEvent =
    Http.post(url, body, headers*)

  def put[BodyType: Postable](headers: RequestHeader.Value*)(body: BodyType)
          : HttpResponse logs HttpEvent =
    Http.put(url, body, headers*)

  def post[BodyType: Postable](body: BodyType): HttpResponse logs HttpEvent = Http.post(url, body)
  def put[BodyType: Postable](body: BodyType): HttpResponse logs HttpEvent = Http.put(url, body)
  def get(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.get(url, headers)

  def options(headers: RequestHeader.Value*): HttpResponse logs HttpEvent =
    Http.options(url, headers*)

  def trace(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.trace(url, headers*)
  def patch(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.patch(url, headers*)
  def head(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.head(url, headers*)
  def delete(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.delete(url, headers*)
  def connect(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.connect(url, headers*)

enum HttpEvent:
  case Response(status: HttpStatus)
  case Request(preview: Text)
  case Send(method: HttpMethod, url: HttpUrl, headers: Seq[RequestHeader.Value])

object HttpEvent:
  given HttpEvent is Communicable =
    case Response(status)           => m"Received response with status $status"
    case Request(preview)           => m"Request [$preview]"
    case Send(method, url, headers) => m"Send $method request to $url"
