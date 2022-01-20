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

import gossamer.*
import rudiments.*
import gesticulate.*
import wisteria.*
import eucalyptus.*
import iridescence.*
import escapade.*
import slalom.*

import java.net.*
import java.io.*
import java.util as ju

import language.dynamics

private[scintillate] given Realm(t"scintillate")

enum HttpBody:
  case Empty
  case Chunked(stream: LazyList[IArray[Byte] throws StreamCutError])
  case Data(data: IArray[Byte])

  def as[T](using readable: HttpReadable[T])
           : T throws StreamCutError | readable.E | ExcessDataError =
    readable.read(HttpStatus.Ok, this)

object QuerySerializer extends ProductDerivation[QuerySerializer]:
  def join[T](ctx: CaseClass[QuerySerializer, T]): QuerySerializer[T] = value =>
    ctx.params.map:
      param => param.typeclass.params(param.deref(value)).prefix(Text(param.label))
    .reduce(_.append(_))

  given QuerySerializer[Text] = str => Params(List((t"", str)))
  given QuerySerializer[Int] = int => Params(List((t"", int.show)))
  given QuerySerializer[Params] = identity(_)
  given [M <: Map[Text, Text]]: QuerySerializer[M] = map => Params(map.to(List))

trait QuerySerializer[T]:
  def params(value: T): Params

trait FallbackPostable:
  given [T: QuerySerializer]: Postable[T] = Postable(media"application/x-www-form-urlencoded",
      value => LazyList(summon[QuerySerializer[T]].params(value).queryString.bytes))

object Postable extends FallbackPostable:
  given Postable[Text] = Postable(media"text/plain", value => LazyList(IArray.from(value.bytes)))
  given Postable[Unit] = Postable(media"text/plain", unit => LazyList())
  given Postable[Bytes] = Postable(media"application/octet-stream", LazyList(_))
  given Postable[LazyList[Bytes]] = Postable(media"application/octet-stream", _.map(identity(_)))
  given Postable[DataStream] = Postable(media"application/octet-stream", identity(_))
  
  given dataStream[T](using response: clairvoyant.HttpResponse[T]): Postable[T] =
    erased given CanThrow[InvalidMediaTypeError] = compiletime.erasedValue
    Postable(Media.parse(response.mediaType.show), response.content(_).map { v => v })
  
class Postable[T](val contentType: MediaType,
                      val content: T => LazyList[Bytes throws StreamCutError]):
  def preview(value: T): Text = content(value).headOption.fold(t""):
    bytes =>
      try
        val sample = bytes.take(256)
      
        val str: Text =
          if sample.forall { b => b >= 32 && b <= 127 } then sample.uString else sample.hex
        
        if bytes.length > 128 then t"$str..." else str
      catch
        case err: StreamCutError => t"[broken stream]"

object HttpMethod:
  given formmethod: clairvoyant.HtmlAttribute["formmethod", HttpMethod] with
    def name: String = "formmethod"
    def serialize(method: HttpMethod): String = summon[AnsiShow[HttpMethod]].ansiShow(method).plain.s

  given AnsiShow[HttpMethod] = method => ansi"${colors.Crimson}[${Showable(method).show.upper}]"

enum HttpMethod:
  case Get, Head, Post, Put, Delete, Connect, Options, Trace, Patch

object HttpReadable:
  given HttpReadable[Text] with
    type E = StreamCutError
    def read(status: HttpStatus, body: HttpBody): Text throws ExcessDataError | StreamCutError | E =
      body match
        case HttpBody.Empty         => t""
        case HttpBody.Data(body)    => body.uString
        case HttpBody.Chunked(body) => body.slurp(limit = 10.mb).uString
  
  given HttpReadable[Bytes] with
    type E = Nothing
    def read(status: HttpStatus, body: HttpBody): Bytes throws ExcessDataError | StreamCutError | E = body match
      case HttpBody.Empty         => IArray()
      case HttpBody.Data(body)    => body
      case HttpBody.Chunked(body) => body.slurp(limit = 10.mb)

  given [T, E2 <: Exception](using reader: clairvoyant.HttpReader[T, E2]): HttpReadable[T] with
    type E = E2
    def read(status: HttpStatus, body: HttpBody): T throws ExcessDataError | StreamCutError | E2 = body match
      case HttpBody.Empty         => reader.read("")
      case HttpBody.Data(data)    => reader.read(data.uString.s)
      case HttpBody.Chunked(data) => reader.read(data.slurp(limit = 10.mb).uString.s)

  given HttpReadable[HttpStatus] with
    type E = StreamCutError
    def read(status: HttpStatus, body: HttpBody) = status

trait HttpReadable[+T]:
  type E <: Exception
  def read(status: HttpStatus, body: HttpBody): T throws ExcessDataError | StreamCutError | E

case class HttpResponse(status: HttpStatus, headers: Map[ResponseHeader, List[String]], body: HttpBody):
  inline def as[T](using readable: HttpReadable[T]): T throws HttpError | ExcessDataError | StreamCutError | readable.E =
    status match
      case status: FailureCase => throw HttpError(status, body)
      case status              => readable.read(status, body)

object Locatable:
  given Locatable[Uri] = identity(_)
  given Locatable[Text] = Uri(_, Params(Nil))
  //given Locatable[Url] = url => Uri(Url.show.show(url), Params(Nil))

trait Locatable[-T]:
  def location(value: T): Uri

object Http:
  def post[T: Postable, L: Locatable]
          (uri: L, content: T = (), headers: RequestHeader.Value*)
          (using Log)
          : HttpResponse throws StreamCutError =
    request[T](summon[Locatable[L]].location(uri), content, HttpMethod.Post, headers)

  def put[T: Postable, L: Locatable]
         (uri: L, content: T = (), headers: RequestHeader.Value*)
         (using Log)
         : HttpResponse throws StreamCutError =
    request[T](summon[Locatable[L]].location(uri), content, HttpMethod.Put, headers)
  
  def get[L: Locatable]
         (uri: L, headers: Seq[RequestHeader.Value] = Nil)
         (using Log)
         : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Get, headers)

  def options[L: Locatable]
             (uri: L, headers: RequestHeader.Value*)
             (using Log)
             : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Options, headers)

  def head[L: Locatable]
          (uri: L, headers: RequestHeader.Value*)
          (using Log)
          : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Head, headers)
  
  def delete[L: Locatable]
            (uri: L, headers: RequestHeader.Value*)
            (using Log)
            : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Delete, headers)
  
  def connect[L: Locatable]
             (uri: L, headers: RequestHeader.Value*)
             (using Log)
             : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Connect, headers)
  
  def trace[L: Locatable]
           (uri: L, headers: RequestHeader.Value*)
           (using Log)
           : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Trace, headers)
  
  def patch[L: Locatable]
           (uri: L, headers: RequestHeader.Value*)
           (using Log)
           : HttpResponse throws StreamCutError =
    request(summon[Locatable[L]].location(uri), (), HttpMethod.Patch, headers)

  private def request[T: Postable]
                     (url: Uri, content: T, method: HttpMethod,
                          headers: Seq[RequestHeader.Value])(using Log)
                     : HttpResponse throws StreamCutError =
    Log.info(ansi"Sending HTTP $method request to $url")
    headers.foreach:
      header => Log.fine(header)
    Log.fine(ansi"HTTP request body: ${summon[Postable[T]].preview(content)}")
    
    URL(url.show.s).openConnection.nn match
      case conn: HttpURLConnection =>
        conn.setRequestMethod(Showable(method).show.upper.s)
        
        conn.setRequestProperty(RequestHeader.ContentType.header.s, summon[Postable[T]]
            .contentType.show.s)
        
        conn.setRequestProperty("User-Agent", "Scintillate/1.0.0")
        
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
          
          HttpBody.Chunked(if len < 0 then LazyList() else IArray(buf.slice(0, len)*) #::
              read(in).stream)
       

        def body: HttpBody =
          try read(conn.getInputStream.nn) catch Exception =>
            try read(conn.getErrorStream.nn) catch Exception => HttpBody.Empty
        
        val HttpStatus(status) = conn.getResponseCode: @unchecked
        Log.info(ansi"Received response with HTTP status ${status.show}")

        val responseHeaders =
          val scalaMap: Map[String | Null, ju.List[String]] = conn.getHeaderFields.nn.asScala.toMap
          
          scalaMap.flatMap:
            case (null, v)              => Nil
            case (ResponseHeader(k), v) => List((k, v.asScala.to(List)))
            case _                      => throw Impossible("Previous cases are irrefutable")
          .to(Map)

        HttpResponse(status, responseHeaders, body)
      
      case conn: URLConnection =>
        throw Impossible("URL connection is not HTTP")
            
case class HttpError(status: HttpStatus & FailureCase, body: HttpBody) extends Error:
  def message: Text = t"HTTP Error ${status.code}: ${status.description}"
  inline def as[T](using readable: HttpReadable[T]): T throws ExcessDataError | StreamCutError | readable.E =
    readable.read(status, body)

trait FailureCase

object HttpStatus:
  private lazy val all: Map[Int, HttpStatus] = values.unsafeImmutable.map { v => v.code -> v }.to(Map)
  def unapply(code: Int): Option[HttpStatus] = all.get(code)

  given Show[HttpStatus] = status => t"${status.code} (${status.description})"

enum HttpStatus(val code: Int, val description: String):
  case Continue extends HttpStatus(100, "Continue"), FailureCase
  case SwitchingProtocols extends HttpStatus(101, "Switching Protocols"), FailureCase
  case EarlyHints extends HttpStatus(103, "Early Hints"), FailureCase
  case Ok extends HttpStatus(200, "OK")
  case Created extends HttpStatus(201, "Created")
  case Accepted extends HttpStatus(202, "Accepted")
  case NonAuthoritativeInformation extends HttpStatus(203, "Non-Authoritative Information")
  case NoContent extends HttpStatus(204, "No Content")
  case ResetContent extends HttpStatus(205, "Reset Content")
  case PartialContent extends HttpStatus(206, "Partial Content")
  case MultipleChoices extends HttpStatus(300, "Multiple Choices")
  case MovedPermanently extends HttpStatus(301, "Moved Permanently"), FailureCase
  case Found extends HttpStatus(302, "Found"), FailureCase
  case SeeOther extends HttpStatus(303, "See Other"), FailureCase
  case NotModified extends HttpStatus(304, "Not Modified"), FailureCase
  case TemporaryRedirect extends HttpStatus(307, "Temporary Redirect"), FailureCase
  case PermanentRedirect extends HttpStatus(308, "Permanent Redirect"), FailureCase
  case BadRequest extends HttpStatus(400, "Bad Request"), FailureCase
  case Unauthorized extends HttpStatus(401, "Unauthorized"), FailureCase
  case PaymentRequired extends HttpStatus(402, "Payment Required"), FailureCase
  case Forbidden extends HttpStatus(403, "Forbidden"), FailureCase
  case NotFound extends HttpStatus(404, "Not Found"), FailureCase
  case MethodNotAllowed extends HttpStatus(405, "Method Not Allowed"), FailureCase
  case NotAcceptable extends HttpStatus(406, "Not Acceptable"), FailureCase
  
  case ProxyAuthenticationRequired
  extends HttpStatus(407, "Proxy Authentication Required"), FailureCase
  
  case RequestTimeout extends HttpStatus(408, "Request Timeout"), FailureCase
  case Conflict extends HttpStatus(409, "Conflict"), FailureCase
  case Gone extends HttpStatus(410, "Gone"), FailureCase
  case LengthRequired extends HttpStatus(411, "Length Required"), FailureCase
  case PreconditionFailed extends HttpStatus(412, "Precondition Failed"), FailureCase
  case PayloadTooLarge extends HttpStatus(413, "Payload Too Large"), FailureCase
  case UriTooLong extends HttpStatus(414, "URI Too Long"), FailureCase
  case UnsupportedMediaType extends HttpStatus(415, "Unsupported Media Type"), FailureCase
  case RangeNotSatisfiable extends HttpStatus(416, "Range Not Satisfiable"), FailureCase
  case ExpectationFailed extends HttpStatus(417, "Expectation Failed"), FailureCase
  case UnprocessableEntity extends HttpStatus(422, "Unprocessable Entity"), FailureCase
  case TooEarly extends HttpStatus(425, "Too Early"), FailureCase
  case UpgradeRequired extends HttpStatus(426, "Upgrade Required"), FailureCase
  case PreconditionRequired extends HttpStatus(428, "Precondition Required"), FailureCase
  case TooManyRequests extends HttpStatus(429, "Too Many Requests"), FailureCase
  
  case RequestHeaderFieldsTooLarge
  extends HttpStatus(431, "Request Header Fields Too Large"), FailureCase
  
  case UnavailableForLegalReasons
  extends HttpStatus(451, "Unavailable For Legal Reasons"), FailureCase
  
  case InternalServerError extends HttpStatus(500, "Internal Server Error"), FailureCase
  case NotImplemented extends HttpStatus(501, "Not Implemented"), FailureCase
  case BadGateway extends HttpStatus(502, "Bad Gateway"), FailureCase
  case ServiceUnavailable extends HttpStatus(503, "Service Unavailable"), FailureCase
  case GatewayTimeout extends HttpStatus(504, "Gateway Timeout"), FailureCase
  case HttpVersionNotSupported extends HttpStatus(505, "HTTP Version Not Supported"), FailureCase
  case VariantAlsoNegotiates extends HttpStatus(506, "Variant Also Negotiates"), FailureCase
  case InsufficientStorage extends HttpStatus(507, "Insufficient Storage"), FailureCase
  case LoopDetected extends HttpStatus(508, "Loop Detected"), FailureCase
  case NotExtended extends HttpStatus(510, "Not Extended"), FailureCase
  
  case NetworkAuthenticationRequired
  extends HttpStatus(511, "Network Authentication Required"), FailureCase

case class Params(values: List[(Text, Text)]):
  def append(more: Params): Params = Params(values ++ more.values)
  def isEmpty: Boolean = values.isEmpty
  
  def prefix(str: Text): Params = Params:
    values.map:
      (k, v) => (if k.isEmpty then str else t"$str.$k", v)

  def queryString: Text = values.map:
    (k, v) => if k.isEmpty then v.urlEncode else t"${k.urlEncode}=${v.urlEncode}"
  .join(t"&")

object Uri:
  given show: Show[Uri] = uri =>
    if uri.params.isEmpty then uri.location else t"${uri.location}?${uri.params.queryString}"

  given AnsiShow[Uri] = uri => ansi"$Underline(${colors.DeepSkyBlue}(${show.show(uri)}))"
  
  given action: clairvoyant.HtmlAttribute["action", Uri] with
    def name: String = "action"
    def serialize(uri: Uri): String = uri.show.s
  
  given codebase: clairvoyant.HtmlAttribute["codebase", Uri] with
    def name: String = "codebase"
    def serialize(uri: Uri): String = uri.show.s
  
  given cite: clairvoyant.HtmlAttribute["cite", Uri] with
    def name: String = "cite"
    def serialize(uri: Uri): String = uri.show.s
  
  given data: clairvoyant.HtmlAttribute["data", Uri] with
    def name: String = "data"
    def serialize(uri: Uri): String = uri.show.s

  given formaction: clairvoyant.HtmlAttribute["formaction", Uri] with
    def name: String = "formaction"
    def serialize(uri: Uri): String = uri.show.s
 
  given poster: clairvoyant.HtmlAttribute["poster", Uri] with
    def name: String = "poster"
    def serialize(uri: Uri): String = uri.show.s

  given src: clairvoyant.HtmlAttribute["src", Uri] with
    def name: String = "src"
    def serialize(uri: Uri): String = uri.show.s
  
  given href: clairvoyant.HtmlAttribute["href", Uri] with
    def name: String = "href"
    def serialize(uri: Uri): String = uri.show.s
  
  given manifest: clairvoyant.HtmlAttribute["manifest", Uri] with
    def name: String = "manifest"
    def serialize(uri: Uri): String = uri.show.s

  given (using Log): Streamable[Uri] = uri => LazyList:
    try uri.get().as[Bytes]
    catch
      case err: ExcessDataError => throw StreamCutError()
      case err: HttpError => throw StreamCutError()

case class Uri(location: Text, params: Params = Params(Nil)) extends Dynamic, Shown[Uri]:
  private def makeQuery[T: QuerySerializer](value: T): Uri =
    Uri(location, params.append(summon[QuerySerializer[T]].params(value)))

  @targetName("slash")
  def /(part: Text): Uri =
    Uri(if location.endsWith(t"/") then t"$location$part" else t"$location/$part", Params(Nil))

  def applyDynamicNamed(method: "query")(params: (String, Text)*): Uri =
    makeQuery(Params(params.map { (k, v) => Text(k) -> v }.to(List)))
  
  def applyDynamic[T: QuerySerializer](method: "query")(value: T) = makeQuery(value)

  def post[T: Postable](headers: RequestHeader.Value*)(body: T)(using Log)
          : HttpResponse throws StreamCutError =
    Http.post(this, body, headers*)
  
  def post[T: Postable](body: T)(using Log): HttpResponse throws StreamCutError =
    Http.post(this, body)
  
  def put[T: Postable](headers: RequestHeader.Value*)(body: T)(using Log)
         : HttpResponse throws StreamCutError =
    Http.put(this, body, headers*)
  
  def put[T: Postable](body: T)(using Log): HttpResponse throws StreamCutError =
    Http.put(this, body)
  
  def get(headers: RequestHeader.Value*)(using Log): HttpResponse throws StreamCutError =
    Http.get(this, headers)
  
  def options(headers: RequestHeader.Value*)(using Log): HttpResponse throws StreamCutError =
    Http.options(this, headers*)
  
  def trace(headers: RequestHeader.Value*)(using Log): HttpResponse throws StreamCutError =
    Http.trace(this, headers*)
  
  def patch(headers: RequestHeader.Value*)(using Log): HttpResponse throws StreamCutError =
    Http.patch(this, headers*)
  
  def head(headers: RequestHeader.Value*)(using Log): HttpResponse throws StreamCutError =
    Http.head(this, headers*)
  
  def delete(headers: RequestHeader.Value*)(using Log): HttpResponse throws StreamCutError =
    Http.delete(this, headers*)
  
  def connect(headers: RequestHeader.Value*)(using Log): HttpResponse throws StreamCutError =
    Http.connect(this, headers*)
  
  def bare: Uri = Uri(location, Params(Nil))

object DomainName:
  given Show[DomainName] = _.parts.join(t".")
  
  def apply(str: Text): DomainName =
    val parts: List[Text] = str.cut(t".").map(_.punycode)
    DomainName(IArray.from(parts))

case class DomainName(parts: IArray[Text]) extends Shown[DomainName]

/*object Url:
  def apply(ssl: Boolean, domain: DomainName, port: Int, parts: IArray[Text]): Url =
    val baseUrl = BaseUrl(ssl, domain, port)
    baseUrl.Url(parts)
  
  given show: Show[Url] = url => serialize(url.ssl, url.domain, url.port, url.parts)

  def serialize(ssl: Boolean, domain: DomainName, port: Int, parts: IArray[Text]): Text =
    val portText = if port == 80 then t"" else t":$port"
    val schemeText = if ssl then t"https" else t"http"

    t"$schemeText://${domain}$portText/${parts.join(t"/")}"
  
trait Url:
  def ssl: Boolean
  def domain: DomainName
  def port: Int
  def parts: IArray[Text]

case class BaseUrl(ssl: Boolean, domain: DomainName, port: Int)
extends Root(t"/", Url.serialize(ssl, domain, port, IArray())), Url:
  inline def baseUrl: this.type = this

  type AbsolutePath = Url
  def makeAbsolute(parts: IArray[Text]) = Url(parts)
  def url: Url = Url(IArray[Text]())

  case class Url(parts: IArray[Text]) extends Path.Absolute(parts), Dynamic, scintillate.Url:
    def ssl: Boolean = baseUrl.ssl
    def domain: DomainName = baseUrl.domain
    def port: Int = baseUrl.port
    
    def post[T: Postable](content: T, headers: RequestHeader.Value*)(using Log): HttpResponse =
      Http.post(this, content, headers*)
    
    def put[T: Postable](content: T, headers: RequestHeader.Value*)(using Log): HttpResponse =
      Http.put(this, content, headers*)
    
    def get(headers: RequestHeader.Value*)(using Log): HttpResponse = Http.get(this, headers)
    def options(headers: RequestHeader.Value*)(using Log): HttpResponse = Http.options(this, headers*)
    def trace(headers: RequestHeader.Value*)(using Log): HttpResponse = Http.trace(this, headers*)
    def patch(headers: RequestHeader.Value*)(using Log): HttpResponse = Http.patch(this, headers*)
    def head(headers: RequestHeader.Value*)(using Log): HttpResponse = Http.head(this, headers*)
    def delete(headers: RequestHeader.Value*)(using Log): HttpResponse = Http.delete(this, headers*)
    def connect(headers: RequestHeader.Value*)(using Log): HttpResponse = Http.connect(this, headers*)
    //def bare: Uri = Uri(location, Params(Nil))
*/ 
extension (ctx: StringContext)
  def uri(subs: Text*): Uri =
    Uri(ctx.parts.zip(subs).map { (k, v) => t"$k$v" }.join(t"", t"", Text(ctx.parts.last)),
        Params(List()))