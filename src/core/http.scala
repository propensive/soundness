/*
    Scintillate, version 0.16.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

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

import scala.collection.immutable.ListMap
import scala.collection.JavaConverters.*
import scala.annotation.tailrec

import java.net.*
import java.io.*
import java.util as ju

import language.dynamics

private[scintillate] given Realm(t"scintillate")

enum Body:
  case Empty
  case Chunked(stream: LazyList[IArray[Byte]])
  case Data(data: IArray[Byte])

object ToQuery extends ProductDerivation[ToQuery]:
  def join[T](ctx: CaseClass[ToQuery, T]): ToQuery[T] = value =>
    ctx.params.map {
      param => param.typeclass.params(param.deref(value)).prefix(Text(param.label))
    }.reduce(_.append(_))

  given ToQuery[Text] = str => Params(List((t"", str)))
  given ToQuery[Int] = int => Params(List((t"", int.show)))
  given ToQuery[Params] = identity(_)
  given [M <: Map[Text, Text]]: ToQuery[M] = map => Params(map.to(List))

trait ToQuery[T]:
  def params(value: T): Params

object Postable:
  given [T: ToQuery]: Postable[T] = Postable(media"application/x-www-form-urlencoded",
      value => LazyList(summon[ToQuery[T]].params(value).queryString.bytes))

  given Postable[Text] = Postable(media"text/plain", value => LazyList(IArray.from(value.bytes)))
  given Postable[Unit] = Postable(media"text/plain", unit => LazyList())
  given Postable[Bytes] = Postable(media"application/octet-stream", LazyList(_))
  given Postable[LazyList[Bytes]] = Postable(media"application/octet-stream", identity(_))

class Postable[T](val contentType: MediaType, val content: T => LazyList[Bytes]):
  def preview(value: T): Text = content(value).headOption.fold(t"") { bytes =>
    val sample = bytes.take(128)
    
    val str: Text =
      if sample.forall { b => b >= 32 && b <= 127 } then sample.uString else sample.hex
    
    if bytes.length > 128 then t"$str..." else str
  }

object Method:
  given formmethod: clairvoyant.HtmlAttribute["formmethod", Method] with
    def name: String = "formmethod"
    def serialize(method: Method): String = method.toString

  given AnsiShow[Method] = method => ansi"${colors.Crimson}[${method.toString.show.upper}]"

enum Method:
  case Get, Head, Post, Put, Delete, Connect, Options, Trace, Patch

object HttpReadable:
  given HttpReadable[Text] with
    type E = Nothing
    def read(body: Body): Text throws TooMuchData | Nothing =
      body match
        case Body.Empty         => t""
        case Body.Data(body)    => body.uString
        case Body.Chunked(body) => body.slurp(maxSize = 10*1024*1024).uString
  
  given HttpReadable[Bytes] with
    type E <: Exception
    def read(body: Body): Bytes throws TooMuchData | E = body match
      case Body.Empty         => IArray()
      case Body.Data(body)    => body
      case Body.Chunked(body) => body.slurp(maxSize = 10*1024*1024)

  given [T, E2 <: Exception](using reader: clairvoyant.HttpReader[T, E2]): HttpReadable[T] with
    type E = E2
    def read(body: Body): T throws TooMuchData | E2 = body match
      case Body.Empty         => reader.read("")
      case Body.Data(data)    => reader.read(data.uString)
      case Body.Chunked(data) => reader.read(data.slurp(maxSize = 10*1024*1024).uString)

trait HttpReadable[+T]:
  type E <: Exception
  def read(body: Body): T throws TooMuchData | E

case class HttpResponse(status: HttpStatus, headers: Map[ResponseHeader, List[String]], body: Body):
  def as[T](using readable: HttpReadable[T]): T throws HttpError | TooMuchData | readable.E =
    status match
      case status: FailureCase => throw HttpError(status, body)
      case status              => readable.read(body)

object Locatable:
  given Locatable[Uri] = identity(_)
  given Locatable[Text] = Uri(_, Params(Nil))

trait Locatable[T]:
  def location(value: T): Uri

object Http:
  def post[T: Postable, L: Locatable]
          (uri: L, content: T = (), headers: RequestHeader.Value*)
          (using Log)
          : HttpResponse =
    request[T](summon[Locatable[L]].location(uri), content, Method.Post, headers)

  def put[T: Postable, L: Locatable]
         (uri: L, content: T = (), headers: RequestHeader.Value*)
         (using Log)
         : HttpResponse =
    request[T](summon[Locatable[L]].location(uri), content, Method.Put, headers)
  
  def get[L: Locatable]
         (uri: L, headers: Seq[RequestHeader.Value] = Nil)
         (using Log)
         : HttpResponse =
    request(summon[Locatable[L]].location(uri), (), Method.Get, headers)

  def options[L: Locatable]
             (uri: L, headers: RequestHeader.Value*)
             (using Log)
             : HttpResponse =
    request(summon[Locatable[L]].location(uri), (), Method.Options, headers)

  def head[L: Locatable]
          (uri: L, headers: RequestHeader.Value*)
          (using Log)
          : HttpResponse =
    request(summon[Locatable[L]].location(uri), (), Method.Head, headers)
  
  def delete[L: Locatable]
            (uri: L, headers: RequestHeader.Value*)
            (using Log)
            : HttpResponse =
    request(summon[Locatable[L]].location(uri), (), Method.Delete, headers)
  
  def connect[L: Locatable]
             (uri: L, headers: RequestHeader.Value*)
             (using Log)
             : HttpResponse =
    request(summon[Locatable[L]].location(uri), (), Method.Connect, headers)
  
  def trace[L: Locatable]
           (uri: L, headers: RequestHeader.Value*)
           (using Log)
           : HttpResponse =
    request(summon[Locatable[L]].location(uri), (), Method.Trace, headers)
  
  def patch[L: Locatable]
           (uri: L, headers: RequestHeader.Value*)
           (using Log)
           : HttpResponse =
    request(summon[Locatable[L]].location(uri), (), Method.Patch, headers)

  private def request[T: Postable](url: Uri, content: T, method: Method,
                                       headers: Seq[RequestHeader.Value])(using Log): HttpResponse =
    Log.info(ansi"Sending HTTP $method request to $url")
    Log.fine(ansi"HTTP request body: ${summon[Postable[T]].preview(content)}")
    URL(url.toString).openConnection.nn match
      case conn: HttpURLConnection =>
        conn.setRequestMethod(method.toString.upper)
        
        conn.setRequestProperty(RequestHeader.ContentType.header.s, summon[Postable[T]]
          .contentType.toString)
        
        conn.setRequestProperty("User-Agent", "Scintillate 1.0.0")
        
        headers.foreach { case RequestHeader.Value(key, value) =>
          conn.setRequestProperty(key.header.s, value.s)
        }
        
        if method == Method.Post || method == Method.Put then
          conn.setDoOutput(true)
          val out = conn.getOutputStream().nn

          summon[Postable[T]].content(content).map(_.to(Array)).foreach(out.write(_))
          out.close()

        val buf = new Array[Byte](65536)

        def read(in: InputStream): Body.Chunked =
          val len = in.read(buf, 0, buf.length)
          
          Body.Chunked(if len < 0 then LazyList() else IArray.from(buf.slice(0, len)) #::
              read(in).stream)
       

        def body: Body =
          try read(conn.getInputStream.nn) catch Exception =>
            try read(conn.getErrorStream.nn) catch Exception => Body.Empty
        
        val HttpStatus(status) = conn.getResponseCode: @unchecked
        
        val responseHeaders =
          val scalaMap: Map[String | Null, ju.List[String]] = conn.getHeaderFields.nn.asScala.toMap
          scalaMap.flatMap {
            case (null, v)              => Nil
            case (ResponseHeader(k), v) => List((k, v.asScala.to(List)))
          }.to(Map)

        HttpResponse(status, responseHeaders, body)
      
      case conn: URLConnection =>
        throw Impossible("URL connection is not HTTP")
            
case class HttpError(status: HttpStatus & FailureCase, body: Body)
    extends Exception(s"HTTP Error ${status.code}: ${status.description}"):
  def as[T](using readable: HttpReadable[T]): T throws TooMuchData | readable.E =
    readable.read(body)

trait FailureCase

object HttpStatus:
  private lazy val all: Map[Int, HttpStatus] = values.map { v => v.code -> v }.to(Map)
  def unapply(code: Int): Option[HttpStatus] = all.get(code)

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
  
  def prefix(str: Text): Params = Params(values.map { (k, v) =>
    (if k.isEmpty then str else t"$str.$k", v)
  })

  def queryString: Text = values.map { (k, v) =>
    if k.isEmpty then Text(v.urlEncode) else t"${k.urlEncode}=${v.urlEncode}"
  }.join(t"&")

object Uri:
  given Show[Uri] = _.text
  given AnsiShow[Uri] = uri => ansi"$Underline(${colors.DeepSkyBlue}(${uri.text}))"
  
  given action: clairvoyant.HtmlAttribute["action", Uri] with
    def name: String = "action"
    def serialize(uri: Uri): String = uri.toString
  
  given cite: clairvoyant.HtmlAttribute["cite", Uri] with
    def name: String = "cite"
    def serialize(uri: Uri): String = uri.toString
  
  given data: clairvoyant.HtmlAttribute["data", Uri] with
    def name: String = "data"
    def serialize(uri: Uri): String = uri.toString

  given formaction: clairvoyant.HtmlAttribute["formaction", Uri] with
    def name: String = "formaction"
    def serialize(uri: Uri): String = uri.toString
 
  given poster: clairvoyant.HtmlAttribute["poster", Uri] with
    def name: String = "poster"
    def serialize(uri: Uri): String = uri.toString

  given src: clairvoyant.HtmlAttribute["src", Uri] with
    def name: String = "src"
    def serialize(uri: Uri): String = uri.toString
  
  given href: clairvoyant.HtmlAttribute["href", Uri] with
    def name: String = "href"
    def serialize(uri: Uri): String = uri.toString
  
  given manifest: clairvoyant.HtmlAttribute["manifest", Uri] with
    def name: String = "manifest"
    def serialize(uri: Uri): String = uri.toString

object DomainName:
  given Show[DomainName] = dn => Text(dn.toString)
  def apply(str: Text): DomainName =
    val parts: List[Text] = str.cut(t".").map(_.punycode)
    DomainName(IArray.from(parts))

case class DomainName(parts: IArray[Text]):
  override def toString: String = parts.join(t".").s

object Url:
  given Show[Url] = _.text

case class Url(ssl: Boolean, domain: DomainName, port: Int, path: Text) extends Dynamic:
  def text: Text =
    t"http${if ssl then "s" else ""}://$domain${if port == 80 then t"" else t":$port"}/$path"
  
  override def toString: String = text.s

case class Uri(location: Text, params: Params) extends Dynamic:
  private def makeQuery[T: ToQuery](value: T): Uri =
    Uri(location, params.append(summon[ToQuery[T]].params(value)))
  
  def applyDynamicNamed(method: "query")(params: (String, String)*): Uri =
    makeQuery(Params(params.map { (k, v) => Text(k) -> Text(v) }.to(List)))
  
  def applyDynamic[T: ToQuery](method: "query")(value: T) = makeQuery(value)

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

  def bare: Uri = Uri(location, Params(Nil))

  def text: Text =
    if params.isEmpty then location else t"$location?${params.queryString}"

  override def toString: String = text.s

extension (ctx: StringContext)
  def uri(subs: Text*): Uri =
    Uri(ctx.parts.zip(subs).map { (k, v) => t"$k$v" }.join(t"", t"", Text(ctx.parts.last)),
        Params(List()))