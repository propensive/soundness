/*
    Scintillate, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import wisteria.*

import scala.collection.immutable.ListMap
import scala.collection.JavaConverters.*
import scala.annotation.tailrec

import java.net.*
import java.io.*
import java.util as ju

import language.dynamics

enum Body:
  case Empty
  case Chunked(stream: LazyList[IArray[Byte]])
  case Data(data: IArray[Byte])

object ToQuery extends ProductDerivation[ToQuery]:
  def join[T](ctx: CaseClass[ToQuery, T]): ToQuery[T] = value =>
    ctx.params.map {
      param => param.typeclass.params(param.deref(value)).prefix(param.label)
    }.reduce(_.append(_))

  given ToQuery[String] = str => Params(List(("", str)))
  given ToQuery[Int] = int => Params(List(("", int.toString)))
  given ToQuery[Params] = identity(_)
  given [M <: Map[String, String]]: ToQuery[M] = map => Params(map.to(List))

trait ToQuery[T]:
  def params(value: T): Params

object Postable:
  given [T: ToQuery]: Postable[T](mime"application/x-www-form-urlencoded",
      value => LazyList(summon[ToQuery[T]].params(value).queryString.bytes))

  given Postable[String](mime"text/plain", value => LazyList(IArray.from(value.bytes)))
  given Postable[Unit](mime"text/plain", unit => LazyList())
  given Postable[Bytes](mime"application/octet-stream", LazyList(_))
  given Postable[LazyList[Bytes]](mime"application/octet-stream", identity(_))

trait Postable[T](val contentType: MediaType, val content: T => LazyList[Bytes])

object Method:
  given formmethod: simplistic.HtmlAttribute["formmethod", Method] with
    def name: String = "formmethod"
    def serialize(method: Method): String = method.toString

enum Method:
  case Get, Head, Post, Put, Delete, Connect, Options, Trace, Patch

object HttpReadable:
  given HttpReadable[String] with
    def read(body: Body): String exposes TooMuchData = body match
      case Body.Empty         => ""
      case Body.Data(body)    => body.string
      case Body.Chunked(body) => body.foldLeft("")(_+_.string)
  
  given HttpReadable[Bytes] with
    def read(body: Body): Bytes exposes TooMuchData = body match
      case Body.Empty         => IArray()
      case Body.Data(body)    => body
      case Body.Chunked(body) => body.slurp(maxSize = 10*1024*1024)

trait HttpReadable[+T]:
  def read(body: Body): T exposes TooMuchData

case class HttpResponse(status: HttpStatus, headers: Map[ResponseHeader, List[String]], body: Body):
  def as[T: HttpReadable]: T exposes HttpError | TooMuchData = status match
    case status: FailureCase => throw HttpError(status, body)
    case status              => summon[HttpReadable[T]].read(body)

object ToLocation:
  given ToLocation[Uri] = _.toString

trait ToLocation[T]:
  def location(value: T): String

object Http:
  def post[T: Postable, L: ToLocation](uri: L, content: T = (), headers: RequestHeader.Value*)
          : HttpResponse =
    request[T](summon[ToLocation[L]].location(uri), content, Method.Post, headers)

  def put[T: Postable, L: ToLocation](uri: L, content: T = (), headers: RequestHeader.Value*)
         : HttpResponse =
    request[T](summon[ToLocation[L]].location(uri), content, Method.Put, headers)
  
  def get[L: ToLocation](uri: L, headers: Seq[RequestHeader.Value] = Nil): HttpResponse =
    request(summon[ToLocation[L]].location(uri), (), Method.Get, headers)

  def options[L: ToLocation](uri: L, headers: RequestHeader.Value*): HttpResponse =
    request(summon[ToLocation[L]].location(uri), (), Method.Options, headers)

  def head[L: ToLocation](uri: L, headers: RequestHeader.Value*): HttpResponse =
    request(summon[ToLocation[L]].location(uri), (), Method.Head, headers)
  
  def delete[L: ToLocation](uri: L, headers: RequestHeader.Value*): HttpResponse =
    request(summon[ToLocation[L]].location(uri), (), Method.Delete, headers)
  
  def connect[L: ToLocation](uri: L, headers: RequestHeader.Value*): HttpResponse =
    request(summon[ToLocation[L]].location(uri), (), Method.Connect, headers)
  
  def trace[L: ToLocation](uri: L, headers: RequestHeader.Value*): HttpResponse =
    request(summon[ToLocation[L]].location(uri), (), Method.Trace, headers)
  
  def patch[L: ToLocation](uri: L, headers: RequestHeader.Value*): HttpResponse =
    request(summon[ToLocation[L]].location(uri), (), Method.Patch, headers)

  private def request[T: Postable](url: String, content: T, method: Method,
                                       headers: Seq[RequestHeader.Value]): HttpResponse =
    URL(url).openConnection.nn match
      case conn: HttpURLConnection =>
        conn.setRequestMethod(method.toString.upper)
        
        conn.setRequestProperty(RequestHeader.ContentType.header, summon[Postable[T]]
          .contentType.toString)
        
        conn.setRequestProperty("User-Agent", "Scintillate 1.0.0")
        
        headers.foreach { case RequestHeader.Value(key, value) =>
          conn.setRequestProperty(key.header, value)
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
  def as[T: HttpReadable]: T exposes TooMuchData = summon[HttpReadable[T]].read(body)

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

case class Params(values: List[(String, String)]):
  def append(more: Params): Params = Params(values ++ more.values)
  def isEmpty: Boolean = values.isEmpty
  
  def prefix(str: String): Params = Params(values.map {
    case ("", value)  => (str, value)
    case (key, value) => (s"$str.$key", value)
  })

  def queryString: String = values.map {
    case ("", value)  => value.urlEncode
    case (key, value) => s"${key.urlEncode}=${value.urlEncode}"
  }.join("&")

object Uri:
  given action: simplistic.HtmlAttribute["action", Uri] with
    def name: String = "action"
    def serialize(uri: Uri): String = uri.toString
  
  given cite: simplistic.HtmlAttribute["cite", Uri] with
    def name: String = "cite"
    def serialize(uri: Uri): String = uri.toString
  
  given data: simplistic.HtmlAttribute["data", Uri] with
    def name: String = "data"
    def serialize(uri: Uri): String = uri.toString

  given formaction: simplistic.HtmlAttribute["formaction", Uri] with
    def name: String = "formaction"
    def serialize(uri: Uri): String = uri.toString
 
  given poster: simplistic.HtmlAttribute["poster", Uri] with
    def name: String = "poster"
    def serialize(uri: Uri): String = uri.toString

  given src: simplistic.HtmlAttribute["src", Uri] with
    def name: String = "src"
    def serialize(uri: Uri): String = uri.toString
  
  given href: simplistic.HtmlAttribute["href", Uri] with
    def name: String = "href"
    def serialize(uri: Uri): String = uri.toString
  
  given manifest: simplistic.HtmlAttribute["manifest", Uri] with
    def name: String = "manifest"
    def serialize(uri: Uri): String = uri.toString

case class Uri(location: String, params: Params) extends Dynamic:
  private def makeQuery[T: ToQuery](value: T): Uri =
    Uri(location, params.append(summon[ToQuery[T]].params(value)))
  
  def applyDynamicNamed(method: "query")(params: (String, String)*): Uri =
    makeQuery(Params(params.to(List)))
  
  def applyDynamic[T: ToQuery](method: "query")(value: T) = makeQuery(value)

  def post[T: Postable](content: T, headers: RequestHeader.Value*): HttpResponse =
    Http.post(this, content, headers*)
  
  def get(headers: RequestHeader.Value*): HttpResponse = Http.get(this, headers)
  
  def put[T: Postable](content: T, headers: RequestHeader.Value*): HttpResponse =
    Http.put(this, content, headers*)
  
  def options(headers: RequestHeader.Value*): HttpResponse = Http.options(this, headers*)
  def trace(headers: RequestHeader.Value*): HttpResponse = Http.trace(this, headers*)
  def patch(headers: RequestHeader.Value*): HttpResponse = Http.patch(this, headers*)
  def head(headers: RequestHeader.Value*): HttpResponse = Http.head(this, headers*)
  def delete(headers: RequestHeader.Value*): HttpResponse = Http.delete(this, headers*)
  def connect(headers: RequestHeader.Value*): HttpResponse = Http.connect(this, headers*)

  def bare: Uri = Uri(location, Params(Nil))

  override def toString: String =
    if params.isEmpty then location else location+"?"+params.queryString

object MediaType:
  enum MainType:
    case Application, Audio, Image, Message, Multipart, Text, Video, Font, Example, Model

  def unapply(str: String): Option[MediaType] = str.cut("/", 2).to(Seq) match
    case Seq(key, subtype) => try Some(MediaType(MainType.valueOf(key.capitalize), subtype))
                              catch _ => None
    case _                 => None
  
  given formenctype: simplistic.HtmlAttribute["formenctype", MediaType] with
    def name: String = "formenctype"
    def serialize(mediaType: MediaType): String = mediaType.toString
  
  given media: simplistic.HtmlAttribute["media", MediaType] with
    def name: String = "media"
    def serialize(mediaType: MediaType): String = mediaType.toString
  
  given enctype: simplistic.HtmlAttribute["enctype", MediaType] with
    def name: String = "enctype"
    def serialize(mediaType: MediaType): String = mediaType.toString
  
  given htype: simplistic.HtmlAttribute["htype", MediaType] with
    def name: String = "type"
    def serialize(mediaType: MediaType): String = mediaType.toString

case class MediaType(mediaType: MediaType.MainType, mediaSubtype: String):
  override def toString = s"${mediaType.toString.lower}/$mediaSubtype"

extension (ctx: StringContext)
  def uri(subs: String*): Uri =
    Uri(ctx.parts.zip(subs).map(_+_).join("", "", ctx.parts.last), Params(List()))
  
  // FIXME: Implement with a macro
  def mime(): MediaType = MediaType.unapply(ctx.parts.head).get