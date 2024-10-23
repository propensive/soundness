/*
    Scintillate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import fulminate.*
import prepositional.*
import digression.*
import vacuous.*
import parasite.*
import turbulence.*
import contingency.*
import gossamer.*
import nettlesome.*
import monotonous.*, alphabets.base64.standard
import gesticulate.*
import telekinesis.*
import anticipation.*
import serpentine.*
import spectacular.*

import scala.compiletime.*

import java.net as jn
import java.text as jt
import com.sun.net.httpserver as csnh

case class MissingParamError(key: Text)(using Diagnostics)
extends Error(m"the parameter $key was not sent in the request")

trait Responder:
  def sendBody(status: Int, body: LazyList[Bytes]): Unit
  def addHeader(key: Text, value: Text): Unit

case class Content(media: MediaType, stream: LazyList[Bytes])

trait Retrievable(val mediaType: MediaType) extends Servable:

  type Self

  def stream(response: Self): LazyList[Bytes]

  final def process
      (content: Self, status: Int, headers: Map[Text, Text], responder: Responder)
          : Unit =

    responder.addHeader(ResponseHeader.ContentType.header, mediaType.show)
    headers.each(responder.addHeader)
    responder.sendBody(status, stream(content))

object Servable:

  def apply[ResponseType](mediaType: MediaType)(lambda: ResponseType => LazyList[Bytes])
          : ResponseType is Servable =
    new Servable:
      type Self = ResponseType

      def process(content: Self, status: Int, headers: Map[Text, Text], responder: Responder): Unit =
        responder.addHeader(ResponseHeader.ContentType.header, mediaType.show)
        headers.each(responder.addHeader)
        responder.sendBody(status, lambda(content))

  given Content is Servable as content:
    def process(content: Content, status: Int, headers: Map[Text, Text], responder: Responder): Unit =
      responder.addHeader(ResponseHeader.ContentType.header, content.media.show)
      headers.each(responder.addHeader)
      responder.sendBody(200, content.stream)

  given [ResponseType: GenericHttpResponseStream] => ResponseType is Servable as bytes =
    Servable(unsafely(Media.parse(ResponseType.mediaType))): value =>
      ResponseType.content(value)//.map(identity)

  given Redirect is Servable as redirect:
    def process(content: Redirect, status: Int, headers: Map[Text, Text], responder: Responder): Unit =
      responder.addHeader(ResponseHeader.Location.header, content.location.show)
      headers.each(responder.addHeader)
      responder.sendBody(301, LazyList())

  given [ResponseType] => NotFound[ResponseType] is Servable as notFound:
    def process(notFound: NotFound[ResponseType], status: Int, headers: Map[Text, Text], responder: Responder)
            : Unit =
      notFound.serve(headers, responder)

  given [ResponseType: Retrievable] => ServeFailure[ResponseType] is Servable as retrievable:
    def process
        (notFound: ServeFailure[ResponseType], status: Int, headers: Map[Text, Text], responder: Responder)
            : Unit =
      responder.addHeader(ResponseHeader.ContentType.header, ResponseType.mediaType.show)
      headers.each(responder.addHeader)
      responder.sendBody(500, ResponseType.stream(notFound.content))

  given Bytes is Servable as data = Servable(media"application/octet-stream")(LazyList(_))

  inline given [ValueType: Media] => ValueType is Servable as media =
    summonFrom:
      case encodable: (ValueType is Encodable in Bytes) =>
        (value, status, headers, responder) =>
          responder.addHeader(ResponseHeader.ContentType.header, ValueType.mediaType(value).show)
          headers.each(responder.addHeader)
          responder.sendBody(200, LazyList(encodable.encode(value)))
      case readable: (ValueType is Readable by Bytes) =>
        (value, status, headers, responder) =>
          responder.addHeader(ResponseHeader.ContentType.header, ValueType.mediaType(value).show)
          headers.each(responder.addHeader)
          responder.sendBody(200, value.stream[Bytes])

object Redirect:
  def apply[HyperlinkType: Hyperlinkable](location: HyperlinkType): Redirect =
    new Redirect(HyperlinkType.hyperlink(location))

case class Redirect(location: Url["http" | "https"])

trait Servable:
  type Self
  def process(content: Self, status: Int, headers: Map[Text, Text], responder: Responder): Unit

case class NotFound[ContentType: Servable](content: ContentType):
  def serve(headers: Map[Text, Text], responder: Responder) =
    ContentType.process(content, 404, headers, responder)

case class ServeFailure[ContentType: Servable](content: ContentType)

object Cookie:
  given ("set-cookie" is GenericHttpRequestParam[Cookie]) as setCookie = _.serialize
  given ("cookie" is GenericHttpRequestParam[Cookie]) as cookie = _.serialize
  val dateFormat: jt.SimpleDateFormat = jt.SimpleDateFormat("dd MMM yyyy HH:mm:ss")

case class Cookie
    (name:   Text,
     value:  Text,
     domain: Optional[Text] = Unset,
     path:   Optional[Text] = Unset,
     expiry: Optional[Long] = Unset,
     ssl:    Boolean        = false):

  def serialize: Text =
    List(
      name        -> Some(value),
      t"Expires"  -> expiry.option.map { t => t"${Cookie.dateFormat.format(t).nn} GMT" },
      t"Domain"   -> domain.option,
      t"Path"     -> path.option,
      t"Secure"   -> ssl,
      t"HttpOnly" -> false
    ).collect:
      case (k, true)    => k
      case (k, Some(v)) => t"$k=$v"
    .join(t"; ")


object HttpResponse:
  def apply[FormatType: Servable](content0: FormatType, status0: HttpStatus = HttpStatus.Found, headers0: Map[ResponseHeader[?], Text] = Map(), cookies0: List[Cookie] = Nil): HttpResponse in FormatType = new HttpResponse:
    type Format = FormatType
    def servable: Format is Servable = summon[FormatType is Servable]
    def content: Format = content0
    def status: HttpStatus = status0
    def headers: Map[ResponseHeader[?], Text] = headers0
    def cookies: List[Cookie] = cookies0

trait HttpResponse:
  type Format
  def servable: Format is Servable
  def content: Format
  def status: HttpStatus
  def headers: Map[ResponseHeader[?], Text]
  def cookies: List[Cookie]

  def respond(responder: Responder): Unit =
    val cookieHeaders: List[(ResponseHeader[?], Text)] = cookies.map(ResponseHeader.SetCookie -> _.serialize)

    servable.process(content, status.code, (headers ++ cookieHeaders).map { case (k, v) => k.header -> v }, responder)

object HttpRequest:
  given HttpRequest is Showable = request =>
    val bodySample: Text =
      try request.body.stream.read[Bytes].utf8 catch
        case err: StreamError  => t"[-/-]"

    val headers: Text =
      request.rawHeaders.map:
        case (key, values) => t"$key: ${values.join(t"; ")}"
      .join(t"\n          ")

    val params: Text = request.params.map:
      case (k, v) => t"$k=\"$v\""
    .join(t"\n          ")

    ListMap[Text, Text](
      t"content"  -> request.contentType.lay(t"application/octet-stream")(_.show),
      t"method"   -> request.method.show,
      t"query"    -> request.query.show,
      t"hostname" -> request.hostname.show,
      t"port"     -> request.port.show,
      t"path"     -> request.pathText,
      t"body"     -> bodySample,
      t"headers"  -> headers,
      t"params"   -> params
    ).map { case (key, value) => t"$key = $value" }.join(t", ")

case class HttpConnection(ssl: Boolean, request: HttpRequest)

case class HttpRequest
    (method: HttpMethod,
     hostname: Hostname,
     body: LazyList[Bytes],
     query: Text,
     port: Int,
     pathText: Text,
     rawHeaders: Map[Text, List[Text]],
     queryParams: Map[Text, List[Text]]):

  def path(using connection: HttpConnection): HttpUrl raises PathError raises UrlError raises HostnameError =
    Url.parse(t"${if connection.ssl then t"https" else t"http"}://$hostname$pathText")

  // FIXME: The exception in here needs to be handled elsewhere
  val params: Map[Text, Text] =
    try
      queryParams.map:
        case (k, vs) => k.urlDecode -> vs.prim.or(t"").urlDecode
      .to(Map) ++ {
        if (method == HttpMethod.Post || method == HttpMethod.Put) &&
            (contentType == Some(media"application/x-www-form-urlencoded") || contentType.absent)
        then
          Map[Text, Text](body.stream.read[Bytes].utf8.cut(t"&").map(_.cut(t"=", 2).to(Seq) match
            case Seq(key: Text)              => key.urlDecode.show -> t""
            case Seq(key: Text, value: Text) => key.urlDecode.show -> value.urlDecode.show
            case _                           => throw Panic(m"key/value pair does not match")
          )*)
        else Map[Text, Text]()
      }
    catch case e: StreamError  => Map()


  lazy val headers: Map[RequestHeader[?], List[Text]] = rawHeaders.map:
    case (RequestHeader(header), values) => header -> values

  lazy val length: Int raises StreamError =
    try throwErrors:
      headers.get(RequestHeader.ContentLength).map(_.head).map(_.decode[Int]).getOrElse:
        body.stream.map(_.length).sum
    catch case err: NumberError => abort(StreamError(0.b))

  lazy val contentType: Optional[MediaType] =
    headers.at(RequestHeader.ContentType).let(_.prim).let(MediaType.unapply(_).optional)

trait RequestServable:
  def listen(handle: (connection: HttpConnection) ?=> HttpResponse)(using Monitor, Codicil): HttpService logs HttpServerEvent

extension (value: Http.type)
  def listen(handle: (connection: HttpConnection) ?=> HttpResponse)(using RequestServable, Monitor, Codicil): HttpService logs HttpServerEvent =
    summon[RequestServable].listen(handle)

inline def request(using inline connection: HttpConnection): HttpRequest = connection.request

inline def param(using HttpConnection)(key: Text): Text raises MissingParamError =
  request.params.get(key).getOrElse:
    abort(MissingParamError(key))

def header(using HttpRequest)(header: RequestHeader[?]): Optional[List[Text]] =
  summon[HttpRequest].headers.get(header).getOrElse(Unset)

object ParamReader:
  given [ParamType](using ext: Unapply[Text, ParamType]): ParamReader[ParamType] = ext.unapply(_)
  given ParamReader[Text] = Some(_)

object UrlPath:
  def unapply(request: HttpRequest): Some[Text] = Some(request.pathText)

trait ParamReader[ParamType]:
  def read(value: Text): Option[ParamType]

object RequestParam:
  given ("name" is GenericHtmlAttribute[RequestParam[?]]) as name:
    def name: Text = t"name"
    def serialize(value: RequestParam[?]): Text = value.key

case class RequestParam[ParamType](key: Text)(using ParamReader[ParamType]):
  def opt(using HttpRequest): Option[ParamType] =
    summon[HttpRequest].params.get(key).flatMap(summon[ParamReader[ParamType]].read(_))

  def unapply(req: HttpRequest): Option[ParamType] = opt(using req)
  def apply()(using HttpRequest): ParamType raises MissingParamError = opt.getOrElse(abort(MissingParamError(key)))

case class HttpService(port: Int, async: Task[Unit], cancel: () => Unit)

case class ServerError(port: Int)(using Diagnostics)
extends Error(m"Could not start an HTTP server on port $port")

case class HttpServer(port: Int)(using Tactic[ServerError]) extends RequestServable:
  def listen(handler: (connection: HttpConnection) ?=> HttpResponse)
      (using Monitor, Codicil)
          : HttpService logs HttpServerEvent =

    def handle(exchange: csnh.HttpExchange | Null) =
      try
        val responder = new Responder:
          def addHeader(key: Text, value: Text): Unit =
            exchange.nn.getResponseHeaders.nn.add(key.s, value.s)

          def sendBody(status: Int, body: LazyList[Bytes]): Unit =
            val length = body match
              case LazyList()     => -1
              case LazyList(data) => data.length
              case _              => 0

            exchange.nn.sendResponseHeaders(status, length)

            try
              body.map(_.mutable(using Unsafe)).each: bytes =>
                exchange.nn.getResponseBody.nn.write(bytes)
            catch case e: StreamError => () // FIXME: Should this be ignored?

            exchange.nn.getResponseBody.nn.flush()
            exchange.nn.close()

        handler(using makeConnection(exchange.nn)).respond(responder)
      catch case NonFatal(exception) => exception.printStackTrace()

    def startServer(): com.sun.net.httpserver.HttpServer raises ServerError =
      try
        val httpServer = csnh.HttpServer.create(jn.InetSocketAddress("localhost", port), 0).nn
        val context = httpServer.createContext("/").nn
        context.setHandler(handle(_))
        httpServer.setExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
        httpServer.start()
        httpServer
      catch
        case error: jn.BindException => abort(ServerError(port))

    val cancel: Promise[Unit] = Promise[Unit]()
    val server = startServer()

    val asyncTask = async(cancel.await() yet server.stop(1))

    HttpService(port, asyncTask, () => safely(cancel.fulfill(())))

  private def streamBody(exchange: csnh.HttpExchange): LazyList[Bytes] =
    val in = exchange.getRequestBody.nn
    val buffer = new Array[Byte](65536)

    def recur(): LazyList[Bytes] =
      val len = in.read(buffer)
      if len > 0 then buffer.slice(0, len).snapshot #:: recur() else LazyList.empty

    recur()

  private def makeConnection(exchange: csnh.HttpExchange): HttpConnection logs HttpServerEvent =
    HttpConnection(false, makeRequest(exchange))

  private def makeRequest(exchange: csnh.HttpExchange): HttpRequest logs HttpServerEvent =
    val uri = exchange.getRequestURI.nn
    val query = Option(uri.getQuery)

    val queryParams: Map[Text, List[Text]] = query.fold(Map()): query =>
      query.nn.show.cut(t"&").foldLeft(Map[Text, List[Text]]()): (map, elem) =>
        val kv = elem.cut(t"=", 2)
        map.updated(kv(0), kv(1) :: map.getOrElse(kv(0), Nil))

    val headers =
      exchange.getRequestHeaders.nn.asScala.view.mapValues(_.nn.asScala.to(List)).to(Map)

    val request =
      HttpRequest
       (method      = HttpMethod.valueOf(exchange.getRequestMethod.nn.show.lower.capitalize.s),
        body        = streamBody(exchange),
        query       = Text(query.getOrElse("").nn),
        hostname    = unsafely(Hostname.parse(Option(uri.getHost).getOrElse(exchange.getLocalAddress.nn.getAddress.nn.getCanonicalHostName).nn.tt)),
        port        = Option(uri.getPort).filter(_ > 0).getOrElse(exchange.getLocalAddress.nn.getPort),
        pathText    = Text(uri.getPath.nn),
        rawHeaders  = headers.map { case (k, v) => Text(k) -> v.map(Text(_)) },
        queryParams = queryParams)

    Log.fine(HttpServerEvent.Received(request))

    request

case class Ttf(content: Bytes)
object Ttf:
  given Ttf is Servable = Servable(media"application/octet-stream"): ttf =>
    LazyList(ttf.content)

def basicAuth(validate: (Text, Text) => Boolean, realm: Text)(response: => HttpResponse)
    (using HttpConnection)
        : HttpResponse =
  request.headers.get(RequestHeader.Authorization) match
    case Some(List(s"Basic $credentials")) =>
      safely(credentials.tt.deserialize[Base64].utf8.cut(t":").to(List)) match
        case List(username: Text, password: Text) if validate(username, password) =>
          response

        case _ =>
          HttpResponse(Bytes(), HttpStatus.Forbidden)

    case _ =>
      val auth = t"""Basic realm="$realm", charset="UTF-8""""
      HttpResponse(Bytes(), HttpStatus.Unauthorized, Map(ResponseHeader.WwwAuthenticate -> auth))

given realm: Realm = realm"scintillate"

enum HttpServerEvent:
  case Received(request: HttpRequest)
  case Processed(request: HttpRequest, duration: Long)

object HttpServerEvent:
  given HttpServerEvent is Communicable =
    case Received(request)            => m"Received request ${request.show}"
    case Processed(request, duration) => m"Processed request ${request.show} in ${duration}ms"

erased trait Http

object Http:
  given (using Monitor, Codicil, HttpServerEvent is Loggable, Tactic[ServerError])
      => Http is Protocolic:
    type Carrier = TcpPort
    type Request = HttpConnection
    type Response = HttpResponse
    type Server = HttpService

    def server(port: TcpPort)(handler: HttpConnection ?=> HttpResponse): HttpService =
      HttpServer(port.number).listen(handler)
