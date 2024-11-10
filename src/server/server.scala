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

import errorDiagnostics.stackTraces

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
      responder.addHeader(ResponseHeader.Location.header, content.location)
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
    new Redirect(HyperlinkType.hyperlink(location).show)

case class Redirect(location: Text)

trait Servable:
  type Self
  def process(content: Self, status: Int, headers: Map[Text, Text], responder: Responder): Unit

object Acceptable:
  given (using Tactic[MultipartError]) => Multipart is Acceptable = request =>
    tend:
      case _: MediaTypeError => MultipartError(MultipartError.Reason.MediaType)
    .within:
      given HttpRequest = request
      val contentType = header(RequestHeader.ContentType).let(_.prim).let(Media.parse(_)).or:
        abort(MultipartError(MultipartError.Reason.MediaType))

      if contentType.base == media"multipart/form-data" then
        val boundary = contentType.at(t"boundary").or(abort(MultipartError(MultipartError.Reason.MediaType)))
        println(t"boundary = '$boundary'")
        Multipart.parse(request.body, boundary)
      else abort(MultipartError(MultipartError.Reason.MediaType))

trait Acceptable:
  type Self
  def accept(request: HttpRequest): Self

case class NotFound[ContentType: Servable](content: ContentType):
  def serve(headers: Map[Text, Text], responder: Responder) =
    ContentType.process(content, 404, headers, responder)

case class ServeFailure[ContentType: Servable](content: ContentType)

object Cookie:
  given ("set-cookie" is GenericHttpRequestParam[CookieValue]) as setCookie = _.serialize
  given ("cookie" is GenericHttpRequestParam[CookieValue]) as cookie = _.serialize
  val dateFormat: jt.SimpleDateFormat = jt.SimpleDateFormat("dd MMM yyyy HH:mm:ss")

  def apply[ValueType: {Encoder, Decoder}](using DummyImplicit)[DurationType: GenericDuration]
    (name:     Text,
     domain:   Optional[Hostname]     = Unset,
     expiry:   Optional[DurationType] = Unset,
     secure:   Boolean                = false,
     httpOnly: Boolean                = false) =
  new Cookie[ValueType, DurationType](name, domain, expiry, secure, httpOnly)

case class Cookie[ValueType: {Encoder, Decoder}, DurationType: GenericDuration]
    (name:     Text,
     domain:   Optional[Hostname],
     expiry:   Optional[DurationType],
     secure:   Boolean,
     httpOnly: Boolean):

  def apply(value: ValueType): CookieValue =
    CookieValue(name, value.encode, domain.let(_.show), Unset, expiry.let(_.milliseconds/1000), secure, httpOnly)

  def apply()(using request: HttpRequest): Optional[ValueType] = request.cookies.at(name).let(_.decode)

case class CookieValue
    (name:     Text,
     value:    Text,
     domain:   Optional[Text] = Unset,
     path:     Optional[Text] = Unset,
     expiry:   Optional[Long] = Unset,
     secure:   Boolean        = false,
     httpOnly: Boolean        = false):

  def serialize: Text =
    List(
      name        -> Some(value),
      t"MaxAge"   -> expiry.option.map(_.show),
      t"Domain"   -> domain.option,
      t"Path"     -> path.option,
      t"Secure"   -> secure,
      t"HttpOnly" -> httpOnly).collect:
      case (k, true)    => k
      case (k, Some(v)) => t"$k=$v"
    .join(t"; ")

object HttpResponse:
  def apply[FormatType: Servable]
      (content: FormatType,
       status: HttpStatus = HttpStatus.Found,
       headers: Map[ResponseHeader[?], Text] = Map(),
       cookies: List[CookieValue] = Nil)
          : HttpResponse in FormatType =
    inline def content0: FormatType = content
    inline def status0: HttpStatus = status
    inline def headers0: Map[ResponseHeader[?], Text] = headers
    inline def cookies0: List[CookieValue] = cookies

    new HttpResponse:
      type Format = FormatType
      def servable: Format is Servable = summon[FormatType is Servable]
      def content: Format = content0
      def status: HttpStatus = status0
      def headers: Map[ResponseHeader[?], Text] = headers0
      def cookies: List[CookieValue] = cookies0

trait HttpResponse:
  type Format
  def servable: Format is Servable
  def content: Format
  def status: HttpStatus
  def headers: Map[ResponseHeader[?], Text]
  def cookies: List[CookieValue]

  def allHeaders: List[(ResponseHeader[?], Text)] =
    headers.to(List) ++ cookies.map(ResponseHeader.SetCookie -> _.serialize)

  def respond(responder: Responder): Unit =
    servable.process(content, status.code, allHeaders.map { case (k, v) => k.header -> v }.to(Map), responder)

  def serialize: Text = Text.construct:
    for (key, value) <- allHeaders do append(t"${key.header}: $value\r\n")
    append(t"\r\n")

case class HttpConnection(secure: Boolean, port: Int, request: HttpRequest)

extension (request: HttpRequest)
  def as[BodyType: Acceptable]: BodyType = BodyType.accept(request)

  def path(using connection: HttpConnection): HttpUrl raises PathError raises UrlError raises HostnameError =
    val scheme = if connection.secure then t"https" else t"http"
    Url.parse(t"$scheme://${request.host}${request.pathText}")

trait RequestServable:
  def listen(handle: (connection: HttpConnection) ?=> HttpResponse)(using Monitor, Codicil): HttpService logs HttpServerEvent

extension (value: Http.type)
  def listen(handle: (connection: HttpConnection) ?=> HttpResponse)(using RequestServable, Monitor, Codicil): HttpService logs HttpServerEvent =
    summon[RequestServable].listen(handle)

inline def param(using request: HttpRequest)(key: Text): Optional[Text] =
  request.params.get(key).getOrElse(Unset)

def request(using request: HttpRequest): HttpRequest = request

def cookie(using request: HttpRequest)(key: Text): Optional[Text] = request.cookies.at(key)

def header(using request: HttpRequest)(header: RequestHeader[?]): Optional[List[Text]] =
  request.header(header).map(_.value)

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
        //httpServer.setExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
        httpServer.setExecutor(null)
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
    val port = Option(exchange.getRequestURI.nn.getPort).filter(_ > 0).getOrElse:
      exchange.getLocalAddress.nn.getPort

    HttpConnection(false, port, makeRequest(exchange))

  private def makeRequest(exchange: csnh.HttpExchange): HttpRequest logs HttpServerEvent =
    val uri = exchange.getRequestURI.nn
    val query = Optional(uri.getQuery)
    val target = uri.getPath.nn.tt+query.let(t"?"+_.tt).or(t"")
    val method = HttpMethod.valueOf(exchange.getRequestMethod.nn.show.lower.capitalize.s)

    val headers: List[RequestHeader.Value] =
      exchange.getRequestHeaders.nn.asScala.view.mapValues(_.nn.asScala.to(List)).flatMap:
        case (RequestHeader(header), values) => values.map: value =>
          header(value.tt)
      .to(List)

    val version: HttpVersion = HttpVersion.parse(exchange.getProtocol.nn.tt)

    val host = unsafely:
       Hostname.parse:
         Optional(uri.getHost).or(exchange.getLocalAddress.nn.getAddress.nn.getCanonicalHostName.nn).tt

    val request =
      HttpRequest
       (method  = method,
        version = version,
        host    = host,
        target  = target,
        body    = streamBody(exchange),
        headers = headers)

    Log.fine(HttpServerEvent.Received(request))

    request

def basicAuth(validate: (Text, Text) => Boolean, realm: Text)(response: => HttpResponse)
    (using connection: HttpConnection)
        : HttpResponse =
  connection.request.header(RequestHeader.Authorization).let(_.map(_.value)).or(Nil) match
    case List(s"Basic $credentials") =>
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
