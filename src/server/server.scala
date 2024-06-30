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
import digression.*
import vacuous.*
import parasite.*
import turbulence.*
import contingency.*
import gossamer.{at as _, slice as _, *}
import nettlesome.*
import monotonous.*, alphabets.base64.standard
import gesticulate.*
import hieroglyph.*
import telekinesis.*
import anticipation.*
import serpentine.*
import spectacular.*, booleanStyles.trueFalse

import java.net.InetSocketAddress
import java.text as jt
import com.sun.net.httpserver as csnh

case class MissingParamError(key: Text) extends Error(msg"the parameter $key was not sent in the request")

trait Responder:
  def sendBody(status: Int, body: HttpBody): Unit
  def addHeader(key: Text, value: Text): Unit

case class Content(media: MediaType, stream: LazyList[Bytes])

object Servable:

  trait Simple[ResponseType](val mediaType: MediaType, val stream: ResponseType => HttpBody)
  extends Servable:
    type Self = ResponseType
    final def process(content: ResponseType, status: Int, headers: Map[Text, Text], responder: Responder): Unit =
      responder.addHeader(ResponseHeader.ContentType.header, mediaType.show)
      headers.each(responder.addHeader)
      responder.sendBody(status, stream(content))

  def apply[ResponseType](mediaType: MediaType)(lambda: ResponseType => HttpBody)
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
      responder.sendBody(200, HttpBody.Chunked(content.stream))


  given [ResponseType: GenericHttpResponseStream](using Errant[MediaTypeError])
          => ResponseType is Servable as bytes =
    Servable(Media.parse(ResponseType.mediaType.show)): value =>
      HttpBody.Chunked(ResponseType.content(value).map(identity))

  given Redirect is Servable:
    def process(content: Redirect, status: Int, headers: Map[Text, Text], responder: Responder): Unit =
      responder.addHeader(ResponseHeader.Location.header, content.location.show)
      headers.each(responder.addHeader)
      responder.sendBody(301, HttpBody.Empty)

  given [ResponseType: Servable.Simple] => NotFound[ResponseType] is Servable:
    def process(notFound: NotFound[ResponseType], status: Int, headers: Map[Text, Text], responder: Responder)
            : Unit =

      responder.addHeader(ResponseHeader.ContentType.header, ResponseType.mediaType.show)
      headers.each(responder.addHeader)
      responder.sendBody(404, ResponseType.stream(notFound.content))

  given [ResponseType: Servable.Simple] => ServerError[ResponseType] is Servable:
    def process
        (notFound: ServerError[ResponseType], status: Int, headers: Map[Text, Text], responder: Responder)
            : Unit =
      responder.addHeader(ResponseHeader.ContentType.header, ResponseType.mediaType.show)
      headers.each(responder.addHeader)
      responder.sendBody(500, ResponseType.stream(notFound.content))

  given Bytes is Servable = Servable(media"application/octet-stream")(HttpBody.Data(_))

  given (using CharEncoder) => Text is Servable = Servable(media"text/plain"): text =>
    HttpBody.Data(text.bytes)

object Redirect:
  def apply[LocationType: Locatable](location: LocationType): Redirect =
    new Redirect(LocationType.location(location))

case class Redirect(location: Url["http" | "https"])

trait Servable:
  type Self
  def process(content: Self, status: Int, headers: Map[Text, Text], responder: Responder): Unit

case class NotFound[ContentType: Servable](content: ContentType)
case class ServerError[ContentType: Servable](content: ContentType)

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

case class Response[ContentType: Servable]
    (content: ContentType,
     status: HttpStatus = HttpStatus.Ok,
     headers: Map[ResponseHeader[?], Text] = Map(),
     cookies: List[Cookie] = Nil):

  def respond(responder: Responder): Unit =
    val cookieHeaders: List[(ResponseHeader[?], Text)] = cookies.map(ResponseHeader.SetCookie -> _.serialize)

    ContentType.process(content, status.code, (headers ++ cookieHeaders).map { case (k, v) => k.header -> v }, responder)

object Request:
  given Request is Showable = request =>
    val bodySample: Text =
      try request.body.stream.readAs[Bytes].utf8 catch
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
      t"ssl"      -> request.ssl.show,
      t"hostname" -> request.hostname.show,
      t"port"     -> request.port.show,
      t"path"     -> request.pathText,
      t"body"     -> bodySample,
      t"headers"  -> headers,
      t"params"   -> params
    ).map { case (key, value) => t"$key = $value" }.join(t", ")

case class Request
    (method: HttpMethod,
     body: HttpBody.Chunked,
     query: Text,
     ssl: Boolean,
     hostname: Text,
     port: Int,
     pathText: Text,
     rawHeaders: Map[Text, List[Text]],
     queryParams: Map[Text, List[Text]]):

  lazy val path: SimplePath raises PathError = pathText.decodeAs[SimplePath]

  // FIXME: The exception in here needs to be handled elsewhere
  val params: Map[Text, Text] =
    try
      queryParams.map:
        case (k, vs) => k.urlDecode -> vs.prim.or(t"").urlDecode
      .to(Map) ++ {
        if (method == HttpMethod.Post || method == HttpMethod.Put) &&
            (contentType == Some(media"application/x-www-form-urlencoded") || contentType.absent)
        then
          Map[Text, Text](body.stream.readAs[Bytes].utf8.cut(t"&").map(_.cut(t"=", 2).to(Seq) match
            case Seq(key: Text)              => key.urlDecode.show -> t""
            case Seq(key: Text, value: Text) => key.urlDecode.show -> value.urlDecode.show
            case _                         => throw Panic(msg"key/value pair does not match")
          )*)
        else Map[Text, Text]()
      }
    catch case e: StreamError  => Map()


  lazy val headers: Map[RequestHeader[?], List[Text]] = rawHeaders.map:
    case (RequestHeader(header), values) => header -> values

  lazy val length: Int raises StreamError =
    try throwErrors:
      headers.get(RequestHeader.ContentLength).map(_.head).map(_.decodeAs[Int]).getOrElse:
        body.stream.map(_.length).sum
    catch case err: NumberError => abort(StreamError(0.b))

  lazy val contentType: Optional[MediaType] =
    headers.at(RequestHeader.ContentType).let(_.prim).let(MediaType.unapply(_).optional)

trait RequestServable:
  def listen(handle: (request: Request) ?=> Response[?])(using Monitor, Codicil): HttpService logs HttpServerEvent

extension (value: Http.type)
  def listen(handle: (request: Request) ?=> Response[?])(using RequestServable, Monitor, Codicil): HttpService logs HttpServerEvent =
    summon[RequestServable].listen(handle)

inline def request(using inline request: Request): Request = request

inline def param(using Request)(key: Text): Text raises MissingParamError =
  request.params.get(key).getOrElse:
    abort(MissingParamError(key))

def header(using Request)(header: RequestHeader[?]): Optional[List[Text]] =
  summon[Request].headers.get(header).getOrElse(Unset)

object ParamReader:
  given [ParamType](using ext: Unapply[Text, ParamType]): ParamReader[ParamType] = ext.unapply(_)
  given ParamReader[Text] = Some(_)

object UrlPath:
  def unapply(request: Request): Some[Text] = Some(request.pathText)

trait ParamReader[ParamType]:
  def read(value: Text): Option[ParamType]

object RequestParam:
  given ("name" is GenericHtmlAttribute[RequestParam[?]]) as name:
    def name: Text = t"name"
    def serialize(value: RequestParam[?]): Text = value.key

case class RequestParam[ParamType](key: Text)(using ParamReader[ParamType]):
  def opt(using Request): Option[ParamType] =
    summon[Request].params.get(key).flatMap(summon[ParamReader[ParamType]].read(_))

  def unapply(req: Request): Option[ParamType] = opt(using req)
  def apply()(using Request): ParamType raises MissingParamError = opt.getOrElse(abort(MissingParamError(key)))

case class HttpService(port: Int, async: Task[Unit], cancel: () => Unit)

case class HttpServer(port: Int) extends RequestServable:
  def listen(handler: (request: Request) ?=> Response[?])(using Monitor, Codicil): HttpService logs HttpServerEvent =
    def handle(exchange: csnh.HttpExchange | Null) =
      try handler(using makeRequest(exchange.nn)).respond(SimpleResponder(exchange.nn))
      catch case NonFatal(exception) => exception.printStackTrace()

    def startServer(): com.sun.net.httpserver.HttpServer =
      val httpServer = csnh.HttpServer.create(InetSocketAddress("localhost", port), 0).nn
      val context = httpServer.createContext("/").nn
      context.setHandler(handle(_))
      httpServer.setExecutor(null)
      httpServer.start()
      httpServer

    val cancel: Promise[Unit] = Promise[Unit]()

    val asyncTask = async:
      val server = startServer()
      try throwErrors(cancel.await()) catch case err: ConcurrencyError => ()
      server.stop(1)

    HttpService(port, asyncTask, () => safely(cancel.fulfill(())))

  private def streamBody(exchange: csnh.HttpExchange): HttpBody.Chunked =
    val in = exchange.getRequestBody.nn
    val buffer = new Array[Byte](65536)

    def recur(): LazyList[Bytes] =
      val len = in.read(buffer)
      if len > 0 then buffer.slice(0, len).snapshot #:: recur() else LazyList.empty

    HttpBody.Chunked(recur())

  private def makeRequest(exchange: csnh.HttpExchange): Request logs HttpServerEvent =
    val uri = exchange.getRequestURI.nn
    val query = Option(uri.getQuery)

    val queryParams: Map[Text, List[Text]] = query.fold(Map()): query =>
      query.nn.show.cut(t"&").foldLeft(Map[Text, List[Text]]()): (map, elem) =>
        val kv = elem.cut(t"=", 2)
        map.updated(kv(0), kv(1) :: map.getOrElse(kv(0), Nil))

    val headers =
      exchange.getRequestHeaders.nn.asScala.view.mapValues(_.nn.asScala.to(List)).to(Map)

    val request =
      Request
       (method      = HttpMethod.valueOf(exchange.getRequestMethod.nn.show.lower.capitalize.s),
        body        = streamBody(exchange),
        query       = Text(query.getOrElse("").nn),
        ssl         = false,
        hostname    = Option(uri.getHost).getOrElse(exchange.getLocalAddress.nn.getAddress.nn.getCanonicalHostName).nn.tt,
        port        = Option(uri.getPort).filter(_ > 0).getOrElse(exchange.getLocalAddress.nn.getPort),
        pathText    = Text(uri.getPath.nn),
        rawHeaders  = headers.map { case (k, v) => Text(k) -> v.map(Text(_)) },
        queryParams = queryParams)

    Log.fine(HttpServerEvent.Received(request))

    request

  class SimpleResponder(exchange: csnh.HttpExchange) extends Responder:
    def addHeader(key: Text, value: Text): Unit = exchange.getResponseHeaders.nn.add(key.s, value.s)

    def sendBody(status: Int, body: HttpBody): Unit =
      val length = body match
        case HttpBody.Empty      => -1
        case HttpBody.Data(body) => body.length
        case HttpBody.Chunked(_) => 0

      exchange.sendResponseHeaders(status, length)

      body match
        case HttpBody.Empty =>
          ()

        case HttpBody.Data(body) =>
          exchange.getResponseBody.nn.write(body.mutable(using Unsafe))

        case HttpBody.Chunked(body) =>
          try body.map(_.mutable(using Unsafe)).each(exchange.getResponseBody.nn.write(_))
          catch case e: StreamError => () // FIXME: Should this be ignored?

      exchange.getResponseBody.nn.flush()
      exchange.close()

case class Ttf(content: Bytes)
object Ttf:
  given Ttf is Servable = Servable(media"application/octet-stream"): ttf =>
    HttpBody.Data(ttf.content)

def basicAuth(validate: (Text, Text) => Boolean, realm: Text)(response: => Response[?])
             (using Request): Response[?] =
  request.headers.get(RequestHeader.Authorization) match
    case Some(List(s"Basic $credentials")) =>
      safely(credentials.tt.deserialize[Base64].utf8.cut(t":").to(List)) match
        case List(username: Text, password: Text) if validate(username, password) =>
          response

        case _ =>
          Response(Bytes(), HttpStatus.Forbidden)

    case _ =>
      val auth = t"""Basic realm="$realm", charset="UTF-8""""
      Response(Bytes(), HttpStatus.Unauthorized, Map(ResponseHeader.WwwAuthenticate -> auth))

given realm: Realm = realm"scintillate"

enum HttpServerEvent:
  case Received(request: Request)
  case Processed(request: Request, duration: Long)

object HttpServerEvent:
  given HttpServerEvent is Communicable =
    case Received(request)            => msg"Received request ${request.show}"
    case Processed(request, duration) => msg"Processed request ${request.show} in ${duration}ms"
