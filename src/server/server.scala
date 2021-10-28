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

import rudiments.*
import gossamer.*
import gastronomy.*
import eucalyptus.*
import gesticulate.*
import escapade.*

import scala.collection.JavaConverters.*
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal

import annotation.targetName

import java.net.InetSocketAddress
import java.io.*
import java.text as jt
import com.sun.net.httpserver.{HttpServer as JavaHttpServer, *}

case class ParamNotSent(key: Text)
extends Exception(t"scintillate: the parameter $key was not sent in the request".s)

trait Responder:
  def sendBody(status: Int, body: Body): Unit
  def addHeader(key: Text, value: Text): Unit

object Handler:
  given [T: Show]: SimpleHandler[T] =
    SimpleHandler(t"text/plain", v => Body.Chunked(LazyList(summon[Show[T]].show(v).s.bytes)))

  given stringHandler[T](using hr: clairvoyant.HttpResponse[T, Text]): SimpleHandler[T] =
    SimpleHandler(Text(hr.mimeType), value => Body.Chunked(LazyList(hr.content(value).bytes)))
  
  given iarrayByteHandler[T](using hr: clairvoyant.HttpResponse[T, LazyList[IArray[Byte]]]): SimpleHandler[T] =
    SimpleHandler(Text(hr.mimeType), value => Body.Chunked(hr.content(value)))

  given Handler[Redirect] with
    def process(content: Redirect, status: Int, headers: Map[Text, Text],
                    responder: Responder): Unit =
      responder.addHeader(ResponseHeader.Location.header, Text(content.location.toString))
      for (k, v) <- headers do responder.addHeader(k, v)
      responder.sendBody(301, Body.Empty)

  given [T: SimpleHandler]: Handler[NotFound[T]] with
    def process(notFound: NotFound[T], status: Int, headers: Map[Text, Text],
                    responder: Responder): Unit =
      val handler = summon[SimpleHandler[T]]
      responder.addHeader(ResponseHeader.ContentType.header, handler.mime)
      for (k, v) <- headers do responder.addHeader(k, v)
      responder.sendBody(404, handler.stream(notFound.content))

  given [T: SimpleHandler]: Handler[ServerError[T]] with
    def process(notFound: ServerError[T], status: Int, headers: Map[Text, Text],
                    responder: Responder): Unit =
      val handler = summon[SimpleHandler[T]]
      responder.addHeader(ResponseHeader.ContentType.header, handler.mime)
      for (k, v) <- headers do responder.addHeader(k, v)
      responder.sendBody(500, handler.stream(notFound.content))

object Redirect:
  def apply[T: Locatable](location: T): Redirect =
    new Redirect(summon[Locatable[T]].location(location))

case class Redirect(location: Uri)

trait Handler[T]:
  def process(content: T, status: Int, headers: Map[Text, Text], responder: Responder): Unit

object SimpleHandler:
  def apply[T](mime: Text, stream: T => Body): SimpleHandler[T] =
    new SimpleHandler(mime, stream) {}

trait SimpleHandler[T](val mime: Text, val stream: T => Body) extends Handler[T]:
  def process(content: T, status: Int, headers: Map[Text, Text], responder: Responder): Unit =
    responder.addHeader(ResponseHeader.ContentType.header, mime)
    for (k, v) <- headers do responder.addHeader(k, v)
    responder.sendBody(status, stream(content))

case class NotFound[T: SimpleHandler](content: T)
case class ServerError[T: SimpleHandler](content: T)

case class Cookie(name: Text, value: Text, domain: Maybe[Text] = Unset,
                      path: Maybe[Text] = Unset, expiry: Maybe[Long] = Unset,
                      ssl: Boolean = false)

case class Response[T: Handler](content: T, status: HttpStatus = HttpStatus.Ok,
                                    headers: Map[ResponseHeader, Text] = Map(),
                                    cookies: List[Cookie] = Nil):


  private val df: jt.SimpleDateFormat = jt.SimpleDateFormat("dd MMM yyyy HH:mm:ss")

  def respond(responder: Responder): Unit =
    val cookieHeaders: List[(ResponseHeader, Text)] = cookies.map { cookie =>
      ResponseHeader.SetCookie -> List[(Text, Boolean | Option[Text])](
        cookie.name   -> Some(cookie.value),
        t"Expires"  -> cookie.expiry.option.map { t => t"${df.format(t).nn} GMT" },
        t"Domain"   -> cookie.domain.option,
        t"Path"     -> cookie.path.option,
        t"Secure"   -> cookie.ssl,
        t"HttpOnly" -> false
      ).collect {
        case (k, true)    => k
        case (k, Some(v)) => t"$k=$v"
      }.join(t"; ")
    }
    summon[Handler[T]].process(content, status.code, (headers ++ cookieHeaders).map { (k, v) =>
        k.header -> v }, responder)

object Request:
  given Show[Request] = request => ListMap(
    t"content"  -> request.contentType.show,
    t"method"   -> request.method.show,
    // ansi"query"    -> request.query.ansiShow,
    // ansi"ssl"      -> request.ssl.ansiShow,
    // ansi"hostname" -> request.hostname.ansiShow,
    // ansi"port"     -> request.port.ansiShow,
    // ansi"path"     -> request.path.ansiShow,
    // //ansi"body"     -> (try request.body.stream.slurp(maxSize = 256).uString catch case TooMuchData() => "[...]"),
    // ansi"headers"  -> request.rawHeaders.map { (k, vs) => ansi"$k: ${vs.join(ansi"; ")}" }.join(ansi"\n          "),
    // ansi"params"   -> request.params.map { (k, v) => ansi"$k=\"$v\"" }.join(ansi"\n          ")
  ).map { (k, v) => t"$k = $v" }.join(t", ")

case class Request(method: Method, body: Body.Chunked, query: Text, ssl: Boolean,
                       hostname: Text, port: Int, path: Text,
                       rawHeaders: Map[Text, List[Text]],
                       queryParams: Map[Text, List[Text]]):

  // FIXME: The exception in here needs to be handled elsewhere
  val params: Map[Text, Text] =
    try
      queryParams.map { (k, vs) => Text(k.urlDecode) -> Text(vs.headOption.getOrElse(t"").urlDecode) }.to(Map) ++ {
        if (method == Method.Post || method == Method.Put) &&
            (contentType == Some(media"application/x-www-form-urlencoded") || contentType == None)
        then
          Map[Text, Text](Text(body.stream.slurp(maxSize = 10485760).uString).cut(t"&").map(_.cut(t"=", 2).to(Seq) match
            case Seq(key: Text)              => key.urlDecode.show -> t""
            case Seq(key: Text, value: Text) => key.urlDecode.show -> value.urlDecode.show
            case _                         => throw Impossible("key/value pair does not match")
          )*)
        else Map[Text, Text]()
      }
    catch case TooMuchData() => Map()

  
  lazy val headers: Map[RequestHeader, List[Text]] =
    rawHeaders.map {
      case (RequestHeader(header), values) => header -> values
      case _                               => throw Impossible("should never match")
    }

  lazy val length: Int =
    headers.get(RequestHeader.ContentLength).fold(body.stream.map(_.length).sum)(_.head.s.toInt)
  
  lazy val contentType: Option[MediaType] =
    headers.get(RequestHeader.ContentType).flatMap(_.headOption).flatMap(MediaType.unapply(_))
  
trait RequestHandler:
  def listen(handler: Request ?=> Response[?])(using Log): HttpService

extension (value: Http.type)
  def listen(handler: Request ?=> Response[?])(using RequestHandler, Log): HttpService =
    summon[RequestHandler].listen(handler)

inline def request(using Request): Request = summon[Request]
inline def param(using Request)(key: Text): Option[Text] = summon[Request].params.get(key)

inline def header(using Request)(header: RequestHeader): List[Text] =
  summon[Request].headers.get(header).getOrElse(Nil)

object ParamReader:
  given ParamReader[Int] = str => Int.unapply(str.s)
  given ParamReader[Text] = Some(_)

trait ParamReader[T]:
  def read(value: Text): Option[T]

object RequestParam:
  given clairvoyant.HtmlAttribute["name", RequestParam[?]] with
    def name: String = "name"
    def serialize(value: RequestParam[?]): String = value.key.s

case class RequestParam[T](key: Text)(using ParamReader[T]):
  def opt(using Request): Option[T] = param(key).flatMap(summon[ParamReader[T]].read(_))
  def unapply(req: Request): Option[T] = opt(using req)
  def apply()(using Request): T throws ParamNotSent = opt.getOrElse(throw ParamNotSent(key))

trait HttpService:
  def stop(): Unit

@targetName("Ampersand")
val `&` = Split

object Split:
  def unapply(req: Request): (Request, Request) = (req, req)

case class HttpServer(port: Int) extends RequestHandler:

  def listen(handler: Request ?=> Response[?])(using Log): HttpService =
    def handle(exchange: HttpExchange | Null) =
      try handler(using makeRequest(exchange.nn)).respond(SimpleResponder(exchange.nn))
      catch case NonFatal(exception) => exception.printStackTrace()
    
    val httpServer = JavaHttpServer.create(InetSocketAddress("localhost", port), 0).nn

    val context = httpServer.createContext("/").nn
    context.setHandler(handle(_))
    httpServer.setExecutor(null)
    httpServer.start()

    val shutdownThread = new Thread:
      override def run(): Unit = httpServer.stop(1)
    
    Runtime.getRuntime.nn.addShutdownHook(shutdownThread)
    
    () => {
      Runtime.getRuntime.nn.removeShutdownHook(shutdownThread)
      httpServer.stop(1)
    }

  private def streamBody(exchange: HttpExchange): Body.Chunked =
    val in = exchange.getRequestBody.nn
    val buffer = new Array[Byte](65536)
    
    def recur(): LazyList[IArray[Byte]] =
      val len = in.read(buffer)
      if len > 0 then IArray.from(buffer.slice(0, len)) #:: recur() else LazyList.empty
    
    Body.Chunked(recur())

  private def makeRequest(exchange: HttpExchange)(using Log): Request =
    val uri = exchange.getRequestURI.nn
    val query = Option(uri.getQuery)
    
    val queryParams: Map[Text, List[Text]] = query.fold(Map()) { query =>
      val paramStrings = query.nn.cut("&")
      
      paramStrings.foldLeft(Map[Text, List[Text]]()) { (map, elem) =>
        val kv = elem.cut("=", 2)
        
        map.updated(kv(0), kv(1) :: map.getOrElse(kv(0), Nil))
      }
    }
    
    val headers = exchange.getRequestHeaders.nn.asScala.view.mapValues(_.asScala.to(List)).to(Map)

    val request = Request(
      method = Method.valueOf(exchange.getRequestMethod.nn.lower.capitalize.nn),
      body = streamBody(exchange),
      query = Text(query.getOrElse("").nn),
      ssl = false,
      Text(Option(uri.getHost).getOrElse(exchange.getLocalAddress.nn.getAddress.nn.getCanonicalHostName
          ).nn),
      Option(uri.getPort).filter(_ > 0).getOrElse(exchange.getLocalAddress.nn.getPort),
      Text(uri.getPath.nn),
      headers.map { (k, v) => Text(k) -> v.map(Text(_)) },
      queryParams
    )

    Log.fine(t"Received request $request")

    request

  class SimpleResponder(exchange: HttpExchange) extends Responder:
    def addHeader(key: Text, value: Text): Unit = exchange.getResponseHeaders.nn.add(key.s, value.s)
    
    def sendBody(status: Int, body: Body): Unit =
      val length = body match
        case Body.Empty      => -1
        case Body.Data(body) => body.length
        case Body.Chunked(_) => 0

      exchange.sendResponseHeaders(status, length)
      
      body match
        case Body.Empty =>
          ()
        
        case Body.Data(body) =>
          exchange.getResponseBody.nn.write(body.unsafeMutable)
        
        case Body.Chunked(body) =>
          body.map(_.unsafeMutable).foreach(exchange.getResponseBody.nn.write(_))
      
      exchange.getResponseBody.nn.flush()
      exchange.close()

case class Svg(content: Text)

object Svg:
  given SimpleHandler[Svg] = SimpleHandler(t"image/svg+xml", svg => Body.Data(svg.content.bytes))

case class Jpeg(content: IArray[Byte])

object Jpeg:
  given SimpleHandler[Jpeg] = SimpleHandler(t"image/jpeg", jpeg => Body.Data(jpeg.content))

case class Gif(content: IArray[Byte])

object Gif:
  given SimpleHandler[Gif] = SimpleHandler(t"image/gif", gif => Body.Data(gif.content))

case class Png(content: IArray[Byte])

object Png:
  given SimpleHandler[Png] = SimpleHandler(t"image/png", png => Body.Data(png.content))

def basicAuth(validate: (Text, Text) => Boolean)(response: => Response[?])
             (using Request): Response[?] =
  request.headers.get(RequestHeader.Authorization) match
    case Some(List(s"Basic $credentials")) =>
      val Seq(username: Text, password: Text) =
        Text(credentials).decode[Base64].uString.cut(t":").to(Seq)
      
      if validate(username, password) then response else Response("", HttpStatus.Forbidden)

    case _ =>
      val auth = t"""Basic realm="Some realm", charset="UTF-8""""
      Response("", HttpStatus.Unauthorized, Map(ResponseHeader.WwwAuthenticate -> auth))
