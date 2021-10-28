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

case class ParamNotSent(key: Txt)
extends Exception(str"scintillate: the parameter $key was not sent in the request".s)

trait Responder:
  def sendBody(status: Int, body: Body): Unit
  def addHeader(key: Txt, value: Txt): Unit

object Handler:
  given [T: Show]: SimpleHandler[T] =
    SimpleHandler(str"text/plain", v => Body.Chunked(LazyList(summon[Show[T]].show(v).s.bytes)))

  given stringHandler[T](using hr: clairvoyant.HttpResponse[T, Txt]): SimpleHandler[T] =
    SimpleHandler(Txt(hr.mimeType), value => Body.Chunked(LazyList(hr.content(value).bytes)))
  
  given iarrayByteHandler[T](using hr: clairvoyant.HttpResponse[T, LazyList[IArray[Byte]]]): SimpleHandler[T] =
    SimpleHandler(Txt(hr.mimeType), value => Body.Chunked(hr.content(value)))

  given Handler[Redirect] with
    def process(content: Redirect, status: Int, headers: Map[Txt, Txt],
                    responder: Responder): Unit =
      responder.addHeader(ResponseHeader.Location.header, Txt(content.location.toString))
      for (k, v) <- headers do responder.addHeader(k, v)
      responder.sendBody(301, Body.Empty)

  given [T: SimpleHandler]: Handler[NotFound[T]] with
    def process(notFound: NotFound[T], status: Int, headers: Map[Txt, Txt],
                    responder: Responder): Unit =
      val handler = summon[SimpleHandler[T]]
      responder.addHeader(ResponseHeader.ContentType.header, handler.mime)
      for (k, v) <- headers do responder.addHeader(k, v)
      responder.sendBody(404, handler.stream(notFound.content))

  given [T: SimpleHandler]: Handler[ServerError[T]] with
    def process(notFound: ServerError[T], status: Int, headers: Map[Txt, Txt],
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
  def process(content: T, status: Int, headers: Map[Txt, Txt], responder: Responder): Unit

object SimpleHandler:
  def apply[T](mime: Txt, stream: T => Body): SimpleHandler[T] =
    new SimpleHandler(mime, stream) {}

trait SimpleHandler[T](val mime: Txt, val stream: T => Body) extends Handler[T]:
  def process(content: T, status: Int, headers: Map[Txt, Txt], responder: Responder): Unit =
    responder.addHeader(ResponseHeader.ContentType.header, mime)
    for (k, v) <- headers do responder.addHeader(k, v)
    responder.sendBody(status, stream(content))

case class NotFound[T: SimpleHandler](content: T)
case class ServerError[T: SimpleHandler](content: T)

case class Cookie(name: Txt, value: Txt, domain: Maybe[Txt] = Unset,
                      path: Maybe[Txt] = Unset, expiry: Maybe[Long] = Unset,
                      ssl: Boolean = false)

case class Response[T: Handler](content: T, status: HttpStatus = HttpStatus.Ok,
                                    headers: Map[ResponseHeader, Txt] = Map(),
                                    cookies: List[Cookie] = Nil):


  private val df: jt.SimpleDateFormat = jt.SimpleDateFormat("dd MMM yyyy HH:mm:ss")

  def respond(responder: Responder): Unit =
    val cookieHeaders: List[(ResponseHeader, Txt)] = cookies.map { cookie =>
      ResponseHeader.SetCookie -> List[(Txt, Boolean | Option[Txt])](
        cookie.name   -> Some(cookie.value),
        str"Expires"  -> cookie.expiry.option.map { t => str"${df.format(t).nn} GMT" },
        str"Domain"   -> cookie.domain.option,
        str"Path"     -> cookie.path.option,
        str"Secure"   -> cookie.ssl,
        str"HttpOnly" -> false
      ).collect {
        case (k, true)    => k
        case (k, Some(v)) => str"$k=$v"
      }.join(str"; ")
    }
    summon[Handler[T]].process(content, status.code, (headers ++ cookieHeaders).map { (k, v) =>
        k.header -> v }, responder)

object Request:
  given Show[Request] = request => ListMap(
    str"content"  -> request.contentType.show,
    str"method"   -> request.method.show,
    // ansi"query"    -> request.query.ansiShow,
    // ansi"ssl"      -> request.ssl.ansiShow,
    // ansi"hostname" -> request.hostname.ansiShow,
    // ansi"port"     -> request.port.ansiShow,
    // ansi"path"     -> request.path.ansiShow,
    // //ansi"body"     -> (try request.body.stream.slurp(maxSize = 256).uString catch case TooMuchData() => "[...]"),
    // ansi"headers"  -> request.rawHeaders.map { (k, vs) => ansi"$k: ${vs.join(ansi"; ")}" }.join(ansi"\n          "),
    // ansi"params"   -> request.params.map { (k, v) => ansi"$k=\"$v\"" }.join(ansi"\n          ")
  ).map { (k, v) => str"$k = $v" }.join(str", ")

case class Request(method: Method, body: Body.Chunked, query: Txt, ssl: Boolean,
                       hostname: Txt, port: Int, path: Txt,
                       rawHeaders: Map[Txt, List[Txt]],
                       queryParams: Map[Txt, List[Txt]]):

  // FIXME: The exception in here needs to be handled elsewhere
  val params: Map[Txt, Txt] =
    try
      queryParams.map { (k, vs) => Txt(k.urlDecode) -> Txt(vs.headOption.getOrElse(str"").urlDecode) }.to(Map) ++ {
        if (method == Method.Post || method == Method.Put) &&
            (contentType == Some(media"application/x-www-form-urlencoded") || contentType == None)
        then
          Map[Txt, Txt](Txt(body.stream.slurp(maxSize = 10485760).uString).cut(str"&").map(_.cut(str"=", 2).to(Seq) match
            case Seq(key: Txt)             => Txt(key.urlDecode) -> str""
            case Seq(key: Txt, value: Txt) => Txt(key.urlDecode) -> Txt(value.urlDecode)
            case _                         => throw Impossible("key/value pair does not match")
          )*)
        else Map[Txt, Txt]()
      }
    catch case TooMuchData() => Map()

  
  lazy val headers: Map[RequestHeader, List[Txt]] =
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
inline def param(using Request)(key: Txt): Option[Txt] = summon[Request].params.get(key)

inline def header(using Request)(header: RequestHeader): List[Txt] =
  summon[Request].headers.get(header).getOrElse(Nil)

object ParamReader:
  given ParamReader[Int] = str => Int.unapply(str.s)
  given ParamReader[Txt] = Some(_)

trait ParamReader[T]:
  def read(value: Txt): Option[T]

object RequestParam:
  given clairvoyant.HtmlAttribute["name", RequestParam[?]] with
    def name: String = "name"
    def serialize(value: RequestParam[?]): String = value.key.s

case class RequestParam[T](key: Txt)(using ParamReader[T]):
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
    
    val queryParams: Map[Txt, List[Txt]] = query.fold(Map()) { query =>
      val paramStrings = query.nn.cut("&")
      
      paramStrings.foldLeft(Map[Txt, List[Txt]]()) { (map, elem) =>
        val kv = elem.cut("=", 2)
        
        map.updated(kv(0), kv(1) :: map.getOrElse(kv(0), Nil))
      }
    }
    
    val headers = exchange.getRequestHeaders.nn.asScala.view.mapValues(_.asScala.to(List)).to(Map)

    val request = Request(
      method = Method.valueOf(exchange.getRequestMethod.nn.lower.capitalize.nn),
      body = streamBody(exchange),
      query = Txt(query.getOrElse("").nn),
      ssl = false,
      Txt(Option(uri.getHost).getOrElse(exchange.getLocalAddress.nn.getAddress.nn.getCanonicalHostName
          ).nn),
      Option(uri.getPort).filter(_ > 0).getOrElse(exchange.getLocalAddress.nn.getPort),
      Txt(uri.getPath.nn),
      headers.map { (k, v) => Txt(k) -> v.map(Txt(_)) },
      queryParams
    )

    Log.fine(str"Received request $request")

    request

  class SimpleResponder(exchange: HttpExchange) extends Responder:
    def addHeader(key: Txt, value: Txt): Unit = exchange.getResponseHeaders.nn.add(key.s, value.s)
    
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

case class Svg(content: Txt)

object Svg:
  given SimpleHandler[Svg] = SimpleHandler(str"image/svg+xml", svg => Body.Data(svg.content.bytes))

case class Jpeg(content: IArray[Byte])

object Jpeg:
  given SimpleHandler[Jpeg] = SimpleHandler(str"image/jpeg", jpeg => Body.Data(jpeg.content))

case class Gif(content: IArray[Byte])

object Gif:
  given SimpleHandler[Gif] = SimpleHandler(str"image/gif", gif => Body.Data(gif.content))

case class Png(content: IArray[Byte])

object Png:
  given SimpleHandler[Png] = SimpleHandler(str"image/png", png => Body.Data(png.content))

def basicAuth(validate: (Txt, Txt) => Boolean)(response: => Response[?])
             (using Request): Response[?] =
  request.headers.get(RequestHeader.Authorization) match
    case Some(List(s"Basic $credentials")) =>
      val Seq(username: Txt, password: Txt) =
        Txt(credentials).decode[Base64].uString.cut(str":").to(Seq)
      
      if validate(username, password) then response else Response("", HttpStatus.Forbidden)

    case _ =>
      val auth = str"""Basic realm="Some realm", charset="UTF-8""""
      Response("", HttpStatus.Unauthorized, Map(ResponseHeader.WwwAuthenticate -> auth))
