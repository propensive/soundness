/*

    Scintillate, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
    in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
    express or implied. See the License for the specific language governing permissions and
    limitations under the License.

*/
package scintillate

import rudiments.*
import gastronomy.*

import scala.collection.JavaConverters.*
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal

import java.net.InetSocketAddress
import java.io.*
import com.sun.net.httpserver.{HttpServer as JavaHttpServer, *}

case class ParamNotSent(key: String)
extends Exception(str"scintillate: the parameter $key was not sent in the request")

trait Responder:
  def sendBody(status: Int, body: Body): Unit
  def addHeader(key: String, value: String): Unit

object Handler:
  given SimpleHandler[String] = SimpleHandler("text/plain", str => LazyList(str.bytes))
  
  given Handler[Redirect] with
    def process(content: Redirect, status: Int, headers: Map[String, String],
                    responder: Responder): Unit =
      responder.addHeader(ResponseHeader.Location.header, content.location)
      for (k, v) <- headers do responder.addHeader(k, v)
      responder.sendBody(301, ())

  given [T: SimpleHandler]: Handler[NotFound[T]] with
    def process(notFound: NotFound[T], status: Int, headers: Map[String, String],
                    responder: Responder): Unit =
      val handler = summon[SimpleHandler[T]]
      responder.addHeader(ResponseHeader.ContentType.header, handler.mime)
      for (k, v) <- headers do responder.addHeader(k, v)
      responder.sendBody(404, handler.stream(notFound.content))

  given [T: SimpleHandler]: Handler[ServerError[T]] with
    def process(notFound: ServerError[T], status: Int, headers: Map[String, String],
                    responder: Responder): Unit =
      val handler = summon[SimpleHandler[T]]
      responder.addHeader(ResponseHeader.ContentType.header, handler.mime)
      for (k, v) <- headers do responder.addHeader(k, v)
      responder.sendBody(500, handler.stream(notFound.content))

object Redirect:
  def apply[T: ToLocation](location: T): Redirect =
    Redirect(summon[ToLocation[T]].location(location))

case class Redirect(location: String)

trait Handler[T]:
  def process(content: T, status: Int, headers: Map[String, String], responder: Responder): Unit

class SimpleHandler[T](val mime: String, val stream: T => Body) extends Handler[T]:
  def process(content: T, status: Int, headers: Map[String, String], responder: Responder): Unit =
    responder.addHeader(ResponseHeader.ContentType.header, mime)
    for (k, v) <- headers do responder.addHeader(k, v)
    responder.sendBody(status, stream(content))

case class NotFound[T: SimpleHandler](content: T)
case class ServerError[T: SimpleHandler](content: T)

case class Cookie(domain: String, name: String, value: String, path: String, expiry: Option[Long],
                      ssl: Boolean)

case class Response[T: Handler](content: T, status: HttpStatus = HttpStatus.Ok,
                                    headers: Map[ResponseHeader, String] = Map(),
                                    cookies: List[Cookie] = Nil):

  def respond(responder: Responder): Unit =
    summon[Handler[T]].process(content, status.code, headers.map { (k, v) =>
        k.header -> v }, responder)

case class Request(method: Method, body: Chunked, query: String, ssl: Boolean, hostname: String,
                       port: Int, path: String, rawHeaders: Map[String, List[String]],
                       queryParams: Map[String, List[String]]):

  val params: Map[String, String] =
    queryParams.map { (k, vs) => k.urlDecode -> vs.headOption.getOrElse("").urlDecode }.to(Map) ++ {
      if (method == Method.Post || method == Method.Put) &&
          contentType == Some(mime"application/x-www-form-urlencoded")
      then
        Map(String(body.slurp(maxSize = 10*1024*1024).unsafeArray,
            "UTF-8").cut("&").map(_.cut("=", 2) match
          case IArray(key)        => key.urlDecode -> ""
          case IArray(key, value) => key.urlDecode -> value.urlDecode
        )*)
      else Map()
    }

  override def toString(): String = ListMap(
    "content"  -> contentType.toString,
    "method"   -> method.toString,
    "query"    -> query,
    "ssl"      -> ssl.toString,
    "hostname" -> hostname,
    "port"     -> port.toString,
    "path"     -> path,
    "body"     -> String(body.slurp(maxSize = 10000).unsafeMutable, "UTF-8"),
    "headers"  -> rawHeaders.map { (k, vs) => s"$k: ${vs.join("; ")}" }.join("\n          "),
    "params"   -> params.map { (k, v) => s"$k=\"$v\"" }.join("\n          ")
  ).map { (k, v) => s"${k.padLeft(8)}: $v" }.join("", "\n", "\n")
  
  lazy val headers: Map[RequestHeader, List[String]] =
    rawHeaders.map { case (RequestHeader(header), values) => header -> values }

  lazy val length: Int = headers.get(RequestHeader.ContentLength).fold(body.map(_.length).sum)(_.head.toInt)
  lazy val contentType: Option[MediaType] =
    headers.get(RequestHeader.ContentType).flatMap(_.headOption).flatMap(MediaType.unapply(_))
  
trait RequestHandler:
  def listen(handler: Request ?=> Response[?]): HttpService

extension (value: Http.type)
  def listen(handler: Request ?=> Response[?])(using RequestHandler): HttpService =
    summon[RequestHandler].listen(handler)

def request(using Request): Request = summon[Request]
def param(using Request)(key: String): Option[String] = summon[Request].params.get(key)

def header(using Request)(header: RequestHeader): List[String] =
  summon[Request].headers.get(header).getOrElse(Nil)

object ParamReader:
  given ParamReader[Int] = Int.unapply(_)
  given ParamReader[String] = Some(_)

trait ParamReader[T]:
  def read(value: String): Option[T]

case class RequestParam[T](key: String)(using ParamReader[T]):
  type Type = T
  def opt(using Request): Option[T] = param(key).flatMap(summon[ParamReader[T]].read(_))
  def unapply(request: Request): Option[T] = opt(using request)
  def apply()(using Request): T exposes ParamNotSent = opt.getOrElse(throw ParamNotSent(key))

trait HttpService:
  def stop(): Unit

object `&`:
  def unapply(request: Request): (Request, Request) = (request, request)

case class HttpServer(port: Int) extends RequestHandler:

  def listen(handler: Request ?=> Response[?]): HttpService =
    def handle(exchange: HttpExchange) =
      try handler(using makeRequest(exchange)).respond(SimpleResponder(exchange))
      catch case NonFatal(exception) => exception.printStackTrace()
    
    val httpServer = JavaHttpServer.create(InetSocketAddress("localhost", port), 0)
    val context = httpServer.createContext("/")
    context.setHandler(handle(_))
    httpServer.setExecutor(null)
    httpServer.start()
    
    Runtime.getRuntime.addShutdownHook { new Thread(() => httpServer.stop(0)) }
    () => httpServer.stop(1)

  private def streamBody(exchange: HttpExchange): LazyList[IArray[Byte]] =
    val in = exchange.getRequestBody
    val buffer = new Array[Byte](65536)
    
    def recur(): LazyList[IArray[Byte]] =
      val len = in.read(buffer)
      if len > 0 then IArray.from(buffer.slice(0, len)) #:: recur() else LazyList.empty
    
    recur()

  private def makeRequest(exchange: HttpExchange): Request =
    val uri = exchange.getRequestURI
    val query = Option(uri.getQuery)
    
    val queryParams: Map[String, List[String]] = query.fold(Map()) { query =>
      val paramStrings = query.cut("&")
      
      paramStrings.foldLeft(Map[String, List[String]]()) { (map, elem) =>
        val kv = elem.cut("=", 2)
        
        map.updated(kv(0), kv(1) :: map.getOrElse(kv(0), Nil))
      }
    }
    
    val headers = exchange.getRequestHeaders.asScala.view.mapValues(_.asScala.to(List)).to(Map)

    Request(
      method = Method.valueOf(exchange.getRequestMethod.toLowerCase.capitalize),
      body = streamBody(exchange),
      query = query.getOrElse(""),
      ssl = false,
      Option(uri.getHost).getOrElse(exchange.getLocalAddress.getAddress.getCanonicalHostName),
      Option(uri.getPort).filter(_ > 0).getOrElse(exchange.getLocalAddress.getPort),
      uri.getPath,
      headers.map(_ -> _),
      queryParams
    )

  class SimpleResponder(exchange: HttpExchange) extends Responder:
    def addHeader(key: String, value: String): Unit = exchange.getResponseHeaders.add(key, value)
    
    def sendBody(status: Int, body: Body): Unit =
      val length = body match
        case body: Unit         => -1
        case body: Array[Byte]  => body.length
        case _                  => 0

      exchange.sendResponseHeaders(status, length)
      
      body match
        case body: Unit =>
          exchange.close()
        case body: IArray[Byte] =>
          exchange.getResponseBody.write(body.unsafeMutable)
          exchange.getResponseBody.flush()
        case body: LazyList[IArray[Byte]] =>
          body.map(_.unsafeMutable).foreach(exchange.getResponseBody.write(_))
          exchange.getResponseBody.flush()
      exchange.close()

case class Svg(content: String)

object Svg:
  given SimpleHandler[Svg] = SimpleHandler("image/svg+xml", _.content.bytes)

case class Jpeg(content: IArray[Byte])

object Jpeg:
  given SimpleHandler[Jpeg] = SimpleHandler("image/jpeg", _.content)

case class Gif(content: IArray[Byte])

object Gif:
  given SimpleHandler[Gif] = SimpleHandler("image/gif", _.content)

case class Png(content: IArray[Byte])

object Png:
  given SimpleHandler[Png] = SimpleHandler("image/png", _.content)

def basicAuth(validate: (String, String) => Boolean)(response: => Response[?])
             (using Request): Response[?] =
  request.headers.get(RequestHeader.Authorization) match
    case Some(List(s"Basic $credentials")) =>
      val IArray(username, password) = credentials.decode[Base64].string.cut(":")
      if validate(username, password) then response else Response("", HttpStatus.Forbidden)

    case _ =>
      val auth = """Basic realm="Some realm", charset="UTF-8""""
      Response("", HttpStatus.Unauthorized, Map(ResponseHeader.WwwAuthenticate -> auth))
