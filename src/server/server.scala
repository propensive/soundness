package scintillate

import rudiments.*

import scala.collection.JavaConverters.*
import scala.util.control.NonFatal

import java.net.InetSocketAddress
import java.io.*
import com.sun.net.httpserver.{HttpServer as JavaHttpServer, *}

trait Responder:
  def appendBody(stream: IArray[Byte]): Unit
  def addHeader(key: String, value: String): Unit
  def setStatus(code: Int): Unit
  def close(): Unit

object Handler:
  given Handler[String] = str => LazyList(str.bytes)

trait Handler[T]:
  def stream(value: T): LazyList[IArray[Byte]]

case class Cookie(domain: String,
                  name: String,
                  value: String,
                  path: String,
                  expiry: Option[Long],
                  ssl: Boolean)

case class Response[T: Handler](content: T,
                                status: HttpStatus = HttpStatus.Ok,
                                headers: Map[ResponseHeader, String] = Map(),
                                cookies: List[Cookie] = Nil):

  def respond(responder: Responder) =
    responder.setStatus(status.code)
    for (k, v) <- headers do responder.addHeader(k.header, v)
    summon[Handler[T]].stream(content).foreach(responder.appendBody(_))
    responder.close()

case class Request(method: Method, body: LazyList[IArray[Byte]], query: String, ssl: Boolean, hostname: String,
    port: Int, path: String, rawHeaders: Map[String, List[String]], rawParams: Map[String, List[String]]):
  
  lazy val headers: Map[RequestHeader, List[String]] =
    rawHeaders.map { case (RequestHeader(header), values) => header -> values }

  lazy val length: Int = headers.get(RequestHeader.ContentLength).fold(body.map(_.length).sum)(_.head.toInt)
  lazy val contentType: Option[String] = headers.get(RequestHeader.ContentType).flatMap(_.headOption)
  
trait RequestHandler:
  def listen(port: Int, handler: Request => Response[?]): HttpServer

object HttpServer:
  def listen(port: Int = 80)(handler: Request => Response[?])(using RequestHandler): HttpServer =
    summon[RequestHandler].listen(port, handler)

trait HttpServer:
  def shutdown(): Unit

object SimpleHttpServer extends RequestHandler:

  def listen(port: Int, handler: Request => Response[?]): HttpServer =
    def handle(exchange: HttpExchange) =
      try handler(makeRequest(exchange)).respond(SimpleResponder(exchange))
      catch case NonFatal(exception) => exception.printStackTrace()
    
    val httpServer = JavaHttpServer.create(InetSocketAddress(port), 0)
    val context = httpServer.createContext("/")
    context.setHandler(handle(_))
    httpServer.setExecutor(null)
    httpServer.start()

    () => httpServer.stop(1)

  private def streamBody(exchange: HttpExchange): LazyList[IArray[Byte]] =
    val in = exchange.getRequestBody
    val output = ByteArrayOutputStream()
    val buffer = new Array[Byte](4096)
    
    def recur: LazyList[IArray[Byte]] =
      val len = in.read(buffer)
      if len > 0 then IArray.from(buffer.slice(0, len)) #:: recur else LazyList.empty
    
    recur

  private def makeRequest(exchange: HttpExchange): Request =
    val uri = exchange.getRequestURI
    val query = Option(uri.getQuery)
    
    val params: Map[String, List[String]] = query.fold(Map()) { query =>
      val paramStrings = query.cut("&")
      
      paramStrings.foldLeft(Map[String, List[String]]()) { (map, elem) =>
        val Array(key, value) = elem.split("=", 2)
        
        map.updated(key, value :: map.getOrElse(key, Nil))
      }
    }
    
    val headers = exchange.getRequestHeaders.asScala.view.mapValues(_.asScala.to(List)).to(Map)

    Request(
      method = Method.valueOf(exchange.getRequestMethod.toLowerCase.capitalize),
      body = streamBody(exchange),
      query = query.getOrElse(""),
      ssl = false,
      uri.getHost,
      uri.getPort,
      uri.getPath,
      headers.map(_ -> _),
      params
    )

  class SimpleResponder(exchange: HttpExchange) extends Responder:
    private[this] var status = 200
    
    def setStatus(code: Int): Unit = status = code
    def addHeader(key: String, value: String): Unit = exchange.getResponseHeaders.add(key, value)
    def close(): Unit = exchange.close()
    
    def appendBody(payload: IArray[Byte]): Unit =
      exchange.sendResponseHeaders(status, payload.length)
      exchange.getResponseBody.write(payload.asInstanceOf[Array[Byte]])
      exchange.getResponseBody.flush()