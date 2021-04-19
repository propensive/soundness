/*

    Scintillate, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package scintillate

import rudiments.*

import scala.collection.immutable.ListMap
import scala.annotation.tailrec

import java.net.*
import java.io.*

import language.dynamics

type Body = Chunked | Unit | IArray[Byte]

object Postable:
  given Postable[String]("text/plain") with
    def content(value: String): IArray[Byte] = IArray.from(value.getBytes("UTF-8"))

  given Postable[Map[String, String]]("multipart/form-data") with
    def content(value: Map[String, String]): IArray[Byte] = IArray.from {
      value.map { (key, value) =>
        s"${URLEncoder.encode(key, "UTF-8")}=${URLEncoder.encode(value, "UTF-8")}"
      }.mkString("&").getBytes("UTF-8")
    }

  given Postable[Unit]("text/plain") with
    def content(value: Unit): IArray[Byte] = IArray()

abstract class Postable[T](val contentType: String):
  def content(value: T): IArray[Byte]

enum Method:
  case Get, Head, Post, Put, Delete, Connect, Options, Trace, Patch

object HttpReadable:
  given HttpReadable[String] with
    def read(body: Body) = body match
      case () =>
        ""
      case body: IArray[Byte] =>
        String(body.asInstanceOf[Array[Byte]], "UTF-8")
      case body: LazyList[IArray[Byte]] =>
        body.foldLeft("") { (acc, next) => acc+(new String(next.asInstanceOf[Array[Byte]], "UTF-8")) }
  
  given HttpReadable[Bytes] with
    def read(body: Body): Bytes = body match
      case () =>
        IArray()
      case body: IArray[Byte] =>
        body
      case body: LazyList[IArray[Byte]] =>
        body.slurp(maxSize = 10*1024*1024)

trait HttpReadable[+T]:
  def read(body: Body): T

case class HttpResponse(status: HttpStatus, headers: ListMap[ResponseHeader, String], body: Body):
  def as[T: HttpReadable]: T = status match
    case status: FailureCase => throw HttpError(status, body)
    case status              => summon[HttpReadable[T]].read(body)

object Http extends Dynamic:
  def post[T: Postable]
          (url: String, content: T, headers: ListMap[RequestHeader, String] = ListMap(), follow: Boolean = true): HttpResponse =
    request[T](url, content, Method.Post, headers, follow)

  def get(url: String,
          params: ListMap[String, String] = ListMap(),
          headers: ListMap[RequestHeader, String] = ListMap(),
          follow: Boolean = true): HttpResponse =
    val paramString = params.map { (k, v) => s"${k.urlEncode}=${v.urlEncode}" }.mkString("&")

    val fullUrl = if params.isEmpty then url else url+"?"+paramString

    request(fullUrl, (), Method.Get, ListMap(), follow)

  private def request[T: Postable](url: String,
                                   content: T,
                                   method: Method,
                                   headers: ListMap[RequestHeader, String],
                                   follow: Boolean): HttpResponse =
    URL(url).openConnection match
      case conn: HttpURLConnection =>
        conn.setRequestMethod(method.toString.toUpperCase)
        //conn.setFollowRedirects(follow)
        conn.setRequestProperty("Content-Type", summon[Postable[T]].contentType)
        conn.setRequestProperty("User-Agent", "Scintillate 1.0.0")
        headers.foreach { (key, value) => conn.setRequestProperty(key.header, value) }
        
        if method == Method.Post || method == Method.Put then
          conn.setDoOutput(true)
          val out = conn.getOutputStream()
          out.write(summon[Postable[T]].content(content).to(Array))
          out.close()

        val buf = new Array[Byte](65536)

        def read(in: InputStream): Chunked =
          val len = in.read(buf, 0, buf.length)
          if len < 0 then LazyList.empty else IArray.from(buf.slice(0, len)) #:: read(in)
       

        def body =
          try read(conn.getInputStream)
          catch case _: Exception =>
            try read(conn.getErrorStream)
            catch case _: Exception => LazyList.empty
        
        val HttpStatus(status) = conn.getResponseCode
        val responseHeaders = ListMap[ResponseHeader, String]()

        HttpResponse(status, responseHeaders, body)
            
case class HttpError(status: HttpStatus & FailureCase, body: Body)
    extends Exception(s"HTTP Error ${status.code}: ${status.description}"):
  def as[T: HttpReadable]: T = summon[HttpReadable[T]].read(body)

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
  case ProxyAuthenticationRequired extends HttpStatus(407, "Proxy Authentication Required"), FailureCase
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
  case RequestHeaderFieldsTooLarge extends HttpStatus(431, "Request Header Fields Too Large"), FailureCase
  case UnavailableForLegalReasons extends HttpStatus(451, "Unavailable For Legal Reasons"), FailureCase
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
  case NetworkAuthenticationRequired extends HttpStatus(511, "Network Authentication Required"), FailureCase
