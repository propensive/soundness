/*
    Scintillate, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import perforate.*
import vacuous.*
import anticipation.*
import turbulence.*
import spectacular.*
import gossamer.*
import telekinesis.*

import javax.servlet.*, http.*

trait Servlet(handle: Request ?=> Response[?]) extends HttpServlet:
  protected case class ServletResponseWriter(response: HttpServletResponse) extends Responder:
    def addHeader(key: Text, value: Text): Unit = response.addHeader(key.s, value.s)
    
    def sendBody(status: Int, body: HttpBody): Unit =
      response.setStatus(status)
      val out = response.getOutputStream.nn
      
      body match
        case HttpBody.Empty         => addHeader(ResponseHeader.ContentLength.header, t"0")
        case HttpBody.Data(body)    => addHeader(ResponseHeader.ContentLength.header, body.length.show)
                                       out.write(body.mutable(using Unsafe))
        case HttpBody.Chunked(body) => addHeader(ResponseHeader.TransferEncoding.header, t"chunked")
                                       unsafely(body.map(_.mutable(using Unsafe)).foreach(out.write(_)))

  protected def streamBody
      (request: HttpServletRequest)(using Raises[StreamError])
      : HttpBody.Chunked =
    val in = request.getInputStream
    val buffer = new Array[Byte](4096)

    HttpBody.Chunked(Readable.inputStream.read(request.getInputStream.nn))
    
  protected def makeRequest(request: HttpServletRequest): Request raises StreamError =
    val query = Option(request.getQueryString)
    
    val params: Map[Text, List[Text]] = query.fold(Map()): query =>
      val paramStrings = query.nn.show.cut(t"&")
      
      paramStrings.foldLeft(Map[Text, List[Text]]()): (map, elem) =>
        elem.cut(t"=", 2).to(Seq) match
          case Seq(key: Text, value: Text) => map.updated(key, value :: map.getOrElse(key, Nil))
          case Seq(key: Text)              => map.updated(key, t"" :: map.getOrElse(key, Nil))
          case _                           => map
    
    val headers = request.getHeaderNames.nn.asScala.to(List).map: key =>
      Text(key).lower -> request.getHeaders(key).nn.asScala.to(List).map(Text(_))
    .to(Map)

    Request(
      method = HttpMethod.valueOf(request.getMethod.nn.show.lower.capitalize.s),
      body = streamBody(request),
      query = query.getOrElse("").nn.tt,
      ssl = false,
      request.getServerName.nn.tt,
      request.getServerPort,
      request.getRequestURI.nn.tt,
      headers,
      params
    )

  def handle(servletRequest: HttpServletRequest, servletResponse: HttpServletResponse): Unit =
    try throwErrors(handle(using makeRequest(servletRequest)).respond(ServletResponseWriter(servletResponse)))
    catch case error: StreamError =>
      () // FIXME


  override def service(request: HttpServletRequest, response: HttpServletResponse): Unit =
    handle(request, response)
