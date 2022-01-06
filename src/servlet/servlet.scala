/*
    Scintillate, version 0.16.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

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
import slalom.*

import javax.servlet.*, http.*

trait Servlet() extends HttpServlet:

  def handle(using Request): Response[?]

  protected case class ServletResponseWriter(response: HttpServletResponse) extends Responder:
    def addHeader(key: Text, value: Text): Unit = response.addHeader(key.s, value.s)
    
    def sendBody(status: Int, body: HttpBody): Unit =
      val length = body match
        case HttpBody.Empty      => -1
        case HttpBody.Data(body) => body.length
        case _               => 0
      
      response.setStatus(status)
      addHeader(ResponseHeader.ContentLength.header, length.show)

      body match
        case HttpBody.Empty =>
          ()

        case HttpBody.Data(body) =>
          response.getOutputStream.nn.write(body.unsafeMutable)
          response.getOutputStream.nn.flush()

        case HttpBody.Chunked(body) =>
          try body.map(_.unsafeMutable).foreach(response.getOutputStream.nn.write(_))
          catch case e: StreamCutError => () // FIXME: Is it correct to just ignore this?
          response.getOutputStream.nn.flush()

  private def streamBody(request: HttpServletRequest): HttpBody.Chunked =
    val in = request.getInputStream
    val buffer = new Array[Byte](4096)
    
    def recur(): DataStream = try
      val len = in.nn.read(buffer)
      if len > 0 then buffer.slice(0, len).snapshot #:: recur() else LazyList.empty
    catch case _: Exception => LazyList(throw StreamCutError())
    
    HttpBody.Chunked(recur())
    
  private def makeRequest(request: HttpServletRequest): Request =
    val query = Option(request.getQueryString)
    
    val params: Map[Text, List[Text]] = query.fold(Map()) { query =>
      val paramStrings = query.nn.show.cut(t"&")
      
      paramStrings.foldLeft(Map[Text, List[Text]]()) { (map, elem) =>
        elem.cut(t"=", 2).to(Seq) match
          case Seq(key: Text, value: Text) => map.updated(key, value :: map.getOrElse(key, Nil))
          case Seq(key: Text)             => map.updated(key, t"" :: map.getOrElse(key, Nil))
          case _                         => map
      }
    }
    
    val headers = request.getHeaderNames.nn.asScala.to(List).map:
      k => Text(k) -> request.getHeaders(k).nn.asScala.to(List).map(Text(_))
    .to(Map)

    Request(
      method = HttpMethod.valueOf(request.getMethod.nn.show.lower.capitalize.s),
      body = streamBody(request),
      query = Text(query.getOrElse("").nn),
      ssl = false,
      Text(request.getServerName.nn),
      request.getServerPort,
      Text(request.getRequestURI.nn),
      headers,
      params
    )

  def handle(servletRequest: HttpServletRequest, servletResponse: HttpServletResponse): Unit =
    val responseWriter = ServletResponseWriter(servletResponse)
    handle(using makeRequest(servletRequest)).respond(responseWriter)

  override def service(request: HttpServletRequest, response: HttpServletResponse): Unit =
    handle(request, response)

extension (path: Base.Path)
  def unapply(request: Request): Option[Text] = Some(request.path)