/*
    Scintillate, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

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
import slalom.*

import scala.collection.JavaConverters.*

import javax.servlet.*, http.*

trait Servlet() extends HttpServlet:

  def handle(using Request): Response[?]

  protected case class ServletResponseWriter(response: HttpServletResponse) extends Responder:
    def addHeader(key: String, value: String) = response.addHeader(key, value)
    
    def sendBody(status: Int, body: Body) =
      val length = body match
        case Body.Empty      => -1
        case Body.Data(body) => body.length
        case _               => 0
      
      response.setStatus(status)
      addHeader(ResponseHeader.ContentLength.header, length.toString)

      body match
        case Body.Empty =>
          ()

        case Body.Data(body) =>
          response.getOutputStream.nn.write(body.unsafeMutable)
          response.getOutputStream.nn.flush()

        case Body.Chunked(body) =>
          body.map(_.unsafeMutable).foreach(response.getOutputStream.nn.write(_))
          response.getOutputStream.nn.flush()

  private def streamBody(request: HttpServletRequest): Body.Chunked =
    val in = request.getInputStream
    val buffer = new Array[Byte](4096)
    
    def recur(): LazyList[IArray[Byte]] =
      val len = in.nn.read(buffer)
      if len > 0 then IArray.from(buffer.slice(0, len)) #:: recur() else LazyList.empty
    
    Body.Chunked(recur())
    
  private def makeRequest(request: HttpServletRequest): Request =
    val query = Option(request.getQueryString)
    
    val params: Map[String, List[String]] = query.fold(Map()) { query =>
      val paramStrings = query.nn.cut("&")
      
      paramStrings.foldLeft(Map[String, List[String]]()) { (map, elem) =>
        val Seq(key, value) = elem.cut("=", 2).to(Seq)
        
        map.updated(key, value :: map.getOrElse(key, Nil))
      }
    }
    
    val headers = request.getHeaderNames.nn.asScala.to(List).map {
      k => k -> request.getHeaders(k).nn.asScala.to(List)
    }.to(Map)

    Request(
      method = Method.valueOf(request.getMethod.nn.lower.capitalize),
      body = streamBody(request),
      query = query.getOrElse("").nn,
      ssl = false,
      request.getServerName.nn,
      request.getServerPort,
      request.getRequestURI.nn,
      headers,
      params
    )

  def handle(servletRequest: HttpServletRequest, servletResponse: HttpServletResponse): Unit =
    val responseWriter = ServletResponseWriter(servletResponse)
    handle(using makeRequest(servletRequest)).respond(responseWriter)

  override def service(request: HttpServletRequest, response: HttpServletResponse): Unit =
    handle(request, response)

extension (path: Base.Path)
  def unapply(request: Request): Option[String] = Some(request.path)