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

import scala.collection.JavaConverters.*

import javax.servlet.*, http.*

trait Servlet() extends HttpServlet:

  def handle(using Request): Response[?]

  def run(): Unit

  protected case class ServletResponseWriter(response: HttpServletResponse) extends Responder:
    def addHeader(key: String, value: String) = response.addHeader(key, value)
    
    def sendBody(status: Int, body: Body) =
      val length = body match
        case body: Unit         => -1
        case body: Array[Byte] => body.length
        case _                  => 0
      
      response.setStatus(status)
      addHeader(ResponseHeader.ContentLength.header, length.toString)

      body match
        case body: Unit =>
          ()

        case body: IArray[Byte] =>
          response.getOutputStream.write(body.asInstanceOf[Array[Byte]])
          response.getOutputStream.flush()

        case body: LazyList[IArray[Byte]] =>
          body.map(_.asInstanceOf[Array[Byte]]).foreach(response.getOutputStream.write(_))
          response.getOutputStream.flush()

  private def streamBody(request: HttpServletRequest): LazyList[IArray[Byte]] =
    val in = request.getInputStream
    val buffer = new Array[Byte](4096)
    
    def recur(): LazyList[IArray[Byte]] =
      val len = in.read(buffer)
      if len > 0 then IArray.from(buffer.slice(0, len)) #:: recur() else LazyList.empty
    
    recur()
    
  private def makeRequest(request: HttpServletRequest): Request =
    val query = Option(request.getQueryString)
    
    val params: Map[String, List[String]] = query.fold(Map()) { query =>
      val paramStrings = query.cut("&")
      
      paramStrings.foldLeft(Map[String, List[String]]()) { (map, elem) =>
        val Array(key, value) = elem.split("=", 2)
        
        map.updated(key, value :: map.getOrElse(key, Nil))
      }
    }
    
    val headers = request.getHeaderNames.asScala.to(List).map {
      k => k -> request.getHeaders(k).asScala.to(List)
    }.to(Map)

    Request(
      method = Method.valueOf(request.getMethod.toLowerCase.capitalize),
      body = streamBody(request),
      query = query.getOrElse(""),
      ssl = false,
      request.getServerName,
      request.getServerPort,
      request.getRequestURI,
      headers,
      params
    )

  def handle(servletRequest: HttpServletRequest, servletResponse: HttpServletResponse): Unit =
    val responseWriter = ServletResponseWriter(servletResponse)
    handle(using makeRequest(servletRequest)).respond(responseWriter)

  override def service(request: HttpServletRequest, response: HttpServletResponse): Unit =
    handle(request, response)

object Path:
  def unapply(request: Request): Option[String] = Some(request.path)