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
import contingency.*
import vacuous.*
import anticipation.*
import turbulence.*
import nettlesome.*
import spectacular.*
import gossamer.*
import telekinesis.{HttpRequest as _, HttpResponse as _, *}

import jakarta.servlet as js, js.http as jsh

open class JavaServlet(handle: HttpRequest ?=> HttpResponse) extends jsh.HttpServlet:
  protected case class ServletResponseWriter(response: jsh.HttpServletResponse) extends Responder:
    def addHeader(key: Text, value: Text): Unit = response.addHeader(key.s, value.s)

    def sendBody(status: Int, body: LazyList[Bytes]): Unit =
      response.setStatus(status)
      val out = response.getOutputStream.nn

      body match
        case LazyList()     => addHeader(ResponseHeader.ContentLength.header, t"0")
        case LazyList(data) => addHeader(ResponseHeader.ContentLength.header, data.length.show)
                               out.write(data.mutable(using Unsafe))
        case body           => addHeader(ResponseHeader.TransferEncoding.header, t"chunked")
                               body.map(_.mutable(using Unsafe)).each(out.write(_))

  protected def streamBody(request: jsh.HttpServletRequest): LazyList[Bytes] raises StreamError =
    val in = request.getInputStream()
    val buffer = new Array[Byte](4096)

    Readable.inputStream.stream(request.getInputStream.nn)

  protected def makeRequest(request: jsh.HttpServletRequest): HttpRequest raises StreamError =
    val query = Option(request.getQueryString)

    val params: Map[Text, List[Text]] = query.fold(Map()): query =>
      val paramStrings = query.nn.show.cut(t"&")

      paramStrings.foldLeft(Map[Text, List[Text]]()): (map, elem) =>
        elem.cut(t"=", 2).to(Seq) match
          case Seq(key: Text, value: Text) => map.updated(key, value :: map.getOrElse(key, Nil))
          case Seq(key: Text)              => map.updated(key, t"" :: map.getOrElse(key, Nil))
          case _                           => map

    val headers = request.getHeaderNames.nn.asScala.to(List).map: key =>
      key.tt.lower -> request.getHeaders(key).nn.asScala.to(List).map(_.tt)
    .to(Map)

    HttpRequest(
      method = HttpMethod.valueOf(request.getMethod.nn.show.lower.capitalize.s),
      hostname = unsafely(Hostname.parse(request.getServerName.nn.tt)),
      body = streamBody(request),
      query = query.getOrElse("").nn.tt,
      ssl = false,
      request.getServerPort,
      request.getRequestURI.nn.tt,
      headers,
      params
    )

  def handle(servletRequest: jsh.HttpServletRequest, servletResponse: jsh.HttpServletResponse): Unit =
    try throwErrors(handle(using makeRequest(servletRequest)).respond(ServletResponseWriter(servletResponse)))
    catch case error: StreamError =>
      () // FIXME

  override def service(request: jsh.HttpServletRequest, response: jsh.HttpServletResponse): Unit =
    handle(request, response)

open class JavaServletFn(handle: HttpRequest => HttpResponse)
extends JavaServlet(request ?=> handle(request))
