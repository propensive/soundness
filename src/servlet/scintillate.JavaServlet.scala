/*
    Scintillate, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import contingency.*
import gossamer.*
import nettlesome.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import vacuous.*

import jakarta.servlet as js, js.http as jsh

open class JavaServlet(handle: HttpConnection ?=> HttpResponse) extends jsh.HttpServlet:
  protected def streamBody(request: jsh.HttpServletRequest): LazyList[Bytes] raises StreamError =
    val in = request.getInputStream()
    val buffer = new Array[Byte](4096)

    Readable.inputStream.stream(request.getInputStream.nn)

  protected def makeConnection
     (request: jsh.HttpServletRequest, servletResponse: jsh.HttpServletResponse)
          : HttpConnection raises StreamError =
    val uri = request.getRequestURI.nn.tt
    val query = Optional(request.getQueryString).let(_.tt)
    val target = uri+query.let(t"?"+_).or(t"")

    val headers: List[RequestHeader.Value] =
      request.getHeaderNames.nn.asScala.to(List).map: key =>
        key.tt.lower -> request.getHeaders(key).nn.asScala.to(List).map(_.tt)

      . flatMap:
          case (RequestHeader(header), values) => values.map(header(_))

      . to(List)

    val httpRequest = HttpRequest
     (method  = request.getMethod.nn.show.decode[HttpMethod],
      version = HttpVersion.parse(request.getProtocol.nn.tt),
      host    = unsafely(Hostname.parse(request.getServerName.nn.tt)),
      target  = target,
      body    = streamBody(request),
      headers = headers)

    def respond(response: HttpResponse): Unit =
      servletResponse.setStatus(response.status.code)

      response.headers.each: (key, value) =>
        servletResponse.addHeader(key.s, value.s)

      val out = servletResponse.getOutputStream.nn

      response.body match
        case LazyList()     => servletResponse.addHeader("content-length", "0")
        case LazyList(data) => servletResponse.addHeader("content-length", data.length.show.s)
                               out.write(data.mutable(using Unsafe))
        case body           => servletResponse.addHeader("transfer-encoding", "chunked")
                               body.map(_.mutable(using Unsafe)).each(out.write(_))

      out.close()

    HttpConnection(false, request.getServerPort, httpRequest, respond)

  def handle(request: jsh.HttpServletRequest, response: jsh.HttpServletResponse): Unit =
    unsafely:
      val connection = makeConnection(request, response)
      connection.respond(handle(using connection))

  override def service(request: jsh.HttpServletRequest, response: jsh.HttpServletResponse): Unit =
    try handle(request, response) catch
      case error: Throwable =>
        println("An error occurred while processing a request: "+error)
        error.printStackTrace(System.out)
