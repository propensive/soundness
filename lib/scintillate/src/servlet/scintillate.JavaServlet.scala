                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package scintillate

import jakarta.servlet as js, js.http as jsh

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*

open class JavaServlet(handle: HttpConnection ?=> Http.Response) extends jsh.HttpServlet:
  protected def streamBody(request: jsh.HttpServletRequest): Stream[Data] raises StreamError =
    Streamable.inputStream.stream(request.getInputStream().nn)


  protected def makeConnection
    ( request: jsh.HttpServletRequest, servletResponse: jsh.HttpServletResponse )
  :   HttpConnection raises StreamError =

    val uri = request.getRequestURI.nn.tt
    val query = Optional(request.getQueryString).let(_.tt)
    val target = uri+query.let(t"?"+_).or(t"")

    val headers: List[Http.Header] =
      request.getHeaderNames.nn.asScala.to(List).map: key =>
        key.tt.lower -> request.getHeaders(key).nn.asScala.to(List).map(_.tt)

      . flatMap:
          case (key, values) => values.map(Http.Header(key, _))

    val httpRequest =
      Http.Request
        ( method      = request.getMethod.nn.show.decode[Http.Method],
          version     = Http.Version.parse(request.getProtocol.nn.tt),
          host        = unsafely(request.getServerName.nn.tt.decode[Hostname]),
          target      = target,
          body        = () => streamBody(request),
          textHeaders = headers )

    def respond(response: Http.Response): Unit =
      servletResponse.setStatus(response.status.code)

      response.textHeaders.each:
        case Http.Header(key, value) =>
          servletResponse.addHeader(key.s, value.s)

      val out = servletResponse.getOutputStream.nn

      response.body match
        case Http.Body.Fixed(data) =>
          servletResponse.addHeader("content-length", data.length.show.s)
          out.write(data.mutable(using Unsafe))

        case Http.Body.Empty =>
          servletResponse.addHeader("content-length", "0")

        case Http.Body.Streaming(stream) =>
          servletResponse.addHeader("transfer-encoding", "chunked")
          stream.map(_.mutable(using Unsafe)).each(out.write(_))

      out.close()

    new HttpConnection(httpRequest, false, request.getServerPort, respond)


  def handle(request: jsh.HttpServletRequest, response: jsh.HttpServletResponse): Unit =
    unsafely:
      val connection = makeConnection(request, response)
      connection.respond(handle(using connection))


  override def service
    ( request: jsh.HttpServletRequest | Null, response: jsh.HttpServletResponse | Null )
  :   Unit =

    if request != null && response != null then try handle(request, response) catch
      case error: Throwable =>
        error.printStackTrace(System.out)
