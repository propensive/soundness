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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import java.io as ji
import jakarta.servlet as js, js.http as jsh
import proscenium.compat.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import telekinesis.*
import prepositional.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

object JavaServlet:
  // JavaServletFn → JavaServlet.Fn
  open class Fn(handle: Http.Connection => Http.Response)
  extends JavaServlet(handle)

// `handle` is a plain function (not a context function): with `Http.Connection` a capability,
// a context-function class parameter cannot be applied from the synthesized superclass
// argument of subclasses like `JavaServlet.Fn` (capture-root unification).
open class JavaServlet(handle: Http.Connection => Http.Response) extends jsh.HttpServlet:
  protected def streamBody(request: jsh.HttpServletRequest)
    ( using Tactic[StreamError] )
  :   Stream[Data] over Credit =

    Streamable.inputStream.stream(request.getInputStream().nn)


  protected def makeConnection
    ( request: jsh.HttpServletRequest, servletResponse: jsh.HttpServletResponse )
    ( using streamError: Tactic[StreamError], hostnameError: Tactic[Hostname.Error] )
  :   Http.Connection^ =

    val uri = request.getRequestURI.nn.tt
    val query = Optional(request.getQueryString).let(_.tt)
    val target = uri+query.let(t"?"+_).or(t"")

    val headers: List[Http.Header] =
      request.getHeaderNames.nn.to[List].map: key =>
        key.tt.lower -> request.getHeaders(key).nn.to[List].map(_.tt)

      . flatMap:
          case (key, values) => values.map(Http.Header(key, _))

    // Rims: under separation checking a method's fresh capability result may not hide its
    // parameters, so nothing the connection retains — the body thunk or the respond sink —
    // may charge them; they cross as `AnyRef` (the `AnyRef`-rim recipe), with the tactic
    // re-typed at each use site.
    val in: AnyRef = request.getInputStream().nn
    val servletResponse0: AnyRef = servletResponse
    val streamError0: AnyRef = summon[Tactic[StreamError]].asInstanceOf[AnyRef]

    val httpRequest =
      Http.Request
        ( method      = request.getMethod.nn.show.as[Http.Method],
          version     = Http.Version.parse(request.getProtocol.nn.tt),
          host        = request.getServerName.nn.tt.as[Hostname],
          target      = target,
          body        = () =>
            Streamable.inputStream
              (using streamError0.asInstanceOf[Tactic[StreamError]])
            . stream(in.asInstanceOf[ji.InputStream]),
          textHeaders = headers )

    val respond: Http.Connection.Respond^ = new Http.Connection.Respond:
      def apply(response: Http.Response^)(using Tactic[StreamError]): Unit =
        val servletResponse1 = servletResponse0.asInstanceOf[jsh.HttpServletResponse]
        servletResponse1.setStatus(response.status.code)

        response.textHeaders.each:
          case Http.Header(key, value) =>
            servletResponse1.addHeader(key.s, value.s)

        val out = servletResponse1.getOutputStream.nn

        response.body match
          case Http.Body.Fixed(data) =>
            servletResponse.addHeader("content-length", data.length.show.s)
            out.write(Array.unsafeJvm(data))

          case Http.Body.Empty =>
            servletResponse.addHeader("content-length", "0")

          case Http.Body.Flowing(source) =>
            servletResponse.addHeader("transfer-encoding", "chunked")
            val stream = source()

            // A while-loop rather than a recursive def: a def capturing the
            // locally bound exclusive stream may not call itself.
            var draining = true

            while draining do stream.refill(Credit(Long.MaxValue)) match
              case count: Int =>
                stream.lend: region =>
                  range =>
                    val interval: Interval = range
                    out.write(unsafely(region.raw.asInstanceOf[scala.Array[Byte]]),
                        interval.start.n0, interval.size)

                out.flush()
                stream.skip(count)

              case _ => draining = false

        out.close()

    new Http.Connection(httpRequest, false, request.getServerPort, respond)


  def handle(request: jsh.HttpServletRequest, response: jsh.HttpServletResponse): Unit =
    recover:
      case error @ StreamError(_) =>
        error.printStackTrace(System.out)

      case error @ Hostname.Error(_, _) =>
        error.printStackTrace(System.out)
        try response.setStatus(400) catch case NonFatal(_) => ()

    . protect:
        val connection = makeConnection(request, response)
        connection.respond(handle(connection))


  override def service
    ( request: jsh.HttpServletRequest | Null, response: jsh.HttpServletResponse | Null )
  :   Unit =

    if request != null && response != null then try handle(request, response) catch
      case error: Throwable =>
        error.printStackTrace(System.out)
