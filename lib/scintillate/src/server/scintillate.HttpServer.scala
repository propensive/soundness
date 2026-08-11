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

import java.net as jn

import com.sun.net.httpserver as csnh

import anticipation.*
import contingency.*
import digression.*
import parasite.*
import rudiments.*
import telekinesis.*
import turbulence.*
import urticose.*

case class HttpServer(port: Int, local: Boolean = true)(using errorPage: WebserverErrorPage)
extends RequestServable:
  def handle(handler: (connection: HttpConnection) ?=> Http.Response^{connection})
    ( using Monitor, Probate )
    ( using (HttpServerEvent is Loggable)^, Tactic[ServerError] )
  :   Service^ =

    def handle(exchange: csnh.HttpExchange | Null): Unit =
      try
        recover:
          case StreamError(length) =>
            Log.warn(HttpServerEvent.BrokenStream(length))

          case error @ Hostname.Error(_, _) =>
            Log.warn(HttpServerEvent.ConnectionFailed(error))

            try
              exchange.nn.sendResponseHeaders(400, -1)
              exchange.nn.close()
            catch case NonFatal(_) => ()

        . protect:
            val connection = HttpConnection(exchange.nn)

            connection.respond:
              try handler(using connection) catch case throwable: Throwable =>
                errorPage.handle(throwable, connection)

      catch case NonFatal(exception) =>
        Log.fail(HttpServerEvent.ConnectionFailed(fulminate.Error(exception)))

    def startServer()(using Tactic[ServerError]): com.sun.net.httpserver.HttpServer =
      try
        val host = if local then "localhost" else "0.0.0.0"
        val httpServer = csnh.HttpServer.create(jn.InetSocketAddress(host, port), 0).nn
        httpServer.createContext("/").nn.setHandler(handle(_))
        httpServer.setExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
        httpServer.start()
        httpServer
      catch
        case error: jn.BindException => abort(ServerError(port))

    val cancel: Promise[Unit] = Promise[Unit]()
    val server = startServer()
    val asyncTask = async(cancel.attend() yet server.stop(1))

    Service: () => safely(cancel.fulfill(()))
