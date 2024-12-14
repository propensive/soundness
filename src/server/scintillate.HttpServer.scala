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

import anticipation.*
import contingency.*
import digression.*
import parasite.*
import rudiments.*
import telekinesis.*
import turbulence.*
import vacuous.*

import scala.compiletime.*

import java.net as jn
import com.sun.net.httpserver as csnh

case class HttpServer(port: Int)(using Tactic[ServerError]) extends RequestServable:
  def listen(handler: (connection: HttpConnection) ?=> HttpResponse)
     (using Monitor, Codicil)
          : HttpService logs HttpServerEvent =

    def handle(exchange: csnh.HttpExchange | Null) =
      try
        val responder = new Responder:
          def addHeader(key: Text, value: Text): Unit =
            exchange.nn.getResponseHeaders.nn.add(key.s, value.s)

          def sendBody(status: Int, body: LazyList[Bytes]): Unit =
            val length = body match
              case LazyList()     => -1
              case LazyList(data) => data.length
              case _              => 0

            exchange.nn.sendResponseHeaders(status, length)

            try
              body.map(_.mutable(using Unsafe)).each: bytes =>
                exchange.nn.getResponseBody.nn.write(bytes)
            catch case e: StreamError => () // FIXME: Should this be ignored?

            exchange.nn.getResponseBody.nn.flush()
            exchange.nn.close()

        val connection = HttpConnection(exchange.nn)

        connection.respond(handler(using connection))

      catch case NonFatal(exception) => exception.printStackTrace()

    def startServer(): com.sun.net.httpserver.HttpServer raises ServerError =
      try
        val httpServer = csnh.HttpServer.create(jn.InetSocketAddress("localhost", port), 0).nn
        httpServer.createContext("/").nn.setHandler(handle(_))
        //httpServer.setExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
        httpServer.setExecutor(null)
        httpServer.start()
        httpServer
      catch
        case error: jn.BindException => abort(ServerError(port))

    val cancel: Promise[Unit] = Promise[Unit]()
    val server = startServer()

    val asyncTask = async(cancel.attend() yet server.stop(1))

    HttpService(port, asyncTask, () => safely(cancel.fulfill(())))

  private def streamBody(exchange: csnh.HttpExchange): LazyList[Bytes] =
    val in = exchange.getRequestBody.nn
    val buffer = new Array[Byte](65536)

    def recur(): LazyList[Bytes] =
      val len = in.read(buffer)
      if len > 0 then buffer.slice(0, len).snapshot #:: recur() else LazyList.empty

    recur()
