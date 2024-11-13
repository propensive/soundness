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
import prepositional.*
import digression.*
import vacuous.*
import parasite.*
import turbulence.*
import contingency.*
import gossamer.*
import nettlesome.*
import telekinesis.{HttpResponse as _, *}
import anticipation.*
import spectacular.*

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

        handler(using makeConnection(exchange.nn)).respond(responder)
      catch case NonFatal(exception) => exception.printStackTrace()

    def startServer(): com.sun.net.httpserver.HttpServer raises ServerError =
      try
        val httpServer = csnh.HttpServer.create(jn.InetSocketAddress("localhost", port), 0).nn
        val context = httpServer.createContext("/").nn
        context.setHandler(handle(_))
        //httpServer.setExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
        httpServer.setExecutor(null)
        httpServer.start()
        httpServer
      catch
        case error: jn.BindException => abort(ServerError(port))

    val cancel: Promise[Unit] = Promise[Unit]()
    val server = startServer()

    val asyncTask = async(cancel.await() yet server.stop(1))

    HttpService(port, asyncTask, () => safely(cancel.fulfill(())))

  private def streamBody(exchange: csnh.HttpExchange): LazyList[Bytes] =
    val in = exchange.getRequestBody.nn
    val buffer = new Array[Byte](65536)

    def recur(): LazyList[Bytes] =
      val len = in.read(buffer)
      if len > 0 then buffer.slice(0, len).snapshot #:: recur() else LazyList.empty

    recur()

  private def makeConnection(exchange: csnh.HttpExchange): HttpConnection logs HttpServerEvent =
    val port = Option(exchange.getRequestURI.nn.getPort).filter(_ > 0).getOrElse:
      exchange.getLocalAddress.nn.getPort

    HttpConnection(false, port, makeRequest(exchange))

  private def makeRequest(exchange: csnh.HttpExchange): HttpRequest logs HttpServerEvent =
    val uri = exchange.getRequestURI.nn
    val query = Optional(uri.getQuery)
    val target = uri.getPath.nn.tt+query.let(t"?"+_.tt).or(t"")
    val method = HttpMethod.valueOf(exchange.getRequestMethod.nn.show.lower.capitalize.s)

    val headers: List[RequestHeader.Value] =
      exchange.getRequestHeaders.nn.asScala.view.mapValues(_.nn.asScala.to(List)).flatMap: pair =>
        (pair: @unchecked) match
          case (RequestHeader(header), values) => values.map: value =>
            header(value.tt)
      .to(List)

    val version: HttpVersion = HttpVersion.parse(exchange.getProtocol.nn.tt)

    val host = unsafely:
       Hostname.parse:
         Optional(uri.getHost).let(_.tt).or:
           exchange.getLocalAddress.nn.getAddress.nn.getCanonicalHostName.nn.tt

    val request =
      HttpRequest
       (method  = method,
        version = version,
        host    = host,
        target  = target,
        body    = streamBody(exchange),
        headers = headers)

    Log.fine(HttpServerEvent.Received(request))

    request
