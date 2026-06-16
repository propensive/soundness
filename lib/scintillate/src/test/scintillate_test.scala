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

import soundness.*

import logging.silent
import strategies.throwUnsafely
import charEncoders.utf8
import webserverErrorPages.minimal
import threading.virtualThreading
import probates.await

object Tests extends Suite(m"Scintillate tests"):
  def run(): Unit =
    def freePort(): Int =
      val socket = java.net.ServerSocket(0)
      val port = socket.getLocalPort
      socket.close()
      port

    def rawRequest(port: Int, request: Text): Text =
      val socket = java.net.Socket("localhost", port)
      val out = socket.getOutputStream.nn
      out.write(request.s.getBytes("US-ASCII").nn)
      out.flush()
      // Half-close so a kept-alive server sees EOF once it has read our request(s)
      // and stops waiting for more; we can still read the response.
      socket.shutdownOutput()
      val response = String(socket.getInputStream.nn.readAllBytes().nn, "US-ASCII")
      socket.close()
      response.tt

    supervise:
      suite(m"Native socket server"):
        test(m"GET returns the handler's response body"):
          val port = freePort()
          val server = SocketServer(port).handle(Http.Response(Http.Ok)(t"hello from native"))
          val response = rawRequest(port, t"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n")
          server.cancel()
          response

        . assert(_.contains(t"hello from native"))

        test(m"Status line carries the handler's status"):
          val port = freePort()
          val server = SocketServer(port).handle(Http.Response(Http.NotFound)(t"nope"))
          val response = rawRequest(port, t"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n")
          server.cancel()
          response.cut(t"\r\n").head

        . assert(_ == t"HTTP/1.1 404 Not Found")

        test(m"Request method and target reach the handler"):
          val port = freePort()

          val server = SocketServer(port).handle:
            Http.Response(Http.Ok)(t"${request.method.show} ${request.target}")

          val response = rawRequest(port, t"GET /foo/bar HTTP/1.1\r\nHost: localhost\r\n\r\n")
          server.cancel()
          response

        . assert(_.contains(t"GET /foo/bar"))

        test(m"A POST body is available to the handler"):
          val port = freePort()

          val server = SocketServer(port).handle:
            Http.Response(Http.Ok)(request.body().read[Data].utf8)

          val response =
            rawRequest(port, t"POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 5\r\n\r\nhello")

          server.cancel()
          response

        . assert(_.contains(t"hello"))

        test(m"A chunked request body is decoded for the handler"):
          val port = freePort()

          val server = SocketServer(port).handle:
            Http.Response(Http.Ok)(request.body().read[Data].utf8)

          val response =
            rawRequest
              ( port,
                t"POST / HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n"
                + t"5\r\nhello\r\n6\r\n world\r\n0\r\n\r\n" )

          server.cancel()
          response

        . assert(_.contains(t"hello world"))

        test(m"Two pipelined requests get two responses on one connection"):
          val port = freePort()
          val server = SocketServer(port).handle(Http.Response(Http.Ok)(t"ok"))

          val response =
            rawRequest
              ( port,
                t"GET /a HTTP/1.1\r\nHost: x\r\n\r\nGET /b HTTP/1.1\r\nHost: x\r\n\r\n" )

          server.cancel()
          response.cut(t"HTTP/1.1 200 OK").length

        . assert(_ == 3)

        test(m"Connection: close stops after one response"):
          val port = freePort()
          val server = SocketServer(port).handle(Http.Response(Http.Ok)(t"bye"))

          val response =
            rawRequest(port, t"GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n")

          server.cancel()
          response

        . assert(_.contains(t"connection: close"))

        test(m"A 101 response upgrades to a raw bidirectional stream"):
          val port = freePort()

          // Echo upgrade: the response body is the post-handshake request stream,
          // piped straight back out with no HTTP framing.
          val server = SocketServer(port).handle:
            Http.Response(Http.SwitchingProtocols)(Http.Body.Streaming(request.body()))

          val response =
            rawRequest
              ( port,
                t"GET / HTTP/1.1\r\nHost: x\r\nConnection: Upgrade\r\nUpgrade: echo\r\n\r\nPING" )

          server.cancel()
          response

        . assert(r => r.contains(t"101 Switching Protocols") && r.ends(t"PING"))

      suite(m"Loopback load over real sockets"):
        val clients = 32
        val perClient = 300

        test(m"Concurrent clients pipelining keep-alive requests all succeed"):
          val port = freePort()
          val server = SocketServer(port).handle(Http.Response(Http.Ok)(t"pong"))
          val payload = (t"GET / HTTP/1.1\r\nHost: x\r\n\r\n"*perClient).s.getBytes("US-ASCII").nn

          val start = java.lang.System.nanoTime()

          val tasks = List.tabulate(clients): _ =>
            async:
              val socket = java.net.Socket("localhost", port)
              val out = socket.getOutputStream.nn
              out.write(payload)
              out.flush()
              socket.shutdownOutput()
              val response = String(socket.getInputStream.nn.readAllBytes().nn, "US-ASCII").tt
              socket.close()
              response.cut(t"HTTP/1.1 200 OK").length - 1

          val total = tasks.map(_.await()).foldLeft(0)(_ + _)
          val millis = (java.lang.System.nanoTime() - start)/1000000.0
          server.cancel()

          val rate = (total/millis*1000).toLong
          java.lang.System.out.nn.println:
            t"Loopback: $total requests across $clients clients in ${millis.toLong}ms ($rate req/s)".s

          total

        . assert(_ == clients*perClient)

      suite(m"TLS"):
        test(m"The server serves a request over TLS"):
          // A throwaway self-signed certificate, generated with the JDK's keytool.
          val dir = java.nio.file.Files.createTempDirectory("scintillate-tls").nn
          val path = dir.resolve("test.p12").nn.toString.nn

          val keytool = java.lang.ProcessBuilder("keytool", "-genkeypair", "-alias", "test",
              "-keyalg", "RSA", "-keysize", "2048", "-validity", "1", "-storetype", "PKCS12",
              "-keystore", path, "-storepass", "changeit", "-dname", "CN=localhost")

          keytool.redirectErrorStream(true)
          keytool.redirectOutput(java.lang.ProcessBuilder.Redirect.DISCARD)
          keytool.start().nn.waitFor()

          val password = "changeit".toCharArray.nn
          val keystore = java.security.KeyStore.getInstance("PKCS12").nn
          keystore.load(java.io.FileInputStream(path), password)
          val keyManagers = javax.net.ssl.KeyManagerFactory.getInstance("SunX509").nn
          keyManagers.init(keystore, password)
          val serverContext = javax.net.ssl.SSLContext.getInstance("TLS").nn
          serverContext.init(keyManagers.getKeyManagers, null, null)

          val port = freePort()

          val server =
            SocketServer(port, ssl = serverContext).handle(Http.Response(Http.Ok)(t"secure"))

          // A client that trusts any certificate, so the self-signed one is accepted.
          val trustManager = new javax.net.ssl.X509TrustManager:
            type Certs = Array[java.security.cert.X509Certificate | Null] | Null

            def getAcceptedIssuers: Certs =
              scala.Array.empty[java.security.cert.X509Certificate | Null]

            def checkClientTrusted(chain: Certs, kind: String | Null): Unit = ()
            def checkServerTrusted(chain: Certs, kind: String | Null): Unit = ()

          val clientContext = javax.net.ssl.SSLContext.getInstance("TLS").nn
          clientContext.init(null, scala.Array(trustManager), java.security.SecureRandom())
          val socket = clientContext.getSocketFactory.nn.createSocket("localhost", port).nn
          val out = socket.getOutputStream.nn
          val request = t"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          out.write(request.s.getBytes("US-ASCII").nn)
          out.flush()
          val response = String(socket.getInputStream.nn.readAllBytes().nn, "US-ASCII").tt
          socket.close()
          server.cancel()
          response

        . assert(_.contains(t"secure"))

    // Drive the connection loop entirely in memory — no socket, no threads — by
    // feeding request bytes through `serveConnection` and capturing the response.
    def inProcess(handler: HttpConnection ?=> Http.Response, request: Text): Text =
      val in = java.io.ByteArrayInputStream(request.s.getBytes("US-ASCII").nn)
      val out = java.io.ByteArrayOutputStream()
      SocketServer(0).serveConnection(handler)(in, out)
      String(out.toByteArray.nn, "US-ASCII").tt

    suite(m"In-process connection loop"):
      test(m"A request is served entirely in-process with no socket"):
        inProcess(Http.Response(Http.Ok)(t"in-process"), t"GET / HTTP/1.1\r\nHost: x\r\n\r\n")

      . assert(_.contains(t"in-process"))

      test(m"1000 pipelined requests produce 1000 responses"):
        val many = t"GET / HTTP/1.1\r\nHost: x\r\n\r\n"*1000
        inProcess(Http.Response(Http.Ok)(t"ok"), many).cut(t"HTTP/1.1 200 OK").length

      . assert(_ == 1001)

      test(m"Every truncation of a valid request is handled without hanging"):
        val bytes = t"POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 5\r\n\r\nhello".s.getBytes("US-ASCII").nn
        var n = 0

        while n <= bytes.length do
          val in = java.io.ByteArrayInputStream(bytes, 0, n)
          SocketServer(0).serveConnection(Http.Response(Http.Ok)(t"ok"))(in, java.io.ByteArrayOutputStream())
          n += 1

        true

      . assert(_ == true)

      test(m"Random byte streams never crash or hang the loop"):
        var seed: Long = 0x2545f4914f6cdd1dL

        def next(): Int =
          seed = seed*6364136223846793005L + 1442695040888963407L
          ((seed >>> 56) & 0xff).toInt

        var iteration = 0

        while iteration < 1000 do
          val length = next()%80
          val bytes = new Array[Byte](length)
          var i = 0

          while i < length do
            bytes(i) = next().toByte
            i += 1

          val in = java.io.ByteArrayInputStream(bytes)
          SocketServer(0).serveConnection(Http.Response(Http.Ok)(t"ok"))(in, java.io.ByteArrayOutputStream())
          iteration += 1

        true

      . assert(_ == true)

      test(m"Expect: 100-continue gets an interim 100 response"):
        val request =
          t"POST / HTTP/1.1\r\nHost: x\r\nExpect: 100-continue\r\nContent-Length: 5\r\n\r\nhello"

        inProcess(Http.Response(Http.Ok)(t"done"), request)

      . assert(r => r.contains(t"100 Continue") && r.contains(t"done"))

      test(m"An over-long request line is rejected with 414"):
        inProcess(Http.Response(Http.Ok)(t"ok"), t"GET /${t"a"*9000} HTTP/1.1\r\nHost: x\r\n\r\n")

      . assert(_.contains(t"414"))

      test(m"Over-large headers are rejected with 431"):
        inProcess(Http.Response(Http.Ok)(t"ok"), t"GET / HTTP/1.1\r\nHost: x\r\nX-Big: ${t"a"*70000}\r\n\r\n")

      . assert(_.contains(t"431"))

      test(m"A streaming response to HTTP/1.0 is close-delimited, not chunked"):
        val body = Http.Body.Streaming(Stream(t"Hello".data, t"World".data))
        inProcess(Http.Response(Http.Ok)(body), t"GET / HTTP/1.0\r\nHost: x\r\n\r\n")

      . assert: response =>
          !response.contains(t"transfer-encoding: chunked")
          && response.contains(t"connection: close")
          && response.contains(t"HelloWorld")

      test(m"A large unconsumed body closes the connection instead of draining"):
        val request =
          t"POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 70000\r\n\r\n${t"a"*70000}"
          + t"GET /second HTTP/1.1\r\nHost: x\r\n\r\n"

        inProcess(Http.Response(Http.Ok)(t"ok"), request).cut(t"HTTP/1.1 200 OK").length - 1

      . assert(_ == 1)
