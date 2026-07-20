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

import soundness.*

import logging.silentLogging
import strategies.throwUnsafely
import charEncoders.utf8Encoder
import webserverErrorPages.minimalErrorPage
import threading.virtualThreading
import probates.awaitProbate

// A value served as a streaming `text/plain` body: `Servable` synthesises an
// `Http.Body.Flowing` whose source runs the text stream through the char-encoder
// duct (`.via`) — the path #1629 hung on. Kept in-module so the test needs no
// honeycomb dependency; a real `Document[Html]` is served the same way.
case class TextPage(lines: List[Text])

object TextPage:
  given media: TextPage is Media:
    extension (value: TextPage) def mediaType: MediaType = media"text/plain"(charset = "UTF-8")

  given streamable: (TextPage is Streamable by Text over Credit) =
    value => Stream(value.lines.iterator)

object Tests extends Suite(m"Scintillate tests"):
  def run(): Unit =
    CaptureTests()

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
            Http.Response(Http.Ok)(request.body().memoize.utf8)

          val response =
            rawRequest(port, t"POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 5\r\n\r\nhello")

          server.cancel()
          response

        . assert(_.contains(t"hello"))

        test(m"A chunked request body is decoded for the handler"):
          val port = freePort()

          val server = SocketServer(port).handle:
            Http.Response(Http.Ok)(request.body().memoize.utf8)

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
            // The upgraded response's stream reads the live connection for the rest
            // of the exchange; the handler result type `Http.Response^{connection}`
            // expresses that retention honestly, so no seal is needed.
            Http.Response(Http.SwitchingProtocols)(Http.Body.Flowing(() => request.body()))

          val response =
            rawRequest
              ( port,
                t"GET / HTTP/1.1\r\nHost: x\r\nConnection: Upgrade\r\nUpgrade: echo\r\n\r\nPING" )

          server.cancel()
          response

        . assert(r => r.contains(t"101 Switching Protocols") && r.ends(t"PING"))

        test(m"A streaming Text body via the char-encoder is returned (#1629)"):
          val port = freePort()

          // Serve a `Streamable by Text` value: `Servable` wraps it as an
          // `Http.Body.Flowing` whose source pipes the text through the
          // char-encoder duct. #1629 hung here (a self-referential `given
          // encoder = summonInline[CharEncoder]` in `Servable` spun at 100% CPU).
          val page = TextPage(List.tabulate(4000)(i => t"line-$i\n"))
          val server = SocketServer(port).handle(Http.Response(Http.Ok)(page))

          val socket = java.net.Socket("localhost", port)
          socket.setSoTimeout(5000)
          val out = socket.getOutputStream.nn
          out.write(t"GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n".s.getBytes("US-ASCII").nn)
          out.flush()
          socket.shutdownOutput()

          val response =
            try String(socket.getInputStream.nn.readAllBytes().nn, "US-ASCII").tt
            catch case _: java.net.SocketTimeoutException => t"<timed out>"

          socket.close()
          server.cancel()
          response

        . assert(r => r.contains(t"line-0\n") && r.contains(t"line-3999\n"))

        test(m"A streaming body from an async producer is fully returned"):
          val port = freePort()

          // A body produced on a *separate fiber* and drained through the
          // producer's bounded queue: larger than the window (window*block) so
          // `put` must block, making the producer genuinely depend on the
          // connection draining it concurrently — the async-producer streaming
          // path the native server previously had no coverage for.
          // Sealed with `unsafeAssumePure` exactly as the `Servable.serve` API
          // boundary seals the monitor the async producer captures (its return
          // type is an unadorned `Http.Response`); this is what lets a real
          // honeycomb page compile through `.handle`.
          val server = SocketServer(port).handle:
            caps.unsafe.unsafeAssumePure:
              Http.Response(Http.Ok):
                Http.Body.Flowing: () =>
                  val producer = Producer[Data](4096)

                  async:
                    var i = 0
                    while i < 4000 do
                      producer.put(t"line-$i\n".in[Data])
                      i += 1
                    producer.finish()

                  Stream(producer.iterator)

          // A read timeout turns a serving deadlock into a fast failure rather
          // than a hung suite.
          val socket = java.net.Socket("localhost", port)
          socket.setSoTimeout(5000)
          val out = socket.getOutputStream.nn
          out.write(t"GET / HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n".s.getBytes("US-ASCII").nn)
          out.flush()
          socket.shutdownOutput()

          val response =
            try String(socket.getInputStream.nn.readAllBytes().nn, "US-ASCII").tt
            catch case _: java.net.SocketTimeoutException => t"<timed out>"

          socket.close()
          server.cancel()
          response

        . assert(r => r.contains(t"line-0\n") && r.contains(t"line-3999\n"))

      suite(m"Loopback load over real sockets"):
        val clients = 32
        val perClient = 300

        test(m"Concurrent clients pipelining keep-alive requests all succeed"):
          val port = freePort()
          val server = SocketServer(port).handle(Http.Response(Http.Ok)(t"pong"))
          val payload = (t"GET / HTTP/1.1\r\nHost: x\r\n\r\n"*perClient).s.getBytes("US-ASCII").nn

          val start = java.lang.System.nanoTime()

          // Handles collected for concurrent await: sealed per the pure-façade convention
          // (D6; the `Seq[Task].sequence` shape).
          val tasks = List.tabulate(clients): _ =>
            caps.unsafe.unsafeAssumePure:
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

      suite(m"Native HTTP/2 server"):
        // A throwaway self-signed certificate, shared by the h2 tests below.
        def serverContext(): javax.net.ssl.SSLContext =
          val dir = java.nio.file.Files.createTempDirectory("scintillate-h2").nn
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
          val context = javax.net.ssl.SSLContext.getInstance("TLS").nn
          context.init(keyManagers.getKeyManagers, null, null)
          context

        // A client SSL context that trusts any certificate.
        def trustAllContext(): javax.net.ssl.SSLContext =
          val trustManager = new javax.net.ssl.X509TrustManager:
            type Certs = Array[java.security.cert.X509Certificate | Null] | Null
            def getAcceptedIssuers: Certs = scala.Array.empty[java.security.cert.X509Certificate | Null]
            def checkClientTrusted(chain: Certs, kind: String | Null): Unit = ()
            def checkServerTrusted(chain: Certs, kind: String | Null): Unit = ()

          val context = javax.net.ssl.SSLContext.getInstance("TLS").nn
          context.init(null, scala.Array(trustManager), java.security.SecureRandom())
          context

        // The JDK HTTP client negotiates ALPN and speaks HTTP/2 automatically
        // over TLS, so it exercises our server against a reference h2 client.
        test(m"serves an HTTP/2 request negotiated by ALPN"):
          val port = freePort()

          val server =
            SocketServer(port, ssl = serverContext()).handle(Http.Response(Http.Ok)(t"h2-secure"))

          val client = java.net.http.HttpClient.newBuilder.nn
            . sslContext(trustAllContext()).nn
            . version(java.net.http.HttpClient.Version.HTTP_2).nn
            . build.nn

          val request = java.net.http.HttpRequest.newBuilder.nn
            . uri(java.net.URI.create(s"https://localhost:$port/").nn).nn
            . build.nn

          val response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString.nn).nn
          server.cancel()
          (response.version.nn.toString.nn.tt, response.body.nn.tt)

        . assert(_ == (t"HTTP_2", t"h2-secure"))

        test(m"serves several HTTP/2 requests on one multiplexed connection"):
          val port = freePort()

          val server = SocketServer(port, ssl = serverContext()).handle:
            Http.Response(Http.Ok)(t"multiplexed")

          val client = java.net.http.HttpClient.newBuilder.nn
            . sslContext(trustAllContext()).nn
            . version(java.net.http.HttpClient.Version.HTTP_2).nn
            . build.nn

          def fetch(): Text =
            val request = java.net.http.HttpRequest.newBuilder.nn
              . uri(java.net.URI.create(s"https://localhost:$port/").nn).nn
              . build.nn

            client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString.nn).nn.body.nn.tt

          val results = List(fetch(), fetch(), fetch())
          server.cancel()
          results

        . assert(_ == List(t"multiplexed", t"multiplexed", t"multiplexed"))

        test(m"a session shares per-connection state across its streams"):
          val port = freePort()

          // The scope sets up a per-connection counter, shared by every stream
          // handled on that connection; capture checking keeps it from leaking
          // to another connection.
          val server = SocketServer(port, ssl = serverContext()).handleSession: session ?=>
            val counter = java.util.concurrent.atomic.AtomicInteger(0)

            session.handle:
              Http.Response(Http.Ok)(t"${counter.incrementAndGet}")

          val client = java.net.http.HttpClient.newBuilder.nn
            . sslContext(trustAllContext()).nn
            . version(java.net.http.HttpClient.Version.HTTP_2).nn
            . build.nn

          def fetch(): Int =
            val request = java.net.http.HttpRequest.newBuilder.nn
              . uri(java.net.URI.create(s"https://localhost:$port/").nn).nn
              . build.nn

            client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString.nn).nn.body.nn.toInt

          // Three requests reuse the one pooled HTTP/2 connection, so the shared
          // counter yields three distinct values.
          val results = List(fetch(), fetch(), fetch())
          server.cancel()
          results.sorted

        . assert(_ == List(1, 2, 3))

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
        val body = Http.Body.Flowing(() => Stream(LazyList(t"Hello".in[Data], t"World".in[Data]).iterator))
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
