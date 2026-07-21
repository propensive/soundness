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
package telekinesis

import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import logging.silentLogging
import strategies.throwUnsafely
import charEncoders.utf8Encoder

case class Address(house: Int, street: Text, city: Text, country: Text)
case class Person(name: Text, address: Address)

object Tests extends Suite(m"Telekinesis tests"):
  def run(): Unit =
    import httpBackends.virtualMachine
    import internetAccess.online

    suite(m"Response construction tests"):
      test(m"Create HTTP response with status and content type"):
        Http.Response(Http.Ok, contentType = media"application/json")(t"hello")

      . assert()

      test(m"Create HTTP response with status only"):
        Http.Response(Http.Found)

      . assert()

      test(m"Create HTTP response with content type only"):
        Http.Response(contentType = media"image/jpeg")

      . assert()

    suite(m"Response construction tests"):
      test(m"Construct a Query"):
        inline given key: ("key" is Parametric to Text) = !!
        inline given param: ("param" is Parametric to Int) = !!
        Query.make(key = t"hello world", param = 24).show

      . assert(_ == t"key=hello+world&param=24")

      case class Person(name: Text, age: Int)
      case class Couple(first: Person, second: Person)

      test(m"Construct a Query by generic derivation"):
        Person(t"Jack", 12).query.show

      . assert(_ == t"name=Jack&age=12")

      test(m"Construct a Query by partial generic derivation"):
        // This import seems to be required
        import queryParameters.arbitraryQueryParameter
        Query.make(person = Person(t"Ken", 39)).show

      . assert(_ == t"person.name=Ken&person.age=39")

      test(m"Construct a Query by nested generic derivation"):
        Couple(Person(t"Jack", 12), Person(t"Jill", 11)).query.show

      . assert(_ == t"first.name=Jack&first.age=12&second.name=Jill&second.age=11")

      val query = Query(List(t"first.name"  -> t"Jack",
                             t"first.age"   -> t"12",
                             t"second.name" -> t"Jill",
                             t"second.age"  -> t"11"))

      // Disabled pending a fix for the `?1` capture-inference leak in legerdemain's Query
      // product decoder: under honest codec capabilities, `summon[T is Decodable in Query]`
      // for a product type (and the `Dynamic` `selectDynamic` dereference below it) leaks
      // `caps.any` into the result inference variable during the inline given's reduction.
      // The `dictcaps` compiler fix resolves the dictionary form but not this inference-
      // variable form; `query.second` is additionally semantically broken (issue #1500,
      // maps `second` to `Person` yet asserts equality with a `Query`). To revisit.
      // inline given second: ("second" is Parametric to Person) = !!
      // test(m"Dereference a query")(query.second)
      // . assert(_ == Query(List(t"name" -> t"Jill", t"age"  -> t"11")))
      //
      // test(m"Decode a query"):
      //   summon[Couple is Decodable in Query].decoded(query)
      // . assert(_ == Couple(Person(t"Jack", 12), Person(t"Jill", 11)))


    suite(m"Response parsing"):
      def chunks(text: Text, size: Int): LazyList[Data] =
        val data: Data = text.in[Data]
        def go(offset: Int): LazyList[Data] =
          if offset >= data.length then LazyList() else
            val end = math.min(offset + size, data.length)
            data.slice(offset, end) #:: go(end)
        go(0)

      def bodyText(response: Http.Response): Text =
        response.body.stream.memoize.utf8

      val blockSizes = List(1, 2, 3, 7, 13, 4096)

      suite(m"Status line"):
        val fixture = t"HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nbody"
        for blockSize <- blockSizes do
          test(m"Parse 200 OK at block size $blockSize"):
            Http.Response.parse(chunks(fixture, blockSize)).status

          . assert(_ == Http.Ok)

          test(m"Parse HTTP/1.1 version at block size $blockSize"):
            Http.Response.parse(chunks(fixture, blockSize)).version

          . assert(_ == 1.1)

          test(m"Parse body at block size $blockSize"):
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_ == t"body")

        for status <- List(Http.Continue, Http.Ok, Http.NoContent, Http.MovedPermanently,
                           Http.NotModified, Http.BadRequest, Http.NotFound,
                           Http.InternalServerError, Http.ServiceUnavailable) do
          test(m"Parse status code ${status.code}"):
            val fixture = t"HTTP/1.1 ${status.code} ${status.description}\r\n\r\n"
            Http.Response.parse(chunks(fixture, 4096)).status

          . assert(_ == status)

        test(m"Parse HTTP/1.0 version"):
          val fixture = t"HTTP/1.0 200 OK\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).version

        . assert(_ == 1.0)

        test(m"Parse status with long reason phrase"):
          val fixture = t"HTTP/1.1 503 Service Temporarily Unavailable, please try again later\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).status

        . assert(_ == Http.ServiceUnavailable)

      suite(m"Headers"):
        for blockSize <- blockSizes do
          test(m"Zero headers at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\n\r\nbody"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == Nil)

          test(m"Single header at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\nX-Foo: bar\r\n\r\nbody"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == List(Http.Header(t"X-Foo", t"bar")))

          test(m"Multiple headers at block size $blockSize"):
            val fixture =
              t"HTTP/1.1 200 OK\r\nX-Foo: a\r\nX-Bar: b\r\nX-Baz: c\r\n\r\nbody"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == List(Http.Header(t"X-Foo", t"a"),
                             Http.Header(t"X-Bar", t"b"),
                             Http.Header(t"X-Baz", t"c")))

          test(m"Header value with leading whitespace at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\nX-Foo:    spacey\r\n\r\n"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == List(Http.Header(t"X-Foo", t"spacey")))

          test(m"Header value with leading tab at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\nX-Foo:\tspacey\r\n\r\n"
            Http.Response.parse(chunks(fixture, blockSize)).textHeaders

          . assert(_ == List(Http.Header(t"X-Foo", t"spacey")))

        test(m"Header with hyphenated mixed-case name"):
          val fixture = t"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).textHeaders

        . assert(_ == List(Http.Header(t"Content-Type", t"text/html")))

        test(m"Long header value"):
          val long = t"a"*500
          val fixture = t"HTTP/1.1 200 OK\r\nX-Long: $long\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).textHeaders.head.value

        . assert(_ == t"a"*500)

        test(m"Duplicate header keys preserved"):
          val fixture = t"HTTP/1.1 200 OK\r\nX-Same: a\r\nX-Same: b\r\n\r\n"
          Http.Response.parse(chunks(fixture, 4096)).textHeaders

        . assert(_ == List(Http.Header(t"X-Same", t"a"), Http.Header(t"X-Same", t"b")))

      suite(m"Body"):
        for blockSize <- blockSizes do
          test(m"Empty body at block size $blockSize"):
            val fixture = t"HTTP/1.1 204 No Content\r\nContent-Length: 0\r\n\r\n"
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_ == t"")

          test(m"Multi-byte body at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\n\r\nHello, world!"
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_ == t"Hello, world!")

          test(m"Body containing CRLF at block size $blockSize"):
            val fixture = t"HTTP/1.1 200 OK\r\n\r\nline1\r\nline2\r\n"
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_ == t"line1\r\nline2\r\n")

          test(m"Long body at block size $blockSize"):
            val payload = (0 until 1000).map(i => (i%10).toString.tt).reduce(_ + _)
            val fixture = t"HTTP/1.1 200 OK\r\n\r\n$payload"
            bodyText(Http.Response.parse(chunks(fixture, blockSize)))

          . assert(_.length == 1000)

      suite(m"Malformed input"):
        test(m"Wrong protocol prefix raises Expectation"):
          capture[HttpResponseError]:
            Http.Response.parse(chunks(t"XTTP/1.1 200 OK\r\n\r\n", 4096))
          . reason

        . assert:
          case HttpResponseError.Reason.Expectation('H', 'X') => true
          case _                                              => false

        test(m"Non-digit in status code raises Status"):
          capture[HttpResponseError]:
            Http.Response.parse(chunks(t"HTTP/1.1 2X0 OK\r\n\r\n", 4096))
          . reason

        . assert:
          case HttpResponseError.Reason.Status(_) => true
          case _                                  => false

        test(m"Status code first digit out of range raises Status"):
          capture[HttpResponseError]:
            Http.Response.parse(chunks(t"HTTP/1.1 700 Weird\r\n\r\n", 4096))
          . reason

        . assert:
          case HttpResponseError.Reason.Status(_) => true
          case _                                  => false

      test(m"Fetch a URL"):
        url"https://httpbin.org/post"
        . submit(Http.Post, contentEncoding = enc"UTF-8", accept = media"application/json")
        . apply(t"Hello world")

      . aspire()

      test(m"Fetch another URL without header names"):
        url"https://httpbin.org/post".submit(Http.Post, enc"UTF-8", accept = media"application/json")
          ( t"Hello world" )

      . aspire()

      test(m"Fetch another URL with just a method"):
        url"https://httpbin.org/put".submit(Http.Put)
          ( t"Hello world" )

      . aspire()

      test(m"Fetch another URL with defaults"):
        url"https://httpbin.org/post".submit()(t"Hello world")

      . aspire()

    suite(m"Request parsing"):
      def chunks(text: Text, size: Int): LazyList[Data] =
        val data: Data = text.in[Data]
        def go(offset: Int): LazyList[Data] =
          if offset >= data.length then LazyList() else
            val end = math.min(offset + size, data.length)
            data.slice(offset, end) #:: go(end)
        go(0)

      def bodyText(request: Http.Request^): Text = request.body().memoize.utf8

      val blockSizes = List(1, 2, 3, 7, 13, 4096)
      val fixture = t"GET /path?q=1 HTTP/1.1\r\nHost: example.com\r\nX-Foo: bar\r\n\r\nbody"

      for blockSize <- blockSizes do
        test(m"Parse method at block size $blockSize"):
          Http.Request.parse(chunks(fixture, blockSize)).method

        . assert(_ == Http.Get)

        test(m"Parse target at block size $blockSize"):
          Http.Request.parse(chunks(fixture, blockSize)).target

        . assert(_ == t"/path?q=1")

        test(m"Parse version at block size $blockSize"):
          Http.Request.parse(chunks(fixture, blockSize)).version

        . assert(_ == 1.1)

        test(m"Parse host at block size $blockSize"):
          Http.Request.parse(chunks(fixture, blockSize)).host.show

        . assert(_ == t"example.com")

        test(m"Parse headers at block size $blockSize"):
          Http.Request.parse(chunks(fixture, blockSize)).textHeaders

        . assert(_ == List(Http.Header(t"Host", t"example.com"), Http.Header(t"X-Foo", t"bar")))

        test(m"Parse body at block size $blockSize"):
          bodyText(Http.Request.parse(chunks(fixture, blockSize)))

        . assert(_ == t"body")

      val methods: List[Http.Method] =
        List(Http.Get, Http.Head, Http.Post, Http.Put, Http.Delete, Http.Patch, Http.Options,
            Http.Trace, Http.Connect)

      for method <- methods do
        test(m"Parse method ${method.show}"):
          val fixture = t"${method.show} / HTTP/1.1\r\nHost: example.com\r\n\r\n"
          Http.Request.parse(chunks(fixture, 4096)).method

        . assert(_ == method)

      test(m"Read payload flag through erased Http.Method type"):
        val method: Http.Method = Http.Post
        method.payload

      . assert(_ == true)

      test(m"Read payload flag for a bodyless method through erased type"):
        val method: Http.Method = Http.Get
        method.payload

      . assert(_ == false)

      test(m"Payload flag is available at the type level"):
        valueOf[Http.Post.Payload]

      . assert(_ == true)

      test(m"Parse HTTP/1.0 version"):
        val fixture = t"GET / HTTP/1.0\r\nHost: example.com\r\n\r\n"
        Http.Request.parse(chunks(fixture, 4096)).version

      . assert(_ == 1.0)

      test(m"Host header with port has the port stripped"):
        val fixture = t"GET / HTTP/1.1\r\nHost: example.com:8080\r\n\r\n"
        Http.Request.parse(chunks(fixture, 4096)).host.show

      . assert(_ == t"example.com")

      test(m"Missing Host header raises Host reason"):
        capture[HttpRequestError]:
          // bound so the fresh Request does not escape the capture block as its result
          val request = Http.Request.parse(chunks(t"GET / HTTP/1.1\r\n\r\n", 4096))
          ()
        . reason

      . assert:
        case HttpRequestError.Reason.Host(_) => true
        case _                               => false

      for blockSize <- blockSizes do
        test(m"fixedBody reads exactly N bytes at block size $blockSize"):
          val cursor = Cursor[Data](chunks(t"hello world", blockSize).iterator)
          Http.Request.fixedBody(cursor, 5).memoize.utf8

        . assert(_ == t"hello")

        test(m"fixedBody leaves the cursor after the body at block size $blockSize"):
          val cursor = Cursor[Data](chunks(t"hello world", blockSize).iterator)
          Http.Request.fixedBody(cursor, 5).memoize
          cursor.remainder.read[Data].utf8

        . assert(_ == t" world")

        test(m"chunkedBody decodes chunks at block size $blockSize"):
          val fixture = t"5\r\nhello\r\n6\r\n world\r\n0\r\n\r\n"
          val cursor = Cursor[Data](chunks(fixture, blockSize).iterator)
          Http.Request.chunkedBody(cursor).memoize.utf8

        . assert(_ == t"hello world")

        test(m"chunkedBody leaves the cursor after the body at block size $blockSize"):
          val fixture = t"3\r\nabc\r\n0\r\n\r\nNEXT"
          val cursor = Cursor[Data](chunks(fixture, blockSize).iterator)
          Http.Request.chunkedBody(cursor).memoize
          cursor.remainder.read[Data].utf8

        . assert(_ == t"NEXT")

    suite(m"Response serialization"):
      def chunks(text: Text, size: Int): LazyList[Data] =
        val data: Data = text.in[Data]
        def go(offset: Int): LazyList[Data] =
          if offset >= data.length then LazyList() else
            val end = math.min(offset + size, data.length)
            data.slice(offset, end) #:: go(end)
        go(0)

      def bodyText(response: Http.Response): Text = response.body.stream.memoize.utf8

      def wire(response: Http.Response, includeBody: Boolean = true): Text =
        Http.Response.serialize(response, includeBody).memoize.utf8

      val blockSizes = List(1, 2, 3, 7, 13, 4096)

      test(m"Status line carries code and reason phrase"):
        wire(Http.Response(Http.NotFound)()).cut(t"\r\n").head

      . assert(_ == t"HTTP/1.1 404 Not Found")

      for blockSize <- blockSizes do
        test(m"Round-trip a fixed body at block size $blockSize"):
          val response = Http.Response(Http.Ok)(t"hello world")
          val parsed = Http.Response.parse(chunks(wire(response), blockSize))
          (parsed.status, bodyText(parsed))

        . assert(_ == (Http.Ok, t"hello world"))

      test(m"Fixed body sets a correct Content-Length"):
        wire(Http.Response(Http.Ok)(t"hello")).contains(t"content-length: 5")

      . assert(_ == true)

      test(m"Empty body sets Content-Length 0"):
        wire(Http.Response(Http.NoContent)()).contains(t"content-length: 0")

      . assert(_ == true)

      test(m"Streaming body is framed with chunked transfer-encoding"):
        val body = Http.Body.Flowing(() => Stream(LazyList(t"Hello".in[Data], t"World".in[Data]).iterator))
        wire(Http.Response(Http.Ok)(body))

      . assert: text =>
          text.contains(t"transfer-encoding: chunked")
          && text.contains(t"5\r\nHello\r\n")
          && text.contains(t"5\r\nWorld\r\n")
          && text.ends(t"0\r\n\r\n")

      test(m"Streaming body skips zero-length blocks"):
        val body = Http.Body.Flowing(() => Stream(LazyList(t"ab".in[Data], t"".in[Data], t"cd".in[Data]).iterator))
        wire(Http.Response(Http.Ok)(body))

      . assert(_.contains(t"2\r\nab\r\n2\r\ncd\r\n"))

      test(m"HEAD response omits the body but keeps headers"):
        val response = Http.Response(Http.Ok)(t"hello")
        val text = wire(response, includeBody = false)
        text.ends(t"\r\n\r\n") && !text.contains(t"hello") && text.contains(t"content-length: 5")

      . assert(_ == true)

    // The native TCP backend, driven against a loopback JDK server: requests
    // travel over a real socket through telekinesis's own wire codec, with no
    // `java.net.http` involvement on the client side.
    suite(m"Native TCP backend"):
      import java.net.InetSocketAddress
      import com.sun.net.httpserver as csnh

      val server = csnh.HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0).nn

      server.createContext("/fixed", { exchange =>
        val data = "Hello, native!".getBytes("UTF-8").nn
        exchange.nn.getResponseHeaders.nn.set("Content-Type", "text/plain")
        exchange.nn.sendResponseHeaders(200, data.length)
        exchange.nn.getResponseBody.nn.write(data)
        exchange.nn.close() })

      server.createContext("/chunked", { exchange =>
        exchange.nn.sendResponseHeaders(200, 0)
        val out = exchange.nn.getResponseBody.nn
        out.write("Hello".getBytes("UTF-8").nn)
        out.flush()
        out.write("World".getBytes("UTF-8").nn)
        exchange.nn.close() })

      server.createContext("/echo", { exchange =>
        val body = exchange.nn.getRequestBody.nn.readAllBytes.nn
        exchange.nn.sendResponseHeaders(200, body.length)
        exchange.nn.getResponseBody.nn.write(body)
        exchange.nn.close() })

      // Answers with the client's ephemeral port: two requests over a reused
      // (kept-alive) connection see the same value.
      server.createContext("/port", { exchange =>
        val data = String.valueOf(exchange.nn.getRemoteAddress.nn.getPort).nn.getBytes("UTF-8").nn
        exchange.nn.sendResponseHeaders(200, data.length)
        exchange.nn.getResponseBody.nn.write(data)
        exchange.nn.close() })

      server.start()
      val port: Int = server.getAddress.nn.getPort

      import socketBackends.virtualMachine

      val backend: Http.Backend = httpBackends.native

      def fetchNative(target: Text, method: Http.Method = Http.Get, body: Text = t"")
      :   Http.Response =

        backend.request
          ( t"http://127.0.0.1:$port$target",
            method,
            Nil,
            () => if body == t"" then Http.emptyBody() else body.in[Data].stream )

      test(m"Fetch a fixed-length response over a raw TCP socket"):
        val response = fetchNative(t"/fixed")
        (response.status, response.body.stream.memoize.utf8)

      . assert(_ == (Http.Ok, t"Hello, native!"))

      test(m"Fetch a chunked response over a raw TCP socket"):
        fetchNative(t"/chunked").body.stream.memoize.utf8

      . assert(_ == t"HelloWorld")

      test(m"An error status is conveyed"):
        fetchNative(t"/missing").status

      . assert(_ == Http.NotFound)

      test(m"A response to HEAD has no body"):
        fetchNative(t"/fixed", Http.Head).body

      . assert(_ == Http.Body.Empty)

      test(m"A request body is transmitted"):
        fetchNative(t"/echo", Http.Post, t"ping-pong").body.stream.memoize.utf8

      . assert(_ == t"ping-pong")

      test(m"Sequential requests reuse a kept-alive connection"):
        val first = fetchNative(t"/port").body.stream.memoize.utf8
        val second = fetchNative(t"/port").body.stream.memoize.utf8
        first == second

      . assert(_ == true)

      test(m"A session pins one connection across its fetches"):
        val target = t"http://127.0.0.1:$port".as[HttpUrl]

        target.session: session ?=>
          val request = Http.Request(Http.Get, 1.1, t"127.0.0.1".as[Host], t"/port", Nil,
              () => Http.emptyBody())

          val first = session.fetch(request).body.stream.memoize.utf8
          val second = session.fetch(request).body.stream.memoize.utf8
          first == second

      . assert(_ == true)

      // A session survives an unconsumed streaming response: the next fetch
      // drains the previous body's remainder to reach the response boundary.
      test(m"An unconsumed response does not derail the next fetch"):
        val target = t"http://127.0.0.1:$port".as[HttpUrl]

        target.session: session ?=>
          val request = Http.Request(Http.Get, 1.1, t"127.0.0.1".as[Host], t"/fixed", Nil,
              () => Http.emptyBody())

          session.fetch(request) // never consumed
          session.fetch(request).body.stream.memoize.utf8

      . assert(_ == t"Hello, native!")

      server.stop(0)

    // The `https` path of the native backend: ALPN negotiation during the TLS
    // handshake selects the HTTP/2 or HTTP/1.1 driver over the same socket.
    suite(m"Native TLS backend"):
      import java.net.InetSocketAddress
      import com.sun.net.httpserver as csnh

      // A throwaway self-signed certificate, generated with the JDK's keytool.
      val dir = java.nio.file.Files.createTempDirectory("telekinesis-tls").nn
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

      // A client that trusts any certificate, so the self-signed one is accepted.
      val trustManager = new javax.net.ssl.X509TrustManager:
        type Certs = Array[java.security.cert.X509Certificate | Null] | Null
        def getAcceptedIssuers: Certs = scala.Array.empty[java.security.cert.X509Certificate | Null]
        def checkClientTrusted(chain: Certs, kind: String | Null): Unit = ()
        def checkServerTrusted(chain: Certs, kind: String | Null): Unit = ()

      val clientContext = javax.net.ssl.SSLContext.getInstance("TLS").nn
      clientContext.init(null, scala.Array(trustManager), java.security.SecureRandom())

      given Tls = Tls(clientContext, verify = false)

      import socketBackends.virtualMachine

      val backend: Http.Backend = httpBackends.native

      test(m"An ALPN-less server falls back to HTTP/1.1 over TLS"):
        val server = csnh.HttpsServer.create(InetSocketAddress("127.0.0.1", 0), 0).nn
        server.setHttpsConfigurator(csnh.HttpsConfigurator(serverContext))

        server.createContext("/tls", { exchange =>
          val data = "secure-native".getBytes("UTF-8").nn
          exchange.nn.sendResponseHeaders(200, data.length)
          exchange.nn.getResponseBody.nn.write(data)
          exchange.nn.close() })

        server.start()
        val port = server.getAddress.nn.getPort

        val response =
          backend.request(t"https://localhost:$port/tls", Http.Get, Nil, () => Http.emptyBody())

        server.stop(0)
        (response.status, response.body.stream.memoize.utf8)

      . assert(_ == (Http.Ok, t"secure-native"))

      import cordillera.{FrameReader, Hpack, HpackEntry}
      import cordillera.Http2.Frame

      // A minimal frame-level HTTP/2 server for a single TLS connection, with
      // ALPN pinned to `h2`: exchange SETTINGS, then answer each request
      // HEADERS with a 200 and a DATA frame.
      def h2Server(): javax.net.ssl.SSLServerSocket =
        val serverSocket =
          serverContext.getServerSocketFactory.nn.createServerSocket(0).nn
          . asInstanceOf[javax.net.ssl.SSLServerSocket]

        val params = serverSocket.getSSLParameters.nn
        params.setApplicationProtocols(scala.Array[String | Null]("h2"))
        serverSocket.setSSLParameters(params)
        serverSocket.setSoTimeout(10000)

        java.lang.Thread.ofVirtual().nn.start({ () =>
          try
            val socket = serverSocket.accept().nn
            val in = socket.getInputStream.nn
            val out = socket.getOutputStream.nn

            def write(frame: Frame): Unit =
              out.write(frame.serialize.mutable(using Unsafe))
              out.flush()

            write(Frame.Settings(Nil, ack = false))
            in.readNBytes(24) // the client connection preface

            unsafely:
              val source = summon[java.io.InputStream is Streamable by Data over Credit].stream(in)
              val reader = FrameReader(source)
              val hpack = Hpack()
              var continue = true

              while continue do (reader.next(): @unchecked) match
                case Unset        => continue = false
                case frame: Frame => frame match
                  case Frame.Settings(_, false) =>
                    write(Frame.Settings(Nil, ack = true))

                  case Frame.Headers(id, _, _, _) =>
                    val head = hpack.encode(List(HpackEntry(t":status", t"200")))
                    write(Frame.Headers(id, head, false, true))
                    write(Frame.Data(id, t"h2-native".in[Data], true))

                  case _ => ()
          catch case error: Exception => () })

        serverSocket

      test(m"ALPN selects h2 and the request is served over HTTP/2"):
        val serverSocket = h2Server()
        val port = serverSocket.getLocalPort

        val response =
          backend.request(t"https://localhost:$port/", Http.Get, Nil, () => Http.emptyBody())

        serverSocket.close()
        (response.status, response.body.stream.memoize.utf8)

      . assert(_ == (Http.Ok, t"h2-native"))

      test(m"A TLS session multiplexes several fetches on one h2 connection"):
        val serverSocket = h2Server()
        val port = serverSocket.getLocalPort
        val target = t"https://localhost:$port".as[HttpUrl]

        val texts = target.session: session ?=>
          val request = Http.Request(Http.Get, 1.1, t"localhost".as[Host], t"/", Nil,
              () => Http.emptyBody())

          List(session.fetch(request), session.fetch(request)).map: response =>
            response.body.stream.memoize.utf8

        serverSocket.close()
        texts

      . assert(_ == List(t"h2-native", t"h2-native"))

    // The suites below depend on external services (httpbin.org, badssl.com)
    // whose availability and configuration fluctuate; they are aspirational
    // pending #676, which replaces them with assertions about telekinesis's
    // own contextual TLS acceptance criteria.
    suite(m"Redirect handling"):
      test(m"Follow a relative redirect chain by default"):
        url"https://httpbin.org/redirect/3".fetch().status

      . aspire(_ == Http.Ok)

      test(m"Follow an absolute redirect chain by default"):
        url"https://httpbin.org/absolute-redirect/3".fetch().status

      . aspire(_ == Http.Ok)

      test(m"Strict mode surfaces the 3xx as HttpError"):
        import httpRedirections.doNotFollowRedirects
        capture[HttpError](url"https://httpbin.org/redirect/1".fetch().receive[Text]).status

      . aspire(_ == Http.Found)

      test(m"HttpRedirection caps the redirect chain"):
        given HttpRedirection = HttpRedirection(1)
        capture[HttpError](url"https://httpbin.org/redirect/3".fetch().receive[Text]).status

      . aspire(_ == Http.Found)

    suite(m"DNS Errors"):
      test(m"Nonexistent DNS"):
        capture[ConnectError](url"http://www.asorbkxoreuatoehudncak.com/".fetch())

      . aspire(_ == ConnectError(ConnectError.Reason.Dns))

    suite(m"badssl.com SSL certificate tests"):
      import ConnectError.Reason.*, ConnectError.Reason.Ssl.Reason.*

      suite(m"Certificate Validation"):
        test(m"Expired SSL certificate"):
          capture[ConnectError](url"https://expired.badssl.com/".fetch())

        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"SSL certificate with wrong host"):
          capture[ConnectError](url"https://wrong.host.badssl.com/".fetch())

        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"Self-signed certificate"):
          capture[ConnectError](url"https://self-signed.badssl.com/".fetch())

        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"SSL certificate with untrusted root"):
          capture[ConnectError](url"https://untrusted-root.badssl.com/".fetch())

        . aspire(_ == ConnectError(Ssl(Handshake)))

      suite(m"Interception Certificates"):
        test(m"superfish")(capture[ConnectError](url"https://superfish.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"edellroot")(capture[ConnectError](url"https://edellroot.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"dsdtestprovider")(capture[ConnectError](url"https://dsdtestprovider.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"preact-cli")(capture[ConnectError](url"https://preact-cli.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"webpack-dev-server")(capture[ConnectError](url"https://webpack-dev-server.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

      suite(m"Broken cryptography"):
        test(m"rc4")(capture[ConnectError](url"https://rc4.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"rc4-md5")(capture[ConnectError](url"https://rc4-md5.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"dh480")(capture[ConnectError](url"https://dh480.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"dh512")(capture[ConnectError](url"https://dh512.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"dh1024")(capture[ConnectError](url"https://dh1024.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"null")(capture[ConnectError](url"https://null.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

      suite(m"Legacy cryptography"):
        test(m"tls-v1-0")(capture[ConnectError](url"https://tls-v1-0.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"tls-v1-1")(capture[ConnectError](url"https://tls-v1-1.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"cbc")(capture[ConnectError](url"https://cbc.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"3des")(capture[ConnectError](url"https://3des.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"dh2048")(capture[ConnectError](url"https://dh2048.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

      suite(m"Domain Security Policies"):
        test(m"revoked")(capture[ConnectError](url"https://revoked.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"pinning-test")(capture[ConnectError](url"https://pinning-test.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

        test(m"no-sct")(capture[ConnectError](url"https://no-sct.badssl.com/".fetch()))
        . aspire(_ == ConnectError(Ssl(Handshake)))

    suite(m"TLS acceptance materialization"):
      test(m"strict acceptance verifies hostnames"):
        val (_, parameters) = TlsAcceptance().materialize()
        parameters.getEndpointIdentificationAlgorithm == "HTTPS"
      . assert(identity)

      test(m"relaxed acceptance disables endpoint identification"):
        import crypto.permitUntrustedCertificates
        val (_, parameters) = TlsAcceptance().permitHostnameMismatch.materialize()
        parameters.getEndpointIdentificationAlgorithm == null
      . assert(identity)

      test(m"an expired-tolerant acceptance materializes a custom context"):
        import crypto.permitUntrustedCertificates
        val (context, _) = TlsAcceptance().permitExpired.materialize()
        context != javax.net.ssl.SSLContext.getDefault
      . assert(identity)

      test(m"protocol restriction is applied to parameters"):
        val acceptance = TlsAcceptance(versions = List(Trust.Version.Tls13))
        val (_, parameters) = acceptance.materialize()
        parameters.getProtocols.nn.to(List).map(_.nn.tt)
      . assert(_ == List(t"TLSv1.3"))

      test(m"an acceptance bridges to a Tls carrying its policy"):
        val acceptance = TlsAcceptance(versions = List(Trust.Version.Tls13))
        val tls = acceptance.tls(List(t"h2"))
        (tls.verify, tls.protocols, tls.versions, tls.context.present)
      . assert(_ == (true, List(t"h2"), List(t"TLSv1.3"), true))

      test(m"a relaxed acceptance bridges with verification off"):
        import crypto.permitUntrustedCertificates
        TlsAcceptance().permitHostnameMismatch.tls().verify
      . assert(_ == false)

    suite(m"Certificate validation with relaxed acceptance"):
      import crypto.permitUntrustedCertificates, crypto.permitUncheckedRevocation

      test(m"expired certificate accepted under permitExpired"):
        given TlsAcceptance = TlsAcceptance().permitExpired
        url"https://expired.badssl.com/".fetch().status
      . aspire(_ == Http.Ok)

      test(m"self-signed certificate accepted under permitSelfSigned"):
        given TlsAcceptance = TlsAcceptance().permitSelfSigned
        url"https://self-signed.badssl.com/".fetch().status
      . aspire(_ == Http.Ok)

      test(m"untrusted root accepted under permitSelfSigned"):
        given TlsAcceptance = TlsAcceptance().permitSelfSigned
        url"https://untrusted-root.badssl.com/".fetch().status
      . aspire(_ == Http.Ok)

      test(m"wrong-host certificate accepted under permitHostnameMismatch"):
        given TlsAcceptance = TlsAcceptance().permitHostnameMismatch
        url"https://wrong.host.badssl.com/".fetch().status
      . aspire(_ == Http.Ok)

      test(m"revoked certificate accepted under permitRevoked"):
        given TlsAcceptance = TlsAcceptance().permitRevoked
        url"https://revoked.badssl.com/".fetch().status
      . aspire(_ == Http.Ok)
