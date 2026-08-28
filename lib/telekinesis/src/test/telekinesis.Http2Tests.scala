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


import strategies.throwUnsafely
import Http2.{Client, Connection, Endpoint, ErrorCode, Flags, Frame, FrameType, Setting,
    SettingId}

object Http2Tests extends Suite(m"Telekinesis HTTP/2 Tests"):
  def run(): Unit =
    def bytes(hex: Text): Data =
      Array.from(hex.s.grouped(2).map(Integer.parseInt(_, 16).toByte).to(List))

    def hex(data: Data): Text =
      data.to[List].map(b => String.format("%02x", java.lang.Integer.valueOf(b & 0xff)).nn).stdlib.mkString.tt

    def ascii(text: Text): Data = Array.unsafeFrozen(text.s.getBytes("US-ASCII").nn)

    suite(m"Huffman (RFC 7541 Appendix C)"):
      // C.4.1: "www.example.com" → Huffman
      test(m"encode www.example.com"):
        hex(Huffman.encode(ascii(t"www.example.com")))
      . assert(_ == t"f1e3c2e5f23a6ba0ab90f4ff")

      test(m"decode www.example.com"):
        Huffman.decode(bytes(t"f1e3c2e5f23a6ba0ab90f4ff")).to[List]
      . assert(_ == ascii(t"www.example.com").to[List])

      // C.4.2: "no-cache"
      test(m"encode no-cache"):
        hex(Huffman.encode(ascii(t"no-cache")))
      . assert(_ == t"a8eb10649cbf")

      test(m"decode no-cache"):
        Huffman.decode(bytes(t"a8eb10649cbf")).to[List]
      . assert(_ == ascii(t"no-cache").to[List])

      // C.4.3: "custom-key" and "custom-value"
      test(m"encode custom-key"):
        hex(Huffman.encode(ascii(t"custom-key")))
      . assert(_ == t"25a849e95ba97d7f")

      test(m"encode custom-value"):
        hex(Huffman.encode(ascii(t"custom-value")))
      . assert(_ == t"25a849e95bb8e8b4bf")

      // C.6.1: "302" status, "private", a date, and a URL — exercises digits + EOS pad
      test(m"encode 302"):
        hex(Huffman.encode(ascii(t"302")))
      . assert(_ == t"6402")

      test(m"encode private"):
        hex(Huffman.encode(ascii(t"private")))
      . assert(_ == t"aec3771a4b")

      test(m"decode private"):
        Huffman.decode(bytes(t"aec3771a4b")).to[List]
      . assert(_ == ascii(t"private").to[List])

      test(m"round-trip a long date string"):
        val date = ascii(t"Mon, 21 Oct 2013 20:13:21 GMT")
        Huffman.decode(Huffman.encode(date)).to[List] == date.to[List]
      . assert(_ == true)

      test(m"round-trip all 256 byte values"):
        val every = Array.from((0 until 256).map(_.toByte))
        Huffman.decode(Huffman.encode(every)).to[List] == every.to[List]
      . assert(_ == true)

    suite(m"HPACK decode (RFC 7541 Appendix C.3 — without Huffman)"):
      // A single decoder fed the three successive request header blocks from C.3,
      // sharing one dynamic table across requests (the point of the example).
      val hpack = Hpack()

      def fields(hex: Text): List[(Text, Text)] =
        hpack.decode(bytes(hex)).map(e => (e.name, e.value))

      test(m"C.3.1 first request"):
        fields(t"828684410f7777772e6578616d706c652e636f6d")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"http"), (t":path", t"/"),
          (t":authority", t"www.example.com")))

      test(m"C.3.2 second request (uses dynamic table + new header)"):
        fields(t"828684be58086e6f2d6361636865")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"http"), (t":path", t"/"),
          (t":authority", t"www.example.com"), (t"cache-control", t"no-cache")))

      test(m"C.3.3 third request (custom header)"):
        fields(t"828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"https"), (t":path", t"/index.html"),
          (t":authority", t"www.example.com"), (t"custom-key", t"custom-value")))

    suite(m"HPACK decode (RFC 7541 Appendix C.4 — with Huffman)"):
      val hpack = Hpack()

      def fields(hex: Text): List[(Text, Text)] =
        hpack.decode(bytes(hex)).map(e => (e.name, e.value))

      test(m"C.4.1 first request, Huffman-coded authority"):
        fields(t"828684418cf1e3c2e5f23a6ba0ab90f4ff")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"http"), (t":path", t"/"),
          (t":authority", t"www.example.com")))

      test(m"C.4.2 second request, Huffman-coded no-cache"):
        fields(t"828684be5886a8eb10649cbf")
      . assert(_ == List((t":method", t"GET"), (t":scheme", t"http"), (t":path", t"/"),
          (t":authority", t"www.example.com"), (t"cache-control", t"no-cache")))

    suite(m"HPACK encode → decode round-trip"):
      val headers = List(
        Hpack.Entry(t":method", t"POST"),
        Hpack.Entry(t":scheme", t"http"),
        Hpack.Entry(t":path", t"/foo/bar"),
        Hpack.Entry(t":authority", t"unix"),
        Hpack.Entry(t"content-type", t"application/grpc"),
        Hpack.Entry(t"te", t"trailers"))

      test(m"a request's pseudo-headers + headers survive a round-trip"):
        val encoded = Hpack().encode(headers)
        Hpack().decode(encoded).stdlib.map(e => (e.name, e.value))
      . assert(_ == headers.map(e => (e.name, e.value)))

    suite(m"Frame codec — golden bytes"):
      test(m"SETTINGS ack serialises to the canonical empty-ack frame"):
        hex(Frame.Settings(Nil, ack = true).serialize)
      . assert(_ == t"000000040100000000")

      test(m"empty SETTINGS (non-ack) is a zero-length frame"):
        hex(Frame.Settings(Nil, ack = false).serialize)
      . assert(_ == t"000000040000000000")

      test(m"WINDOW_UPDATE on stream 1 with increment 65535"):
        hex(Frame.WindowUpdate(1, 65535).serialize)
      . assert(_ == t"0000040800000000010000ffff")

      test(m"a DATA frame's 9-byte header carries length, type, flags and stream"):
        hex(Frame.Data(3, ascii(t"hi"), endStream = true).serialize)
      . assert(_ == t"00000200010000000368 69".sub(t" ", t""))

      test(m"PING ack echoes its 8 opaque bytes"):
        hex(Frame.Ping(bytes(t"0102030405060708"), ack = true).serialize)
      . assert(_ == t"000008060100000000 0102030405060708".sub(t" ", t""))

    suite(m"Frame codec — round-trips"):
      def roundTrip(frame: Frame): Frame raises Http2.Error =
        Frame.decode(frame.serialize, 0)(0)

      test(m"SETTINGS with parameters round-trips"):
        val settings = List(Setting(SettingId.InitialWindowSize.id, 65535),
            Setting(SettingId.MaxFrameSize.id, 16384))
        roundTrip(Frame.Settings(settings, ack = false)) == Frame.Settings(settings, false)
      . assert(_ == true)

      test(m"DATA round-trips with payload and END_STREAM"):
        roundTrip(Frame.Data(7, ascii(t"hello"), endStream = true)) match
          case Frame.Data(id, p, end) => (id, p.to[List], end) == (7, ascii(t"hello").to[List], true)
          case _                      => false
      . assert(_ == true)

      test(m"HEADERS round-trips its block + flags"):
        roundTrip(Frame.Headers(1, ascii(t"block"), endStream = false, endHeaders = true)) match
          case Frame.Headers(id, b, es, eh) => (id, b.to[List], es, eh)
              == (1, ascii(t"block").to[List], false, true)
          case _                            => false
      . assert(_ == true)

      test(m"GOAWAY round-trips last-stream-id and error code"):
        roundTrip(Frame.GoAway(5, ErrorCode.ProtocolError.code, ascii(t""))) match
          case Frame.GoAway(last, code, _) => (last, code) == (5, 0x1L)
          case _                           => false
      . assert(_ == true)

      test(m"WINDOW_UPDATE round-trips its increment"):
        roundTrip(Frame.WindowUpdate(3, 1000)) == Frame.WindowUpdate(3, 1000)
      . assert(_ == true)

      test(m"a padded DATA frame decodes to its unpadded payload"):
        // length=5: padLength byte (0x02) + ("hi": String) + 2 pad bytes; PADDED flag = 0x08
        val padded = bytes(t"0000050008000000030268690000")
        Frame.decode(padded, 0)(0) match
          case Frame.Data(_, p, _) => p.to[List] == ascii(t"hi").to[List]
          case _                   => false
      . assert(_ == true)

    suite(m"End-to-end over an in-memory Duplex (the whole stack)"):
      import threading.virtualThreading
      import probates.cancelProbate

      // An in-memory `Duplex` pair: bytes written to one side surface on the other's
      // stream. Backed by `Spool`s so reads block until data arrives, like a socket.
      def pair(): (Duplex, Duplex) = Duplex.pair()

      // A minimal in-process HTTP/2 server on the given duplex side. It sends its own
      // SETTINGS (so the client's handshake completes), reads the client's preface +
      // frames, and on the request HEADERS replies with response HEADERS (200 +
      // content-type), a DATA frame, and trailing HEADERS carrying a grpc-status
      // trailer. Returned as a Daemon so the caller can cancel it.
      def runServer(serverSide: Duplex)(using Monitor, Probate): Daemon = daemon:
        safely:
          serverSide.send(zephyrine.Stream(Frame.Settings(Nil, ack = false).serialize))

          // Skip the 24-byte client connection preface before frame-parsing: consume
          // exactly the preface; anything after it stays in the endpoint's window.
          val source = serverSide.source
          var skipped = 0

          while skipped < 24 do source.refill(zephyrine.Credit(4096)) match
            case count: Int =>
              if count > 0 then
                val take = count.min(24 - skipped)
                source.skip(take)
                skipped += take

            case _ =>
              skipped = 24

          val reader = FrameReader(source)
          val hpack = Hpack()
          var continue = true

          while continue do (reader.next(): @unchecked) match
            case Unset        => continue = false
            case f: Frame     => f match
              case Frame.Settings(_, false) =>
                serverSide.send(zephyrine.Stream(Frame.Settings(Nil, ack = true).serialize))

              case Frame.Headers(id, _, _, _) =>
                val respHeaders = hpack.encode(List(Hpack.Entry(t":status", t"200"),
                    Hpack.Entry(t"content-type", t"application/grpc")))

                val trailers = hpack.encode(List(Hpack.Entry(t"grpc-status", t"0")))
                serverSide.send(zephyrine.Stream(Frame.Headers(id, respHeaders, false, true).serialize))
                serverSide.send(zephyrine.Stream(Frame.Data(id, ascii(t"pong"), false).serialize))
                serverSide.send(zephyrine.Stream(Frame.Headers(id, trailers, true, true).serialize))

              case _ => ()

      test(m"a unary request round-trips status, body and trailers"):
        supervise:
          val (clientSide, serverSide) = pair()
          val server = runServer(serverSide)
          val connection = Http2.Connection(clientSide)
          connection.start()

          val request = Http.Request(Http.Post, 2.0, unsafely(t"unix".as[Host]),
              t"/echo.Service/Call", Nil, () => Stream(ascii(t"ping")))

          val (stream, response) = connection.fetch(request, t"http", t"unix")
          val bodyText = ascii(t"pong").to[List] == response.body.stream.memoize.to[List]
          val statusCode = response.status.code
          val grpcStatus = stream.trailers.await().stdlib.find(_.name == t"grpc-status").map(_.value)
          server.cancel()
          (statusCode, bodyText, grpcStatus.getOrElse(t"?"))
      . assert(_ == (200, true, t"0"))

      test(m"the Http.Client given resolves and drives a request over h2c"):
        supervise:
          val (clientSide, serverSide) = pair()
          runServer(serverSide)

          import Http2.Client.http2
          import logging.silentLogging

          // A `Connectable` whose connect() hands back the client side of the pair —
          // lets the real `Http.Client` given (which calls `target.connect()`) run
          // against the loopback without a socket.
          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          // Summon the HTTP/2 client given exactly as telekinesis's fetch machinery
          // would, and invoke its `request` — verifying it captures the ambient
          // Monitor/Probate and produces a telekinesis `Http.Response`.
          val client = summon[Http.Client onto Http2.Endpoint[Loopback]]
          val endpoint = Http2.Endpoint(Loopback(clientSide), t"unix")

          val request = Http.Request(Http.Get, 2.0, unsafely(t"unix".as[Host]),
              t"/echo.Service/Call", Nil, () => Iterator.empty[Data].stream)

          client.request(request, endpoint).status.code
      . assert(_ == 200)

      test(m"the server role serves the client role over an in-memory pair"):
        supervise:
          val (clientSide, serverSide) = pair()
          val server = Http2.ServerConnection(serverSide)

          // The serve loop: one handler per accepted stream, echoing the
          // decoded request's method and target through a real `Http.Response`.
          // The connection crosses into the daemon as a neutral carrier: a
          // daemon body may not capture a capability.
          val serverRef: AnyRef = server.asInstanceOf[AnyRef]

          daemon:
            safely:
              val server0 = serverRef.asInstanceOf[Http2.ServerConnection]

              server0.accepted.stream.records.each: stream =>
                unsafely:
                  val entries = stream.headers.await()
                  val request = PseudoHeaders.requestOf(entries, () => Http.emptyBody())

                  // A `Data` (byte) body selects the `Fixed` servable, so the
                  // response body is a plain fixed stream.
                  val payload: Data = ascii(t"echo:${request.method.show}:${request.target}")
                  val response = Http.Response(Http.Ok)(payload)

                  server0.sendHeaders(stream.id, PseudoHeaders.entries(response), false)
                  server0.sendData(stream.id, response.body.stream.memoize, true)

          val client = Http2.Connection(clientSide)
          val serverStarted = scala.caps.unsafe.unsafeAssumeSeparate(async(server.start()))
          client.start()
          scala.caps.unsafe.unsafeAssumeSeparate:
            serverStarted.await()

          val request = Http.Request(Http.Get, 2.0, unsafely(t"unix".as[Host]), t"/hello", Nil,
              () => Http.emptyBody())

          val (_, response) = client.fetch(request, t"http", t"unix")
          val body = response.body.stream.memoize.utf8
          client.close()
          // Stop the server's reader/writer and the serve loop (via
          // `accepted.stop()`), so the enclosing `supervise` can return.
          server.close()
          body

      . assert(_ == t"echo:GET:/hello")

      test(m"the server role emits response trailers the client reads"):
        supervise:
          val (clientSide, serverSide) = pair()
          val server = Http2.ServerConnection(serverSide)
          val serverRef: AnyRef = server.asInstanceOf[AnyRef]

          daemon:
            safely:
              val server0 = serverRef.asInstanceOf[Http2.ServerConnection]

              server0.accepted.stream.records.each: stream =>
                unsafely:
                  stream.headers.await()
                  // HEADERS + DATA left open, then a trailing HEADERS block
                  // (gRPC status) closes the stream.
                  val head = List(Hpack.Entry(t":status", t"200"))
                  server0.sendHeaders(stream.id, head, endStream = false)
                  server0.sendData(stream.id, ascii(t"body"), endStream = false)
                  server0.sendTrailers(stream.id, List(Hpack.Entry(t"grpc-status", t"0")))

          val client = Http2.Connection(clientSide)
          val serverStarted = scala.caps.unsafe.unsafeAssumeSeparate(async(server.start()))
          client.start()
          scala.caps.unsafe.unsafeAssumeSeparate:
            serverStarted.await()

          val request = Http.Request(Http.Post, 2.0, unsafely(t"unix".as[Host]), t"/call", Nil,
              () => Stream(ascii(t"ping")))

          val (stream, response) = client.fetch(request, t"http", t"unix")
          val body = response.body.stream.memoize.utf8
          val grpcStatus = stream.trailers.await().seek(_.name == t"grpc-status").let(_.value)
          client.close()
          server.close()
          (body, grpcStatus.or(t"?"))

      . assert(_ == (t"body", t"0"))

      test(m"a flow window drains in bounded chunks and blocks until replenished"):
        supervise:
          val window = FlowWindow(10)
          val a = window.acquire(4)    // all requested: 4
          val b = window.acquire(100)  // capped at the remaining 6

          // The window is now empty; this acquire blocks until `release` tops it
          // up. The result is 3 whichever way `release` and the blocked `acquire`
          // interleave, so the test needs no timing.
          val blocked: Promise[Int] = Promise()
          async(blocked.offer(window.acquire(5)))
          window.release(3)
          val c = blocked.await()

          (a, b, c)

      . assert(_ == (4, 6, 3))

      test(m"one release wakes every waiter parked on an empty window"):
        supervise:
          val window = FlowWindow(0)
          val first: Promise[Int] = Promise()
          val second: Promise[Int] = Promise()
          async(first.offer(window.acquire(2)))
          async(second.offer(window.acquire(2)))

          // `release` signals all waiters, and each requests no more than half the
          // grant, so both must complete whichever order they park and wake; a
          // single-signal implementation would leave one parked forever.
          window.release(4)
          (first.await(), second.await())

      . assert(_ == (2, 2))

      test(m"a response larger than the connection window streams intact"):
        supervise:
          val (clientSide, serverSide) = pair()
          val server = Http2.ServerConnection(serverSide)
          val serverRef: AnyRef = server.asInstanceOf[AnyRef]

          // A body well over the 65535-byte connection window, so the server
          // must split DATA frames and wait for the client's WINDOW_UPDATEs.
          val size = 200000
          val payload: Data = Array.tabulate(size)(i => (i%256).toByte)
          val payloadRef: AnyRef = payload.asInstanceOf[AnyRef]

          daemon:
            safely:
              val server0 = serverRef.asInstanceOf[Http2.ServerConnection]
              val body = payloadRef.asInstanceOf[Data]

              server0.accepted.stream.records.each: stream =>
                unsafely:
                  stream.headers.await()
                  server0.sendHeaders(stream.id, List(Hpack.Entry(t":status", t"200")), false)
                  server0.sendData(stream.id, body, endStream = true)

          val client = Http2.Connection(clientSide)
          val serverStarted = scala.caps.unsafe.unsafeAssumeSeparate(async(server.start()))
          client.start()
          scala.caps.unsafe.unsafeAssumeSeparate:
            serverStarted.await()

          val request = Http.Request(Http.Get, 2.0, unsafely(t"unix".as[Host]), t"/big", Nil,
              () => Http.emptyBody())

          val (_, response) = client.fetch(request, t"http", t"unix")
          val received = response.body.stream.memoize
          client.close()
          server.close()
          (received.length, received.to[List] == payload.to[List])

      . assert(_ == (200000, true))

      // Receive-side flow control: the client advertises a small window and
      // does not read the body, so the server's send side must stall at window
      // exhaustion — after four full 1000-byte chunks (the fifth takes only the
      // 96-byte remainder and parks). Draining the body replenishes the window
      // in batched WINDOW_UPDATEs and the transfer completes.
      test(m"a peer stalls at a small advertised window until the body drains"):
        supervise:
          val (clientSide, serverSide) = pair()
          val server = Http2.ServerConnection(serverSide)
          val serverRef: AnyRef = server.asInstanceOf[AnyRef]
          val sent = java.util.concurrent.atomic.AtomicInteger(0)

          daemon:
            safely:
              val server0 = serverRef.asInstanceOf[Http2.ServerConnection]

              server0.accepted.stream.records.each: stream =>
                unsafely:
                  stream.headers.await()
                  server0.sendHeaders(stream.id, List(Hpack.Entry(t":status", t"200")), false)
                  var index = 0

                  while index < 20 do
                    val chunk: Data = Array.tabulate(1000)(i => ((index*1000 + i)%256).toByte)
                    server0.sendData(stream.id, chunk, endStream = index == 19)
                    sent.incrementAndGet()
                    index += 1

          val client = Http2.Connection(clientSide, window = 4096)
          val serverStarted = scala.caps.unsafe.unsafeAssumeSeparate(async(server.start()))
          client.start()
          scala.caps.unsafe.unsafeAssumeSeparate:
            serverStarted.await()

          val request = Http.Request(Http.Get, 2.0, unsafely(t"unix".as[Host]), t"/slow", Nil,
              () => Http.emptyBody())

          val (_, response) = client.fetch(request, t"http", t"unix")

          // Bounded poll until the sender goes quiet: a too-early sample can
          // only under-read, and a correct implementation freezes at four.
          var frozen = -1
          var stable = 0
          var attempts = 0

          while stable < 5 && attempts < 500 do
            val current = sent.get()
            if current == frozen then stable += 1 else { frozen = current; stable = 0 }
            Thread.sleep(10)
            attempts += 1

          val received = response.body.stream.memoize
          client.close()
          server.close()
          (frozen, received.length, sent.get())

      . assert(_ == (4, 20000, 20))

      // The reverse direction: the server advertises a small window and the
      // client's request-body upload must stall at exactly that window until
      // WINDOW_UPDATEs grant more — previously the client ignored flow control
      // entirely and sent the whole body as one frame. A hand-rolled
      // frame-level peer observes the wire directly, so receipt is counted
      // without consuming anything.
      test(m"a client upload stalls at the peer's advertised window"):
        supervise:
          val (clientSide, serverSide) = pair()
          val received = java.util.concurrent.atomic.AtomicLong(0)
          val streamId = java.util.concurrent.atomic.AtomicInteger(0)

          daemon:
            safely:
              serverSide.send(zephyrine.Stream(Frame.Settings(
                  List(Setting(SettingId.InitialWindowSize.id, 4096)), ack = false).serialize))

              val source = serverSide.source
              var skipped = 0

              while skipped < 24 do source.refill(zephyrine.Credit(4096)) match
                case count: Int =>
                  if count > 0 then
                    val take = count.min(24 - skipped)
                    source.skip(take)
                    skipped += take

                case _ =>
                  skipped = 24

              val reader = FrameReader(source)
              var continue = true

              while continue do (reader.next(): @unchecked) match
                case Unset    => continue = false
                case f: Frame => f match
                  case Frame.Settings(_, false) =>
                    serverSide.send(zephyrine.Stream(Frame.Settings(Nil, ack = true).serialize))

                  case Frame.Headers(id, _, _, _) => streamId.set(id)
                  case Frame.Data(_, payload, _)  => received.addAndGet(payload.length)
                  case _                          => ()

          val client = Http2.Connection(clientSide)
          client.start()

          val payload: Data = Array.tabulate(20000)(i => (i%256).toByte)

          val request = Http.Request(Http.Post, 2.0, unsafely(t"unix".as[Host]), t"/upload",
              Nil, () => Stream(payload))

          val fetched = scala.caps.unsafe.unsafeAssumeSeparate:
            async:
              val (_, response) = client.fetch(request, t"http", t"unix")
              response.status.code

          // Wait for the first DATA to land, then poll until the wire goes
          // quiet: a too-early sample can only under-read, and a correct
          // client freezes at exactly the advertised window.
          var warm = 0

          while received.get() == 0 && warm < 500 do
            Thread.sleep(10)
            warm += 1

          var frozen = -1L
          var stable = 0
          var attempts = 0

          while stable < 5 && attempts < 500 do
            val current = received.get()
            if current == frozen then stable += 1 else { frozen = current; stable = 0 }
            Thread.sleep(10)
            attempts += 1

          // Grant more credit: the parked upload must resume and complete.
          serverSide.send(zephyrine.Stream(Frame.WindowUpdate(0, 65535).serialize))
          serverSide.send(zephyrine.Stream(Frame.WindowUpdate(streamId.get, 65535).serialize))

          var drained = 0

          while received.get() < 20000 && drained < 500 do
            Thread.sleep(10)
            drained += 1

          val hpack = Hpack()
          val respHeaders = hpack.encode(List(Hpack.Entry(t":status", t"200")))

          serverSide.send:
            zephyrine.Stream(Frame.Headers(streamId.get, respHeaders, true, true).serialize)

          val status = scala.caps.unsafe.unsafeAssumeSeparate(fetched.await())
          client.close()
          (frozen, received.get(), status)

      . assert(_ == ((4096L, 20000L, 200)))

      test(m"a session lends one connection to several multiplexed requests"):
        supervise:
          val (clientSide, serverSide) = pair()
          runServer(serverSide)

          import logging.silentLogging

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val endpoint = Http2.Endpoint(Loopback(clientSide), t"loopback")

          // Both fetches multiplex on the single connection the session lends;
          // the connection is torn down when the scope ends.
          endpoint.session: connection ?=>
            val request = Http.Request(Http.Post, 2.0, unsafely(t"unix".as[Host]),
                t"/echo.Service/Call", Nil, () => Stream(ascii(t"ping")))

            val (_, first) = connection.fetch(request, t"http", t"loopback")
            val (_, second) = connection.fetch(request, t"http", t"loopback")

            List(first, second).count: response =>
              ascii(t"pong").to[List] == response.body.stream.memoize.to[List]

      . assert(_ == 2)
