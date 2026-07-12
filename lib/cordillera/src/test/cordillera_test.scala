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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package cordillera

import soundness.*

import strategies.throwUnsafely
import Http2.*

object Tests extends Suite(m"Cordillera HTTP/2 Tests"):
  def run(): Unit =
    def bytes(hex: Text): Data =
      IArray.from(hex.s.grouped(2).map(Integer.parseInt(_, 16).toByte).to(List))

    def hex(data: Data): Text =
      data.to(List).map(b => String.format("%02x", (b & 0xff): Integer).nn).mkString.tt

    def ascii(text: Text): Data = IArray.from(text.s.getBytes("US-ASCII").nn.to(List))

    suite(m"Huffman (RFC 7541 Appendix C)"):
      // C.4.1: "www.example.com" → Huffman
      test(m"encode www.example.com"):
        hex(Huffman.encode(ascii(t"www.example.com")))
      . assert(_ == t"f1e3c2e5f23a6ba0ab90f4ff")

      test(m"decode www.example.com"):
        Huffman.decode(bytes(t"f1e3c2e5f23a6ba0ab90f4ff")).to(List)
      . assert(_ == ascii(t"www.example.com").to(List))

      // C.4.2: "no-cache"
      test(m"encode no-cache"):
        hex(Huffman.encode(ascii(t"no-cache")))
      . assert(_ == t"a8eb10649cbf")

      test(m"decode no-cache"):
        Huffman.decode(bytes(t"a8eb10649cbf")).to(List)
      . assert(_ == ascii(t"no-cache").to(List))

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
        Huffman.decode(bytes(t"aec3771a4b")).to(List)
      . assert(_ == ascii(t"private").to(List))

      test(m"round-trip a long date string"):
        val date = ascii(t"Mon, 21 Oct 2013 20:13:21 GMT")
        Huffman.decode(Huffman.encode(date)).to(List) == date.to(List)
      . assert(_ == true)

      test(m"round-trip all 256 byte values"):
        val every = IArray.from((0 until 256).map(_.toByte))
        Huffman.decode(Huffman.encode(every)).to(List) == every.to(List)
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
        HpackEntry(t":method", t"POST"),
        HpackEntry(t":scheme", t"http"),
        HpackEntry(t":path", t"/foo/bar"),
        HpackEntry(t":authority", t"unix"),
        HpackEntry(t"content-type", t"application/grpc"),
        HpackEntry(t"te", t"trailers"))

      test(m"a request's pseudo-headers + headers survive a round-trip"):
        val encoded = Hpack().encode(headers)
        Hpack().decode(encoded).map(e => (e.name, e.value))
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
      def roundTrip(frame: Frame): Frame raises Http2Error =
        Frame.decode(frame.serialize, 0)(0)

      test(m"SETTINGS with parameters round-trips"):
        val settings = List(Setting(SettingId.InitialWindowSize.id, 65535),
            Setting(SettingId.MaxFrameSize.id, 16384))
        roundTrip(Frame.Settings(settings, ack = false)) == Frame.Settings(settings, false)
      . assert(_ == true)

      test(m"DATA round-trips with payload and END_STREAM"):
        roundTrip(Frame.Data(7, ascii(t"hello"), endStream = true)) match
          case Frame.Data(id, p, end) => (id, p.to(List), end) == (7, ascii(t"hello").to(List), true)
          case _                      => false
      . assert(_ == true)

      test(m"HEADERS round-trips its block + flags"):
        roundTrip(Frame.Headers(1, ascii(t"block"), endStream = false, endHeaders = true)) match
          case Frame.Headers(id, b, es, eh) => (id, b.to(List), es, eh)
              == (1, ascii(t"block").to(List), false, true)
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
        // length=5: padLength byte (0x02) + "hi" + 2 pad bytes; PADDED flag = 0x08
        val padded = bytes(t"0000050008000000030268690000")
        Frame.decode(padded, 0)(0) match
          case Frame.Data(_, p, _) => p.to(List) == ascii(t"hi").to(List)
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
                val respHeaders = hpack.encode(List(HpackEntry(t":status", t"200"),
                    HpackEntry(t"content-type", t"application/grpc")))

                val trailers = hpack.encode(List(HpackEntry(t"grpc-status", t"0")))
                serverSide.send(zephyrine.Stream(Frame.Headers(id, respHeaders, false, true).serialize))
                serverSide.send(zephyrine.Stream(Frame.Data(id, ascii(t"pong"), false).serialize))
                serverSide.send(zephyrine.Stream(Frame.Headers(id, trailers, true, true).serialize))

              case _ => ()

      test(m"a unary request round-trips status, body and trailers"):
        supervise:
          val (clientSide, serverSide) = pair()
          val server = runServer(serverSide)
          val connection = Http2Connection(clientSide)
          connection.start()

          val request = Http.Request(Http.Post, 2.0, unsafely(t"unix".decode[Host]),
              t"/echo.Service/Call", Nil, () => Stream(ascii(t"ping")))

          val (stream, response) = connection.fetch(request, t"http", t"unix")
          val bodyText = ascii(t"pong").to(List) == response.body.stream.memoize.to(List)
          val statusCode = response.status.code
          val grpcStatus = stream.trailers.await().find(_.name == t"grpc-status").map(_.value)
          server.cancel()
          (statusCode, bodyText, grpcStatus.getOrElse(t"?"))
      . assert(_ == (200, true, t"0"))

      test(m"the HttpClient given resolves and drives a request over h2c"):
        supervise:
          val (clientSide, serverSide) = pair()
          runServer(serverSide)

          import Http2.Client.http2
          import logging.silentLogging

          // A `Connectable` whose connect() hands back the client side of the pair —
          // lets the real `HttpClient` given (which calls `target.connect()`) run
          // against the loopback without a socket.
          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          // Summon the HTTP/2 client given exactly as telekinesis's fetch machinery
          // would, and invoke its `request` — verifying it captures the ambient
          // Monitor/Probate and produces a telekinesis `Http.Response`.
          val client = summon[HttpClient onto Http2.Endpoint[Loopback]]
          val endpoint = Http2.Endpoint(Loopback(clientSide), t"unix")

          val request = Http.Request(Http.Get, 2.0, unsafely(t"unix".decode[Host]),
              t"/echo.Service/Call", Nil, () => Stream(Iterator.empty[Data]))

          client.request(request, endpoint).status.code
      . assert(_ == 200)
