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
package obligatory

import soundness.*

import charEncoders.utf8Encoder
import strategies.throwUnsafely
import Http2.*

// Simple protobuf messages for the gRPC loopback tests.
case class Ping(message: Text)
case class Pong(message: Text)

// A service interface for deriving a gRPC client stub with `Grpc.remote`.
trait Echo:
  @rpc def call(request: Ping): Pong

object Tests extends Suite(m"Obligatory Tests"):
  def run(): Unit =
    suite(m"Unframing tests"):
      test(m"Unframe by carriage-return lines"):
        Chain(t"one\rtwo\r", t"three").iterator.frames[CarriageReturn].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by carriage-return lines, without terminal line"):
        Chain(t"one\rtwo", t"\rthree\r").iterator.frames[CarriageReturn].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by linefeed lines"):
        Chain(t"one\ntwo\nth", t"ree").iterator.frames[Linefeed].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by linefeed lines, without terminal line"):
        Chain(t"one\ntwo\nthree\n").iterator.frames[Linefeed].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by cr/lf lines"):
        Chain(t"""one\r\ntwo\r\nthree""").iterator.frames[CrLf].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Unframe by cr/lf lines, without terminal line"):
        Chain(t"""one\r\ntwo\r\nthree\r\n""").iterator.frames[CrLf].to(List)
      . assert(_ == List("one", "two", "three"))

      test(m"Length-prefixed chunks"):
        Chain(Data(0, 0, 0, 3, 50, 100, -100, 0, 0, 0, 1, -128, 0, 0, 0, 5, 5, 4, 3, 2, 1))
        . iterator
        . frames[LengthPrefix]
        . to(List)
        . map(_.to[List])
      . assert(_ == List(List(50, 100, -100), List(-128), List(5, 4, 3, 2, 1)))

      test(m"Content-Length-prefixed chunks"):
        val input =
          t"Content-Type: x\r\nContent-Length: 5\r\n\r\n12345Content-Length: 3\r\n\r\nabc"

        Iterator(input.in[Data]).frames[ContentLength].map(_.utf8).to(List)
      . assert(_ == List("12345", "abc"))

      test(m"Content-Length counts bytes, not characters"):
        val body = t"""{"text":"café"}"""
        val input = t"Content-Length: ${body.in[Data].readable.length}\r\n\r\n"+body

        Iterator(input.in[Data]).frames[ContentLength].map(_.utf8).to(List)
      . assert(_ == List(t"""{"text":"café"}"""))

      test(m"Server-side events"):
        val input = t"data: foobar\ndata: baz\n\ndata: hello world\n\n"

        Iterator(input).frames[Sse].to(List)
      . assert(_ == List("data: foobar\ndata: baz", "data: hello world"))

      test(m"Server-side events without terminal newlines"):
        val input = t"data: foobar\ndata: baz\n\ndata: hello world"

        Iterator(input).frames[Sse].to(List)
      . assert(_ == List("data: foobar\ndata: baz", "data: hello world"))

      test(m"Typed server-side events"):
        val input = t"event: one\ndata: foobar\ndata: baz\n\ndata: hello world"

        Iterator(input).frames[Sse].map(_.as[Sse]).to(List)
      . assert(_ == List(Sse("one", List("foobar", "baz")), Sse("message", List("hello world"))))

      test(m"Typed server-side events with more fields"):
        val input = t"event: one\nid: 123\ndata: foobar\ndata: baz\n\ndata: hello world\nretry: 54321"

        Iterator(input).frames[Sse].map(_.as[Sse]).to(List)
      . assert(_ == List(Sse("one", List("foobar", "baz"), "123"), Sse("message", List("hello world"), Unset, 54321L)))

    suite(m"gRPC message framing"):
      def ascii(text: Text): Data = Array.unsafeFrozen(text.s.getBytes("US-ASCII").nn)

      test(m"encode prefixes a flag byte and 4-byte length"):
        Grpc.Framing.encode(ascii(t"hi")).to[List]
      . assert(_ == Array.frozen(Data(0, 0, 0, 0, 2).readable ++ ascii(t"hi").readable).to[List])

      test(m"round-trip a single message"):
        val framed = Grpc.Framing.encode(ascii(t"hello"))
        Chain(framed).iterator.frames[Grpc.Framing].to(List).map(_.readable.to(List))
      . assert(_ == List(ascii(t"hello").to[List]))

      test(m"split two concatenated messages"):
        val framed = Array.frozen(Grpc.Framing.encode(ascii(t"one")).readable ++ Grpc.Framing.encode(ascii(t"two")).readable)
        Chain(framed).iterator.frames[Grpc.Framing].to(List).map(_.readable.to(List))
      . assert(_ == List(ascii(t"one").to[List], ascii(t"two").to[List]))

      test(m"gzip-compressed message round-trips"):
        val framed = Grpc.Framing.encode(ascii(t"compress me please"), compress = true)
        Chain(framed).iterator.frames[Grpc.Framing].to(List).map(_.readable.to(List))
      . assert(_ == List(ascii(t"compress me please").to[List]))

      test(m"status code maps to the canonical name"):
        Grpc.Status.of(5)
      . assert(_ == Grpc.Status.NotFound)

    suite(m"gRPC over HTTP/2 (loopback)"):
      import threading.virtualThreading
      import probates.cancelProbate
      import errorDiagnostics.stackTracesDiagnostics

      def pair(): (Duplex, Duplex) = Duplex.pair()

      def okHeaders(hpack: Hpack, id: Int): Frame =
        val block = hpack.encode(List(HpackEntry(t":status", t"200"),
            HpackEntry(t"content-type", t"application/grpc")))

        Frame.Headers(id, block, endStream = false, endHeaders = true)

      def trailers(hpack: Hpack, id: Int, fields: List[HpackEntry], endStream: Boolean): Frame =
        Frame.Headers(id, hpack.encode(fields), endStream, endHeaders = true)

      // A minimal in-process gRPC server: completes the HTTP/2 handshake, then on the
      // request HEADERS replies with whatever frames `responder` builds.
      def runServer(serverSide: Duplex, responder: (Hpack, Int) => List[Frame])
          ( using Monitor, Probate )
      :   Daemon =

        daemon:
          safely:
            serverSide.send(zephyrine.Stream(Frame.Settings(Nil, ack = false).serialize))
            // Skip the 24-byte client connection preface: consume exactly the
            // preface; anything after it stays in the endpoint's window.
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
              case Unset    => continue = false

              case f: Frame => f match
                case Frame.Settings(_, false) =>
                  serverSide.send(zephyrine.Stream(Frame.Settings(Nil, ack = true).serialize))

                case Frame.Headers(id, _, _, _) =>
                  responder(hpack, id).each: frame =>
                    serverSide.send(zephyrine.Stream(frame.serialize))

                case _ => ()

      val method = Grpc.Method(t"echo.Echo", t"Call")

      test(m"a unary call decodes a typed response and OK status"):
        supervise:
          val (clientSide, serverSide) = pair()

          runServer(serverSide, (hpack, id) =>
            List
              ( okHeaders(hpack, id),
                Frame.Data(id, Grpc.Framing.encode(Pong(t"pong").in[Protobuf].encode), endStream = false),
                trailers(hpack, id, List(HpackEntry(t"grpc-status", t"0")), true) ))

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val channel = Grpc.Channel(Http2.Endpoint(Loopback(clientSide), t"localhost"))
          channel.unary[Ping, Pong](method, Ping(t"ping")).message
      . assert(_ == t"pong")

      test(m"a scoped channel session makes a unary call and tears down with the scope"):
        supervise:
          val (clientSide, serverSide) = pair()

          runServer(serverSide, (hpack, id) =>
            List
              ( okHeaders(hpack, id),
                Frame.Data(id, Grpc.Framing.encode(Pong(t"pong").in[Protobuf].encode), endStream = false),
                trailers(hpack, id, List(HpackEntry(t"grpc-status", t"0")), true) ))

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          Grpc.Endpoint(Http2.Endpoint(Loopback(clientSide), t"localhost")).session: channel ?=>
            channel.unary[Ping, Pong](method, Ping(t"ping")).message
      . assert(_ == t"pong")

      test(m"a non-Ok trailing status raises a Grpc.Error"):
        supervise:
          val (clientSide, serverSide) = pair()

          runServer(serverSide, (hpack, id) =>
            List(trailers(hpack, id, List(HpackEntry(t":status", t"200"),
                HpackEntry(t"grpc-status", t"5"), HpackEntry(t"grpc-message", t"absent")), true)))

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val channel = Grpc.Channel(Http2.Endpoint(Loopback(clientSide), t"localhost"))
          capture[Grpc.Error](channel.unary[Ping, Pong](method, Ping(t"ping"))).status
      . assert(_ == Grpc.Status.NotFound)

      test(m"a server-streaming call decodes every response message"):
        supervise:
          val (clientSide, serverSide) = pair()

          val body =
            Array.frozen
             ( Grpc.Framing.encode(Pong(t"a").in[Protobuf].encode).readable
               ++ Grpc.Framing.encode(Pong(t"b").in[Protobuf].encode).readable
               ++ Grpc.Framing.encode(Pong(t"c").in[Protobuf].encode).readable )

          runServer(serverSide, (hpack, id) =>
            List
              ( okHeaders(hpack, id),
                Frame.Data(id, body, endStream = false),
                trailers(hpack, id, List(HpackEntry(t"grpc-status", t"0")), true) ))

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val channel = Grpc.Channel(Http2.Endpoint(Loopback(clientSide), t"localhost"))
          channel.serverStreaming[Ping, Pong](method, Ping(t"ping")).map(_.message).stdlib.to(List)
      . assert(_ == List(t"a", t"b", t"c"))

      test(m"a derived @rpc client stub round-trips a unary call"):
        supervise:
          val (clientSide, serverSide) = pair()

          runServer(serverSide, (hpack, id) =>
            List
              ( okHeaders(hpack, id),
                Frame.Data(id, Grpc.Framing.encode(Pong(t"pong").in[Protobuf].encode), endStream = false),
                trailers(hpack, id, List(HpackEntry(t"grpc-status", t"0")), true) ))

          case class Loopback(duplex: Duplex)
          given (Loopback is Connectable) = (loopback, _) => loopback.duplex
          given (Loopback is Showable) = _ => t"loopback"

          val channel = Grpc.Channel(Http2.Endpoint(Loopback(clientSide), t"localhost"))
          val echo = Grpc.remote[Echo](channel, t"echo.Echo")
          echo.call(Ping(t"ping")).message
      . assert(_ == t"pong")

    suite(m"JSON-RPC error responses"):
      import dynamicJsonAccess.enabled
      import Json.jsonEncodableInText

      test(m"a failure carries the fault in the error member"):
        JsonRpc.failure(-32601, t"Method not found", 7.in[Json]).error.code.as[Int]
      . assert(_ == -32601)

      test(m"a failure echoes the failing request's id"):
        JsonRpc.failure(-32601, t"Method not found", 7.in[Json]).id.as[Int]
      . assert(_ == 7)

      test(m"a failure has no result member"):
        JsonRpc.failure(-32601, t"Method not found", 7.in[Json]).encode
      . assert(!_.contains(t"result"))

      test(m"an unknowable id is an explicit null"):
        JsonRpc.failure(-32700, t"Parse error").encode
      . assert(_.contains(t"\"id\":null"))
