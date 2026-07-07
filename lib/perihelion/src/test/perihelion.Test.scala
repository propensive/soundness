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
package perihelion

import soundness.*
import scintillate.SocketServer

import logging.silentLogging
import internetAccess.online
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import webserverErrorPages.minimalErrorPage
import threading.virtualThreading
import probates.awaitProbate
import formatting.compactJsonFormatting
import charEncoders.utf8Encoder
import charDecoders.utf8Decoder
import textSanitizers.skipSanitizer

import Control.*

case class Ping(value: Int) derives CanEqual

object Tests extends Suite(m"Perihelion tests"):
  def run(): Unit =
    def freePort(): Int =
      val socket = java.net.ServerSocket(0)
      val port = socket.getLocalPort
      socket.close()
      port

    // A throwaway self-signed `CN=localhost` certificate (via the JDK's keytool) and the
    // matching contexts: a server context that presents it, and a trust-all client context
    // (used with `verify = false`, since the cert carries no SAN). Mirrors the scintillate
    // TLS test.
    def tlsContexts(): (javax.net.ssl.SSLContext, javax.net.ssl.SSLContext) =
      val dir = java.nio.file.Files.createTempDirectory("perihelion-tls").nn
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

      val trustManager = new javax.net.ssl.X509TrustManager:
        type Certs = Array[java.security.cert.X509Certificate | Null] | Null
        def getAcceptedIssuers: Certs = scala.Array.empty[java.security.cert.X509Certificate | Null]
        def checkClientTrusted(chain: Certs, kind: String | Null): Unit = ()
        def checkServerTrusted(chain: Certs, kind: String | Null): Unit = ()

      val clientContext = javax.net.ssl.SSLContext.getInstance("TLS").nn
      clientContext.init(null, scala.Array(trustManager), java.security.SecureRandom())

      (serverContext, clientContext)

    // Build a masked client frame (clients must mask).
    def clientFrame(opcode: Int, payload: Array[Byte]): Array[Byte] =
      val mask = Array[Byte](0x12, 0x34, 0x56, 0x78)
      val masked = new Array[Byte](payload.length)
      var i = 0
      while i < payload.length do
        masked(i) = (payload(i)^mask(i%4)).toByte
        i += 1

      val length = payload.length

      val header =
        if length <= 125 then Array[Byte]((0x80|opcode).toByte, (0x80|length).toByte)
        else Array[Byte]((0x80|opcode).toByte, (0x80|126).toByte, (length >> 8).toByte, length.toByte)

      header ++ mask ++ masked

    // Read one unmasked server frame: (opcode, payload).
    def serverFrame(in: java.io.InputStream): (Int, Array[Byte]) =
      val opcode = in.read & 0x0f
      val length7 = in.read & 0x7f

      val length =
        if length7 == 126 then (in.read << 8) | in.read
        else if length7 == 127 then
          var value = 0
          var index = 0
          while index < 8 do { value = (value << 8) | in.read; index += 1 }
          value
        else length7

      (opcode, in.readNBytes(length).nn)

    def readHead(in: java.io.InputStream): Text =
      val builder = StringBuilder()
      while !builder.toString.endsWith("\r\n\r\n") do builder.append(in.read.toChar)
      builder.toString.tt

    def octets(text: String): Array[Byte] = text.getBytes("UTF-8").nn

    // Build a frame (masked by default, as a client must); `fin` and `masked`
    // are configurable for protocol tests, with a fixed mask key.
    def frame(opcode: Int, payload: Array[Byte], fin: Boolean = true, masked: Boolean = true)
    :   Array[Byte] =

      val first = ((if fin then 0x80 else 0) | opcode).toByte
      val length = payload.length
      val maskBit = if masked then 0x80 else 0

      val header =
        if length <= 125 then scala.Array[Byte](first, (maskBit | length).toByte)
        else if length <= 0xffff
        then scala.Array[Byte](first, (maskBit | 126).toByte, (length >> 8).toByte, length.toByte)
        else
          scala.Array[Byte]
           ( first, (maskBit | 127).toByte, 0, 0, 0, 0, (length >> 24).toByte,
             (length >> 16).toByte, (length >> 8).toByte, length.toByte )

      if !masked then header ++ payload else
        val key = scala.Array[Byte](0x12, 0x34, 0x56, 0x78)
        val coded = new scala.Array[Byte](length)
        var i = 0

        while i < length do
          coded(i) = (payload(i)^key(i%4)).toByte
          i += 1

        header ++ key ++ coded

    def closeBytes(code: Int, reason: String): Array[Byte] =
      scala.Array[Byte]((code >> 8).toByte, code.toByte) ++ octets(reason)

    def parseFrame(bytes: Array[Byte]): Optional[Frame] =
      Frame.parse(Cursor[Data](Stream(bytes.immutable(using Unsafe)).iterator))

    def readMessages(frames: Array[Byte]*): List[perihelion.Message] =
      val stream = Stream(frames*).map(_.immutable(using Unsafe))
      Reader(() => stream, Channel()).messages.toList

    def texts(messages: List[perihelion.Message]): List[Text] = messages.map:
      case perihelion.Message.Text(text) => text
      case perihelion.Message.Binary(_)  => t"<binary>"

    // The pure-codec tests below read frames as a server does (inbound frames are the
    // client's, and masked); the client direction is exercised in "Client masking".
    given Masking = Masking.Server

    suite(m"Frame codec"):
      test(m"A masked text frame decodes to its payload"):
        parseFrame(frame(0x1, octets("hi"))) match
          case Frame.Text(fin, data) => (fin, data.utf8)
          case _                     => (false, t"")
      . assert(_ == (true, t"hi"))

      test(m"A masked binary frame decodes to Binary"):
        parseFrame(frame(0x2, octets("xy"))) match
          case Frame.Binary(fin, data) => (fin, data.utf8)
          case _                       => (false, t"")
      . assert(_ == (true, t"xy"))

      test(m"An unmasked client frame is rejected"):
        capture[WebsocketError](parseFrame(frame(0x1, octets("x"), masked = false))).reason
      . assert(_ == WebsocketError.Reason.Unmasked)

      test(m"A frame with a reserved bit set is rejected"):
        val bytes = frame(0x1, octets("x"))
        bytes(0) = (bytes(0) | 0x40).toByte

        capture[WebsocketError](parseFrame(bytes)).reason
      . assert(_ == WebsocketError.Reason.ReservedBits)

      test(m"A reserved opcode is rejected"):
        capture[WebsocketError](parseFrame(frame(0x3, octets("x")))).reason
      . assert(_ == WebsocketError.Reason.BadOpcode(0x3))

      test(m"A fragmented control frame is rejected"):
        capture[WebsocketError](parseFrame(frame(0x9, octets("x"), fin = false))).reason
      . assert(_ == WebsocketError.Reason.BadControl)

      test(m"An over-long control frame is rejected"):
        capture[WebsocketError](parseFrame(frame(0x9, scala.Array.fill(126)(0x61.toByte)))).reason
      . assert(_ == WebsocketError.Reason.BadControl)

      test(m"A 16-bit length frame parses fully"):
        parseFrame(frame(0x1, scala.Array.fill(200)(0x61.toByte))) match
          case Frame.Text(_, data) => data.length
          case _                   => 0
      . assert(_ == 200)

      test(m"A close frame carries its code and reason"):
        parseFrame(frame(0x8, closeBytes(1000, "bye"))) match
          case Frame.Close(code, reason) => (code, reason.utf8)
          case _                         => (0, t"")
      . assert(_ == (1000, t"bye"))

      test(m"A close frame with no payload yields the 1005 sentinel"):
        parseFrame(frame(0x8, scala.Array[Byte]())) match
          case Frame.Close(code, _) => code
          case _                    => 0
      . assert(_ == 1005)

      test(m"A one-byte close payload is rejected"):
        capture[WebsocketError](parseFrame(frame(0x8, scala.Array[Byte](0x03)))).reason
      . assert(_ == WebsocketError.Reason.BadClose)

      test(m"An invalid close code is rejected"):
        capture[WebsocketError](parseFrame(frame(0x8, closeBytes(1004, "")))).reason
      . assert(_ == WebsocketError.Reason.BadClose)

    suite(m"Message reassembly"):
      test(m"A single text frame yields one message"):
        texts(readMessages(frame(0x1, octets("hello"))))
      . assert(_ == List(t"hello"))

      test(m"A fragmented text message is reassembled"):
        texts(readMessages(frame(0x1, octets("he"), fin = false), frame(0x0, octets("llo"))))
      . assert(_ == List(t"hello"))

      test(m"A ping may interleave between fragments"):
        val start = frame(0x1, octets("he"), fin = false)
        val ping = frame(0x9, octets("p"))
        val rest = frame(0x0, octets("llo"))

        texts(readMessages(start, ping, rest))
      . assert(_ == List(t"hello"))

      test(m"A new data frame mid-message is rejected"):
        capture[WebsocketError]:
          readMessages(frame(0x1, octets("he"), fin = false), frame(0x1, octets("llo")))
        . reason
      . assert(_ == WebsocketError.Reason.BadFragmentation)

      test(m"A continuation with nothing to continue is rejected"):
        capture[WebsocketError](readMessages(frame(0x0, octets("x")))).reason
      . assert(_ == WebsocketError.Reason.BadFragmentation)

      test(m"A text frame with invalid UTF-8 is rejected"):
        capture[WebsocketError](readMessages(frame(0x1, scala.Array[Byte](0xc3.toByte, 0x28)))).reason
      . assert(_ == WebsocketError.Reason.InvalidText)

    suite(m"Typed messages"):
      test(m"A Ping round-trips through the composed over-Json codec"):
        val outgoing = infer[(Ping over Json) is Transmissible]
        val incoming = infer[(Ping over Json) is Ingressive]

        // `outgoing` now yields a complete (unmasked) Text frame; the reader unframes it
        // before `incoming` decodes the JSON payload.
        val frameBytes = outgoing.serialize(Ping(7).over[Json]).foldLeft(Data())(_ ++ _)

        val payload = Frame.parse(Cursor[Data](Stream(frameBytes).iterator))(using Masking.Client()) match
          case Frame.Text(_, data) => data
          case _                   => Data()

        incoming.deserialize(payload)
      . assert(_ == Ping(7))

    suite(m"Client masking"):
      def parseAs(masking: Masking, bytes: Data): Optional[Frame] =
        Frame.parse(Cursor[Data](Stream(bytes).iterator))(using masking)

      test(m"A client-masked frame is readable by the server"):
        val masked: Data = Masking.Client().outbound(Frame.Text(true, t"hi".data).encode)
        parseAs(Masking.Server, masked) match
          case Frame.Text(_, data) => data.utf8
          case _                   => t"?"
      . assert(_ == t"hi")

      test(m"A client masks with a fresh key each time"):
        val client = Masking.Client()
        val frame = Frame.Text(true, t"hello".data).encode
        client.outbound(frame) != client.outbound(frame)
      . assert(_ == true)

      test(m"A client accepts an unmasked server frame"):
        parseAs(Masking.Client(), Frame.Text(true, t"hi".data).encode) match
          case Frame.Text(_, data) => data.utf8
          case _                   => t"?"
      . assert(_ == t"hi")

      test(m"A client rejects a masked server frame"):
        val masked: Data = Masking.Client().outbound(Frame.Text(true, t"hi".data).encode)
        capture[WebsocketError](parseAs(Masking.Client(), masked)).reason
      . assert(_ == WebsocketError.Reason.Masked)

    supervise:
      suite(m"WebSocket echo"):
        test(m"A text message is echoed back, and the handshake is accepted"):
          val port = freePort()

          val server = SocketServer(port).handle:
            webSocket(): (message: perihelion.Message) =>
              Reply(message, ())

          val socket = java.net.Socket("localhost", port)
          socket.setSoTimeout(5000)
          val out = socket.getOutputStream.nn
          val in = socket.getInputStream.nn

          val key = t"dGhlIHNhbXBsZSBub25jZQ=="

          val upgrade =
            t"GET / HTTP/1.1\r\nHost: x\r\nConnection: Upgrade\r\nUpgrade: websocket\r\n"
            + t"Sec-WebSocket-Key: $key\r\nSec-WebSocket-Version: 13\r\n\r\n"

          out.write(upgrade.s.getBytes("US-ASCII").nn)
          out.flush()

          val head = readHead(in)

          out.write(clientFrame(0x1, "hello".getBytes("US-ASCII").nn))
          out.flush()

          val (opcode, payload) = serverFrame(in)
          val echoed = String(payload, "US-ASCII").tt

          socket.close()
          server.cancel()

          (head.contains(t"101"), opcode, echoed)

        . assert(_ == (true, 0x1, t"hello"))

      suite(m"Typed echo"):
        test(m"A Ping message round-trips over the wire as JSON"):
          val port = freePort()

          val server = SocketServer(port).handle:
            webSocket(): (ping: Ping over Json) =>
              Reply(Ping(ping.value + 1).over[Json], ())

          val socket = java.net.Socket("localhost", port)
          socket.setSoTimeout(5000)
          val out = socket.getOutputStream.nn
          val in = socket.getInputStream.nn

          val key = t"dGhlIHNhbXBsZSBub25jZQ=="

          val upgrade =
            t"GET / HTTP/1.1\r\nHost: x\r\nConnection: Upgrade\r\nUpgrade: websocket\r\n"
            + t"Sec-WebSocket-Key: $key\r\nSec-WebSocket-Version: 13\r\n\r\n"

          out.write(upgrade.s.getBytes("US-ASCII").nn)
          out.flush()
          val head = readHead(in)

          out.write(clientFrame(0x1, Ping(7).json.show.s.getBytes("UTF-8").nn))
          out.flush()

          val (opcode, replyBytes) = serverFrame(in)
          val reply = String(replyBytes, "UTF-8").tt.read[Json].as[Ping]

          socket.close()
          server.cancel()

          (head.contains(t"101"), opcode, reply.value)

        . assert(_ == (true, 0x1, 8))

      suite(m"WebSocket client"):
        // The client is Coaxial's `exchange` over the `WsUrl is Serviceable` transport. It
        // is reactive (it acts on inbound messages), so the server greets on connect via
        // its `Channel`, and the client decodes the typed greeting and concludes.
        test(m"A client handshakes and decodes a server-pushed typed message"):
          val port = freePort()

          val server = SocketServer(port).handle:
            val websocket = webSocket(): (message: perihelion.Message) => Continue(())
            websocket.channel.send(perihelion.Message.Text(Ping(7).json.show))
            websocket

          val url = t"ws://localhost:$port/".decode[WsUrl]

          val value = url.exchange(0): (ping: Ping over Json) =>
            Conclude(Ping(ping.value + 1).over[Json], ping.value)

          server.cancel()
          value

        . assert(_ == 7)

        // The full-duplex `transceive` gives the caller a `Sender` before the loop, so
        // the client speaks first: it sends `Ping(7)`, the server replies `Ping(8)`.
        test(m"A client sends proactively, then reacts to the reply"):
          val port = freePort()

          val server = SocketServer(port).handle:
            webSocket(): (ping: Ping over Json) =>
              Reply(Ping(ping.value + 1).over[Json], ())

          val url = t"ws://localhost:$port/".decode[WsUrl]

          val handle: (state: Int) ?=> (Ping over Json) => Control[Int] =
            ping => Conclude(Ping(0).over[Json], ping.value)

          val value = url.transceive(0)(handle)(_.send(Ping(7).over[Json]))

          server.cancel()
          value

        . assert(_ == 8)

        // The whole client stack over TLS: a self-signed `wss://` echo server, and a client
        // that trusts it (verification off for the self-signed cert). Proves the TLS
        // `Duplex` carries the handshake and framed, masked messages.
        test(m"A wss:// client completes a TLS handshake and round-trips a message"):
          val (serverContext, clientContext) = tlsContexts()
          val port = freePort()

          val server = SocketServer(port, ssl = serverContext).handle:
            webSocket(): (ping: Ping over Json) =>
              Reply(Ping(ping.value + 1).over[Json], ())

          given Tls = Tls(clientContext, verify = false)
          val url = t"wss://localhost:$port/".decode[WsUrl]

          val handle: (state: Int) ?=> (Ping over Json) => Control[Int] =
            ping => Conclude(Ping(0).over[Json], ping.value)

          val value = url.transceive(0)(handle)(_.send(Ping(7).over[Json]))

          server.cancel()
          value

        . assert(_ == 8)
