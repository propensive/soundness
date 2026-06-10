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
package perihelion

import soundness.*
import scintillate.SocketServer

import logging.silent
import strategies.throwUnsafely
import webserverErrorPages.minimal
import threading.virtual
import codicils.await

import Control.*

object Tests extends Suite(m"Perihelion tests"):
  def run(): Unit =
    def freePort(): Int =
      val socket = java.net.ServerSocket(0)
      val port = socket.getLocalPort
      socket.close()
      port

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

    supervise:
      suite(m"WebSocket echo"):
        test(m"A text message is echoed back, and the handshake is accepted"):
          val port = freePort()

          val server = SocketServer(port).handle:
            webSocket(()): message =>
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
