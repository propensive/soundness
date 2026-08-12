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
package vivisection

import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import threading.platformThreading
import probates.awaitProbate
import logging.silentLogging
import strategies.throwUnsafely

case class Attach(duplex: Duplex)

object Attach:
  given connectable: (Attach is Connectable) = new Connectable:
    type Self = Attach
    def connect(attach: Attach, interface: Optional[MacAddress]): Duplex = attach.duplex

  given showable: Attach is Showable = _ => t"attach"

object Tests extends Suite(m"Vivisection tests"):
  def run(): Unit =
    val sizes = Jdwp.IdSizes.bootstrap

    def roundtrip[value](write: Jdwp.Writer => Unit)(read: Jdwp.Reader => value): value =
      val writer = Jdwp.Writer(sizes)
      write(writer)
      read(Jdwp.Reader(writer.data, sizes))

    test(m"byte round-trips"):
      roundtrip(_.byte(0x7f.toByte))(_.byte())
    . assert(_ == 0x7f.toByte)

    test(m"negative byte round-trips"):
      roundtrip(_.byte((-3).toByte))(_.byte())
    . assert(_ == (-3).toByte)

    test(m"boolean round-trips"):
      roundtrip(_.boolean(true))(_.boolean())
    . assert(_ == true)

    test(m"short round-trips"):
      roundtrip(_.short(0x1234.toShort))(_.short())
    . assert(_ == 0x1234.toShort)

    test(m"int round-trips"):
      roundtrip(_.int(0x12345678))(_.int())
    . assert(_ == 0x12345678)

    test(m"negative int round-trips"):
      roundtrip(_.int(-1))(_.int())
    . assert(_ == -1)

    test(m"long round-trips"):
      roundtrip(_.long(0x0123456789abcdefL))(_.long())
    . assert(_ == 0x0123456789abcdefL)

    test(m"char round-trips"):
      roundtrip(_.char('Z'))(_.char())
    . assert(_ == 'Z')

    test(m"ASCII string round-trips"):
      roundtrip(_.string(t"HelloJDWP"))(_.string())
    . assert(_ == t"HelloJDWP")

    test(m"string with embedded null round-trips"):
      roundtrip(_.string(t"a\u0000b"))(_.string())
    . assert(_ == t"a\u0000b")

    test(m"multi-byte string round-trips"):
      roundtrip(_.string(t"caf\u00e9"))(_.string())
    . assert(_ == t"caf\u00e9")

    val eightByteSizes = Jdwp.IdSizes(8, 8, 8, 8, 8)
    val fourByteSizes = Jdwp.IdSizes(4, 4, 4, 4, 4)

    def idRoundtrip(idSizes: Jdwp.IdSizes): Long =
      val writer = Jdwp.Writer(idSizes)
      writer.objectId(Jdwp.Ref(0x0a0b0c0dL))
      Jdwp.Reader(writer.data, idSizes).objectId().long

    test(m"objectId round-trips at 8-byte width"):
      idRoundtrip(eightByteSizes)
    . assert(_ == 0x0a0b0c0dL)

    test(m"objectId round-trips at 4-byte width"):
      idRoundtrip(fourByteSizes)
    . assert(_ == 0x0a0b0c0dL)

    val location = Jdwp.Location(Jdwp.TypeTag.Class, Jdwp.Ref(11L), Jdwp.Ref(22L), 33L)

    test(m"location round-trips"):
      roundtrip(_.location(location))(_.location())
    . assert(_ == location)

    test(m"int value round-trips"):
      roundtrip(_.value(Jdwp.Value.OfInt(999)))(_.value())
    . assert(_ == Jdwp.Value.OfInt(999))

    test(m"boolean value round-trips"):
      roundtrip(_.value(Jdwp.Value.OfBoolean(true)))(_.value())
    . assert(_ == Jdwp.Value.OfBoolean(true))

    test(m"object reference value round-trips"):
      val value = Jdwp.Value.Reference(Jdwp.Tag.ObjectTag, Jdwp.Ref(77L))
      roundtrip(_.value(value))(_.value())
    . assert(_ == Jdwp.Value.Reference(Jdwp.Tag.ObjectTag, Jdwp.Ref(77L)))

    test(m"void value round-trips"):
      roundtrip(_.value(Jdwp.Value.Void))(_.value())
    . assert(_ == Jdwp.Value.Void)

    test(m"command packet decodes to its fields"):
      val body = Jdwp.Writer(sizes).int(42).data
      Jdwp.Packet.decode(Jdwp.Packet.command(7, 1, 9, body))
    . assert: packet =>
        packet.id == 7 && !packet.reply && packet.commandSet == 1 && packet.command == 9

    test(m"reply packet decodes with its error code"):
      Jdwp.Packet.decode(Jdwp.Packet.reply(7, 13, Jdwp.Writer(sizes).data))
    . assert: packet =>
        packet.id == 7 && packet.reply && packet.code == 13

    test(m"composite event decodes a breakpoint"):
      val writer = Jdwp.Writer(sizes)
      writer.byte(Jdwp.SuspendPolicy.All.id)
      writer.int(1)
      writer.byte(Jdwp.EventKind.Breakpoint.id.toByte)
      writer.int(55)
      writer.objectId(Jdwp.Ref(88L))
      writer.location(location)
      Jdwp.Event.composite(Jdwp.Reader(writer.data, sizes)).events

    . assert:
        case Jdwp.Event.Breakpoint(request, thread, where) :: Nil =>
          request == 55 && thread.long == 88L && where == location
        case _ =>
          false

    test(m"composite event stops at an unrecognized kind"):
      val writer = Jdwp.Writer(sizes)
      writer.byte(Jdwp.SuspendPolicy.None.id)
      writer.int(2)
      writer.byte(200.toByte)
      writer.int(1)
      Jdwp.Event.composite(Jdwp.Reader(writer.data, sizes)).events

    . assert:
        case Jdwp.Event.Unknown(kind, request) :: Nil => kind == 200 && request == 1
        case _                                         => false

    test(m"error reason decodes a known JDWP code"):
      Debugger.Error.Reason(13)
    . assert(_ == Debugger.Error.Reason.ThreadNotSuspended)

    test(m"error reason decodes an unknown code as Other"):
      Debugger.Error.Reason(9999)
    . assert(_ == Debugger.Error.Reason.Other(9999))

    test(m"a session handshakes, negotiates id sizes, and reads the VM version"):
      supervise:
        val (vmSide, clientSide) = Duplex.pair()

        // A scripted fake VM: echo the handshake, answer IDSizes, then answer Version. Chunk
        // boundaries survive `Duplex.pair`, so each command arrives as its own chunk.
        val vm = async:
          val incoming = vmSide.source.toProgression.stdlib.iterator
          vmSide.send(Stream(incoming.next()))

          val idSizes = Jdwp.Packet.decode(incoming.next())
          val idBody = Jdwp.Writer(sizes).int(8).int(8).int(8).int(8).int(8).data
          vmSide.send(Stream(Jdwp.Packet.reply(idSizes.id, 0, idBody)))

          val version = Jdwp.Packet.decode(incoming.next())

          val versionBody =
            Jdwp.Writer(sizes).string(t"a fake VM").int(1).int(8).string(t"1.8.0")
              .string(t"FakeVM").data

          vmSide.send(Stream(Jdwp.Packet.reply(version.id, 0, versionBody)))

        Jdwp.Connection.exchange(clientSide): connection =>
          connection.version()

    . assert(_.vmName == t"FakeVM")

    test(m"a full attach session connects, handshakes and reads the VM version"):
      supervise:
        val (vmSide, clientSide) = Duplex.pair()

        val vm = async:
          val incoming = vmSide.source.toProgression.stdlib.iterator
          vmSide.send(Stream(incoming.next()))

          val idSizes = Jdwp.Packet.decode(incoming.next())
          val idBody = Jdwp.Writer(sizes).int(8).int(8).int(8).int(8).int(8).data
          vmSide.send(Stream(Jdwp.Packet.reply(idSizes.id, 0, idBody)))

          val version = Jdwp.Packet.decode(incoming.next())

          val versionBody =
            Jdwp.Writer(sizes).string(t"a fake VM").int(1).int(8).string(t"1.8.0")
              .string(t"FakeVM").data

          vmSide.send(Stream(Jdwp.Packet.reply(version.id, 0, versionBody)))

        // A fake attach target whose `Connectable` hands back the client end of the pair.
        Debugger(Attach(clientSide)).session: debug ?=>
          debug.version()

    . assert(_.vmName == t"FakeVM")
