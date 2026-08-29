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

// `Variable` is excluded so it resolves to this package's `vivisection.Variable` rather than the
// `ambience.Variable` (an environment variable) which `soundness` also publishes.
import soundness.{Variable as _, *}

import errorDiagnostics.stackTracesDiagnostics
import threading.platformThreading
import probates.awaitProbate
import logging.silentLogging
import strategies.throwUnsafely
import internetAccess.online
import workingDirectories.javaWorkingDirectory
import socketBackends.virtualMachineSockets
import systems.javaSystem

case class Attach(duplex: Duplex)

object Attach:
  given connectable: (Attach is Connectable) = new Connectable:
    type Self = Attach
    def connect(attach: Attach, interface: Optional[MacAddress]): Duplex = attach.duplex

  given showable: Attach is Showable = _ => t"attach"

object Tests extends Suite(m"Vivisection tests"):
  // An ephemeral free port, so live cases never collide on a fixed number and can run alongside
  // one another. The brief gap between closing the probe socket and the debuggee binding is a
  // negligible race for a test.
  def freePort(): Int =
    val socket = java.net.ServerSocket(0)
    try socket.getLocalPort finally socket.close()

  // The debuggee's classpath (this JVM's), for launching it and for opening evaluations against.
  def fixtureClasspath: LocalClasspath = System.properties.java.`class`.path().as[LocalClasspath]

  // Launches a fixture under the JDWP agent, breaks once at `source`:`line`, runs `handler` there
  // and returns its result — the shared shape behind every live case. Call it inside a `supervise`
  // block: the handler is defined there, so its `Monitor` (needed to open an evaluation) resolves
  // from that scope, and `debugFixture` reuses the same monitor for the session. The handler runs
  // on the dispatcher; `remain()` holds the thread so the result is read before the debuggee is
  // torn down. The socket/system capabilities resolve from this file's given imports.
  // `capture^` names whatever the handler closes over (typically the session monitor, via an
  // evaluation opened inside it); `Monitor^{capture}` then declares the monitor to be among those
  // captures, so a handler that captures the ambient monitor is honest rather than a separation
  // failure. (See the same pattern in `exegesis`.)
  def debugFixture[result, capture^](fixtureClass: Text, source: Text, line: Ordinal)
    ( handler: (Halt^) ?->{capture} result )
    ( using Monitor^{capture} )
  :   result =

    val classpathText = System.properties.java.`class`.path()
    val outcome = Promise[result]()
    val command: Command = sh"java -classpath $classpathText $fixtureClass"
    val debuggee: Debuggee = Debuggee(command, freePort())

    debuggee.session: debug ?=>
      // The breakpoint is set by source position while the VM stands suspended at startup, before
      // any fixture class is loaded; deferred binding resolves it as each matching class is
      // prepared — deterministically, since the loading thread stands suspended while the
      // breakpoint is installed, strictly before execution can reach it. This is the launch
      // ordering a frontend uses (breakpoints placed before resuming), exercised by every live
      // test.
      debug.breakpoint(source, line): stop ?=>
        outcome.offer(handler(using stop))
        stop.remain()

      debug.resume()
      outcome.await()

  // The visible variables at a stop, keyed by name, for concise assertions.
  def named(variables: List[Variable]): scala.collection.immutable.Map[Text, Variable] =
    variables.stdlib.groupBy(_.name).view.mapValues(_.head).toMap

  // Launches a fixture and installs an exception request rather than a breakpoint; the first
  // stop it reports is delivered to `handler`, exactly as `debugFixture` delivers a breakpoint
  // hit. The `within` filter scopes the request to throws from fixture code, keeping the
  // platform's own exception-driven control flow out of a caught-exception request.
  def exceptionFixture[result, capture^](fixtureClass: Text, uncaught: Boolean, caught: Boolean)
    ( handler: (Halt^) ?->{capture} result )
    ( using Monitor^{capture} )
  :   result =

    val classpathText = System.properties.java.`class`.path()
    val outcome = Promise[result]()
    val command: Command = sh"java -classpath $classpathText $fixtureClass"
    val debuggee: Debuggee = Debuggee(command, freePort())

    debuggee.session: debug ?=>
      debug.exceptions(uncaught, caught, within = t"vivisection.*"): stop ?=>
        outcome.offer(handler(using stop))
        stop.remain()

      debug.resume()
      outcome.await()

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

    test(m"untagged int value round-trips"):
      roundtrip(_.untaggedValue(Jdwp.Value.OfInt(1234)))(_.untaggedValue(Jdwp.Tag.IntTag))
    . assert(_ == Jdwp.Value.OfInt(1234))

    test(m"untagged reference value round-trips"):
      val value = Jdwp.Value.Reference(Jdwp.Tag.ObjectTag, Jdwp.Ref(66L))
      roundtrip(_.untaggedValue(value))(_.untaggedValue(Jdwp.Tag.ObjectTag))
    . assert(_ == Jdwp.Value.Reference(Jdwp.Tag.ObjectTag, Jdwp.Ref(66L)))

    test(m"a primitive arrayregion round-trips its untagged elements"):
      val region = Jdwp.Writer(sizes)
      region.byte(Jdwp.Tag.IntTag.id.toByte)
      region.int(3)
      region.int(10).int(20).int(30)
      Jdwp.Reader(region.data, sizes).arrayRegion()
    . assert(_ == List(Jdwp.Value.OfInt(10), Jdwp.Value.OfInt(20), Jdwp.Value.OfInt(30)))

    test(m"an object arrayregion round-trips its tagged elements"):
      val region = Jdwp.Writer(sizes)
      region.byte(Jdwp.Tag.ObjectTag.id.toByte)
      region.int(2)
      region.value(Jdwp.Value.Reference(Jdwp.Tag.StringTag, Jdwp.Ref(1L)))
      region.value(Jdwp.Value.Reference(Jdwp.Tag.ObjectTag, Jdwp.Ref(2L)))
      Jdwp.Reader(region.data, sizes).arrayRegion()
    . assert: values =>
        values == List(Jdwp.Value.Reference(Jdwp.Tag.StringTag, Jdwp.Ref(1L)),
            Jdwp.Value.Reference(Jdwp.Tag.ObjectTag, Jdwp.Ref(2L)))

    test(m"an int value inspects as its literal"):
      Jdwp.Value.OfInt(3).inspect
    . assert(_ == t"3")

    test(m"a long value inspects with its suffix"):
      Jdwp.Value.OfLong(3L).inspect
    . assert(_ == t"3L")

    test(m"a reference value inspects as tag and identity"):
      Jdwp.Value.Reference(Jdwp.Tag.ObjectTag, Jdwp.Ref(77L)).inspect
    . assert(_ == t"L＠77")

    test(m"a null reference value inspects as null"):
      Jdwp.Value.Reference(Jdwp.Tag.ObjectTag, Jdwp.Ref(0L)).inspect
    . assert(_ == t"null")

    test(m"an object snapshot inspects as its simple name and identity"):
      Variable.Snapshot.Obj(Jdwp.Ref(4021L), t"scala.collection.immutable.List").inspect
    . assert(_ == t"List＠4021")

    test(m"a string snapshot inspects as text"):
      Variable.Snapshot.Str(Jdwp.Ref(1L), t"answer").inspect
    . assert(_ == t"t\"answer\"")

    test(m"an unforced variable inspects with the unforced marker"):
      val provenance = Variable.Provenance.Field(t"Holder", Jdwp.Ref(9L), Jdwp.Ref(1L))

      Variable(t"x", Unset, t"Int", Unset, provenance, false, Variable.State.Unforced).inspect
    . assert(_ == t"x:∿∿∿")

    test(m"a demangled primitive signature names the Scala type"):
      Variable.demangle(t"I")
    . assert(_ == t"Int")

    test(m"a demangled class signature reads as a dotted name"):
      Variable.demangle(t"Lscala/collection/immutable/List;")
    . assert(_ == t"scala.collection.immutable.List")

    test(m"a demangled array signature nests"):
      Variable.demangle(t"[I")
    . assert(_ == t"Array[Int]")

    test(m"a captured field recovers the written name"):
      Variable.captured(t"seed$$1")
    . assert(_ == t"seed")

    test(m"a plain field is not treated as a capture"):
      Variable.captured(t"plain")
    . assert(_ == Unset)

    test(m"a lazy backing field recovers the written name"):
      Variable.lazyField(t"squared$$lzy1")
    . assert(_ == t"squared")

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

    test(m"a session sets a breakpoint and reads the hit from the event stream"):
      supervise:
        val (vmSide, clientSide) = Duplex.pair()

        val vm = async:
          val incoming = vmSide.source.toProgression.stdlib.iterator
          vmSide.send(Stream(incoming.next()))

          val idSizes = Jdwp.Packet.decode(incoming.next())
          val idBody = Jdwp.Writer(sizes).int(8).int(8).int(8).int(8).int(8).data
          vmSide.send(Stream(Jdwp.Packet.reply(idSizes.id, 0, idBody)))

          // The EventRequest.Set for the breakpoint; reply with request id 1.
          val request = Jdwp.Packet.decode(incoming.next())
          vmSide.send(Stream(Jdwp.Packet.reply(request.id, 0, Jdwp.Writer(sizes).int(1).data)))

          // A Composite command (command set 64, command 100) carrying one Breakpoint event.
          val event = Jdwp.Writer(sizes)
          event.byte(Jdwp.SuspendPolicy.All.id)
          event.int(1)
          event.byte(Jdwp.EventKind.Breakpoint.id.toByte)
          event.int(1)
          event.objectId(Jdwp.Ref(5L))
          event.location(location)
          vmSide.send(Stream(Jdwp.Packet.command(999, 64, 100, event.data)))

        Debugger(Attach(clientSide)).session: debug ?=>
          debug.breakpoint(location, Jdwp.SuspendPolicy.All)
          debug.events.stdlib.head.events

    . assert:
        case Jdwp.Event.Breakpoint(request, thread, where) :: Nil =>
          request == 1 && thread.long == 5L && where == location
        case _ =>
          false

    test(m"a breakpoint handler runs on the dispatcher with the stopped thread"):
      supervise:
        val (vmSide, clientSide) = Duplex.pair()
        val fired = Promise[Long]()

        // The composite is sent only after the client's `resume`, mirroring real JDWP (a
        // suspended VM emits no events until resumed) and making handler registration deterministic
        // — the client has installed the handler before it resumes.
        val vm = async:
          val incoming = vmSide.source.toProgression.stdlib.iterator
          vmSide.send(Stream(incoming.next()))

          val idSizes = Jdwp.Packet.decode(incoming.next())
          val idBody = Jdwp.Writer(sizes).int(8).int(8).int(8).int(8).int(8).data
          vmSide.send(Stream(Jdwp.Packet.reply(idSizes.id, 0, idBody)))

          val request = Jdwp.Packet.decode(incoming.next())
          vmSide.send(Stream(Jdwp.Packet.reply(request.id, 0, Jdwp.Writer(sizes).int(1).data)))

          val resume = Jdwp.Packet.decode(incoming.next())
          vmSide.send(Stream(Jdwp.Packet.reply(resume.id, 0, Jdwp.Writer(sizes).data)))

          val event = Jdwp.Writer(sizes)
          event.byte(Jdwp.SuspendPolicy.All.id)
          event.int(1)
          event.byte(Jdwp.EventKind.Breakpoint.id.toByte)
          event.int(1)
          event.objectId(Jdwp.Ref(5L))
          event.location(location)
          vmSide.send(Stream(Jdwp.Packet.command(999, 64, 100, event.data)))

          // Close so the client reader reaches EOF: chunk order delivers the composite first, then
          // the composites stream stops, the dispatcher's pump returns, and the session tears down
          // without relying on cancellation.
          vmSide.close()

        Debugger(Attach(clientSide)).session: debug ?=>
          debug.breakpoint(location): halt ?=>
            fired.offer(halt.thread.long)
            halt.remain()

          debug.resume()
          fired.await()

    . assert(_ == 5L)

    test(m"an unclaimed composite resumes exactly once, by policy"):
      supervise:
        val (vmSide, clientSide) = Duplex.pair()
        val resumed = Promise[(Int, Int)]()

        val vm = async:
          val incoming = vmSide.source.toProgression.stdlib.iterator
          vmSide.send(Stream(incoming.next()))

          val idSizes = Jdwp.Packet.decode(incoming.next())
          val idBody = Jdwp.Writer(sizes).int(8).int(8).int(8).int(8).int(8).data
          vmSide.send(Stream(Jdwp.Packet.reply(idSizes.id, 0, idBody)))

          val request = Jdwp.Packet.decode(incoming.next())
          vmSide.send(Stream(Jdwp.Packet.reply(request.id, 0, Jdwp.Writer(sizes).int(1).data)))

          // The client's own resume (before any event); reply so it returns.
          val resume = Jdwp.Packet.decode(incoming.next())
          vmSide.send(Stream(Jdwp.Packet.reply(resume.id, 0, Jdwp.Writer(sizes).data)))

          // One composite with TWO breakpoint events; a handler claims both, so the dispatcher
          // must resume the whole VM once — never once per event.
          val event = Jdwp.Writer(sizes)
          event.byte(Jdwp.SuspendPolicy.All.id)
          event.int(2)
          event.byte(Jdwp.EventKind.Breakpoint.id.toByte).int(1).objectId(Jdwp.Ref(5L))
          event.location(location)
          event.byte(Jdwp.EventKind.Breakpoint.id.toByte).int(1).objectId(Jdwp.Ref(5L))
          event.location(location)
          vmSide.send(Stream(Jdwp.Packet.command(999, 64, 100, event.data)))

          // The dispatcher's auto-resume for the composite: report its (set, command), then close
          // so its reply promise fails and the dispatcher unwinds rather than blocking on a reply
          // we never send.
          val auto = Jdwp.Packet.decode(incoming.next())
          resumed.offer((auto.commandSet, auto.command))
          vmSide.close()

        Debugger(Attach(clientSide)).session: debug ?=>
          debug.breakpoint(location): halt ?=>
            ()

          debug.resume()
          resumed.await()

    . assert(_ == (1, 9))

    // Launches a real JVM under the JDWP agent and recovers the variables at a breakpoint: the
    // marker method's parameters (a primitive, a string, an array) as local slots, and the
    // enclosing `Specimen`'s state through `this` — a field, and an unforced lazy val.
    test(m"a live session recovers the variables at a breakpoint"):
      supervise:
        debugFixture(t"vivisection.Fixture", t"vivisection.Fixture.scala", Ordinal.uniary(67)):
          stop ?=> stop.variables()

    . assert: variables =>
        val byName = named(variables)

        def snapshot(name: Text): Optional[Variable.Snapshot] =
          byName.get(name).flatMap(_.value.option).getOrElse(Unset)

        val total = snapshot(t"total") == Variable.Snapshot.Primitive(Jdwp.Value.OfInt(42))

        val tag = snapshot(t"tag") match
          case Variable.Snapshot.Str(_, text) => text == t"answer"
          case _                              => false

        val values = snapshot(t"values") match
          case Variable.Snapshot.Arr(_, Jdwp.Tag.IntTag, 3, _) => true
          case _                                               => false

        val seed = snapshot(t"seed") == Variable.Snapshot.Primitive(Jdwp.Value.OfInt(7))
        val squared = byName.get(t"squared").map(_.state).contains(Variable.State.Unforced)

        total && tag && values && seed && squared

    // Compiles `total + 1` against the debuggee's classpath, injects the classfiles over JDWP, and
    // runs the synthetic class in the debuggee — evaluating an expression over a live local.
    test(m"a live session evaluates an expression over a local"):
      supervise:
        val classpath = fixtureClasspath

        debugFixture(t"vivisection.Fixture", t"vivisection.Fixture.scala", Ordinal.uniary(67)):
          stop ?=>
            stop.evaluator(classpath): eval ?=>
              eval(t"total + 1") match
                case Variable.Snapshot.Str(_, text) => text
                case other                          => other.inspect

    . assert(_ == t"43")

    // Renders a live local through its `Inspectable` instance, resolved and invoked in the
    // debuggee: the array is typed as `Array[Int]` in the synthetic class, so its own notation
    // (`⦋…⦌`) is produced — typeclass-driven rendering, not `toString`.
    test(m"a live session renders a local through its Inspectable instance"):
      supervise:
        val classpath = fixtureClasspath

        debugFixture(t"vivisection.Fixture", t"vivisection.Fixture.scala", Ordinal.uniary(67)):
          stop ?=> stop.evaluator(classpath) { eval ?=> eval.inspect(t"values") }

    . assert(_.starts(t"⦋"))

    // The headline case: `port` erases to `Int`, but Purview recovers its declared type `Port` from
    // TASTy, so the synthetic class types it as `Port` and `.inspect` selects `Port`'s own instance
    // — rendering the domain value `⟨port 8080⟩` rather than the bare `8080` its runtime class would.
    test(m"a live session renders an opaque local through its declared type's instance"):
      supervise:
        val classpath = fixtureClasspath

        debugFixture(t"vivisection.Fixture", t"vivisection.Fixture.scala", Ordinal.uniary(67)):
          stop ?=> stop.evaluator(classpath) { eval ?=> eval.inspect(t"port") }

    . assert(_ == t"⟨port 8080⟩")

    // The declared static type of a binding, recovered from TASTy and rendered through stenography,
    // surfaced to the caller as `Variable.static`: `port` is reported as its opaque type `Port`,
    // not the `Int` it erases to.
    test(m"a live session reports a binding's stenography-rendered static type"):
      supervise:
        val classpath = fixtureClasspath

        debugFixture(t"vivisection.Fixture", t"vivisection.Fixture.scala", Ordinal.uniary(67)):
          stop ?=>
            stop.evaluator(classpath): eval ?=>
              val port = eval.variables().stdlib.find(_.name == t"port")
              port.flatMap(_.static.option).getOrElse(t"«none»")

    . assert(_ == t"vivisection.Fixture.Port")

    // Static types for a method's *body* locals, not just its parameters: `gateway` is a local
    // `val` in `marker`, and Purview recovers its declared `Port` from the method's tree — reported
    // as its static type and used to render it through `Port`'s own instance.
    test(m"a live session recovers a body-local val's static type and renders it"):
      supervise:
        val classpath = fixtureClasspath

        debugFixture(t"vivisection.Fixture", t"vivisection.Fixture.scala", Ordinal.uniary(67)):
          stop ?=>
            stop.evaluator(classpath): eval ?=>
              val gateway = eval.variables().stdlib.find(_.name == t"gateway")
              val static = gateway.flatMap(_.static.option).getOrElse(t"«none»")
              (static, eval.inspect(t"gateway"))

    . assert(_ == (t"vivisection.Fixture.Port", t"⟨port 443⟩"))

    // ── Variable-recovery matrix ────────────────────────────────────────────────────────────────
    // One launch of `Menagerie` captures every local at a single breakpoint; the cases below are
    // granular assertions over that one snapshot, so the whole width of value recovery costs one
    // debuggee.
    val menagerie: scala.collection.immutable.Map[Text, Variable] =
      supervise:
        debugFixture(t"vivisection.Menagerie", t"vivisection.Menagerie.scala", Ordinal.uniary(57)):
          stop ?=> named(stop.variables())

    def valueOf(name: Text): Optional[Variable.Snapshot] =
      menagerie.get(name).flatMap(_.value.option).getOrElse(Unset)

    def erasedOf(name: Text): Optional[Text] =
      menagerie.get(name).map(_.erased).getOrElse(Unset)

    test(m"a byte local is recovered with its value"):
      valueOf(t"byte")
    . assert(_ == Variable.Snapshot.Primitive(Jdwp.Value.OfByte(-7)))

    test(m"a short local is recovered with its value"):
      valueOf(t"short")
    . assert(_ == Variable.Snapshot.Primitive(Jdwp.Value.OfShort(1234)))

    test(m"an int local is recovered with its value"):
      valueOf(t"int")
    . assert(_ == Variable.Snapshot.Primitive(Jdwp.Value.OfInt(42)))

    test(m"a long local is recovered with its value"):
      valueOf(t"long")
    . assert(_ == Variable.Snapshot.Primitive(Jdwp.Value.OfLong(9999999999L)))

    test(m"a float local is recovered with its value"):
      valueOf(t"float")
    . assert(_ == Variable.Snapshot.Primitive(Jdwp.Value.OfFloat(3.5f)))

    test(m"a double local is recovered with its value"):
      valueOf(t"double")
    . assert(_ == Variable.Snapshot.Primitive(Jdwp.Value.OfDouble(2.5)))

    test(m"a char local is recovered with its value"):
      valueOf(t"char")
    . assert(_ == Variable.Snapshot.Primitive(Jdwp.Value.OfChar('Z')))

    test(m"a boolean local is recovered with its value"):
      valueOf(t"boolean")
    . assert(_ == Variable.Snapshot.Primitive(Jdwp.Value.OfBoolean(true)))

    test(m"a string local is recovered with its text"):
      valueOf(t"text") match
        case Variable.Snapshot.Str(_, text) => text
        case _                              => t"«not a string»"
    . assert(_ == t"hello")

    test(m"an empty string local is recovered"):
      valueOf(t"empty") match
        case Variable.Snapshot.Str(_, text) => text
        case _                              => t"«not a string»"
    . assert(_ == t"")

    test(m"an int-array local is recovered with component and length"):
      valueOf(t"ints") match
        case Variable.Snapshot.Arr(_, tag, length, _) => (tag, length)
        case _                                        => (Jdwp.Tag.VoidTag, -1)
    . assert(_ == (Jdwp.Tag.IntTag, 3))

    test(m"a byte-array local is recovered with component and length"):
      valueOf(t"bytes") match
        case Variable.Snapshot.Arr(_, tag, length, _) => (tag, length)
        case _                                        => (Jdwp.Tag.VoidTag, -1)
    . assert(_ == (Jdwp.Tag.ByteTag, 3))

    test(m"a long array reports full length but a bounded prefix"):
      valueOf(t"many") match
        case Variable.Snapshot.Arr(_, _, length, prefix) => (length, prefix.stdlib.length)
        case _                                           => (-1, -1)
    . assert(_ == (13, 10))

    test(m"an int local reports its erased type"):
      erasedOf(t"int")
    . assert(_ == t"Int")

    test(m"an int-array local reports its erased type"):
      erasedOf(t"ints")
    . assert(_ == t"Array[Int]")

    test(m"a string local reports its erased type"):
      erasedOf(t"text")
    . assert(_ == t"java.lang.String")

    // ── Captured-state matrix ───────────────────────────────────────────────────────────────────
    // At a breakpoint inside a local class's method, nothing is an ordinary local slot: every
    // binding is recovered by un-flattening `this`'s captured fields and walking its `$outer` chain.
    val closures: scala.collection.immutable.Map[Text, Variable] =
      supervise:
        debugFixture(t"vivisection.Closures", t"vivisection.Closures.scala", Ordinal.uniary(56)):
          stop ?=> named(stop.variables())

    test(m"captured bindings are recovered by their written names"):
      closures.keys.toList.map(_.s).sorted.mkString(",")
    . assert(_ == "cached,label,seed,tally")

    test(m"a captured val is recovered with its value"):
      closures.get(t"label").flatMap(_.value.option).getOrElse(Unset) match
        case Variable.Snapshot.Str(_, text) => text
        case _                              => t"«absent»"
    . assert(_ == t"captured")

    test(m"a captured var is unboxed from its ref cell and marked mutable"):
      val tally = closures.get(t"tally")
      val value = tally.flatMap(_.value.option).getOrElse(Unset)
      (value == Variable.Snapshot.Primitive(Jdwp.Value.OfInt(100)), tally.map(_.mutable))
    . assert(_ == (true, scala.Some(true)))

    test(m"a binding captured through the outer chain is recovered"):
      closures.get(t"seed").flatMap(_.value.option).getOrElse(Unset)
    . assert(_ == Variable.Snapshot.Primitive(Jdwp.Value.OfInt(100)))

    test(m"an unforced lazy val is reported unforced and never evaluated"):
      closures.get(t"cached").map(_.state)
    . assert(_ == scala.Some(Variable.State.Unforced))

    // ── Rendering / purity matrix ───────────────────────────────────────────────────────────────
    // Three locals whose types render differently: a derived (real, pure) instance renders cleanly;
    // a Showable-only type is borrowed under `⸢…⸣`; a toString-only type falls to `“…”`. The
    // markers are how the debugger signals a value was not rendered through a verified-pure instance.
    val renderings: (Text, Text, Text) =
      supervise:
        debugFixture(t"vivisection.Renderings", t"vivisection.Renderings.scala", Ordinal.uniary(62)):
          stop ?=>
            stop.evaluator(fixtureClasspath): eval ?=>
              (eval.inspect(t"point"), eval.inspect(t"tagged"), eval.inspect(t"plain"))

    test(m"a derived Inspectable renders structurally with no fallback marker"):
      renderings(0)
    . assert(_ == t"Point(x:3 ╱ y:4)")

    test(m"a Showable-only type renders under the borrowed marker"):
      renderings(1)
    . assert(_ == t"⸢tag:alpha⸣")

    test(m"a toString-only type renders under the toString marker"):
      renderings(2)
    . assert(_ == t"“Plain#7”")

    // ── Static-type matrix ──────────────────────────────────────────────────────────────────────
    // Richer declared types recovered from TASTy and rendered through stenography, keyed by name.
    val typeShapes: scala.collection.immutable.Map[Text, Text] =
      supervise:
        debugFixture(t"vivisection.Types", t"vivisection.Types.scala", Ordinal.uniary(48)):
          stop ?=>
            stop.evaluator(fixtureClasspath): eval ?=>
              val bindings = eval.variables().stdlib.flatMap: variable =>
                variable.static.option.map(static => (variable.name, static))

              bindings.toMap

    test(m"a generic collection's static type keeps its type argument"):
      typeShapes.get(t"list")
    . assert(_ == scala.Some(t"List[Int]"))

    test(m"a tuple's static type is rendered in tuple syntax"):
      typeShapes.get(t"pair")
    . assert(_ == scala.Some(t"(Int, java.lang.String)"))

    test(m"a function's static type is rendered in arrow syntax"):
      typeShapes.get(t"function")
    . assert(_ == scala.Some(t"Int => java.lang.String"))

    test(m"an optional's static type is recovered"):
      typeShapes.get(t"option")
    . assert(_ == scala.Some(t"scala.Option[Int]"))

    // ── Evaluation matrix ───────────────────────────────────────────────────────────────────────
    // Compile-and-run expressions over the `Menagerie` locals: arithmetic, a comparison, a method
    // call, and array indexing, each producing a value read back as text.
    val evaluations: (Text, Text, Text, Text) =
      supervise:
        debugFixture(t"vivisection.Menagerie", t"vivisection.Menagerie.scala", Ordinal.uniary(57)):
          stop ?=>
            stop.evaluator(fixtureClasspath): eval ?=>
              def text(expression: Text): Text = eval(expression) match
                case Variable.Snapshot.Str(_, string) => string
                case other                            => other.inspect

              (text(t"int*2"), text(t"int > 40"), text(t"text.length"), text(t"ints(0)"))

    test(m"an arithmetic expression over a local evaluates"):
      evaluations(0)
    . assert(_ == t"84")

    test(m"a comparison expression over a local evaluates"):
      evaluations(1)
    . assert(_ == t"true")

    test(m"a method call on a local evaluates"):
      evaluations(2)
    . assert(_ == t"5")

    test(m"an array-indexing expression over a local evaluates"):
      evaluations(3)
    . assert(_ == t"10")

    // A launch session drains the debuggee's console from the moment of the fork: its output is
    // readable as a stream, and its exit status resolves when it terminates — so a debuggee
    // can never block against a full pipe, and a frontend can relay its output.
    test(m"a launch session captures the debuggee's output and exit status"):
      supervise:
        val classpathText = System.properties.java.`class`.path()
        val command: Command = sh"java -classpath $classpathText vivisection.Recount"
        val debuggee: Debuggee = Debuggee(command, freePort())

        debuggee.session: debug ?=>
          debug.resume()

          debug.console.let: console =>
            // The agent's own "Listening for transport" banner precedes the program's output.
            val text = console.stdout.stdlib.toList.map(_.utf8).mkString.tt.trim
            (text.ends(t"mark"), console.exited.await())

    . assert(_ == (true, Exit.Ok))

    // An exception request scoped to uncaught throws skips the caught `IllegalStateException`
    // and stops at the `RuntimeException` which ends the run, reporting its class, its message
    // (read from `detailMessage` directly — no debuggee code is invoked) and that nothing
    // catches it.
    test(m"an uncaught-exception request stops at the uncaught throw only"):
      supervise:
        exceptionFixture(t"vivisection.Exceptions", uncaught = true, caught = false):
          stop ?=> stop.exceptionInfo()

    . assert(_ == Halt.ExceptionInfo(t"java.lang.RuntimeException", t"unhandled", false))

    // With caught throws included, the first stop is the `IllegalStateException` inside `flaky`,
    // reported as caught.
    test(m"a caught-exception request stops at the caught throw first"):
      supervise:
        exceptionFixture(t"vivisection.Exceptions", uncaught = true, caught = true):
          stop ?=> stop.exceptionInfo()

    . assert(_ == Halt.ExceptionInfo(t"java.lang.IllegalStateException", t"recoverable", true))

    // Assignment writes through provenance: a local slot is written in place and observed
    // changed when the variables are read again at the same stop.
    test(m"assigning a local slot changes its value at the stop"):
      supervise:
        debugFixture(t"vivisection.Menagerie", t"vivisection.Menagerie.scala", Ordinal.uniary(57)):
          stop ?=>
            named(stop.variables()).get(t"int").foreach: variable =>
              stop.assign(variable, Jdwp.Value.OfInt(99))

            named(stop.variables()).get(t"int").map(_.value)

    . assert(_ == scala.Some(Variable.Snapshot.Primitive(Jdwp.Value.OfInt(99))))

    // A captured `var` lives in a ref cell; its assignment routes through the cell's `elem`.
    test(m"assigning a captured var writes through its ref cell"):
      supervise:
        debugFixture(t"vivisection.Closures", t"vivisection.Closures.scala", Ordinal.uniary(56)):
          stop ?=>
            named(stop.variables()).get(t"tally").foreach: variable =>
              stop.assign(variable, Jdwp.Value.OfInt(7))

            named(stop.variables()).get(t"tally").map(_.value)

    . assert(_ == scala.Some(Variable.Snapshot.Primitive(Jdwp.Value.OfInt(7))))

    // A watchpoint placed once `Account` is loaded (from a function-breakpoint stop at
    // `deposit`) reports each write to `balance` before it lands. The handler holds the thread
    // only for the write it cares about — the remain-based conditional pattern — so the `30`
    // write resumes automatically and the `100` write is the one observed.
    test(m"a watchpoint reports a field write with its incoming value"):
      supervise:
        val classpathText = System.properties.java.`class`.path()
        val outcome = Promise[Jdwp.Value]()
        val command: Command = sh"java -classpath $classpathText vivisection.Ledger"
        val debuggee: Debuggee = Debuggee(command, freePort())

        debuggee.session: debug ?=>
          // The watch needs `Account` loaded, so a deferred function breakpoint holds the VM at
          // the first `deposit`; the watch is then installed from this thread and the entry
          // breakpoint cleared before resuming.
          val loaded = Promise[Unit]()

          val entry = debug.breakpoint(t"vivisection.Ledger$$Account", t"deposit"): stop ?=>
            loaded.offer(())
            stop.remain()

          debug.resume()
          loaded.await()
          entry.clear()

          debug.watch(t"vivisection.Ledger$$Account", t"balance"): stop ?=>
            stop.cause match
              case Halt.Cause.Modification(_, _, _, incoming) =>
                if incoming == Jdwp.Value.OfInt(100) then
                  outcome.offer(incoming)
                  stop.remain()

              case _ =>
                ()

          debug.resume()
          outcome.await()

    . assert(_ == Jdwp.Value.OfInt(100))

    // A function breakpoint named before its class is loaded binds on preparation and stops at
    // the method's entry.
    test(m"a function breakpoint set before class load stops at method entry"):
      supervise:
        val classpathText = System.properties.java.`class`.path()
        val outcome = Promise[Boolean]()
        val command: Command = sh"java -classpath $classpathText vivisection.Recount"
        val debuggee: Debuggee = Debuggee(command, freePort())

        debuggee.session: debug ?=>
          debug.breakpoint(t"vivisection.Recount$$", t"tally"): stop ?=>
            outcome.offer(true)
            stop.remain()

          debug.resume()
          outcome.await()

    . assert(_ == true)

    // Popping the stopped frame and resuming re-executes the call: `tally` runs once from
    // `main`, so a second hit at the same line is proof of the restart.
    test(m"popping the stopped frame re-executes the call on resume"):
      supervise:
        val classpathText = System.properties.java.`class`.path()
        val outcome = Promise[Int]()
        val hits = java.util.concurrent.atomic.AtomicInteger(0)
        val command: Command = sh"java -classpath $classpathText vivisection.Recount"
        val debuggee: Debuggee = Debuggee(command, freePort())

        debuggee.session: debug ?=>
          debug.breakpoint(t"vivisection.Recount.scala", Ordinal.uniary(43)): stop ?=>
            if hits.incrementAndGet() == 1
            then stop.frames().stdlib.headOption.foreach { (frame, _) => stop.pop(frame) }
            else
              outcome.offer(hits.get)
              stop.remain()

          debug.resume()
          outcome.await()

    . assert(_ == 2)

    // An assignment through the evaluator compiles the right-hand side at the variable's declared
    // type, writes it into the slot, and a subsequent evaluation over the same frame sees the new
    // value.
    test(m"an evaluated assignment writes a local which evaluation then sees"):
      supervise:
        debugFixture(t"vivisection.Menagerie", t"vivisection.Menagerie.scala", Ordinal.uniary(57)):
          stop ?=>
            stop.evaluator(fixtureClasspath): eval ?=>
              eval.assign(t"int", t"int + 58")

              eval(t"int") match
                case Variable.Snapshot.Str(_, text) => text
                case other                          => other.inspect

    . assert(_ == t"100")
