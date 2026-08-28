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
      Variable(t"x", Unset, t"Int", Unset, Variable.Provenance.Field(t"Holder"), false,
          Variable.State.Unforced).inspect
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
        val classpath = System.properties.java.`class`.path()
        val captured = Promise[List[Variable]]()
        val marker = Ordinal.uniary(72)

        // Ascribed to the bare types: `sh"…"` infers a singleton-refined `Command` and the case
        // class tracks the field's identity, which the invariant `Sessional.Self` would not match.
        val command: Command = sh"java -classpath $classpath vivisection.Fixture"
        val debuggee: Debuggee = Debuggee(command, 5099)

        debuggee.session: debug ?=>
          // Resume from the agent's start-up suspension, then wait for `Specimen` to load so its
          // line table resolves. `Fixture.main` pauses long enough for this to win the race.
          debug.resume()

          def waitFor(remaining: Int): List[Jdwp.Location] =
            val locations = debug.locate(t"vivisection.Fixture.scala", marker)

            if locations.stdlib.nonEmpty then locations
            else if remaining <= 0 then locations
            else
              Thread.sleep(50)
              waitFor(remaining - 1)

          waitFor(120).stdlib.foreach: location =>
            debug.breakpoint(location): halt ?=>
              captured.offer(halt.variables())
              halt.remain()

          captured.await()

    . assert: variables =>
        val byName = variables.stdlib.groupBy(_.name).view.mapValues(_.head).toMap

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
        val classpathText = System.properties.java.`class`.path()
        val classpath = classpathText.as[LocalClasspath]
        val evaluated = Promise[Text]()
        val marker = Ordinal.uniary(72)

        val command: Command = sh"java -classpath $classpathText vivisection.Fixture"
        val debuggee: Debuggee = Debuggee(command, 5100)

        debuggee.session: debug ?=>
          debug.resume()

          def waitFor(remaining: Int): List[Jdwp.Location] =
            val locations = debug.locate(t"vivisection.Fixture.scala", marker)

            if locations.stdlib.nonEmpty then locations
            else if remaining <= 0 then locations
            else
              Thread.sleep(50)
              waitFor(remaining - 1)

          waitFor(120).stdlib.foreach: location =>
            debug.breakpoint(location): halt ?=>
              halt.evaluator(classpath): eval ?=>
                eval(t"total + 1") match
                  case Variable.Snapshot.Str(_, text) => evaluated.offer(text)
                  case other                          => evaluated.offer(other.inspect)

              halt.remain()

          evaluated.await()

    . assert(_ == t"43")

    // Renders a live local through its `Inspectable` instance, resolved and invoked in the
    // debuggee: the array is typed as `Array[Int]` in the synthetic class, so its own notation
    // (`⦋…⦌`) is produced — typeclass-driven rendering, not `toString`.
    test(m"a live session renders a local through its Inspectable instance"):
      supervise:
        val classpathText = System.properties.java.`class`.path()
        val classpath = classpathText.as[LocalClasspath]
        val rendered = Promise[Text]()
        val marker = Ordinal.uniary(72)

        val command: Command = sh"java -classpath $classpathText vivisection.Fixture"
        val debuggee: Debuggee = Debuggee(command, 5101)

        debuggee.session: debug ?=>
          debug.resume()

          def waitFor(remaining: Int): List[Jdwp.Location] =
            val locations = debug.locate(t"vivisection.Fixture.scala", marker)

            if locations.stdlib.nonEmpty then locations
            else if remaining <= 0 then locations
            else
              Thread.sleep(50)
              waitFor(remaining - 1)

          waitFor(120).stdlib.foreach: location =>
            debug.breakpoint(location): halt ?=>
              halt.evaluator(classpath): eval ?=>
                rendered.offer(eval.inspect(t"values"))

              halt.remain()

          rendered.await()

    . assert(_.starts(t"⦋"))

    // The headline case: `port` erases to `Int`, but Purview recovers its declared type `Port` from
    // TASTy, so the synthetic class types it as `Port` and `.inspect` selects `Port`'s own instance
    // — rendering the domain value `⟨port 8080⟩` rather than the bare `8080` its runtime class would.
    test(m"a live session renders an opaque local through its declared type's instance"):
      supervise:
        val classpathText = System.properties.java.`class`.path()
        val classpath = classpathText.as[LocalClasspath]
        val rendered = Promise[Text]()
        val marker = Ordinal.uniary(72)

        val command: Command = sh"java -classpath $classpathText vivisection.Fixture"
        val debuggee: Debuggee = Debuggee(command, 5102)

        debuggee.session: debug ?=>
          debug.resume()

          def waitFor(remaining: Int): List[Jdwp.Location] =
            val locations = debug.locate(t"vivisection.Fixture.scala", marker)

            if locations.stdlib.nonEmpty then locations
            else if remaining <= 0 then locations
            else
              Thread.sleep(50)
              waitFor(remaining - 1)

          waitFor(120).stdlib.foreach: location =>
            debug.breakpoint(location): halt ?=>
              halt.evaluator(classpath): eval ?=>
                rendered.offer(eval.inspect(t"port"))

              halt.remain()

          rendered.await()

    . assert(_ == t"⟨port 8080⟩")

    // The declared static type of a binding, recovered from TASTy and rendered through stenography,
    // surfaced to the caller as `Variable.static`: `port` is reported as its opaque type `Port`,
    // not the `Int` it erases to.
    test(m"a live session reports a binding's stenography-rendered static type"):
      supervise:
        val classpathText = System.properties.java.`class`.path()
        val classpath = classpathText.as[LocalClasspath]
        val reported = Promise[Text]()
        val marker = Ordinal.uniary(72)

        val command: Command = sh"java -classpath $classpathText vivisection.Fixture"
        val debuggee: Debuggee = Debuggee(command, 5103)

        debuggee.session: debug ?=>
          debug.resume()

          def waitFor(remaining: Int): List[Jdwp.Location] =
            val locations = debug.locate(t"vivisection.Fixture.scala", marker)

            if locations.stdlib.nonEmpty then locations
            else if remaining <= 0 then locations
            else
              Thread.sleep(50)
              waitFor(remaining - 1)

          waitFor(120).stdlib.foreach: location =>
            debug.breakpoint(location): halt ?=>
              halt.evaluator(classpath): eval ?=>
                val port = eval.variables().stdlib.find(_.name == t"port")
                reported.offer(port.flatMap(_.static.option).getOrElse(t"«none»"))

              halt.remain()

          reported.await()

    . assert(_ == t"vivisection.Fixture.Port")

    // Static types for a method's *body* locals, not just its parameters: `gateway` is a local
    // `val` in `marker`, and Purview recovers its declared `Port` from the method's tree — reported
    // as its static type and used to render it through `Port`'s own instance.
    test(m"a live session recovers a body-local val's static type and renders it"):
      supervise:
        val classpathText = System.properties.java.`class`.path()
        val classpath = classpathText.as[LocalClasspath]
        val outcome = Promise[(Text, Text)]()
        val marker = Ordinal.uniary(72)

        val command: Command = sh"java -classpath $classpathText vivisection.Fixture"
        val debuggee: Debuggee = Debuggee(command, 5104)

        debuggee.session: debug ?=>
          debug.resume()

          def waitFor(remaining: Int): List[Jdwp.Location] =
            val locations = debug.locate(t"vivisection.Fixture.scala", marker)

            if locations.stdlib.nonEmpty then locations
            else if remaining <= 0 then locations
            else
              Thread.sleep(50)
              waitFor(remaining - 1)

          waitFor(120).stdlib.foreach: location =>
            debug.breakpoint(location): halt ?=>
              halt.evaluator(classpath): eval ?=>
                val gateway = eval.variables().stdlib.find(_.name == t"gateway")
                val static = gateway.flatMap(_.static.option).getOrElse(t"«none»")
                outcome.offer((static, eval.inspect(t"gateway")))

              halt.remain()

          outcome.await()

    . assert(_ == (t"vivisection.Fixture.Port", t"⟨port 443⟩"))
