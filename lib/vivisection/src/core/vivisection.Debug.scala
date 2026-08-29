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

import scala.caps
import scala.collection.immutable as sci

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import parasite.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*

object Debug:
  // A launched debuggee's console and lifecycle, present only in a launch session: windows over
  // its standard output and error — drained continuously from the moment of the fork, so the
  // child can never block against a full pipe — and a promise of its exit status. An attach
  // session has no console: the target's streams belong to whoever started it.
  class Console private[vivisection]():
    private[vivisection] val out: Relay[Data] = Relay()
    private[vivisection] val err: Relay[Data] = Relay()

    val exited: Promise[Exit] = Promise()

    def stdout: Chain[Data] = out.lazyList
    def stderr: Chain[Data] = err.lazyList

  object Event:
    given communicable: Event is Communicable =
      case Logpoint(message) => m"$message"

  // The loggable events a session emits on its own initiative — currently just logpoint messages.
  enum Event:
    case Logpoint(message: Text) extends Event, Log.Runtime

  // A breakpoint handler: run on the dispatcher task when a hit is claimed, with a `Halt` lent for
  // the duration of the stop. The halt carries the tactic the dispatcher supplied, so the handler
  // needs no error capability of its own.
  type Handler = (halt: Halt^) ?=> Unit

// A live debug session: the capability lent inside `target.session { debug ?=> … }`. Sealed
// (`ExclusiveCapability`), so it cannot outlive the session that lends it. Its operations are the
// programmer-facing surface over the wire-level `Jdwp.Connection` it wraps; event dispatch and the
// handler registry live in the connection, which owns the session monitor.
class Debug private[vivisection]
  ( connection: Jdwp.Connection, val console: Optional[Debug.Console] = Unset )
extends caps.ExclusiveCapability:
  @scala.caps.unsafe.untrackedCaptures
  private var capabilities0: Optional[Jdwp.Capabilities] = Unset

  // The VM's advertised capabilities, fetched once and memoized; a frontend forwards several of
  // these flags when it announces what it supports.
  def capabilities()(using Tactic[Debugger.Error]): Jdwp.Capabilities =
    capabilities0.or:
      val fetched = connection.capabilitiesNew()
      capabilities0 = fetched
      fetched

  def version()(using Tactic[Debugger.Error]): Jdwp.Version = connection.version()
  def threads()(using Tactic[Debugger.Error]): List[ThreadId] = connection.allThreads()
  def suspend()(using Tactic[Debugger.Error]): Unit = connection.suspendAll()
  def resume()(using Tactic[Debugger.Error]): Unit = connection.resumeAll()

  def suspend(thread: ThreadId)(using Tactic[Debugger.Error]): Unit =
    connection.suspendThread(thread)

  def resume(thread: ThreadId)(using Tactic[Debugger.Error]): Unit =
    connection.resumeThread(thread)

  def name(thread: ThreadId)(using Tactic[Debugger.Error]): Text = connection.threadName(thread)

  def frames(thread: ThreadId)(using Tactic[Debugger.Error]): List[(FrameId, Jdwp.Location)] =
    connection.frames(thread, 0, connection.frameCount(thread))

  // The event stream: every composite the (suspending or running) VM sends back which no
  // registered handler claimed. This is the primitive; a caller drains it and reacts, or awaits
  // a particular event, and owns any resumption its suspend policy calls for. Reading it
  // consumes the events as they arrive.
  def events: Chain[Jdwp.Event.Composite] = connection.unclaimed.lazyList

  // Sets a breakpoint at a resolved location, returning the request id used to `clear` it. The VM
  // reports each hit as a `Breakpoint` event on `events`, suspending per the given policy.
  def breakpoint(location: Jdwp.Location, policy: Jdwp.SuspendPolicy = Jdwp.SuspendPolicy.All)
    ( using Tactic[Debugger.Error] )
  :   Int =

    val modifiers: List[Jdwp.Modifier] = List(Jdwp.Modifier.LocationOnly(location))
    connection.eventRequestSet(Jdwp.EventKind.Breakpoint, policy, modifiers)

  // Requests notification when a reference type whose name matches `pattern` is prepared (loaded
  // and linked). Each match arrives as a `ClassPrepared` event on `events`, suspending the loading
  // thread — so a caller can resolve a breakpoint in a class not yet loaded when the session began.
  // `pattern` follows the JDWP class-match form: an exact name, or one bounded by a single `*`.
  def classPrepare(pattern: Text)(using Tactic[Debugger.Error]): Int =
    val modifiers: List[Jdwp.Modifier] = List(Jdwp.Modifier.ClassMatch(pattern))
    val policy = Jdwp.SuspendPolicy.EventThread
    connection.eventRequestSet(Jdwp.EventKind.ClassPrepare, policy, modifiers)

  // Requests a single step on a thread; the VM reports it as a `SingleStep` event on `events`.
  def step
    ( thread: ThreadId,
      depth:  Jdwp.StepDepth = Jdwp.StepDepth.Over,
      size:   Jdwp.StepSize = Jdwp.StepSize.Line )
    ( using Tactic[Debugger.Error] )
  :   Int =

    val modifiers: List[Jdwp.Modifier] =
      List(Jdwp.Modifier.Step(thread, size, depth), Jdwp.Modifier.Count(1))

    connection.eventRequestSet(Jdwp.EventKind.SingleStep, Jdwp.SuspendPolicy.EventThread, modifiers)

  // Cancels a previously-set event request.
  def clear(kind: Jdwp.EventKind, request: Int)(using Tactic[Debugger.Error]): Unit =
    connection.eventRequestClear(kind, request)

  // Sets a breakpoint whose hits run `handler` on the dispatcher task, resuming afterwards by
  // the suspend policy unless the handler calls `remain()`. A conditional breakpoint is simply a
  // handler which tests its condition and does nothing otherwise. The returned handle revokes
  // the breakpoint.
  def breakpoint(location: Jdwp.Location)(handler: Debug.Handler)
    ( using Tactic[Debugger.Error] )
  :   Breakpoint^{this} =

    val request = breakpoint(location, Jdwp.SuspendPolicy.All)
    connection.register(Jdwp.EventKind.Breakpoint, request): halt => handler(using halt)
    new Breakpoint(this, Jdwp.EventKind.Breakpoint, request)

  // Requests notification of thrown exceptions — uncaught ones, caught ones, or both — running
  // `handler` at each with `Cause.Thrown` describing the exception in flight (see
  // `Halt.exceptionInfo`). `within` optionally restricts the request to exceptions thrown from
  // classes matching a class pattern (an exact name, or one bounded by a single `*`): a
  // caught-exception request without one stops inside the platform's own exception-driven
  // control flow, which starts long before any application class runs.
  def exceptions
    ( uncaught: Boolean = true,
      caught:   Boolean = false,
      within:   Optional[Text] = Unset )
    ( handler: Debug.Handler )
    ( using Tactic[Debugger.Error] )
  :   Breakpoint^{this} =

    val exceptionOnly = Jdwp.Modifier.ExceptionOnly(Jdwp.Ref.empty, caught, uncaught)

    val modifiers: List[Jdwp.Modifier] =
      within.lay(List(exceptionOnly)): pattern =>
        List(exceptionOnly, Jdwp.Modifier.ClassMatch(pattern))

    val request =
      connection.eventRequestSet(Jdwp.EventKind.Exception, Jdwp.SuspendPolicy.All, modifiers)

    connection.register(Jdwp.EventKind.Exception, request): halt => handler(using halt)
    new Breakpoint(this, Jdwp.EventKind.Exception, request)

  // Requests notification when a field of the named (runtime) class is written — or, with
  // `access`, read — running `handler` at each with `Cause.Modification` carrying the incoming
  // value, observed before the write lands (or `Cause.Access`). The class must already be
  // loaded, which it is whenever the watch is placed from a live variables view. Absent when the
  // class or field cannot be found, or the VM lacks the capability.
  def watch(className: Text, field: Text, access: Boolean = false)(handler: Debug.Handler)
    ( using Tactic[Debugger.Error] )
  :   Optional[Breakpoint^{this}] =

    val capable =
      if access then capabilities().canWatchFieldAccess
      else capabilities().canWatchFieldModification

    if !capable then Unset else
      val signature = t"L${className.s.replace('.', '/').nn};"

      connection.classesBySignature(signature).stdlib.headOption match
        case scala.Some(info) =>
          connection.fields(info.cls).stdlib.find(_.name == field) match
            case scala.Some(fieldInfo) =>
              val kind =
                if access then Jdwp.EventKind.FieldAccess else Jdwp.EventKind.FieldModified

              val modifiers = List(Jdwp.Modifier.FieldOnly(info.cls, fieldInfo.field))
              val request = connection.eventRequestSet(kind, Jdwp.SuspendPolicy.All, modifiers)
              connection.register(kind, request): halt => handler(using halt)
              new Breakpoint(this, kind, request)

            case _ =>
              Unset

        case _ =>
          Unset

  // Sets a breakpoint at the entry of every method with the given name on the named class — the
  // runtime class name, so an `object`'s methods live on its `$`-suffixed class — binding now if
  // the class is loaded and deferring to its preparation otherwise, exactly as a source
  // breakpoint defers. Every overload binds; methods without executable line information
  // (abstract, native) are skipped.
  def breakpoint(className: Text, method: Text)(handler: Debug.Handler)
    ( using Tactic[Debugger.Error] )
  :   SourceBreakpoint^{this} =

    breakpoint(className, method, (_: Jdwp.Location) => ())(handler)

  def breakpoint(className: Text, method: Text, bound: Jdwp.Location => Unit)
    ( handler: Debug.Handler )
    ( using Tactic[Debugger.Error] )
  :   SourceBreakpoint^{this} =

    val modifiers: List[Jdwp.Modifier] = List(Jdwp.Modifier.ClassMatch(className))
    val policy = Jdwp.SuspendPolicy.EventThread
    val prepare = connection.eventRequestSet(Jdwp.EventKind.ClassPrepare, policy, modifiers)
    val handle = new SourceBreakpoint(this, prepare)

    def bind(location: Jdwp.Location)(using Tactic[Debugger.Error]): Unit =
      if handle.admits(location) then
        val request = breakpoint(location, Jdwp.SuspendPolicy.All)
        connection.register(Jdwp.EventKind.Breakpoint, request): halt => handler(using halt)

        handle.record(location, request) match
          case revoked: Int => remove(Jdwp.EventKind.Breakpoint, revoked)
          case _            => bound(location)

    def entries(tag: Jdwp.TypeTag, cls: ReferenceTypeId)(using Tactic[Debugger.Error])
    :   sci.List[Jdwp.Location] =

      connection.methods(cls).stdlib.filter(_.name == method).flatMap: info =>
        safely(connection.lineTable(cls, info.method)) match
          case table: Jdwp.LineTable if table.start >= 0 =>
            sci.List(Jdwp.Location(tag, cls, info.method, table.start))

          case _ =>
            sci.List()

    // Laundered for the same reason as the source-position form above.
    val prepareHandler: Jdwp.Event.ClassPrepared => Unit =
      caps.unsafe.unsafeAssumePure: event =>
        val outcome: Optional[Unit] = safely[Debugger.Error]:
          entries(event.tag, event.cls).foreach(bind(_))

        outcome.let(identity)

    connection.registerPrepare(prepare)(prepareHandler)

    val signature = t"L${className.s.replace('.', '/').nn};"

    connection.classesBySignature(signature).stdlib.foreach: info =>
      entries(info.tag, info.cls).foreach(bind(_))

    handle

  // Installs a logpoint: a breakpoint which logs its message and immediately resumes, so the
  // program is never paused beyond the handler itself.
  def logpoint(location: Jdwp.Location)(message: (halt: Halt^) ?=> Text)
    ( using Tactic[Debugger.Error], (Debug.Event is Loggable)^ )
  :   Breakpoint^{this} =

    breakpoint(location):
      Log.info(Debug.Event.Logpoint(message))

  private[vivisection] def remove(kind: Jdwp.EventKind, request: Int)
    ( using Tactic[Debugger.Error] )
  :   Unit =

    connection.unregister(kind, request)
    connection.eventRequestClear(kind, request)

  private[vivisection] def removePrepare(request: Int)(using Tactic[Debugger.Error]): Unit =
    connection.unregisterPrepare(request)
    connection.eventRequestClear(Jdwp.EventKind.ClassPrepare, request)

  // Sets a breakpoint by source position: it binds now in every loaded class the position
  // resolves in, and later in each matching class as it is prepared — so a breakpoint may be set
  // before its class is loaded, which is how a frontend works (breakpoints are placed before
  // launch). Binding on prepare resolves against the one freshly-prepared class, never a whole-VM
  // scan (see `locateIn`), and happens while the loading thread stands suspended, so the
  // breakpoint is installed strictly before execution can reach it. Where the VM supports
  // source-name filters, prepare notifications are narrowed to the file; otherwise every
  // prepared class is offered and non-matches are discarded.
  def breakpoint(source: Text, line: Ordinal)(handler: Debug.Handler)
    ( using Tactic[Debugger.Error] )
  :   SourceBreakpoint^{this} =

    breakpoint(source, line, (_: Jdwp.Location) => ())(handler)

  // `bound` is notified at each successful binding — after the initial sweep for a loaded
  // class, or from the dispatcher when a deferred binding lands — which is how a frontend
  // learns that an unverified breakpoint has become real.
  def breakpoint(source: Text, line: Ordinal, bound: Jdwp.Location => Unit)
    ( handler: Debug.Handler )
    ( using Tactic[Debugger.Error] )
  :   SourceBreakpoint^{this} =

    // HotSpot does not support source-name filters (JDI emulates them in its front end), so the
    // fallback receives every prepared class and discards non-matches — except the platform's own
    // namespaces, excluded VM-side: each notification suspends the loading thread for a round
    // trip, and a platform class is never the subject of a source breakpoint under a name which
    // doesn't match its file (one which does match still binds through the sweep of loaded
    // classes).
    val modifiers: List[Jdwp.Modifier] =
      if capabilities().canUseSourceNameFilters
      then List(Jdwp.Modifier.SourceNameMatch(source))
      else List
        ( Jdwp.Modifier.ClassExclude(t"java.*"),
          Jdwp.Modifier.ClassExclude(t"javax.*"),
          Jdwp.Modifier.ClassExclude(t"jdk.*"),
          Jdwp.Modifier.ClassExclude(t"sun.*"),
          Jdwp.Modifier.ClassExclude(t"com.sun.*"),
          Jdwp.Modifier.ClassExclude(t"scala.*") )

    val policy = Jdwp.SuspendPolicy.EventThread
    val prepare = connection.eventRequestSet(Jdwp.EventKind.ClassPrepare, policy, modifiers)
    val handle = new SourceBreakpoint(this, prepare)

    def bind(location: Jdwp.Location)(using Tactic[Debugger.Error]): Unit =
      if handle.admits(location) then
        val request = breakpoint(location, Jdwp.SuspendPolicy.All)
        connection.register(Jdwp.EventKind.Breakpoint, request): halt => handler(using halt)

        handle.record(location, request) match
          case revoked: Int => remove(Jdwp.EventKind.Breakpoint, revoked)
          case _            => bound(location)

    // The prepare handler is registered before the sweep of already-loaded classes, so a class
    // prepared between the two is bound by both and deduplicated by the handle, rather than
    // missed by both. Laundered: the handler captures this session, which the connection's
    // registry cannot name (see `Connection.PrepareSlot`), but it lives and dies with it.
    val prepareHandler: Jdwp.Event.ClassPrepared => Unit =
      caps.unsafe.unsafeAssumePure: event =>
        val outcome: Optional[Unit] = safely[Debugger.Error]:
          locateIn(event.tag, event.cls, source, line).stdlib.foreach(bind(_))

        outcome.let(identity)

    connection.registerPrepare(prepare)(prepareHandler)

    locate(source, line).stdlib.foreach(bind(_))
    handle

  // Resolves a source position to the executable locations currently loaded for it: every method
  // of every loaded class compiled from that file whose line table covers the line. Classes
  // named after the file are tried first, so the common case costs a handful of round trips; a
  // full sweep of loaded classes backstops it. Classes not yet loaded are not found, and inlined
  // code is not mapped (resolution on ClassPrepare and SMAP awareness belong to the stepping
  // campaign).
  def locate(source: Text, line: Ordinal)(using Tactic[Debugger.Error]): List[Jdwp.Location] =
    val base = source.s.indexOf('.') match
      case -1    => source.s
      case index => source.s.substring(0, index).nn

    val (likely, rest) = connection.allClasses().stdlib.partition(_.signature.s.contains(base))

    def resolve(info: Jdwp.ClassInfo): sci.List[Jdwp.Location] =
      locateIn(info.tag, info.cls, source, line).stdlib

    // The backstop sweep costs a round trip per class, so it skips array types and the platform's
    // own classes — a fresh VM already has a couple of thousand loaded, none of which can be the
    // subject of a source breakpoint under a name which doesn't match its file. A platform class
    // whose name does match the file is still found by the fast pass above.
    def sweepable(info: Jdwp.ClassInfo): Boolean =
      val signature = info.signature.s

      !signature.startsWith("[") && !signature.startsWith("Ljava/") &&
        !signature.startsWith("Ljavax/") && !signature.startsWith("Ljdk/") &&
        !signature.startsWith("Lsun/") && !signature.startsWith("Lcom/sun/")

    val found = likely.flatMap(resolve(_))
    val results = if found.isEmpty then rest.filter(sweepable).flatMap(resolve(_)) else found
    List(results*)

  // Resolves a source position within one specific reference type — every method of `cls` compiled
  // from `source` whose line table covers `line`. Unlike `locate`, it never enumerates all loaded
  // classes, so it is safe to call while a thread is suspended at a `ClassPrepare` event (a
  // whole-VM class scan can deadlock against the class-loading lock that thread still holds). A
  // caller receiving a `ClassPrepared` event resolves against its reported type directly.
  def locateIn(tag: Jdwp.TypeTag, cls: ReferenceTypeId, source: Text, line: Ordinal)
    ( using Tactic[Debugger.Error] )
  :   List[Jdwp.Location] =

    val sourced = safely(connection.sourceFile(cls)).let(_ == source).or(false)

    val results =
      if !sourced then sci.List[Jdwp.Location]() else
        connection.methods(cls).stdlib.flatMap: method =>
          safely(connection.lineTable(cls, method.method)) match
            case table: Jdwp.LineTable =>
              table.lines.stdlib.filter(_.line == line.n1).take(1).map: entry =>
                Jdwp.Location(tag, cls, method.method, entry.index)

            case _ =>
              sci.List()

    List(results*)
