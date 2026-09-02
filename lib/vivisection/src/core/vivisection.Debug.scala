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

import anticipation.*
import contingency.*
import denominative.*

// The last line-table entry covering a location is genuinely needed, and the table is already
// traversed once to filter it.
import denominative.dysasymptotics.linearSize
import fulminate.*
import gossamer.*
import parasite.*
import proscenium.*
import rudiments.*
import symbolism.*
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

    def stdout: Chain[Data] = out.chain
    def stderr: Chain[Data] = err.chain

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
  def events: Chain[Jdwp.Event.Composite] = connection.unclaimed.chain

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

  // The logical reading of a location: the source position it stands for — the innermost
  // inline origin for a synthetic line, or the raw position for a real one — and, for
  // synthetic lines only, the real call-site position the inline chain leads back to. Two
  // locations share a logical position exactly when they stand for the same line the
  // programmer wrote; the call site is what lets a step *over* a line pass through inlining
  // that originates from it.
  private def logical(location: Jdwp.Location)(using Tactic[Debugger.Error])
  :   ((Text, Int), Optional[(Text, Int)]) =

    val source = safely(connection.sourceFile(location.cls)).or(t"")

    val line = safely(connection.lineTable(location.cls, location.method)) match
      case table: Jdwp.LineTable =>
        table.lines.filter(_.index <= location.index).last.let(_.line).or(0)

      case _ =>
        0

    connection.smap(location.cls).let(_.expand(line)) match
      case expansion: digression.Smap.Expansion =>
        val callSite: Optional[(Text, Int)] = expansion.line.let: line => (source, line)

        expansion.inlined.prim match
          case origin: digression.Smap.Origin => ((origin.file, origin.line), callSite)
          case _                              => ((source, expansion.line.or(line)), Unset)

      case _ =>
        ((source, line), Unset)

  // Requests a *logical* step whose completion runs `handler` on the dispatcher, exactly as a
  // breakpoint hit does; the step suspends only the stepped thread. A logical step completes
  // when the logical source position changes: landings on synthetic lines still standing for
  // the starting position are stepped through silently, so stepping over a line containing an
  // inline call no longer descends into the inlined body, while stepping into one stops at its
  // first line — as if it were a call. An unreadable landing, or the iteration cap (a safety
  // net against pathological SMAPs), reports the stop as-is.
  def step(thread: ThreadId, depth: Jdwp.StepDepth)(handler: Debug.Handler)
    ( using Tactic[Debugger.Error] )
  :   Unit =

    // The thread stands suspended at request time, so its top frame gives the starting
    // logical reading.
    val start: Optional[((Text, Int), Optional[(Text, Int)])] =
      connection.frames(thread, 0, 1).prim match
        case (_, location: Jdwp.Location) => logical(location)
        case _                            => Unset

    stepUntil(thread, depth, start, 0)(handler)

  private def stepUntil
    ( thread:    ThreadId,
      depth:     Jdwp.StepDepth,
      start:     Optional[((Text, Int), Optional[(Text, Int)])],
      iteration: Int )
    ( handler: Debug.Handler )
    ( using Tactic[Debugger.Error] )
  :   Unit =

    val request = step(thread, depth, Jdwp.StepSize.Line)

    // Laundered like the prepare handlers: the closure captures this session, which the
    // connection's registry cannot name, but it dies with it.
    val onStep: Halt => Unit =
      caps.unsafe.unsafeAssumePure: halt =>
        // The `Count(1)` request is spent the moment it fires; without this it would linger in
        // the VM's request table for the rest of the session.
        val cleared: Optional[Unit] = safely[Debugger.Error]:
          connection.eventRequestClear(Jdwp.EventKind.SingleStep, request)

        cleared.let(identity)
        connection.unregister(Jdwp.EventKind.SingleStep, request)

        val landing: Optional[((Text, Int), Optional[(Text, Int)])] =
          safely[Debugger.Error](logical(halt.location))

        // Stepping *over* advances the real (outermost) source line — the call-site line when
        // inside inlining — so the whole of a line, inlined expansions included, is one step.
        // Stepping *into* (or out) advances the logical (innermost) line, so entering an
        // inline body stops at its first line, and stepping within a body moves line by line.
        val skip = start.lay(false): (startPosition, startCall) =>
          landing.lay(false): (position, callSite) =>
            if depth == Jdwp.StepDepth.Over
            then callSite.or(position) == startCall.or(startPosition)
            else position == startPosition

        if skip && iteration < 64 then
          // Still on the same logical line: request the next step and return without running
          // the user handler — the dispatcher's auto-resume continues the thread.
          val stepped: Optional[Unit] = safely[Debugger.Error]:
            stepUntil(thread, depth, start, iteration + 1)(handler)

          stepped.let(identity)
        else
          handler(using halt)

    connection.register(Jdwp.EventKind.SingleStep, request)(onStep(_))

  // Suspends a thread on the caller's initiative — no event marks it — and hands back a halt
  // over its topmost frame, so a client-driven pause offers the same view of the stopped thread
  // as a breakpoint would. The caller owns the suspension: dropping the halt does not resume.
  // Laundered exactly as the dispatcher's halts are: it lives and dies with this session.
  def pause(thread: ThreadId)(using Tactic[Debugger.Error]): Halt =
    connection.suspendThread(thread)

    val location = connection.frames(thread, 0, 1).prim.let(_(1)).or:
      Jdwp.Location(Jdwp.TypeTag.Class, Jdwp.Ref.empty, Jdwp.Ref.empty, 0L)

    caps.unsafe.unsafeAssumePure
      ( new Halt(connection, thread, location, Halt.Cause.Stopped, Halt.Retention()) )

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

      connection.classesBySignature(signature).prim match
        case info: Jdwp.ClassInfo =>
          connection.fields(info.cls).seek(_.name == field) match
            case fieldInfo: Jdwp.FieldInfo =>
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
    :   List[Jdwp.Location] =

      connection.methods(cls).filter(_.name == method).flatMap: info =>
        safely(connection.lineTable(cls, info.method)) match
          case table: Jdwp.LineTable if table.start >= 0 =>
            List(Jdwp.Location(tag, cls, info.method, table.start))

          case _ =>
            List()

    // Laundered for the same reason as the source-position form above.
    val prepareHandler: Jdwp.Event.ClassPrepared => Unit =
      caps.unsafe.unsafeAssumePure: event =>
        val outcome: Optional[Unit] = safely[Debugger.Error]:
          entries(event.tag, event.cls).each(bind(_))

        outcome.let(identity)

    connection.registerPrepare(prepare)(prepareHandler)

    val signature = t"L${className.s.replace('.', '/').nn};"

    connection.classesBySignature(signature).each: info => entries(info.tag, info.cls).each(bind(_))

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
          locateIn(event.tag, event.cls, source, line).each(bind(_))

        outcome.let(identity)

    connection.registerPrepare(prepare)(prepareHandler)

    locate(source, line).each(bind(_))
    handle

  // Resolves a source position to the executable locations currently loaded for it: every method
  // of every loaded class compiled from that file whose line table covers the line. Classes
  // named after the file are tried first, so the common case costs a handful of round trips; a
  // full sweep of loaded classes backstops it. Classes not yet loaded are not found here — the
  // source-breakpoint form defers to their preparation — and each resolved class is also checked
  // for inlined copies of the position through its SMAP (see `locateIn`).
  def locate(source: Text, line: Ordinal)(using Tactic[Debugger.Error]): List[Jdwp.Location] =
    val base = source.s.indexOf('.') match
      case -1    => source.s
      case index => source.s.substring(0, index).nn

    val (likely, rest) = connection.allClasses().partition(_.signature.s.contains(base))

    def resolve(info: Jdwp.ClassInfo): List[Jdwp.Location] =
      locateIn(info.tag, info.cls, source, line)

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
    if found.nil then rest.filter(sweepable).flatMap(resolve(_)) else found

  // Resolves a source position within one specific reference type — every method of `cls` compiled
  // from `source` whose line table covers `line`. Unlike `locate`, it never enumerates all loaded
  // classes, so it is safe to call while a thread is suspended at a `ClassPrepare` event (a
  // whole-VM class scan can deadlock against the class-loading lock that thread still holds). A
  // caller receiving a `ClassPrepared` event resolves against its reported type directly.
  def locateIn(tag: Jdwp.TypeTag, cls: ReferenceTypeId, source: Text, line: Ordinal)
    ( using Tactic[Debugger.Error] )
  :   List[Jdwp.Location] =

    val sourced = safely(connection.sourceFile(cls)).let(_ == source).or(false)

    // Only the first line-table entry at the requested line binds, so the search is `seek` rather
    // than a filter which is then truncated.
    val exact =
      if !sourced then List[Jdwp.Location]() else
        connection.methods(cls).flatMap: method =>
          val entry: Optional[Jdwp.LineEntry] =
            safely(connection.lineTable(cls, method.method)) match
              case table: Jdwp.LineTable => table.lines.seek(_.line == line.n1)
              case _                     => Unset

          entry.lay(List[Jdwp.Location]()): entry =>
            List(Jdwp.Location(tag, cls, method.method, entry.index))

    // The SMAP pass, unguarded by the class's own source file: its SMAP's file table decides
    // whether the requested position was inlined into this class, and `sites` answers the
    // generated-line ranges standing for it — one binding per site, at the first line-table
    // entry each range covers.
    val ranges = connection.smap(cls).lay(List[(Int, Int)]())(_.sites(source, line.n1))

    val inlined =
      if ranges.nil then List[Jdwp.Location]() else
        connection.methods(cls).flatMap: method =>
          safely(connection.lineTable(cls, method.method)) match
            case table: Jdwp.LineTable =>
              ranges.flatMap: (start, end) =>
                val within: Optional[Jdwp.LineEntry] = table.lines.seek: entry =>
                  entry.line >= start && entry.line < end

                within.lay(List[Jdwp.Location]()): entry =>
                  List(Jdwp.Location(tag, cls, method.method, entry.index))

            case _ =>
              List()

    // Two methods can bind the same position (an exact hit and an inlined copy), so a location is
    // identified by its method and bytecode index: de-duplication is by this key, not by the
    // locations' own equality. The concatenation is bound first so `deduplicate`'s implicit
    // search never runs against an uninstantiated result variable (the wildApprox hazard).
    def key(location: Jdwp.Location): (Long, Long) = (location.method.long, location.index)

    val all: List[Jdwp.Location] = exact + inlined
    all.deduplicate(key(_))
