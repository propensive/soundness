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

import java.lang as jl
import java.util.concurrent.atomic as juca

import scala.caps
import scala.collection.concurrent as scc

// `ambience` is imported selectively: its wildcard would publish `ambience.Variable` (an
// environment variable) over this package's `vivisection.Variable`.
import ambience.{System, WorkingDirectory}
import anthology.*
import anticipation.*
import aperture.*
// `session` is hidden: `aperture` exports the same extension, and importing both is ambiguous.
import coaxial.{session as _, *}
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gigantism.*
import gossamer.*
import guillotine.*
import hellenism.*
import jacinta.*
import parasite.*
import proscenium.*
import rudiments.*
import spectacular.*
import urticose.*
import vacuous.*

private[vivisection] object DapSession:
  // Holds a retained halt (or a breakpoint handle) out of the capture-tracked world: each
  // captures the debug session, which no registry value type can name, but the adapter's own
  // state machine encloses their lifetimes within the session task's.
  private class HaltSlot(@caps.unsafe.untrackedCaptures val halt: Halt)
  private class SourceSlot(@caps.unsafe.untrackedCaptures val handle: SourceBreakpoint, val id: Int)
  private class RequestSlot(@caps.unsafe.untrackedCaptures val handle: Breakpoint)

  // What a `variablesReference` handle refers to: a frame's local scope, or a structured value
  // to expand one level.
  private enum Node:
    case Locals(thread: Int, frame: FrameId, location: Jdwp.Location)
    case Structure(thread: Int, snapshot: Variable.Snapshot)

// The stateful adapter between one DAP client and one debug session: it decodes each incoming
// request, drives the debug session, and writes every outgoing message — responses and events
// alike — through `emit`, whose serial use preserves the protocol's ordering guarantees (the
// `initialized` event follows the `initialize` response, a `stopped` event follows the response
// to the request that caused it). Requests are handled sequentially on the transport loop; the
// backend serialises on the session anyway.
private[vivisection] class DapSession(emit: Json => Unit)
  ( using online:     Online,
          monitor:    Monitor,
          probate:    Probate,
          backend:    Socket.Backend,
          options:    Every[Socket.Option.Tcp],
          system:     System,
          asyncError: Tactic[Async.Error],
          working:    WorkingDirectory,
          loggable:   (Socket.Event is Loggable)^,
          exec:       (Exec.Event is Loggable)^,
          compile:    (CompileEvent is Loggable)^,
          note:       Diagnostics ):

  import strategies.throwUnsafely
  import dynamicJsonAccess.enabled

  // This adapter juggles capabilities whose lifetimes its own state machine encloses. Its
  // callbacks — breakpoint handlers, bind notifications, session tasks — refer to it through
  // this laundered alias, keeping their capture sets empty; every one of them dies with the
  // session (see `Connection.Slot` for the underlying pattern).
  private val self: DapSession = caps.unsafe.unsafeAssumePure(this)

  private val outgoing: juca.AtomicInteger = juca.AtomicInteger(0)
  private val counter: juca.AtomicInteger = juca.AtomicInteger(0)

  // The open session, laundered into a field: the session task below holds its loan open until
  // `disconnect`, so the field never outlives it.
  @caps.unsafe.untrackedCaptures
  private var debug0: Optional[Debug] = Unset

  @caps.unsafe.untrackedCaptures
  private var classpath0: Optional[LocalClasspath] = Unset

  private val ready: Promise[Unit] = Promise()
  private val terminate: Promise[Unit] = Promise()

  @caps.unsafe.untrackedCaptures
  private var sessionTask: Optional[Task[Unit]] = Unset

  // DAP handles are `Int`s; JDWP identifiers are `Long`s. Threads are registered on first
  // sight; frame and variables-reference handles live only between a stop and the next resume.
  private val threadIds: scc.TrieMap[Long, Int] = scc.TrieMap()
  private val threads: scc.TrieMap[Int, ThreadId] = scc.TrieMap()
  private val stops: scc.TrieMap[Int, DapSession.HaltSlot] = scc.TrieMap()
  private val frames: scc.TrieMap[Int, (Int, FrameId, Jdwp.Location)] = scc.TrieMap()
  private val nodes: scc.TrieMap[Int, DapSession.Node] = scc.TrieMap()

  // Breakpoint registrations, for the protocol's replace semantics: a `setBreakpoints` for a
  // source replaces every breakpoint previously set in it.
  private val bySource: scc.TrieMap[Text, scala.List[DapSession.SourceSlot]] = scc.TrieMap()

  @caps.unsafe.untrackedCaptures
  private var exceptionRequests: scala.List[DapSession.RequestSlot] = scala.List()

  @caps.unsafe.untrackedCaptures
  private var functionRequests: scala.List[DapSession.SourceSlot] = scala.List()

  @caps.unsafe.untrackedCaptures
  private var watchRequests: scala.List[DapSession.RequestSlot] = scala.List()

  private def nextSeq(): Int = outgoing.incrementAndGet()

  // Tears down any open session — called when the transport loop ends, so a client that drops
  // the connection without a `disconnect` request still releases the debuggee and unwinds the
  // session task. Offering `terminate` releases the task's `await`, and it then unwinds the
  // debuggee on its own (exactly as a normal session teardown does); the task is left for the
  // enclosing supervision scope to await, rather than cancelled mid-unwind. Idempotent: a prior
  // `disconnect` has already offered `terminate`.
  def close(): Unit = terminate.offer(())

  private def respond(request: Dap.Envelope, body: Optional[Json] = Unset): Unit =
    emit(Dap.response(nextSeq(), request, body))

  private def fail(request: Dap.Envelope, message: Text): Unit =
    emit(Dap.failure(nextSeq(), request, message))

  private def send(name: Text, body: Optional[Json] = Unset): Unit =
    emit(Dap.event(nextSeq(), name, body))

  private def threadHandle(thread: ThreadId): Int =
    threadIds.get(thread.long) match
      case scala.Some(id) =>
        id

      case scala.None =>
        val id = counter.incrementAndGet()

        threadIds.putIfAbsent(thread.long, id) match
          case scala.Some(existing) =>
            existing

          case scala.None =>
            threads(id) = thread
            id

  // An ephemeral port for the forked debuggee's agent, mirroring the test harness.
  private def freePort(): Int =
    val socket = java.net.ServerSocket(0)
    try socket.getLocalPort finally socket.close()

  // Holds a freshly-opened session for the adapter's lifetime: the field is read by every
  // subsequent request, and the session task parks here until `disconnect`. A launch session's
  // console is relayed as `output` events, and its exit as `exited` and `terminated` — the
  // relays are laundered pure thunks, like the session task itself.
  private def opened(debug: Debug^): Unit =
    debug0 = caps.unsafe.unsafeAssumePure(debug)

    debug.console.let: console =>
      val adapter = self

      val out: () => Unit =
        caps.unsafe.unsafeAssumePure: () =>
          console.stdout.stdlib.foreach: data =>
            adapter.send(t"output", Dap.OutputBody(data.utf8, t"stdout").in[Json])

      val err: () => Unit =
        caps.unsafe.unsafeAssumePure: () =>
          console.stderr.stdlib.foreach: data =>
            adapter.send(t"output", Dap.OutputBody(data.utf8, t"stderr").in[Json])

      val exit: () => Unit =
        caps.unsafe.unsafeAssumePure: () =>
          safely(console.exited.await()).let: status =>
            val code = status match
              case Exit.Fail(code) => code
              case _               => 0

            adapter.send(t"exited", Dap.ExitedBody(code).in[Json])
            adapter.send(t"terminated")

      val outTask: Task[Unit] = async(out())
      val errTask: Task[Unit] = async(err())
      val exitTask: Task[Unit] = async(exit())
      ()

    ready.offer(())
    safely(terminate.await())
    ()

  // The shared stop path: retain the halt against its thread, hold the suspension, and report
  // the stop. Runs on the backend dispatcher.
  private def onStop(reason: Text, hits: scala.List[Int], all: Boolean)(using halt: Halt^): Unit =
    val id = threadHandle(halt.thread)
    stops(id) = DapSession.HaltSlot(caps.unsafe.unsafeAssumePure(halt))
    halt.remain()
    send(t"stopped", Dap.StoppedBody(reason, id, all, List(hits*)).in[Json])

  // Clears every per-stop registry; handles minted before a resume are invalid after it.
  private def clearStops(): Unit =
    stops.clear()
    frames.clear()
    nodes.clear()

  private def basename(path: Text): Text =
    val raw = path.s
    val slash = raw.lastIndexOf('/') max raw.lastIndexOf('\\')
    (if slash < 0 then raw else raw.substring(slash + 1).nn).tt

  def handle(json: Json): Unit =
    val request = Dap.envelope(json)

    if request.`type` == t"request" then
      try dispatch(request, json)
      catch case error: Exception => fail(request, error.toString.tt)

  private def dispatch(request: Dap.Envelope, json: Json): Unit =
    request.command.or(t"") match
      case t"initialize" =>
        respond(request, Dap.Capabilities().in[Json])
        send(t"initialized")

      case t"launch" =>
        val arguments = json.arguments.as[Dap.LaunchArguments]
        classpath0 = arguments.classpath.as[LocalClasspath]

        // The session task's body is laundered into a pure thunk: opening the session uses
        // this adapter's capabilities, which the task must not be seen to smuggle; the task
        // dies with the adapter.
        val body: () => Unit =
          caps.unsafe.unsafeAssumePure: () =>
            val outcome: Optional[Unit] = safely[Debugger.Error]:
              val command: Command =
                sh"java -classpath ${arguments.classpath} ${arguments.mainClass}"

              val debuggee: Debuggee = Debuggee(command, self.freePort())

              debuggee.session: debug ?=> self.opened(debug)

            outcome.let(identity)

        sessionTask = async(body())
        ready.await()
        respond(request)

      case t"attach" =>
        val arguments = json.arguments.as[Dap.AttachArguments]

        val endpoint: Endpoint[Tcp.Port] =
          Endpoint(arguments.hostName.or(t"localhost"), Port[Tcp](arguments.port))

        val body: () => Unit =
          caps.unsafe.unsafeAssumePure: () =>
            val outcome: Optional[Unit] = safely[Debugger.Error]:
              // Connected directly rather than through `Debugger.session`, for the same
              // reason `Debuggee` documents: rebuilding the `Connectable` from the captured
              // `Online` fails `.duplex`'s empty-capture-set requirement.
              val duplex =
                summon[(Endpoint[Tcp.Port] is Connectable)^].connect(endpoint, Unset)

              val open: Jdwp.Connection => Unit =
                caps.unsafe.unsafeAssumePure: connection => self.opened(new Debug(connection))

              try Jdwp.Connection.exchange(duplex)(open) finally duplex.close()

            outcome.let(identity)

        sessionTask = async(body())
        ready.await()
        respond(request)

      case t"setBreakpoints" =>
        val arguments = json.arguments.as[Dap.SetBreakpointsArguments]
        val path = arguments.source.path.or(arguments.source.name.or(t""))
        val source = basename(path)

        withDebug(request): debug =>
          val adapter = self
          bySource.remove(source).foreach(_.foreach { slot => safely(slot.handle.clear()) })

          val created = arguments.breakpoints.stdlib.map: spec =>
            val id = counter.incrementAndGet()

            // Laundered: the callback captures this adapter, which the breakpoint's
            // registries cannot name, but it dies with the session.
            val verified: Jdwp.Location => Unit =
              caps.unsafe.unsafeAssumePure: _ =>
                self.send(t"breakpoint", Dap.BreakpointEventBody(t"changed",
                    Dap.Breakpoint(true, id, spec.line)).in[Json])

            val handle = debug.breakpoint(source, Ordinal.uniary(spec.line), verified):
              stop ?=> adapter.onStop(t"breakpoint", scala.List(id), true)(using stop)

            (DapSession.SourceSlot(caps.unsafe.unsafeAssumePure(handle), id), spec.line)

          bySource(source) = created.map(_(0))

          val breakpoints = created.map: (slot, line) =>
            Dap.Breakpoint(slot.handle.bound, slot.id, line)

          respond(request, Dap.BreakpointsBody(List(breakpoints*)).in[Json])

      case t"setExceptionBreakpoints" =>
        val arguments = json.arguments.as[Dap.SetExceptionBreakpointsArguments]

        withDebug(request): debug =>
          val adapter = self
          exceptionRequests.foreach: slot => safely(slot.handle.clear())

          val uncaught = arguments.filters.stdlib.contains(t"uncaught")
          val caught = arguments.filters.stdlib.contains(t"caught")

          exceptionRequests =
            if !uncaught && !caught then scala.List() else
              val handle = debug.exceptions(uncaught, caught):
                stop ?=> adapter.onStop(t"exception", scala.List(), true)(using stop)

              scala.List(DapSession.RequestSlot(caps.unsafe.unsafeAssumePure(handle)))

          val breakpoints = arguments.filters.stdlib.map { _ => Dap.Breakpoint(true) }
          respond(request, Dap.BreakpointsBody(List(breakpoints*)).in[Json])

      case t"setFunctionBreakpoints" =>
        val arguments = json.arguments.as[Dap.SetFunctionBreakpointsArguments]

        withDebug(request): debug =>
          val adapter = self
          functionRequests.foreach: slot => safely(slot.handle.clear())

          val created = arguments.breakpoints.stdlib.map: spec =>
            val id = counter.incrementAndGet()
            val dot = spec.name.s.lastIndexOf('.')
            val cls = if dot < 0 then spec.name else spec.name.s.substring(0, dot).nn.tt
            val method = if dot < 0 then t"" else spec.name.s.substring(dot + 1).nn.tt

            val handle = debug.breakpoint(cls, method):
              stop ?=> adapter.onStop(t"function breakpoint", scala.List(id), true)(using stop)

            DapSession.SourceSlot(caps.unsafe.unsafeAssumePure(handle), id)

          functionRequests = created

          val breakpoints = created.map: slot => Dap.Breakpoint(slot.handle.bound, slot.id)
          respond(request, Dap.BreakpointsBody(List(breakpoints*)).in[Json])

      case t"dataBreakpointInfo" =>
        val arguments = json.arguments.as[Dap.DataBreakpointInfoArguments]

        val target = arguments.variablesReference.let(nodes.get(_).getOrElse(scala.None)) match
          case DapSession.Node.Locals(thread, frame, location) =>
            stops.get(thread).map: slot =>
              slot.halt.variables(frame, location).stdlib.find(_.name == arguments.name)

            . getOrElse(scala.None)

          case _ =>
            scala.None

        target match
          case scala.Some(variable) => variable.provenance match
            case Variable.Provenance.Field(owner, _, _) =>
              respond(request, Dap.DataBreakpointInfoBody(t"$owner:${arguments.name}",
                  t"${arguments.name} on $owner", List(t"write")).in[Json])

            case _ =>
              respond(request, Dap.DataBreakpointInfoBody(Unset,
                  t"only a member field supports a data breakpoint").in[Json])

          case _ =>
            respond(request, Dap.DataBreakpointInfoBody(Unset, t"unknown variable").in[Json])

      case t"setDataBreakpoints" =>
        val arguments = json.arguments.as[Dap.SetDataBreakpointsArguments]

        withDebug(request): debug =>
          val adapter = self
          watchRequests.foreach: slot => safely(slot.handle.clear())

          val created = arguments.breakpoints.stdlib.flatMap: spec =>
            val colon = spec.dataId.s.lastIndexOf(':')

            if colon < 0 then scala.None else
              val cls = spec.dataId.s.substring(0, colon).nn.tt
              val field = spec.dataId.s.substring(colon + 1).nn.tt

              val handle = debug.watch(cls, field):
                stop ?=> adapter.onStop(t"data breakpoint", scala.List(), true)(using stop)

              handle match
                case watch: Breakpoint =>
                  scala.Some(DapSession.RequestSlot(caps.unsafe.unsafeAssumePure(watch)))

                case _ =>
                  scala.None

          watchRequests = created

          val breakpoints = arguments.breakpoints.stdlib.map: spec =>
            Dap.Breakpoint(created.nonEmpty)

          respond(request, Dap.BreakpointsBody(List(breakpoints*)).in[Json])

      case t"configurationDone" =>
        withDebug(request): debug =>
          debug.resume()
          respond(request)

      case t"threads" =>
        withDebug(request): debug =>
          val all = debug.threads().stdlib.map: thread =>
            Dap.ThreadInfo(threadHandle(thread), safely(debug.name(thread)).or(t"?"))

          respond(request, Dap.ThreadsBody(List(all*)).in[Json])

      case t"stackTrace" =>
        val arguments = json.arguments.as[Dap.StackTraceArguments]

        withStop(request, arguments.threadId): halt =>
          // One DAP frame per *logical* position: a physical frame inside inlined code expands
          // into its inline origins (marked `subtle`) followed by the frame itself at its real
          // line. Every logical frame shares the physical frame's registry entry, so scopes,
          // variables, evaluation and restart against an inline frame resolve to the enclosing
          // physical frame.
          val trace = halt.frames().stdlib.flatMap: (frame, location) =>
            halt.positions(location).stdlib.map: position =>
              val id = counter.incrementAndGet()
              frames(id) = (arguments.threadId, frame, location)
              val source = position.source.let(Dap.Source(_, position.path))
              val hint: Optional[Text] = if position.inlined then t"subtle" else Unset
              Dap.StackFrame(id, position.name, position.line, 0, source, hint)

          respond(request, Dap.StackTraceBody(List(trace*), trace.length).in[Json])

      case t"scopes" =>
        val arguments = json.arguments.as[Dap.FrameArguments]

        frames.get(arguments.frameId) match
          case scala.Some((thread, frame, location)) =>
            val ref = counter.incrementAndGet()
            nodes(ref) = DapSession.Node.Locals(thread, frame, location)
            respond(request, Dap.ScopesBody(List(Dap.Scope(t"Locals", ref))).in[Json])

          case _ =>
            fail(request, t"unknown frame")

      case t"variables" =>
        val arguments = json.arguments.as[Dap.VariablesArguments]

        nodes.get(arguments.variablesReference) match
          case scala.Some(DapSession.Node.Locals(thread, frame, location)) =>
            withStop(request, thread): halt =>
              val all = halt.variables(frame, location).stdlib.map(variableInfo(thread, _))
              respond(request, Dap.VariablesBody(List(all*)).in[Json])

          case scala.Some(DapSession.Node.Structure(thread, snapshot)) =>
            withStop(request, thread): halt =>
              val all = halt.children(snapshot).stdlib.map(variableInfo(thread, _))
              respond(request, Dap.VariablesBody(List(all*)).in[Json])

          case _ =>
            fail(request, t"unknown variables reference")

      case t"continue" =>
        withDebug(request): debug =>
          clearStops()
          debug.resume()
          respond(request, Dap.ContinueBody().in[Json])

      case t"next" =>
        stepWith(request, json, Jdwp.StepDepth.Over)

      case t"stepIn" =>
        stepWith(request, json, Jdwp.StepDepth.Into)

      case t"stepOut" =>
        stepWith(request, json, Jdwp.StepDepth.Out)

      case t"pause" =>
        val arguments = json.arguments.as[Dap.ThreadArguments]

        withDebug(request): debug =>
          threads.get(arguments.threadId) match
            case scala.Some(thread) =>
              val halt = debug.pause(thread)
              stops(arguments.threadId) = DapSession.HaltSlot(halt)
              respond(request)

              send(t"stopped",
                  Dap.StoppedBody(t"pause", arguments.threadId, false).in[Json])

            case _ =>
              fail(request, t"unknown thread")

      case t"setVariable" =>
        val arguments = json.arguments.as[Dap.SetVariableArguments]

        nodes.get(arguments.variablesReference) match
          case scala.Some(DapSession.Node.Locals(thread, frame, location)) =>
            withStop(request, thread): halt =>
              halt.variables(frame, location).stdlib.find(_.name == arguments.name) match
                case scala.Some(variable) =>
                  parseValue(halt, variable.erased, arguments.value) match
                    case value: Jdwp.Value =>
                      halt.assign(frame, variable, value)
                      respond(request, Dap.SetVariableBody(arguments.value).in[Json])

                    case _ =>
                      fail(request, t"the value is not expressible in ${variable.erased}")

                case _ =>
                  fail(request, t"unknown variable")

          case _ =>
            fail(request, t"only a local scope supports assignment")

      case t"evaluate" =>
        val arguments = json.arguments.as[Dap.EvaluateArguments]

        withFrame(request, arguments.frameId): (thread, halt) =>
          withClasspath(request): classpath =>
            val result = halt.evaluator(classpath): eval ?=> eval(arguments.expression)

            val rendered = result match
              case Variable.Snapshot.Str(_, text) => text
              case other                          => other.inspect

            respond(request, Dap.EvaluateBody(rendered).in[Json])

      case t"setExpression" =>
        val arguments = json.arguments.as[Dap.SetExpressionArguments]

        withFrame(request, arguments.frameId): (thread, halt) =>
          withClasspath(request): classpath =>
            halt.evaluator(classpath): eval ?=> eval.assign(arguments.expression, arguments.value)
            respond(request, Dap.SetVariableBody(arguments.value).in[Json])

      case t"exceptionInfo" =>
        val arguments = json.arguments.as[Dap.ThreadArguments]

        withStop(request, arguments.threadId): halt =>
          halt.exceptionInfo() match
            case info: Halt.ExceptionInfo =>
              val mode = if info.caught then t"always" else t"unhandled"
              respond(request, Dap.ExceptionInfoBody(info.className, mode, info.message).in[Json])

            case _ =>
              fail(request, t"the thread is not stopped at an exception")

      case t"restartFrame" =>
        val arguments = json.arguments.as[Dap.FrameArguments]

        frames.get(arguments.frameId) match
          case scala.Some((thread, frame, _)) =>
            withStop(request, thread): halt =>
              halt.pop(frame)
              frames.clear()
              nodes.clear()
              respond(request)
              send(t"stopped", Dap.StoppedBody(t"restart", thread).in[Json])

          case _ =>
            fail(request, t"unknown frame")

      case t"disconnect" =>
        clearStops()
        respond(request)
        terminate.offer(())

      case _ =>
        fail(request, t"unrecognized command")

  private def stepWith(request: Dap.Envelope, json: Json, depth: Jdwp.StepDepth): Unit =
    val arguments = json.arguments.as[Dap.ThreadArguments]

    withDebug(request): debug =>
      val adapter = self

      threads.get(arguments.threadId) match
        case scala.Some(thread) =>
          debug.step(thread, depth):
            stop ?=> adapter.onStop(t"step", scala.List(), false)(using stop)

          clearStops()
          debug.resume()
          respond(request)

        case _ =>
          fail(request, t"unknown thread")

  private def variableInfo(thread: Int, variable: Variable): Dap.VariableInfo =
    val ref = variable.value match
      case snapshot @ (Variable.Snapshot.Obj(_, _) | Variable.Snapshot.Arr(_, _, _, _)) =>
        val id = counter.incrementAndGet()
        nodes(id) = DapSession.Node.Structure(thread, snapshot)
        id

      case _ =>
        0

    Dap.VariableInfo(variable.name, variable.value.inspect, ref,
        variable.static.or(variable.erased))

  // Reads the laundered session field back at a pure type, so inline expansion sites are not
  // poisoned by the untracked var's inferred capture.
  private def currentDebug: Optional[Debug] = caps.unsafe.unsafeAssumePure(debug0)

  private inline def withDebug(request: Dap.Envelope)(inline body: Debug => Unit): Unit =
    currentDebug match
      case debug: Debug => body(debug)
      case _            => fail(request, t"no debug session is open")

  private inline def withStop(request: Dap.Envelope, thread: Int)(inline body: Halt => Unit): Unit =
    stops.get(thread) match
      case scala.Some(slot) => body(slot.halt)
      case _                => fail(request, t"the thread is not stopped")

  private inline def withFrame(request: Dap.Envelope, frameId: Optional[Int])
    ( inline body: (Int, Halt) => Unit )
  :   Unit =

    frameId.let(frames.get(_).getOrElse(scala.None)) match
      case (thread: Int, _, _) => withStop(request, thread): halt => body(thread, halt)
      case _                   => fail(request, t"unknown frame")

  private inline def withClasspath(request: Dap.Envelope)(inline body: LocalClasspath => Unit)
  :   Unit =

    classpath0 match
      case classpath: LocalClasspath => body(classpath)
      case _                         => fail(request, t"no classpath was given at launch")

  // Parses a plain DAP `setVariable` value at the variable's erased type: the primitives, plus
  // a fresh remote string. Anything richer belongs to `setExpression`.
  private def parseValue(halt: Halt, erased: Text, value: Text): Optional[Jdwp.Value] =
    try erased.s match
      case "Int"              => Jdwp.Value.OfInt(jl.Integer.parseInt(value.s))
      case "Long"             => Jdwp.Value.OfLong(jl.Long.parseLong(value.s))
      case "Boolean"          => Jdwp.Value.OfBoolean(jl.Boolean.parseBoolean(value.s))
      case "Double"           => Jdwp.Value.OfDouble(jl.Double.parseDouble(value.s))
      case "Float"            => Jdwp.Value.OfFloat(jl.Float.parseFloat(value.s))
      case "Short"            => Jdwp.Value.OfShort(jl.Short.parseShort(value.s))
      case "Byte"             => Jdwp.Value.OfByte(jl.Byte.parseByte(value.s))

      case "java.lang.String" =>
        val string = halt.connection.createString(value)
        Jdwp.Value.Reference(Jdwp.Tag.StringTag, Jdwp.Ref(string.long))

      case _ =>
        Unset

    catch case _: Exception => Unset
