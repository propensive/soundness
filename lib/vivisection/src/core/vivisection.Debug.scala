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
import proscenium.*
import vacuous.*

object Debug:
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
class Debug private[vivisection] (connection: Jdwp.Connection)
extends caps.ExclusiveCapability:
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
    connection.eventRequestSet(Jdwp.EventKind.ClassPrepare, Jdwp.SuspendPolicy.EventThread, modifiers)

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
    new Breakpoint(this, request)

  // Installs a logpoint: a breakpoint which logs its message and immediately resumes, so the
  // program is never paused beyond the handler itself.
  def logpoint(location: Jdwp.Location)(message: (halt: Halt^) ?=> Text)
    ( using Tactic[Debugger.Error], (Debug.Event is Loggable)^ )
  :   Breakpoint^{this} =

    breakpoint(location):
      Log.info(Debug.Event.Logpoint(message))

  private[vivisection] def remove(request: Int)(using Tactic[Debugger.Error]): Unit =
    connection.unregister(Jdwp.EventKind.Breakpoint, request)
    connection.eventRequestClear(Jdwp.EventKind.Breakpoint, request)

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

    def locations(info: Jdwp.ClassInfo): sci.List[Jdwp.Location] =
      val sourced = safely(connection.sourceFile(info.cls)).let(_ == source).or(false)

      if !sourced then sci.List() else
        connection.methods(info.cls).stdlib.flatMap: method =>
          safely(connection.lineTable(info.cls, method.method)) match
            case table: Jdwp.LineTable =>
              table.lines.stdlib.filter(_.line == line.n1).take(1).map: entry =>
                Jdwp.Location(info.tag, info.cls, method.method, entry.index)

            case _ =>
              sci.List()

    val (likely, rest) = connection.allClasses().stdlib.partition(_.signature.s.contains(base))
    val found = likely.flatMap(locations(_))
    val results = if found.isEmpty then rest.flatMap(locations(_)) else found
    List(results*)
