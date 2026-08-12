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
import proscenium.*

// A live debug session: the capability lent inside `target.session { debug ?=> … }`. Sealed
// (`ExclusiveCapability`), so it cannot outlive the session that lends it. Its operations are the
// programmer-facing surface over the wire-level `Jdwp.Connection` it wraps.
class Debug private[vivisection] (connection: Jdwp.Connection) extends caps.ExclusiveCapability:
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

  // The event stream: every composite the (suspending or running) VM sends back. This is the
  // primitive; a caller drains it and reacts, or awaits a particular event. Reading it consumes
  // the events as they arrive.
  def events: Chain[Jdwp.Event.Composite] = connection.composites.lazyList

  // Sets a breakpoint at a resolved location, returning the request id used to `clear` it. The VM
  // reports each hit as a `Breakpoint` event on `events`, suspending per the given policy.
  def breakpoint(location: Jdwp.Location, policy: Jdwp.SuspendPolicy = Jdwp.SuspendPolicy.All)
    ( using Tactic[Debugger.Error] )
  :   Int =

    val modifiers: List[Jdwp.Modifier] = List(Jdwp.Modifier.LocationOnly(location))
    connection.eventRequestSet(Jdwp.EventKind.Breakpoint, policy, modifiers)

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
