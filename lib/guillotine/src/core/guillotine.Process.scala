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
package guillotine

import language.experimental.pureFunctions

import anticipation.*
import contingency.*
import prepositional.*
import vacuous.*

object Process:
  private def allHandles = ProcessHandle.allProcesses.nn.iterator.nn.asScala.to(List)

  def apply(pid: Pid)(using pidError: Tactic[PidError]^): Process =
    val handle = ProcessHandle.of(pid.value).nn
    if handle.isPresent then new Process(handle.get.nn) else abort(PidError(pid))

  // While-loops rather than `map`: a fresh capability may not be minted inside a lambda
  // whose result type another scope owns (the Spring rule); built in the method body, the
  // handles box into the (capture-boxed) list element type.
  private def processes(handles: List[ProcessHandle]): List[Process] =
    val builder = List.newBuilder[Process]
    var remaining = handles

    while !remaining.isEmpty do
      builder += new Process(remaining.head)
      remaining = remaining.tail

    builder.result()

  def all: List[Process] = processes(allHandles)
  def roots: List[Process] = processes(allHandles.filter(!_.parent.nn.isPresent))
  def apply(): Process = new Process(ProcessHandle.current.nn)

// A `Process` is a *capability*, like `Job`: a live handle to a running operating-system
// process.
class Process private (java: ProcessHandle) extends ProcessRef, caps.ExclusiveCapability:
  def pid: Pid = Pid(java.pid)
  def kill(): Unit logs ExecEvent = java.destroy()
  def abort(): Unit logs ExecEvent = java.destroyForcibly()
  def alive: Boolean = java.isAlive
  def attend(): Unit = java.onExit.nn.get()

  def parent: Optional[Process] =
    val parent = java.parent.nn
    if parent.isPresent then new Process(parent.get.nn) else Unset

  def children: List[Process] =
    Process.processes(java.children.nn.iterator.nn.asScala.to(List))

  def startTime[instantiable: Instantiable across Instants from Long]: Optional[instantiable] =
    val instant = java.info.nn.startInstant.nn
    if instant.isPresent then instantiable(instant.get.nn.toEpochMilli) else Unset

  def cpuUsage[instantiable: Instantiable across Durations from Long]: Optional[instantiable] =
    val duration = java.info.nn.totalCpuDuration.nn
    if duration.isPresent then instantiable(duration.get.nn.toNanos) else Unset
