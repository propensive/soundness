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
package guillotine

import java.io as ji
import scala.caps

import scala.language.experimental.pureFunctions

import anticipation.*
import contingency.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

object Process:
  private def allHandles = ProcessHandle.allProcesses.nn.iterator.nn.to[List]

  def apply(pid: Pid)(using pidError: Tactic[Pid.Error]^): Process =
    val handle = ProcessHandle.of(pid.value).nn
    if handle.isPresent then new Process(handle.get.nn) else abort(Pid.Error(pid))

  // While-loops rather than `map`: a fresh capability may not be minted inside a lambda
  // whose result type another scope owns (the Spring rule); built in the method body, the
  // handles box into the (capture-boxed) list element type.
  private def processes(handles: List[ProcessHandle]): List[Process] =
    val builder = scala.collection.immutable.List.newBuilder[Process]
    var remaining = handles.stdlib

    while !remaining.isEmpty do
      builder += new Process(remaining.head)
      remaining = remaining.tail

    List.of(builder.result())

  def all: List[Process] = processes(allHandles)
  def roots: List[Process] = processes(List.of(allHandles.stdlib.filter(!_.parent.nn.isPresent)))
  def apply(): Process = new Process(ProcessHandle.current.nn)

  // ProcessInput → Process.Input
  // The standard input of a `Subprocess`, exposed as a sink so that writing to a process's input
  // never surfaces `java.io.OutputStream` in guillotine's public API. The `Writable` instances
  // delegate to turbulence's `OutputStream` writers.
  object Input:
    given data: (streamCut: Emit[StreamError])
    =>  ((Process.Input is Writable by Data)^{streamCut}) =
      (stdin, stream) =>
        summon[(ji.OutputStream is Writable by Data)^].write(stdin.outputStream, stream)

    given text: (streamCut: Emit[StreamError], encoder: CharEncoder)
    =>  ((Process.Input is Writable by Text)^{streamCut}) =
      (stdin, stream) =>
        summon[(ji.OutputStream is Writable by Text)^].write(stdin.outputStream, stream)

  class Input private[guillotine] (private[guillotine] val outputStream: ji.OutputStream)

  // ProcessRef → Process.Ref
  trait Ref:
    def pid: Pid
    // Real `using` clauses rather than the `logs` sugar: a context-function result would hide
    // the capability `this` of implementing classes, which the separation checker rejects.
    def kill()(using (Exec.Event is Loggable)^): Unit
    def abort()(using (Exec.Event is Loggable)^): Unit
    def alive: Boolean
    def attend(): Unit
    def startTime[instant: Instantiable across Instants from Long]: Optional[instant]
    def cpuUsage[instantiable: Instantiable across Durations from Long]: Optional[instantiable]

// A `Process` is a *capability*, like `Job`: a live handle to a running operating-system
// process.
class Process private (java: ProcessHandle) extends Process.Ref, caps.ExclusiveCapability:
  def pid: Pid = Pid(java.pid)
  def kill()(using (Exec.Event is Loggable)^): Unit = java.destroy()
  def abort()(using (Exec.Event is Loggable)^): Unit = java.destroyForcibly()
  def alive: Boolean = java.isAlive
  def attend(): Unit = java.onExit.nn.get()

  def parent: Optional[Process] =
    val parent = java.parent.nn
    if parent.isPresent then new Process(parent.get.nn) else Unset

  def children: List[Process] =
    Process.processes(java.children.nn.iterator.nn.to[List])

  def startTime[instantiable: Instantiable across Instants from Long as instant0]: Optional[instantiable] =
    val instant = java.info.nn.startInstant.nn
    if instant.isPresent then instant0.apply(instant.get.nn.toEpochMilli) else Unset

  def cpuUsage[instantiable: Instantiable across Durations from Long as duration0]: Optional[instantiable] =
    val duration = java.info.nn.totalCpuDuration.nn
    if duration.isPresent then duration0.apply(duration.get.nn.toNanos) else Unset
