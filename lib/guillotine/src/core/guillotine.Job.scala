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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import java.io as ji
import java.util.concurrent as juc

import scala.jdk.StreamConverters.StreamHasToScala

import anticipation.*
import contingency.*
import gossamer.*
import parasite.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

object Job:
  given writable: [chunk, command <: Label, result]
  =>  ProcessInput is Writable by chunk
  =>  Job[command, result] is Writable by chunk =

    (process, stream) => process.stdin(stream)


  given writableText: [command <: Label, result] => Emit[StreamError]
  =>  Job[command, result] is Writable by Text =

    (process, stream) => process.stdin(stream.map(_.sysData))


class Job[+exec <: Label, result] private[guillotine] (process: java.lang.Process)
extends Subprocess, ProcessRef:
  def pid: Pid = Pid(process.pid)
  def alive: Boolean = process.isAlive
  def attend(): Unit = process.waitFor()

  def stdout(): Stream[Data] raises StreamError =
    Streamable.inputStream.stream(process.getInputStream.nn)

  def stderr(): Stream[Data] raises StreamError =
    Streamable.inputStream.stream(process.getErrorStream.nn)

  def text(): Text = String(process.getInputStream.nn.readAllBytes().nn, "UTF-8").nn.tt
  def errorText(): Text = String(process.getErrorStream.nn.readAllBytes().nn, "UTF-8").nn.tt

  def lines(): Stream[Text] =
    val reader = ji.BufferedReader(ji.InputStreamReader(process.getInputStream))
    reader.lines().nn.toScala(Stream).map(_.tt)

  def status(): Int = process.waitFor()


  def stdin[chunk](stream: Stream[chunk])(using writable: ProcessInput is Writable by chunk): Unit =
    writable.write(ProcessInput(process.getOutputStream.nn), stream)


  def await()(using computable: result is Computable): result = computable.compute(this)

  def await[duration: Abstractable across Durations to Long](duration: duration)
    ( using computable: result is Computable )
  :   result raises AsyncError =

    if process.waitFor(duration.generic/1_000_000L, juc.TimeUnit.MILLISECONDS)
    then computable.compute(this)
    else contingency.abort(AsyncError(AsyncError.Reason.Timeout))

  def exitStatus(): Exit logs ExecEvent = process.waitFor() match
    case 0     => Log.fine(ExecEvent.ProcessExit(pid, 0)); Exit.Ok
    case other => Log.warn(ExecEvent.ProcessExit(pid, other)); Exit.Fail(other)

  def abort(): Unit logs ExecEvent =
    Log.info(ExecEvent.AbortProcess(pid))
    process.destroy()

  def kill(): Unit logs ExecEvent =
    Log.warn(ExecEvent.KillProcess(pid))
    process.destroyForcibly()

  def process(using Tactic[PidError]) = Process(pid)

  def startTime[instant: Instantiable across Instants from Long]: Optional[instant] =
    try
      import strategies.throwUnsafely
      process.startTime[instant]
    catch case _: PidError => Unset

  def cpuUsage[instant: Instantiable across Durations from Long]: Optional[instant] =
    try
      import strategies.throwUnsafely
      process.cpuUsage[instant]
    catch case _: PidError => Unset
