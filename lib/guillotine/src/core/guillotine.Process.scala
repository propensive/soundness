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
┃    Soundness, version 0.37.0.                                                                    ┃
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

import scala.compiletime.*

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*

object Process:
  given writable: [chunk, command <: Label, result]
        =>  ji.OutputStream is Writable by chunk
        =>  Process[command, result] is Writable by chunk =

    (process, stream) => process.stdin(stream)

  given writableText: [command <: Label, result] => Tactic[StreamError]
        =>  Process[command, result] is Writable by Text =
    (process, stream) => process.stdin(stream.map(_.sysBytes))

class Process[+exec <: Label, result](process: java.lang.Process) extends ProcessRef:
  def pid: Pid = Pid(process.pid)
  def alive: Boolean = process.isAlive
  def attend(): Unit = process.waitFor()

  def stdout(): Stream[Bytes] raises StreamError =
    Readable.inputStream.stream(process.getInputStream.nn)

  def stderr(): Stream[Bytes] raises StreamError =
    Readable.inputStream.stream(process.getErrorStream.nn)


  def stdin[chunk](stream: Stream[chunk])(using writable: ji.OutputStream is Writable by chunk)
  : Unit =

      writable.write(process.getOutputStream.nn, stream)


  def await()(using computable: result is Computable): result = computable.compute(process)

  def exitStatus(): Exit = process.waitFor() match
    case 0     => Exit.Ok
    case other => Exit.Fail(other)

  def abort(): Unit logs ExecEvent =
    Log.info(ExecEvent.AbortProcess(pid))
    process.destroy()

  def kill(): Unit logs ExecEvent =
    Log.warn(ExecEvent.KillProcess(pid))
    process.destroyForcibly()

  def osProcess(using Tactic[PidError]) = OsProcess(pid)

  def startTime[instant: Instantiable across Instants from Long]: Optional[instant] =
    try
      import strategies.throwUnsafely
      osProcess.startTime[instant]
    catch case _: PidError => Unset

  def cpuUsage[instant: SpecificDuration]: Optional[instant] =
    try
      import strategies.throwUnsafely
      osProcess.cpuUsage[instant]
    catch case _: PidError => Unset
