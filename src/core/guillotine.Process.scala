/*
    Guillotine, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package guillotine

import language.experimental.pureFunctions

import java.io as ji

import scala.compiletime.*

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

object Process:
  given [ChunkType, CommandType <: Label, ResultType]
     (using ji.OutputStream is Writable by ChunkType)
      => Process[CommandType, ResultType] is Writable by ChunkType as writable =

    (process, stream) => process.stdin(stream)

  given [CommandType <: Label, ResultType](using Tactic[StreamError])
      => Process[CommandType, ResultType] is Writable by Text as writableText =
    (process, stream) => process.stdin(stream.map(_.sysBytes))

class Process[+ExecType <: Label, ResultType](process: java.lang.Process) extends ProcessRef:
  def pid: Pid = Pid(process.pid)
  def alive: Boolean = process.isAlive
  def attend(): Unit = process.waitFor()

  def stdout(): LazyList[Bytes] raises StreamError =
    Readable.inputStream.stream(process.getInputStream.nn)

  def stderr(): LazyList[Bytes] raises StreamError =
    Readable.inputStream.stream(process.getErrorStream.nn)

  def stdin[ChunkType](stream: LazyList[ChunkType])
     (using writable: ji.OutputStream is Writable by ChunkType)
          : Unit =

    writable.write(process.getOutputStream.nn, stream)

  def await()(using computable: ResultType is Computable): ResultType = computable.compute(process)

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

  def startTime[InstantType: SpecificInstant]: Optional[InstantType] =
    try
      import strategies.throwUnsafely
      osProcess.startTime[InstantType]
    catch case _: PidError => Unset

  def cpuUsage[InstantType: SpecificDuration]: Optional[InstantType] =
    try
      import strategies.throwUnsafely
      osProcess.cpuUsage[InstantType]
    catch case _: PidError => Unset
