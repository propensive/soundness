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
import zephyrine.*

object Job:
  // Polymorphic over the capability instance (`job <: Job[…]^`, the galilei `Handle` recipe):
  // a bare `Job[command, result]` Self cannot match a tracked `Job` value.
  given writable: [chunk, command <: Label, result, job <: Job[command, result]^]
  =>  (writable0: (ProcessInput is Writable by chunk)^)
  =>  ((job is Writable by chunk)^{writable0}) =

    (process, stream) => process.stdin(stream)


  given writableText: [command <: Label, result, job <: Job[command, result]^]
  =>  (streamCut: Emit[StreamError])
  =>  ((job is Writable by Text)^{streamCut}) =

    (process, stream) => process.stdin(stream.map(_.sysData))


// A `Job` is a *capability*: it is the live handle to a running subprocess (its streams and
// its lifecycle), tracked fresh from `fork()`. `Exclusive` because a subprocess's stdio has
// a single owner.
class Job[+exec <: Label, result] private[guillotine] (process: java.lang.Process)
extends Subprocess, ProcessRef, caps.ExclusiveCapability:
  def pid: Pid = Pid(process.pid)
  def alive: Boolean = process.isAlive
  def attend(): Unit = process.waitFor()

  // The process's standard output (or error) as a single-owner pull endpoint:
  // each call reads on from the live pipe, and explicit `memoize` replaces any
  // implicit caching.
  def stdout()(using Tactic[StreamError]): (Stream[Data] over Credit)^ =
    summon[ji.InputStream is Source by Data over Credit].stream(process.getInputStream.nn)

  def stderr()(using Tactic[StreamError]): (Stream[Data] over Credit)^ =
    summon[ji.InputStream is Source by Data over Credit].stream(process.getErrorStream.nn)

  def text(): Text = String(process.getInputStream.nn.readAllBytes().nn, "UTF-8").nn.tt
  def errorText(): Text = String(process.getErrorStream.nn.readAllBytes().nn, "UTF-8").nn.tt

  // Standard output as a record stream of its lines: UTF-8 through the duct
  // kernel, with adaptive line separation — matching the treatment of `\n`,
  // `\r\n` and `\r` by `BufferedReader.readLine`, which this replaces.
  def lines()(using Tactic[StreamError]): (Stream[IArray[Text]] over Credit)^ =
    import hieroglyph.charDecoders.utf8Decoder, hieroglyph.textSanitizers.substituteSanitizer
    import turbulence.lineSeparation.adaptiveLinefeedLineSeparation
    stdout().delineate

  def status(): Int = process.waitFor()


  def stdin[chunk](stream: LazyList[chunk])
    ( using writable: (ProcessInput is Writable by chunk)^ )
  :   Unit =
    writable.write(ProcessInput(process.getOutputStream.nn), stream)


  def await()(using computable: (result is Computable)^): result =
    computable.compute(this)

  def await[duration: Abstractable across Durations to Long](duration: duration)
    ( using computable: (result is Computable)^ )
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

  def process(using Tactic[PidError]^): Process^ = Process(pid)

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
