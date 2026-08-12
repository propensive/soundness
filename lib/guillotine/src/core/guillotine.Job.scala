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

import scala.caps

import scala.language.experimental.pureFunctions

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
  =>  (writable0: (Process.Input is Writable by chunk)^)
  =>  ((job is Writable by chunk)^{writable0}) =

    (process, stream) => process.stdin(stream)

  given writableText: [command <: Label, result, job <: Job[command, result]^]
  =>  (streamCut: Emit[StreamError])
  =>  ((job is Writable by Text)^{streamCut}) =

    (process, stream) =>
      process.stdin
        ( stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^]
          . via(hieroglyph.CharEncoder.system).asInstanceOf[(Stream[Data] over Credit)^] )

// A `Job` is a *capability*: it is the live handle to a running subprocess (its streams and
// its lifecycle), tracked fresh from `fork()`. `Exclusive` because a subprocess's stdio has
// a single owner.
//
// A pipeline has a process at each end, and they are not the same one: its output comes from the
// last process, and its input goes to the first. `process` is therefore the tail — whose output,
// exit status and pid speak for the pipeline — and `head` is where input is written, which for a
// single command is the same process. The distinction is not cosmetic: `startPipeline` replaces
// every process's standard input but the first's with a null stream that throws on any write.
class Job[+exec <: Label, result] private[guillotine]
   ( process: java.lang.Process, head: java.lang.Process )
extends Subprocess, Process.Ref, caps.ExclusiveCapability:

  private[guillotine] def this(process: java.lang.Process) = this(process, process)

  def pid: Pid = Pid(process.pid)
  def alive: Boolean = process.isAlive
  def attend(): Unit = process.waitFor()

  // The process's standard output (or error) as a single-owner pull endpoint:
  // each call reads on from the live pipe, and explicit `memoize` replaces any
  // implicit caching.
  def stdout()(using Tactic[StreamError]): (Stream[Data] over Credit)^ =
    summon[ji.InputStream is Streamable by Data over Credit].stream(process.getInputStream.nn)

  def stderr()(using Tactic[StreamError]): (Stream[Data] over Credit)^ =
    summon[ji.InputStream is Streamable by Data over Credit].stream(process.getErrorStream.nn)

  def text(): Text = String(process.getInputStream.nn.readAllBytes().nn, "UTF-8").nn.tt
  def errorText(): Text = String(process.getErrorStream.nn.readAllBytes().nn, "UTF-8").nn.tt

  // Standard output as a record stream of its lines: UTF-8 through the duct
  // kernel, with adaptive line separation — matching the treatment of `\n`,
  // `\r\n` and `\r` by `BufferedReader.readLine`, which this replaces.
  def lines()(using Tactic[StreamError]): (Stream[Array[Text]^{}] over Credit)^ =
    import hieroglyph.charDecoders.utf8Decoder, hieroglyph.textSanitizers.substituteSanitizer
    import turbulence.lineSeparation.adaptiveLinefeedLineSeparation
    stdout().delineate

  def status(): Int = process.waitFor()


  def stdin[chunk](stream: (Stream[chunk] over Credit)^)
    ( using writable: (Process.Input is Writable by chunk)^ )
  :   Unit =
    writable.write(Process.Input(head.getOutputStream.nn), stream)

  // Standard input as a push endpoint. `stdin` writes a whole stream and closes the pipe when it
  // ends, which suits a process that consumes its input and exits; a long-lived child that must be
  // driven message-by-message — a language server, a REPL — needs the incremental form instead:
  // each window is written and flushed as it arrives, and the pipe is closed only by `finish`.
  def intake(using sink: (ji.OutputStream is Sink by Data over Credit)^)
  :   (Intake[Data] over Credit)^ =

    sink.intake(head.getOutputStream.nn)


  def await()(using computable: (result is Computable)^): result =
    computable.compute(this)

  def await[duration: Abstractable across Durations to Long](duration: duration)
    ( using computable: (result is Computable)^ )
  :   (Tactic[Async.Error]^) ?->{this, computable} result =

    if process.waitFor(duration.generic/1_000_000L, juc.TimeUnit.MILLISECONDS)
    then computable.compute(this)
    else contingency.abort(Async.Error(Async.Error.Reason.Timeout))

  // Real `using` clauses rather than the `logs` sugar, as for `Executable.exec`.
  def exitStatus()(using (Exec.Event is Loggable)^): Exit = process.waitFor() match
    case 0     => Log.fine(Exec.Event.ProcessExit(pid, 0)); Exit.Ok
    case other => Log.warn(Exec.Event.ProcessExit(pid, other)); Exit.Fail(other)

  def abort()(using (Exec.Event is Loggable)^): Unit =
    Log.info(Exec.Event.AbortProcess(pid))
    process.destroy()

  def kill()(using (Exec.Event is Loggable)^): Unit =
    Log.warn(Exec.Event.KillProcess(pid))
    process.destroyForcibly()

  def process(using Tactic[Pid.Error]^): Process^ = Process(pid)

  def startTime[instant: Instantiable across Instants from Long]: Optional[instant] =
    try
      import strategies.throwUnsafely
      process.startTime[instant]
    catch case _: Pid.Error => Unset

  def cpuUsage[instant: Instantiable across Durations from Long]: Optional[instant] =
    try
      import strategies.throwUnsafely
      process.cpuUsage[instant]
    catch case _: Pid.Error => Unset
