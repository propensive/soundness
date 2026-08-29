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

import java.net as jn

import scala.caps

import ambience.*
import anticipation.*, abstractables.durationAbstractable
import coaxial.*
import contingency.*
import fulminate.*
import gigantism.*
import gossamer.*
import guillotine.*
import parasite.*
import proscenium.*
import rudiments.*
import spectacular.*
import urticose.*
import vacuous.*
import zephyrine.*

// A launch target: a JVM to start under the JDWP agent and then debug. `target.session { debug ?=>
// … }` forks the process with the agent listening, waits for it to bind its port, attaches, and
// lends a `Debug`; the debuggee is killed when the block ends.
object Debuggee:
  private val interval: Long = 50L*1000L*1000L
  private val attempts: Int = 200

  // A plain-socket readiness probe. With `server=y` the agent binds before `main` runs, so a
  // successful connect means it is ready to be attached to. The connection is dropped before any
  // handshake, which the OpenJDK agent tolerates and re-listens after.
  private def listening(port: Int): Boolean =
    try
      val socket = jn.Socket()
      socket.connect(jn.InetSocketAddress("localhost", port), 200)
      socket.close()
      true
    catch case _: Throwable => false

  class Sessional
    ( using online:     Online,
            monitor:    Monitor,
            probate:    Probate,
            backend:    Socket.Backend,
            options:    Every[Socket.Option.Tcp],
            asyncError: Tactic[Async.Error],
            working:    WorkingDirectory,
            loggable:   (Socket.Event is Loggable)^,
            exec:       (Exec.Event is Loggable)^,
            tactic:     Tactic[Debugger.Error],
            note:       Diagnostics )
  extends aperture.Sessional:
    type Self = Debuggee
    type Result = Debug^

    def session[result](target: Debuggee)(lambda: (session: Debug^) ?=> result): result =
      given Diagnostics = note

      val launchFailed = Debugger.Error(Debugger.Error.Reason.Disconnected, t"could not launch")
      val badPort = Debugger.Error(Debugger.Error.Reason.Disconnected, t"the port was invalid")
      val unresponsive = Debugger.Error(Debugger.Error.Reason.Disconnected, t"never listened")

      given execTactic: (Tactic[Exec.Error]^) = tactic.contramap(_ => launchFailed)
      given portTactic: (Tactic[Port.Error]^) = tactic.contramap(_ => badPort)

      val job = target.agented.fork[Exit]()
      val console = new Debug.Console()

      // The child's output and error pipes are drained from the moment of the fork — before the
      // readiness probe, since a chatty debuggee can fill the OS pipe buffer (and then block)
      // long before its agent port answers. Each drain closes its window at the pipe's end, and
      // the exit watcher settles the exit promise; all three end naturally when the process
      // dies, whether by running to completion or by the `abort` below.
      val outDrain: Task[Unit] = async:
        safely(job.stdout().toProgression.stdlib.iterator.foreach(console.out.put(_)))
        console.out.stop()

      val errDrain: Task[Unit] = async:
        safely(job.stderr().toProgression.stdlib.iterator.foreach(console.err.put(_)))
        console.err.stop()

      val exitWatch: Task[Unit] = async(console.exited.offer(job.exitStatus()))

      try
        def waitFor(remaining: Int): Unit =
          if listening(target.port) then ()
          else if remaining <= 0 then abort(unresponsive)
          else
            snooze(interval)
            waitFor(remaining - 1)

        waitFor(attempts)

        // Connected directly (not via `Debugger(endpoint).session`): delegating would rebuild the
        // socket `Connectable` from the captured `online`, whose capture set `.duplex` rejects.
        // `connect` returns a plain `Duplex`, so `online` never has to flow into an empty set.
        val endpoint = Endpoint(t"localhost", Port[Tcp](target.port))
        val duplex = summon[(Endpoint[Tcp.Port] is Connectable)^].connect(endpoint, Unset)

        try
          Jdwp.Connection.exchange(duplex): connection =>
            lambda(using new Debug(connection, console))
        finally duplex.close()
      finally
        job.abort()
        outDrain.attend()
        errDrain.attend()
        exitWatch.attend()

  given sessional: ( online:     Online,
                     monitor:    Monitor,
                     probate:    Probate,
                     backend:    Socket.Backend,
                     options:    Every[Socket.Option.Tcp],
                     asyncError: Tactic[Async.Error],
                     working:    WorkingDirectory,
                     loggable:   (Socket.Event is Loggable)^,
                     exec:       (Exec.Event is Loggable)^,
                     tactic:     Tactic[Debugger.Error],
                     note:       Diagnostics )
  =>  ( Sessional^{online, monitor, asyncError, loggable, exec, tactic, caps.any} ) = Sessional()

case class Debuggee(command: Command, port: Int = 5005, suspended: Boolean = true):
  // The command with the jdwp agent option inserted just after the executable.
  private[vivisection] def agented: Command =
    val wait = if suspended then t"y" else t"n"
    val flag = t"-agentlib:jdwp=transport=dt_socket,server=y,suspend=$wait,address=*:$port"
    val args = command.arguments

    Command((args.take(1) ++ (flag +: args.drop(1)))*)
