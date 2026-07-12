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
package perihelion

import soundness.*

import logging.silentLogging
import internetAccess.online
import strategies.throwUnsafely
import threading.virtualThreading
import probates.awaitProbate

// A conformance harness for driving the WebSocket *client* against the Autobahn|Testsuite
// `fuzzingserver` — the counterpart of `AutobahnServer` (which drives the server against a
// `fuzzingclient`). Start `wstest -m fuzzingserver` (listening on 9001 by default), then run
// `java -cp <test-classpath> perihelion.AutobahnClient [host] [port]` and read its HTML report.
// Not part of the test suite — it only runs when invoked directly.
//
// The fuzzingserver drives every interaction, so the client is a pure echo: for each case it
// connects, replies to every message verbatim, and answers Ping/Close per the RFC (handled by
// the shared `Reader`). It drives that `Reader`/`Channel` directly rather than through Coaxial's
// `react`/`exchange`, which flatten each message to bytes — losing the Text/Binary frame type an
// echo must preserve — and offer no hook to send an RFC close code on a protocol violation.
object AutobahnClient:
  private val agent: Text = t"soundness-perihelion"

  def main(args: Array[String]): Unit =
    val host: Text = (if args.length > 0 then args(0) else "localhost").tt
    val port: Int = if args.length > 1 then Integer.parseInt(args(1)) else 9001

    supervise:
      // Open one connection, hand each inbound message to `consume`, and always close cleanly;
      // a protocol violation surfaces as a `WebsocketError` and is answered with its close code.
      def session(path: Text)(consume: Channel => perihelion.Message => Unit): Unit =
        val url = t"ws://$host:$port$path".as[WsUrl]
        val connection = summon[WsUrl is Duplexable].connect(url, Unset)
        given Masking = connection.masking

        try
          recover:
            case error: WebsocketError =>
              safely(connection.channel.close(error.reason.closeCode))

          . protect:
              Reader
                ( () =>
                    connection.inbound
                    . asInstanceOf[zephyrine.Stream[Data] over zephyrine.Credit],
                  connection.channel )
              . messages.each:
                consume(connection.channel)(_)

        finally
          connection.channel.stop()
          safely(connection.pump.attend())
          connection.duplex.close()

      var count: Int = 0

      // How many cases does the fuzzingserver have? It sends the count as one Text message.
      session(t"/getCaseCount"): channel =>
        case perihelion.Message.Text(text) => count = safely(Integer.parseInt(text.s.trim)).or(count)
        case _                             => ()

      // Echo each case verbatim, preserving the Text/Binary frame type.
      for index <- 1 to count do
        safely(session(t"/runCase?case=$index&agent=$agent")(channel => channel.send(_)))

      // Ask the server to write its reports.
      safely(session(t"/updateReports?agent=$agent")(_ => _ => ()))
