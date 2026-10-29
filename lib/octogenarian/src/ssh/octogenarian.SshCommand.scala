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
package octogenarian

import ambience.*
import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import guillotine.*
import rudiments.*
import symbolism.*
import turbulence.*
import vacuous.*
import zephyrine.*

object SshCommand:
  import Git.Error.Reason as Reason

  // One `ssh` process per exchange: the server begins by advertising, which is read (and, for an
  // exchange, discarded) before the request is written and the response read. Nothing is kept
  // between exchanges, so a fetch costs a connection for `ls-refs` and one for `fetch`.
  class Channel(address: SshAddress)
    ( using working:     WorkingDirectory,
            environment: Environment,
            loggable:    (Exec.Event is Loggable)^ )
  extends octogenarian.Channel:

    private val protocolVariable: Text = t"GIT_PROTOCOL"
    private val protocolVersion: Text = t"version=2"

    // The given environment, with `GIT_PROTOCOL` added.
    private object childEnvironment extends Environment:
      def variable(name: Text): Optional[Text] =
        if name == protocolVariable then protocolVersion else environment.variable(name)

      override def entries: Optional[Map[Text, Text]] =
        environment.entries.let: entries =>
          (entries.to[List] + List((protocolVariable, protocolVersion))).to[Map]

    private def command(service: Text): Command =
      val destination = address.user.lay(address.host): user => t"$user@${address.host}"
      val portArguments = address.port.let { port => List(t"-p", port.toString.tt) }.or(Nil)
      val remote = t"$service '${address.path}'"
      val options = List(t"-o", t"SendEnv=$protocolVariable", destination, remote)
      Command((List(t"ssh") + portArguments + options)*)

    // Starts the service, writes `request` (if any) to it and returns its output.
    private def start(service: Text, request: Optional[Data])
      ( using tactic: Tactic[Git.Error] )
    :   Pktline.Reader =

      given execution: Tactic[Exec.Error] = tactic.contramap: error =>
        Git.Error(Reason.Unreachable(address.host), error.message.text)(using Diagnostics.omit)

      given truncation: Tactic[Truncation.Error] = tactic.contramap: error =>
        Git.Error(Reason.Unreachable(address.host), error.message.text)(using Diagnostics.omit)

      val job = command(service).fork[Unit]()(using working, childEnvironment)
      job.stdin(request.or(Data()).stream)
      Pktline.Reader.from(job.stdout())

    def advertise(service: Text)(using Tactic[Git.Error]): Pktline.Reader = start(service, Unset)

    def exchange(service: Text, request: Data)(using Tactic[Git.Error]): Pktline.Reader =
      val reader = start(service, request)
      // The advertisement comes first on every connection; this exchange's reply follows it.
      reader.section()
      reader

    def close(): Unit = ()

// Remote repositories reached through a spawned `ssh`, as git itself reaches them: one process
// per service, running `git-upload-pack` or `git-receive-pack` on the server with protocol
// version 2 requested through the `GIT_PROTOCOL` variable (which `SendEnv` forwards, where the
// server's `AcceptEnv` allows it). Both `ssh://[user@]host[:port]/path` and the `scp`-like
// `[user@]host:path` forms are understood.

class SshCommand()
  ( using working:     WorkingDirectory,
          environment: Environment,
          loggable:    (Exec.Event is Loggable)^ )
extends Transport:
  def accepts(url: Text): Boolean = SshAddress.parse(url).present

  def connect(url: Text)(using Tactic[Git.Error]): Peer^ =
    val address =
      SshAddress.parse(url)
      . or(abort(Git.Error(Git.Error.Reason.Unsupported(t"the transport for $url"))))
    Protocol(SshCommand.Channel(address))
