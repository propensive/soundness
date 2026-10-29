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

import scala.caps

import anticipation.*
import coaxial.*
import contingency.*
import fulminate.*
import gastronomy.*
import gigantism.*
import gossamer.*
import prepositional.*
import proscenium.*
import susurration.*
import urticose.*
import vacuous.*
import zephyrine.*

import Git.Error.Reason

object SshNative:
  // One SSH connection per peer, with a channel per exchange: the server runs
  // `git-upload-pack` or `git-receive-pack` on the path, with protocol version 2 requested
  // through the `GIT_PROTOCOL` variable (which the server applies where its `AcceptEnv` allows
  // it, as a spawned `ssh` would send it). A previous exchange's channel is closed before the
  // next opens, since the transport is driven by one thread. The connection stays open until
  // `close`, which a peer is not yet asked for, so it lives as long as the peer.
  class Channel(address: SshAddress, session: Session, duplex: Duplex)
    ( using diagnostics: Diagnostics )
  extends octogenarian.Channel:

    // [field-purity] the exchange in progress, replaced by the next
    @caps.unsafe.untrackedCaptures
    private var current: Optional[susurration.Channel] = Unset

    private def start(service: Text, request: Optional[Data])(using tactic: Tactic[Git.Error])
    :   Pktline.Reader =

      given ssh: Tactic[Ssh.Error] = tactic.contramap: error =>
        Git.Error(Reason.Unreachable(address.host), error.message.text)(using Diagnostics.omit)

      current.let(_.close())
      val channel = session.open()
      current = channel
      channel.env(t"GIT_PROTOCOL", t"version=2")
      channel.exec(t"$service '${address.path}'")
      request.let(channel.write(_))
      channel.eof()
      Pktline.Reader.from(Stream(channel.stdout()))

    def advertise(service: Text)(using Tactic[Git.Error]): Pktline.Reader = start(service, Unset)

    def exchange(service: Text, request: Data)(using Tactic[Git.Error]): Pktline.Reader =
      val reader = start(service, request)
      // The advertisement comes first on every channel; this exchange's reply follows it.
      reader.section()
      reader

    def close(): Unit =
      safely:
        current.let(_.close())
        session.close()

      duplex.close()

// Remote repositories over the native SSH client: no `ssh` binary, the host key checked
// against the `Ssh.HostKeys` policy in scope, and the user's identities from `Ssh.Credentials`.
// It connects through the socket backend directly rather than through the `Online`-gated
// `Connectable`, so that it captures nothing: `Native` takes a list of pure transports. The
// `Online` evidence is demanded by the `gitTransports.soundnessSsh` given instead, as the HTTP
// client's givens demand it.
class SshNative()
  ( using backend:     Socket.Backend,
          options:     Every[Socket.Option.Tcp],
          provider:    Ssh.Provider,
          algorithms:  Ssh.Algorithms,
          hostKeys:    Ssh.HostKeys,
          credentials: Ssh.Credentials,
          sha256:      Hash in Sha2[256],
          diagnostics: Diagnostics )
extends octogenarian.Transport:

  def accepts(url: Text): Boolean = SshAddress.parse(url).present

  def connect(url: Text)(using tactic: Tactic[Git.Error]): Peer^ =
    val address =
      SshAddress.parse(url).or(abort(Git.Error(Reason.Unsupported(t"the transport for $url"))))

    given ssh: Tactic[Ssh.Error] = tactic.contramap: error =>
      Git.Error(Reason.Unreachable(address.host), error.message.text)(using Diagnostics.omit)

    val port = address.port.or(22)
    val user = address.user.or(credentials.user(address.host)).or(t"git")

    if port < 1 || port > 65535 then abort(Git.Error(Reason.Unsupported(t"the port $port")))

    val endpoint = Endpoint(address.host, Port.unsafe[Tcp](port))
    val duplex = backend.duplexTcp(endpoint, Unset, options.values.to(List))
    val transport = susurration.Transport(duplex, address.host, port)
    transport.connect()
    transport.authenticate(user, credentials.signers(address.host, user))
    Protocol(SshNative.Channel(address, Session(transport), duplex))
