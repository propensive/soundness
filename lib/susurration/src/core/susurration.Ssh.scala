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
package susurration

import scala.caps

import anticipation.*
import coaxial.*
import contingency.*
import enigmatic.*
import fulminate.*
import gastronomy.*
import gigantism.*
import gossamer.*
import prepositional.*
import urticose.*
import vacuous.*

// The SSH-2 protocol's constants and the decisions a client makes: which algorithms it offers,
// and whether to trust a host's key.
object Ssh:
  // The identification string this client sends (RFC 4253 §4.2).
  val Version: Text = t"SSH-2.0-susurration_0.72"

  // Message numbers (RFC 4250 §4.1.2).
  object Message:
    val Disconnect: Byte = 1
    val Ignore: Byte = 2
    val Unimplemented: Byte = 3
    val Debug: Byte = 4
    val ServiceRequest: Byte = 5
    val ServiceAccept: Byte = 6
    val ExtInfo: Byte = 7
    val KexInit: Byte = 20
    val NewKeys: Byte = 21
    val KexEcdhInit: Byte = 30
    val KexEcdhReply: Byte = 31
    val UserauthRequest: Byte = 50
    val UserauthFailure: Byte = 51
    val UserauthSuccess: Byte = 52
    val UserauthBanner: Byte = 53
    val UserauthPkOk: Byte = 60
    val GlobalRequest: Byte = 80
    val RequestSuccess: Byte = 81
    val RequestFailure: Byte = 82
    val ChannelOpen: Byte = 90
    val ChannelOpenConfirmation: Byte = 91
    val ChannelOpenFailure: Byte = 92
    val ChannelWindowAdjust: Byte = 93
    val ChannelData: Byte = 94
    val ChannelExtendedData: Byte = 95
    val ChannelEof: Byte = 96
    val ChannelClose: Byte = 97
    val ChannelRequest: Byte = 98
    val ChannelSuccess: Byte = 99
    val ChannelFailure: Byte = 100

  // The provider members the transport needs, beyond the baseline every provider has.
  type Provider = Crypto {
    def x25519: Crypto.KeyAgreement
    def ecdh: Crypto.KeyAgreement
    def ed25519: Crypto.SignatureScheme
    def ecdsa(digest: Text): Crypto.SignatureScheme
    def aesGcm: Crypto.Aead
    def chaCha20: Crypto.StreamCipher
  }

  // The algorithms offered, in order of preference, as the names RFC 4250 registers them.
  object Algorithms:
    given default: Algorithms = Algorithms()

  case class Algorithms
    ( kex:         List[Text] = List(t"curve25519-sha256", t"curve25519-sha256@libssh.org",
                                     t"ecdh-sha2-nistp256"),
      hostKey:     List[Text] = List(t"ssh-ed25519", t"ecdsa-sha2-nistp256", t"rsa-sha2-512",
                                     t"rsa-sha2-256"),
      cipher:      List[Text] = List(t"chacha20-poly1305@openssh.com", t"aes256-gcm@openssh.com",
                                     t"aes128-gcm@openssh.com"),
      mac:         List[Text] = List(t"hmac-sha2-256"),
      compression: List[Text] = List(t"none") )

  // The decision on a host's key, made before anything is trusted: given the host, port, the
  // key's type and OpenSSH's blob for it, whether to continue. A `KnownHosts` file is the usual
  // policy; `trustOnFirstUse` is what `ssh` does with `StrictHostKeyChecking=accept-new`, and
  // `insecurelyAcceptAll` is for tests against a server whose key is not worth recording.
  object HostKeys:
    given knownHosts: (known: KnownHosts, crypto: Crypto) => HostKeys =
      (hostname, port, keyType, key) =>
        known.verify(hostname, port, keyType, key) == KnownHosts.Verdict.Known

    def trustOnFirstUse(known: KnownHosts)(using Crypto): HostKeys =
      (hostname, port, keyType, key) =>
        known.verify(hostname, port, keyType, key) match
          case KnownHosts.Verdict.Known | KnownHosts.Verdict.Unknown => true
          case _                                                     => false

    val insecurelyAcceptAll: HostKeys = (_, _, _, _) => true

  trait HostKeys:
    def accept(hostname: Text, port: Int, keyType: Text, key: Data): Boolean

  // A destination: a host and port, the user to be, and the signers that prove it. A session
  // (`endpoint.session: session ?=> …`) connects, verifies the host key against the `HostKeys`
  // policy in scope, authenticates and lends an `Ssh.Session`.
  object Endpoint:
    given sessional: (online: Online)
    =>  ( backend:    Socket.Backend,
          options:    Every[Socket.Option.Tcp],
          provider:   Provider,
          algorithms: Algorithms,
          hostKeys:   HostKeys,
          sha256:     Hash in Sha2[256],
          tactic:     Tactic[Ssh.Error],
          diagnostics: Diagnostics )
    =>  ( (EndpointSessional)^{online, tactic, caps.any} ) =
      EndpointSessional()

  case class Endpoint(host: Text, port: Int, user: Text, signers: List[Signer]):
    def resolved(config: SshConfig): Endpoint =
      val resolution = config.resolve(host)
      Endpoint(resolution.hostName, resolution.port, resolution.user.or(user), signers)

  // A named instance class rather than an anonymous given: an anonymous given freshens the
  // inferred `Result` member.
  class EndpointSessional()
    ( using online:      Online,
            backend:     Socket.Backend,
            options:     Every[Socket.Option.Tcp],
            provider:    Provider,
            algorithms:  Algorithms,
            hostKeys:    HostKeys,
            sha256:      Hash in Sha2[256],
            tactic:      Tactic[Ssh.Error],
            diagnostics: Diagnostics )
  extends aperture.Sessional:
    type Self = Endpoint
    type Result = Session^

    def session[result](target: Endpoint)(lambda: (session: Session^) ?=> result): result =
      if target.port < 1 || target.port > 65535
      then abort(Ssh.Error(Error.Reason.Malformed(t"the port ${target.port} is out of range")))

      val endpoint = urticose.Endpoint(target.host, urticose.Port.unsafe[urticose.Tcp](target.port))
      val connectable = summon[(urticose.Endpoint[urticose.Tcp.Port] is Connectable)^]
      val duplex = connectable.connect(endpoint, Unset)

      try
        val transport = Transport(duplex, target.host, target.port)
        transport.connect()
        transport.authenticate(target.user, target.signers)
        val session: Session^ = Session(transport)
        try lambda(using session) finally session.close()
      finally duplex.close()

  object Error:
    given communicable: Reason is Communicable =
      case Reason.Malformed(detail)        => m"the server sent a malformed message: $detail"
      case Reason.Unexpected(message)      => m"the server sent an unexpected message $message"
      case Reason.NoCommonAlgorithm(kind)  => m"the server offers no supported $kind algorithm"
      case Reason.HostKeyRejected(keyType) => m"the server's $keyType host key was not accepted"
      case Reason.BadSignature             => m"the server's signature over the exchange is wrong"
      case Reason.Disconnected(code, text) => m"the server disconnected with code $code: $text"
      case Reason.ClosedConnection         => m"the connection was closed"
      case Reason.Integrity                => m"a packet failed its integrity check"
      case Reason.ServiceRefused(service)  => m"the server did not accept the service $service"
      case Reason.Unauthenticated(methods) => m"no identity was accepted; allowed: $methods"
      case Reason.Agent(detail)            => m"the SSH agent $detail"
      case Reason.ChannelRefused(detail)   => m"the server refused to open a channel: $detail"
      case Reason.RequestRefused(kind)     => m"the server refused the $kind request"

    enum Reason(val number: Int) extends Clarification:
      case Malformed(detail: Text) extends Reason(1)
      case Unexpected(message: Int) extends Reason(2)
      case NoCommonAlgorithm(kind: Text) extends Reason(3)
      case HostKeyRejected(keyType: Text) extends Reason(4)
      case BadSignature extends Reason(5)
      case Disconnected(code: Int, text: Text) extends Reason(6)
      case ClosedConnection extends Reason(7)
      case Integrity extends Reason(8)
      case ServiceRefused(service: Text) extends Reason(9)
      case Unauthenticated(methods: Text) extends Reason(10)
      case Agent(detail: Text) extends Reason(11)
      case ChannelRefused(detail: Text) extends Reason(12)
      case RequestRefused(kind: Text) extends Reason(13)

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(600, reason.number)(m"the SSH connection failed because $reason")
