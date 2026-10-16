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

import java.io as ji

import scala.caps

import anticipation.*
import coaxial.*
import contingency.*
import fulminate.*
import gossamer.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

import Ssh.Error.Reason

// An SSH agent (draft-miller-ssh-agent): a process holding keys, reached over the Unix-domain
// socket `SSH_AUTH_SOCK` names, which lists its public keys and signs with the private ones
// without ever releasing them. Each operation is one request and one reply over a fresh
// connection, framed as a `uint32` length and a message.
@unexported
object Agent:
  private val RequestIdentities: Byte = 11
  private val IdentitiesAnswer: Byte = 12
  private val SignRequest: Byte = 13
  private val SignResponse: Byte = 14

  // `rsa-sha2-256` is requested by flag (§4.5.1) for an RSA key; other keys sign as themselves.
  private val Sha256Flag: Int = 2

// Reached from the umbrella as a member of `Ssh`: see `soundness_susurration_core.scala`.
@unexported
class Agent(path: Text)(using backend: Socket.Backend, diagnostics: Diagnostics):
  import Agent.*

  // The keys the agent holds, each as a signer that defers to the agent.
  def identities()(using Tactic[Ssh.Error]): List[Signer] =
    given wire: Tactic[Wire.Error] = malformed
    val request = Wire.Writer()
    request.byte(RequestIdentities)
    val reply = Wire.Reader(exchange(request.data))

    if reply.byte() != IdentitiesAnswer
    then abort(Ssh.Error(Reason.Agent(t"did not list its keys")))

    val count = reply.uint32()

    List.range(0, count).map: _ =>
      val blob = reply.string()
      reply.string() // the comment
      signer(blob)

  private def signer(key: Data): Signer = new Signer:
    private val keyType: Text = safely(Wire.Reader(key).text()).or(t"")
    def algorithm: Text = if keyType == t"ssh-rsa" then t"rsa-sha2-256" else keyType
    def blob: Data = key

    // Signing is total from the signer's side: the agent's refusal surfaces as an empty
    // signature, which the server then rejects.
    def sign(data: Data): Data = safely(request(key, data)).or(Data())

  private def request(key: Data, data: Data)(using Tactic[Ssh.Error]): Data =
    given wire: Tactic[Wire.Error] = malformed
    val keyType = safely(Wire.Reader(key).text()).or(t"")
    val request = Wire.Writer()
    request.byte(SignRequest)
    request.string(key)
    request.string(data)
    request.uint32(if keyType == t"ssh-rsa" then Sha256Flag else 0)
    val reply = Wire.Reader(exchange(request.data))
    if reply.byte() != SignResponse then abort(Ssh.Error(Reason.Agent(t"refused to sign")))
    reply.string()

  private def exchange(message: Data)(using Tactic[Ssh.Error]): Data =
    val duplex = backend.duplexDomain(DomainSocket(path), Nil)

    try
      val framed = Wire.Writer()
      framed.string(message)
      duplex.send(Stream(framed.data))

      // Sealed as the transport seals its input: read once here, by this thread.
      // [construction-fresh] fresh InputStream over the connection's read side
      val input: ji.InputStream = caps.unsafe.unsafeAssumePure(duplex.source.inputStream)

      def readFully(count: Int): Data =
        val output = Array.scratch[Byte](count)

        try ji.DataInputStream(input).readFully(output)
        catch case _: ji.EOFException => abort(Ssh.Error(Reason.Agent(t"closed the connection")))

        // [java-boundary] a scratch array filled by the JDK, retained by nobody else
        Array.unsafeFrozen(output)

      given wire: Tactic[Wire.Error] = malformed
      val length = Wire.Reader(readFully(4)).uint32()

      if length <= 0 || length > Wire.Limit
      then abort(Ssh.Error(Reason.Agent(t"sent a bad length")))

      readFully(length)
    finally duplex.close()

  private def malformed(using Tactic[Ssh.Error]): Tactic[Wire.Error] =
    summon[Tactic[Ssh.Error]].contramap: error => Ssh.Error(Reason.Agent(error.message.text))
