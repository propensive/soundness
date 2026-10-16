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

import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import contingency.*
import distillate.*
import enigmatic.*
import gossamer.*
import prepositional.*
import vacuous.*

// How a cipher's keys identify a user to an SSH server (RFC 4252 §7): the algorithm name the
// request carries, and the signature in its wire form — raw for Ed25519 (RFC 8709), `r` and `s`
// as `mpint`s for ECDSA (RFC 5656 §3.1.2), and under SHA-256 for RSA (RFC 8332; `ssh-rsa` with
// SHA-1 is not offered). The instance summons its own cipher from the provider, so that an ECDSA
// key signs under the digest its curve pairs with, whatever `Signature.Digest` is in scope.
@unexported
object Identity:
  given ed25519: (crypto: Ssh.Provider) => Ed25519 is Identity = new Identity:
    type Self = Ed25519
    def algorithm: Text = t"ssh-ed25519"

    def sign(key: PrivateKey[Ed25519]^, data: Data): Data =
      given Ed25519 = Ed25519(crypto.ed25519)
      key.sign(data).bytes

  given ecdsa: [bits <: 256 | 384 | 521: ValueOf]
  =>  ( crypto: Ssh.Provider )
  =>  Ecdsa[bits] is Identity =
    new Identity:
      type Self = Ecdsa[bits]
      def algorithm: Text = t"ecdsa-sha2-nistp${valueOf[bits]}"

      def sign(key: PrivateKey[Ecdsa[bits]]^, data: Data): Data =
        val digest = valueOf[bits] match
          case 256 => t"SHA256"
          case 384 => t"SHA384"
          case _   => t"SHA512"

        given Ecdsa[bits] = Ecdsa(crypto.ecdsa(digest))

        // The provider's signature is DER: `SEQUENCE { r INTEGER, s INTEGER }`.
        safely(Der(key.sign(data).bytes).as[Asn1]) match
          case Asn1.Sequence(List(Asn1.Integer(r), Asn1.Integer(s))) =>
            val writer = Wire.Writer()
            writer.mpint(r)
            writer.mpint(s)
            writer.data

          case _ =>
            Data()

  given rsa: [bits <: 2048 | 3072 | 4096: ValueOf]
  =>  ( crypto: Ssh.Provider )
  =>  Rsa[bits] is Identity =
    new Identity:
      type Self = Rsa[bits]
      def algorithm: Text = t"rsa-sha2-256"

      def sign(key: PrivateKey[Rsa[bits]]^, data: Data): Data =
        given Rsa[bits] = Rsa(crypto.rsa, crypto.rsaSignature(t"SHA256"))
        key.sign(data).bytes

// Reached from the umbrella as a member of `Ssh`: see `soundness_susurration_core.scala`.
@unexported
trait Identity extends Typeclass:
  type Self <: Cipher
  def algorithm: Text
  def sign(key: PrivateKey[Self]^, data: Data): Data

// Something that can prove an identity: a loaded private key, or a key held by an agent. `blob`
// is the public key in its wire form, `algorithm` the signature algorithm the server is offered,
// and `sign` produces the signature blob (RFC 4253 §6.6: the algorithm's name, then its
// signature) over the authentication request — which is what an agent returns as it is.
object Signer:
  // A signer over a private key held in this process.
  def apply[cipher <: Cipher: {Identity as identity, KeyAlgorithm}](key: PrivateKey[cipher]^)
    ( using cipher )
  :   Signer^{key} =

    val public = OpenSsh.blob(key.public).or(Data())

    new Signer:
      def algorithm: Text = identity.algorithm
      def blob: Data = public

      def sign(data: Data): Data =
        val writer = Wire.Writer()
        writer.text(identity.algorithm)
        writer.string(identity.sign(key, data))
        writer.data

trait Signer:
  def algorithm: Text
  def blob: Data
  def sign(data: Data): Data
