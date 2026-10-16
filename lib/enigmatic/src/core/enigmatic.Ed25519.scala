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
package enigmatic

import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import gossamer.*
import vacuous.*
import gastronomy.Signing

// Ed25519 (RFC 8032), the Edwards-curve signature scheme over Curve25519 that is OpenSSH's default
// key type and SSHSIG's, and one of TLS 1.3's. It signs the message directly (the "pure" variant),
// so no `Signature.Digest` participates; and the key size is fixed by the curve, so there is no
// size parameter. Like `Ecdsa` and `MlDsa`, it is not part of the mandatory provider baseline, so
// it is reached through a structural refinement, and a provider that does not offer it is a
// compile error at the use site rather than a failure at run time.
object Ed25519:
  given value: (crypto: Crypto { def ed25519: Crypto.SignatureScheme }) => Ed25519 =
    Ed25519(crypto.ed25519)

  given keyAlgorithm: Ed25519 is KeyAlgorithm = new KeyAlgorithm:
    type Self = Ed25519
    def name: Text = t"Ed25519"
    def sshName: Optional[Text] = t"ssh-ed25519"
    def algorithm: Optional[Asn1] = Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Ed25519)))

    // RFC 8410 requires the parameters to be absent; a key is the curve's fixed 32 bytes.
    def conforms(algorithm: Asn1, material: KeyAlgorithm.Material): Boolean =
      KeyAlgorithm.identifier(algorithm).lay(false): (arcs, parameters) =>
        arcs == KeyAlgorithm.Ed25519 && parameters.absent

class Ed25519(scheme: Crypto.SignatureScheme) extends Cipher, Signing:
  type Size = 256

  def keySize: 256 = 256
  def genKey(): Data = scheme.generateKeyPair(keySize)
  def privateToPublic(keyData: Data): Data = scheme.privateToPublic(keyData)
  def sign(data: Data, keyData: Data): Data = scheme.sign(data, keyData)

  def verify(data: Data, signature: Data, keyData: Data): Boolean =
    scheme.verify(data, signature, keyData)
