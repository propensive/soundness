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
import vacuous.*

// ChaCha20 (RFC 8439) as a bare keystream, with the 96-bit nonce and 32-bit block counter of the
// IETF variant, and ChaCha20-Poly1305, the AEAD built on it with a 128-bit Poly1305 tag: TLS 1.3's
// other cipher, WireGuard's, and the base of OpenSSH's `chacha20-poly1305@openssh.com`, whose
// construction differs (two keys, and an encrypted length) and is assembled from `ChaCha20` and
// `Poly1305` by the SSH transport. Both are reached through structural refinements of the
// provider, and the key is the fixed 256 bits.
object ChaCha20:
  given value: (crypto: Crypto { def chaCha20: Crypto.StreamCipher }) => ChaCha20 =
    ChaCha20(crypto.chaCha20)

class ChaCha20(cipher: Crypto.StreamCipher) extends Cipher, Symmetric, Keystream:
  type Size = 256

  def keySize: 256 = 256
  def genKey(): Data = cipher.generateKey(keySize)
  def privateToPublic(key: Data): Data = key
  def nonceSize: Int = 12
  def xor(key: Data, nonce: Data, counter: Int, data: Data): Data =
    cipher.xor(key, nonce, counter, data)

object ChaCha20Poly1305:
  given value: (crypto: Crypto { def chaCha20Poly1305: Crypto.Aead }) => ChaCha20Poly1305 =
    ChaCha20Poly1305(crypto.chaCha20Poly1305)

class ChaCha20Poly1305(aead: Crypto.Aead) extends Cipher, Symmetric, Authenticated:
  type Size = 256

  def keySize: 256 = 256
  def genKey(): Data = aead.generateKey(keySize)
  def privateToPublic(key: Data): Data = key
  def nonceSize: Int = 12
  def tagSize: Int = 16

  def seal(key: Data, nonce: Data, associated: Data, plaintext: Data): Data =
    aead.seal(key, nonce, associated, plaintext)

  def unseal(key: Data, nonce: Data, associated: Data, ciphertext: Data): Optional[Data] =
    aead.unseal(key, nonce, associated, ciphertext)
