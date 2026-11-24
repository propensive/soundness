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
import corpuscular.*
import gastronomy.*
import prepositional.*

// RSAES-OAEP (RFC 8017 §7.1): RSA encryption with Optimal Asymmetric Encryption Padding, in
// place of the PKCS#1 v1.5 padding `Rsa` encrypts with, which is open to Bleichenbacher's
// padding-oracle attack and should not be chosen for anything new. The hash, named as the
// second type parameter, is both OAEP's own and its mask-generation function's, with the empty
// label: `RsaOaep[2048, Sha1]` is JOSE's `RSA-OAEP` and `RsaOaep[2048, Sha2[256]]` its
// `RSA-OAEP-256`. (SHA-1's collision weakness does not bear on OAEP, so no `Permit` is asked.)
//
// The key is an ordinary RSA key; as with `RsaPss`, the type is what selects the padding. It is
// reached through a structural refinement of the provider.
object RsaOaep:
  given value: [bits <: 2048 | 3072 | 4096: ValueOf, hash <: Algorithm]
  =>  ( hash:   Hash in hash,
        crypto: Crypto { def rsaOaep(digest: Text): Crypto.PublicKeyCipher } )
  =>  RsaOaep[bits, hash] =
    RsaOaep(crypto.rsaOaep(hash.name))

  given keyAlgorithm: [bits <: 2048 | 3072 | 4096: ValueOf, hash <: Algorithm]
  =>  RsaOaep[bits, hash] is KeyAlgorithm =
    KeyAlgorithm.rsa[RsaOaep[bits, hash]](valueOf[bits])

class RsaOaep[bits <: 2048 | 3072 | 4096: ValueOf, hash <: Algorithm]
  ( cipher: Crypto.PublicKeyCipher )
extends Cipher, Encryption:
  type Size = bits
  def keySize: bits = valueOf[bits]
  def genKey(): Data = cipher.generateKeyPair(keySize)
  def privateToPublic(bytes: Data): Data = cipher.privateToPublic(bytes)
  def decrypt(bytes: Data, key: Data): Data = cipher.decrypt(bytes, key)

  // RSA uses no initialization vector; the explicit `iv` is ignored.
  def encrypt(bytes: Data, key: Data, iv: InitializationVector): Data = cipher.encrypt(bytes, key)
