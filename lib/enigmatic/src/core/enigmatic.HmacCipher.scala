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

import anticipation.*
import corpuscular.*
import gastronomy.*
import prepositional.*

// HMAC as a `Cipher & Signing & Symmetric`, so that a `SymmetricKey[HmacCipher[Sha2[256]]]`
// signs and verifies through the same `PrivateKey`/`SymmetricKey` machinery an asymmetric key
// does — which is how COSE's `HMAC 256/256` and JOSE's `HS256` see it. (`enigmatic.Hmac` is the
// *output* of the `hmac` extension, not a cipher.) The HMAC and the random key are the in-scope
// `Crypto` provider's.
//
// A generated key is as long as the hash's output — 32 bytes for SHA-256, 48 for SHA-384, 64
// for SHA-512 — which is the minimum RFC 7518 §3.2 allows and the length RFC 2104 recommends.
object HmacCipher:
  given value: [algorithm <: Algorithm] => (hash: Hash in algorithm) => (crypto: Crypto)
  =>  HmacCipher[algorithm] =
    HmacCipher[algorithm]()

class HmacCipher[algorithm <: Algorithm]()(using hash: Hash in algorithm, crypto: Crypto)
extends Cipher, Signing, Symmetric:
  // The width of the hash, measured: a `Hash` does not record it. The JDK refuses an empty
  // HMAC key, so the probe's is one byte.
  final lazy val bits: Int =
    val probe: Data = Array.fill[Byte](1)(0)
    crypto.hmac(hash.hmacName).mac(probe, Data()).length*8

  type Size = bits.type
  def keySize: bits.type = bits
  def genKey(): Data = crypto.random.bytes(bits/8)
  def privateToPublic(key: Data): Data = key
  def sign(data: Data, keyData: Data): Data = crypto.hmac(hash.hmacName).mac(keyData, data)

  def verify(data: Data, signature: Data, keyData: Data): Boolean =
    Crypto.constantTimeEquals(sign(data, keyData), signature)
