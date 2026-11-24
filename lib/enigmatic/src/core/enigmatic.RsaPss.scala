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

import gastronomy.Signing

// RSASSA-PSS (RFC 8017 §8.1): RSA signatures with a randomized, provably-secure padding, in
// place of the deterministic PKCS#1 v1.5 padding `Rsa` signs with. The message digest, the
// mask-generation function's digest and the salt length all follow the ambient
// `Signature.Digest` — SHA-256 with a 32-byte salt unless overridden — which is the
// parameterization JOSE's `PS256`, `PS384` and `PS512`, TLS 1.3 and COSE all fix.
//
// The key is an ordinary RSA key: a `PrivateKey[RsaPss[2048]]` and a `PrivateKey[Rsa[2048]]`
// are the same document. The distinct type is what selects the padding, and keeps one key from
// being used under both. Like `Ecdsa`, it is reached through a structural refinement of the
// provider, so a provider without it is a compile error at the use site.
object RsaPss:
  given value: [bits <: 2048 | 3072 | 4096: ValueOf]
  =>  ( digest: Signature.Digest,
        crypto: Crypto { def rsaPss(digest: Text): Crypto.SignatureScheme } )
  =>  RsaPss[bits] =
    RsaPss(crypto.rsaPss(digest.token))

  given keyAlgorithm: [bits <: 2048 | 3072 | 4096: ValueOf] => RsaPss[bits] is KeyAlgorithm =
    KeyAlgorithm.rsa[RsaPss[bits]](valueOf[bits])

class RsaPss[bits <: 2048 | 3072 | 4096: ValueOf](scheme: Crypto.SignatureScheme)
extends Cipher, Signing:
  type Size = bits
  def keySize: bits = valueOf[bits]
  def genKey(): Data = scheme.generateKeyPair(keySize)
  def privateToPublic(keyData: Data): Data = scheme.privateToPublic(keyData)
  def sign(data: Data, keyData: Data): Data = scheme.sign(data, keyData)

  def verify(data: Data, signature: Data, keyData: Data): Boolean =
    scheme.verify(data, signature, keyData)
