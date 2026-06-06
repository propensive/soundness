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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import vacuous.*

// A pluggable cryptographic provider: it supplies the raw algorithmic
// implementations (the JDK's JCE, BouncyCastle, OpenSSL, …) that the typed
// enigmatic API delegates to. Pick one with an explicit import, e.g.
// `import cryptoProviders.javaStdlibCrypto`.
//
// Every provider must implement the mandatory baseline below — the modern,
// universally-supported primitives. Legacy or provider-specific algorithms (DES,
// Blowfish, RC2, DSA, …) are contributed as additional members and reached
// through a structural refinement, so an algorithm a provider does not offer is a
// compile error at the use site rather than a runtime failure.
//
// All inputs and outputs are byte arrays (`Data`) and string descriptors
// (`Text`); no platform crypto types appear in this contract. Key material
// follows JCE-compatible encodings: symmetric keys are raw bytes; asymmetric
// private keys are PKCS#8, public keys X.509 (SubjectPublicKeyInfo); a symmetric
// ciphertext is `iv ++ rawCiphertext` when the mode uses an IV.

trait Crypto:
  def random: Crypto.Random
  def aes: Crypto.SymmetricCipher
  def rsa: Crypto.PublicKeyCipher
  def hmac(algorithm: Text): Crypto.Mac

object Crypto:
  // A symmetric block cipher. `transformation` is the full JCE-style spec
  // (e.g. `t"AES/CBC/PKCS5Padding"`); `encrypt`/`decrypt` frame the IV as the
  // leading block of the ciphertext when one is supplied.
  trait SymmetricCipher:
    def encrypt(transformation: Text, key: Data, iv: Optional[Data], data: Data): Data
    def decrypt(transformation: Text, key: Data, ivSize: Optional[Int], data: Data): Data
    def blockSize(transformation: Text): Int
    def generateKey(bits: Int): Data
    def stream(transformation: Text, key: Data, iv: Optional[Data]): CipherSession

  trait PublicKeyCipher:
    def encrypt(input: Data, publicKey: Data): Data
    def decrypt(input: Data, privateKey: Data): Data
    def generateKeyPair(bits: Int): Data
    def privateToPublic(privateKey: Data): Data

  trait SignatureScheme:
    def sign(data: Data, privateKey: Data): Data
    def verify(data: Data, signature: Data, publicKey: Data): Boolean
    def generateKeyPair(bits: Int): Data
    def privateToPublic(privateKey: Data): Data

  trait Mac:
    def mac(key: Data, data: Data): Data

  trait Random:
    def bytes(size: Int): Data
