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
import fulminate.*

// The Scala.js twin of the JVM `JavaBaseCrypto` provider. `javax.crypto` does not exist on
// Scala.js, so the algorithms `SoundnessCrypto` implements in Scala are forwarded to it — which
// makes `import providers.javaBaseProvider` work unchanged for AES, HMAC, AES-GCM, ChaCha20,
// X25519 and Ed25519 — and the rest panic: the object exists so that the platform-neutral
// `Crypto.javaBaseCrypto` given (and code naming the provider, like `OpensslCrypto.rsa`'s
// delegation) compiles unchanged, and selecting one of those algorithms *and using it* is the error.
object JavaBaseCrypto extends Crypto:
  private def unavailable(algorithm: String): Nothing =
    panic(m"the Java standard library's $algorithm is unavailable on Scala.js, and SoundnessCrypto does not provide it")

  def random: Crypto.Random = SoundnessCrypto.random
  def aes: Crypto.SymmetricCipher = SoundnessCrypto.aes
  def hmac(algorithm: Text): Crypto.Mac = SoundnessCrypto.hmac(algorithm)
  def rsa: Crypto.PublicKeyCipher = unavailable("RSA")
  def rsaSignature(digest: Text): Crypto.SignatureScheme = unavailable("RSA")

  def aesGcm: Crypto.Aead = SoundnessCrypto.aesGcm
  def chaCha20: Crypto.StreamCipher = SoundnessCrypto.chaCha20
  def chaCha20Poly1305: Crypto.Aead = SoundnessCrypto.chaCha20Poly1305
  def x25519: Crypto.KeyAgreement = SoundnessCrypto.x25519
  def ed25519: Crypto.SignatureScheme = SoundnessCrypto.ed25519

  // Structural members exist here only where something references them — `OpensslCrypto`
  // delegates its `ecdsa` to this object, so the stub must carry it (as `dsa`, which nothing
  // delegates, need not be).
  def ecdsa(digest: Text): Crypto.SignatureScheme = unavailable("ECDSA")

  def des: Crypto.SymmetricCipher = unavailable("DES")
  def tripleDes: Crypto.SymmetricCipher = unavailable("Triple DES")
  def blowfish: Crypto.SymmetricCipher = unavailable("Blowfish")
  def rc2: Crypto.SymmetricCipher = unavailable("RC2")
