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
import gastronomy.*
import gossamer.*
import vacuous.*

// The pure-Scala `Crypto` provider: the algorithms enigmatic's typed API needs, implemented in
// Scala over gastronomy's hashes, so that cryptography runs wherever Scala does — Scala.js, WASI
// and Scala Native (without OpenSSL) as well as the JVM. Selected, with the pure hashes, by
// `import providers.soundnessProvider`; on Scala.js and Scala Native it is also what the
// `JavaBaseCrypto` twins forward to, so `import providers.javaBaseProvider` works there unchanged.
//
// It offers AES in the five block modes with the three paddings (streaming included), AES-GCM,
// HMAC over every gastronomy hash, ChaCha20 and ChaCha20-Poly1305, X25519 and Ed25519 — the
// primitives SSH, TLS 1.3 and the rest of the modern internet are built on. It does not offer
// RSA, DSA, ECDSA, ML-DSA or the legacy block ciphers; those remain the JDK's and OpenSSL's, and
// since the asymmetric ones other than RSA are structural members of `Crypto`, asking this
// provider for them is a compile-time error.
//
// Caveat: this is portable, reviewable code, not a hardened library. It has had no independent
// audit, its AES is a byte-oriented software implementation (so not immune to cache timing), and
// its Curve25519 arithmetic uses `BigInt`, whose operations are not constant-time. Where a
// platform offers a native provider — the JDK's or OpenSSL — prefer it for keys an adversary can
// time; this one exists so that the browser and WASI are not left with nothing.
object SoundnessCrypto extends Crypto:
  def random: Crypto.Random = new Crypto.Random:
    def bytes(size: Int): Data = Entropy.bytes(size)

  def aes: Crypto.SymmetricCipher = SoundnessBlockCipher.aes

  // RFC 2104 over the named hash: the key is hashed down when it is longer than the block, and
  // padded with zeros to it; the block sizes are the standards' (64 bytes for the 32-bit-word
  // hashes and BLAKE3, 128 for SHA-384 and SHA-512).
  def hmac(algorithm: Text): Crypto.Mac =
    val selection: (Hashing.Function, Int) =
      if algorithm == t"HmacMD5" then (SoundnessHashing.md5, 64)
      else if algorithm == t"HmacSHA1" then (SoundnessHashing.sha1, 64)
      else if algorithm == t"HmacSHA224" then (SoundnessHashing.sha2(224), 64)
      else if algorithm == t"HmacSHA256" then (SoundnessHashing.sha2(256), 64)
      else if algorithm == t"HmacSHA384" then (SoundnessHashing.sha2(384), 128)
      else if algorithm == t"HmacSHA512" then (SoundnessHashing.sha2(512), 128)
      else if algorithm == t"HMAC-BLAKE3" then (SoundnessHashing.blake3, 64)
      else CryptoFailure.invalidAlgorithm(s"no HMAC over $algorithm")

    val function: Hashing.Function = selection(0)
    val blockSize: Int = selection(1)

    def digest(a: Data): Data =
      val digestion = function.digestion()
      digestion.append(a)
      digestion.digest()

    def digestPair(a: Data, b: Data): Data =
      val digestion = function.digestion()
      digestion.append(a)
      digestion.append(b)
      digestion.digest()

    new Crypto.Mac:
      def mac(key: Data, data: Data): Data =
        val shortened = if key.length > blockSize then digest(key) else key
        val inner = Array.allocate[Byte](blockSize)
        val outer = Array.allocate[Byte](blockSize)
        inner.place(shortened, 0, 0, shortened.length)
        outer.place(shortened, 0, 0, shortened.length)

        SoundnessCrypto.pad(inner, outer, blockSize, 0)
        digestPair(Array.freeze(outer), digestPair(Array.freeze(inner), data))

  private def pad(inner: Array[Byte]^, outer: Array[Byte]^, blockSize: Int, index: Int): Unit =
    if index < blockSize then
      inner(index) = (inner(index) ^ 0x36).toByte
      outer(index) = (outer(index) ^ 0x5c).toByte
      pad(inner, outer, blockSize, index + 1)

  private def unavailable(algorithm: String): Nothing =
    panic(m"$algorithm is not provided by SoundnessCrypto; select the JDK or OpenSSL provider")

  def rsa: Crypto.PublicKeyCipher = unavailable("RSA")
  def rsaSignature(digest: Text): Crypto.SignatureScheme = unavailable("RSA")

  def aesGcm: Crypto.Aead = new Crypto.Aead:
    def generateKey(bits: Int): Data = Entropy.bytes(bits/8)

    def seal(key: Data, nonce: Data, associated: Data, plaintext: Data): Data =
      Gcm.seal(key, nonce, associated, plaintext)

    def unseal(key: Data, nonce: Data, associated: Data, ciphertext: Data): Optional[Data] =
      Gcm.unseal(key, nonce, associated, ciphertext)

  def chaCha20: Crypto.StreamCipher = new Crypto.StreamCipher:
    def generateKey(bits: Int): Data = Entropy.bytes(32)

    def xor(key: Data, nonce: Data, counter: Int, data: Data): Data =
      ChaCha.xor(key, nonce, counter, data)

  def chaCha20Poly1305: Crypto.Aead = new Crypto.Aead:
    def generateKey(bits: Int): Data = Entropy.bytes(32)

    def seal(key: Data, nonce: Data, associated: Data, plaintext: Data): Data =
      ChaCha.seal(key, nonce, associated, plaintext)

    def unseal(key: Data, nonce: Data, associated: Data, ciphertext: Data): Optional[Data] =
      ChaCha.unseal(key, nonce, associated, ciphertext)

  def x25519: Crypto.KeyAgreement = Curve25519.x25519Agreement
  def ed25519: Crypto.SignatureScheme = Curve25519.ed25519Scheme
