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
import gossamer.*
import rudiments.*
import vacuous.*

// The `AES_CBC_HMAC_SHA2` authenticated-encryption composites of RFC 7518 §5.2 (after
// draft-mcgrew-aead-aes-cbc-hmac-sha2): AES in CBC mode with PKCS#7 padding, then HMAC over the
// associated data, the IV, the ciphertext and the associated data's length, truncated to half
// its width. They are JOSE's `A128CBC-HS256`, `A192CBC-HS384` and `A256CBC-HS512`.
//
// The size is that of the *whole* key, which is also the width of the SHA-2 hash: the key is
// two halves, the first for HMAC and the second for AES, so `AesCbcHmac[256]` is AES-128 with
// HMAC-SHA-256, and `AesCbcHmac[512]` is AES-256 with HMAC-SHA-512. The "nonce" is CBC's
// sixteen-byte IV, which must be unpredictable as well as unrepeated; draw it at random.
//
// It composes two baseline provider members, `aes` and `hmac`, so it runs under every provider.
// Encrypt-then-MAC is checked in the right order: `unseal` compares the tag, in constant time,
// before any ciphertext is decrypted, so a forgery learns nothing from the padding.
object AesCbcHmac:
  given value: [bits <: 256 | 384 | 512: ValueOf] => (crypto: Crypto) => AesCbcHmac[bits] =
    AesCbcHmac(crypto.aes, crypto.hmac(t"HmacSHA${valueOf[bits]}"), crypto.random)

  private val Transformation: Text = t"AES/CBC/PKCS5Padding"
  private val Block: Int = 16

  private def be64(value: Long): Data =
    Array.tabulate[Byte](8): index => (value >>> (56 - 8*index)).toByte

class AesCbcHmac[bits <: 256 | 384 | 512: ValueOf]
  ( aes: Crypto.SymmetricCipher, mac: Crypto.Mac, random: Crypto.Random )
extends Cipher, Symmetric, Authenticated:
  type Size = bits
  def keySize: bits = valueOf[bits]
  def genKey(): Data = random.bytes(keySize/8)
  def privateToPublic(key: Data): Data = key
  def nonceSize: Int = AesCbcHmac.Block
  def tagSize: Int = keySize/16

  private def tag(macKey: Data, nonce: Data, associated: Data, ciphertext: Data): Data =
    val length = AesCbcHmac.be64(associated.length*8L)
    val prefix: Data = Array.concat(associated, nonce, ciphertext)
    mac.mac(macKey, Array.concat(prefix, length)).keep(tagSize)

  def seal(key: Data, nonce: Data, associated: Data, plaintext: Data): Data =
    val half = keySize/16
    val macKey = key.keep(half)
    val encryptionKey = key.skip(half)

    // The provider frames the IV as the leading block of what it returns.
    val ciphertext =
      aes.encrypt(AesCbcHmac.Transformation, encryptionKey, nonce, plaintext).skip(AesCbcHmac.Block)

    Array.concat(ciphertext, tag(macKey, nonce, associated, ciphertext))

  def unseal(key: Data, nonce: Data, associated: Data, sealedData: Data): Optional[Data] =
    val half = keySize/16

    if key.length != 2*half || nonce.length != AesCbcHmac.Block || sealedData.length < tagSize
    then Unset
    else
      val macKey = key.keep(half)
      val encryptionKey = key.skip(half)
      val ciphertext = sealedData.keep(sealedData.length - tagSize)
      val found = sealedData.skip(sealedData.length - tagSize)

      if !Crypto.constantTimeEquals(tag(macKey, nonce, associated, ciphertext), found) then Unset
      else
        // The tag has verified, so the padding is the sealer's own; a provider that still
        // rejects it has been given a key of a size it does not support.
        try
          aes.decrypt
            ( AesCbcHmac.Transformation,
              encryptionKey,
              AesCbcHmac.Block,
              Array.concat(nonce, ciphertext) )
        catch case error: Exception => Unset
