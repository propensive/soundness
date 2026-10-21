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
import vacuous.*

// ChaCha20 (RFC 8439 §2.3) and ChaCha20-Poly1305 (§2.8) in Scala, for the `SoundnessCrypto`
// provider: the IETF variant with a 256-bit key, a 96-bit nonce and a 32-bit block counter. The
// AEAD derives its one-time Poly1305 key from block zero of the keystream, encrypts from block
// one, and authenticates the associated data and ciphertext each zero-padded to sixteen bytes,
// followed by their lengths as little-endian 64-bit integers; the tag is appended to the
// ciphertext. The arithmetic is additions, rotations and xors on 32-bit words, so it runs in
// constant time with respect to the data.
private[enigmatic] object ChaCha:
  private val KeySize: Int = 32
  private val NonceSize: Int = 12
  private val BlockSize: Int = 64
  private val TagSize: Int = 16

  private def le32(bytes: scala.Array[Byte], offset: Int): Int =
    (bytes(offset) & 0xff) |
      ((bytes(offset + 1) & 0xff) << 8) |
      ((bytes(offset + 2) & 0xff) << 16) |
      ((bytes(offset + 3) & 0xff) << 24)

  private def put32(bytes: scala.Array[Byte]^, offset: Int, value: Int): Unit =
    bytes(offset) = value.toByte
    bytes(offset + 1) = (value >>> 8).toByte
    bytes(offset + 2) = (value >>> 16).toByte
    bytes(offset + 3) = (value >>> 24).toByte

  private def put64(bytes: scala.Array[Byte]^, offset: Int, value: Long): Unit =
    put32(bytes, offset, value.toInt)
    put32(bytes, offset + 4, (value >>> 32).toInt)

  private def quarter(state: scala.Array[Int]^, a: Int, b: Int, c: Int, d: Int): Unit =
    state(a) += state(b); state(d) = Integer.rotateLeft(state(d) ^ state(a), 16)
    state(c) += state(d); state(b) = Integer.rotateLeft(state(b) ^ state(c), 12)
    state(a) += state(b); state(d) = Integer.rotateLeft(state(d) ^ state(a), 8)
    state(c) += state(d); state(b) = Integer.rotateLeft(state(b) ^ state(c), 7)

  // One 64-byte keystream block for the key, counter and nonce, into `output`.
  private def block
    ( key: scala.Array[Byte], counter: Int, nonce: scala.Array[Byte], output: scala.Array[Byte]^ )
  :   Unit =

    // "expand 32-byte k", as four little-endian words, then the key, counter and nonce.
    val initial: scala.Array[Int]^ = Array.scratch[Int](16)
    initial(0) = 0x61707865
    initial(1) = 0x3320646e
    initial(2) = 0x79622d32
    initial(3) = 0x6b206574
    load(initial, key, 0)
    initial(12) = counter
    initial(13) = le32(nonce, 0)
    initial(14) = le32(nonce, 4)
    initial(15) = le32(nonce, 8)

    val state: scala.Array[Int]^ = Array.scratch[Int](16)
    java.lang.System.arraycopy(initial, 0, state, 0, 16)
    rounds(state, 10)
    serialize(state, initial, output, 0)

  // The exclusive arrays are parameters rather than captures: separation checking forbids a
  // local function closing over a fresh exclusive array.
  private def load(initial: scala.Array[Int]^, key: scala.Array[Byte], index: Int): Unit =
    if index < 8 then
      initial(4 + index) = le32(key, index*4)
      load(initial, key, index + 1)

  private def rounds(state: scala.Array[Int]^, remaining: Int): Unit =
    if remaining > 0 then
      quarter(state, 0, 4, 8, 12)
      quarter(state, 1, 5, 9, 13)
      quarter(state, 2, 6, 10, 14)
      quarter(state, 3, 7, 11, 15)
      quarter(state, 0, 5, 10, 15)
      quarter(state, 1, 6, 11, 12)
      quarter(state, 2, 7, 8, 13)
      quarter(state, 3, 4, 9, 14)
      rounds(state, remaining - 1)

  private def serialize
    ( state: scala.Array[Int], initial: scala.Array[Int], output: scala.Array[Byte]^, index: Int )
  :   Unit =

    if index < 16 then
      put32(output, index*4, state(index) + initial(index))
      serialize(state, initial, output, index + 1)

  private def check(key: Data, nonce: Data): Unit =
    if key.length != KeySize then CryptoFailure.invalidKey(s"a ChaCha20 key must be $KeySize bytes")

    if nonce.length != NonceSize
    then CryptoFailure.invalidKey(s"a ChaCha20 nonce must be $NonceSize bytes")

  // The keystream from block `counter` applied to `data`.
  def xor(key: Data, nonce: Data, counter: Int, data: Data): Data =
    check(key, nonce)
    // [java-boundary] read-only views of the key, nonce and data
    val key0 = Array.unsafeJvm(key)
    val nonce0 = Array.unsafeJvm(nonce)
    Array.freeze(keystream(key0, nonce0, counter, Array.unsafeJvm(data)))

  private def keystream
    ( key: scala.Array[Byte], nonce: scala.Array[Byte], counter: Int, data: scala.Array[Byte] )
  :   Array[Byte]^ =

    val output = Array.allocate[Byte](data.length)
    val stream: scala.Array[Byte]^ = Array.scratch[Byte](BlockSize)
    blocks(key, nonce, counter, data, 0, stream, output)
    output

  private def blocks
    ( key:     scala.Array[Byte],
      nonce:   scala.Array[Byte],
      counter: Int,
      data:    scala.Array[Byte],
      offset:  Int,
      stream:  scala.Array[Byte]^,
      output:  Array[Byte]^ )
  :   Unit =

    if offset < data.length then
      block(key, counter, nonce, stream)
      mix(data, offset, stream, output, 0, Math.min(BlockSize, data.length - offset))
      blocks(key, nonce, counter + 1, data, offset + BlockSize, stream, output)

  private def mix
    ( data:   scala.Array[Byte],
      offset: Int,
      stream: scala.Array[Byte],
      output: Array[Byte]^,
      index:  Int,
      count:  Int )
  :   Unit =

    if index < count then
      output(offset + index) = (data(offset + index) ^ stream(index)).toByte
      mix(data, offset, stream, output, index + 1, count)

  // The Poly1305 input of the AEAD construction: the padded associated data and ciphertext and
  // their lengths; and the tag over it under the one-time key from keystream block zero.
  private def tag
    ( key: scala.Array[Byte], nonce: scala.Array[Byte], associated: scala.Array[Byte],
      ciphertext: scala.Array[Byte], count: Int )
  :   Data =

    val otk: scala.Array[Byte]^ = Array.scratch[Byte](BlockSize)
    block(key, 0, nonce, otk)
    val oneTimeKey = Array.allocate[Byte](32)
    java.lang.System.arraycopy(otk, 0, oneTimeKey.raw, 0, 32)

    def padded(length: Int): Int = (length + 15)/16*16

    val message = Array.allocate[Byte](padded(associated.length) + padded(count) + 16)
    java.lang.System.arraycopy(associated, 0, message.raw, 0, associated.length)
    java.lang.System.arraycopy(ciphertext, 0, message.raw, padded(associated.length), count)
    put64(message.raw, padded(associated.length) + padded(count), associated.length.toLong)
    put64(message.raw, padded(associated.length) + padded(count) + 8, count.toLong)

    Poly1305.mac(Array.freeze(oneTimeKey), Array.freeze(message))

  def seal(key: Data, nonce: Data, associated: Data, plaintext: Data): Data =
    check(key, nonce)
    // [java-boundary] read-only views of the key, nonce, associated data and plaintext
    val key0 = Array.unsafeJvm(key)
    val nonce0 = Array.unsafeJvm(nonce)
    val ciphertext = keystream(key0, nonce0, 1, Array.unsafeJvm(plaintext))
    val t = tag(key0, nonce0, Array.unsafeJvm(associated), ciphertext.raw, ciphertext.length)
    val output = Array.allocate[Byte](ciphertext.length + TagSize)
    java.lang.System.arraycopy(ciphertext.raw, 0, output.raw, 0, ciphertext.length)
    output.place(t, 0, ciphertext.length, TagSize)
    Array.freeze(output)

  def unseal(key: Data, nonce: Data, associated: Data, ciphertext: Data): Optional[Data] =
    check(key, nonce)
    if ciphertext.length < TagSize then Unset else
      // [java-boundary] read-only views of the key, nonce, associated data and ciphertext
      val key0 = Array.unsafeJvm(key)
      val nonce0 = Array.unsafeJvm(nonce)
      val input = Array.unsafeJvm(ciphertext)
      val count = input.length - TagSize
      val expected = tag(key0, nonce0, Array.unsafeJvm(associated), input, count)

      // [java-boundary] read-only view of the freshly computed tag
      val verified = Gcm.constantTimeEquals(Array.unsafeJvm(expected), 0, input, count, TagSize)

      if !verified then Unset else
        val body = Array.scratch[Byte](count)
        java.lang.System.arraycopy(input, 0, body, 0, count)
        Array.freeze(keystream(key0, nonce0, 1, body))
