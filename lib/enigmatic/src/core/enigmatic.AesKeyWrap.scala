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
import vacuous.*

// The AES Key Wrap algorithm of RFC 3394 (NIST SP 800-38F's KW), with its default initial
// value: the key management behind JOSE's `A128KW`, `A192KW` and `A256KW`, CMS and PKCS#12.
// The material wrapped must be a whole number of 64-bit blocks, and at least two; the result
// is one block longer. It is six passes of raw AES over the material, so it needs only the
// provider's baseline `aes`, and runs under every provider.
object AesKeyWrap:
  given value: [bits <: 128 | 192 | 256: ValueOf] => (crypto: Crypto) => AesKeyWrap[bits] =
    AesKeyWrap(crypto.aes)

  private val Transformation: Text = t"AES/ECB/NoPadding"

  // RFC 3394 §2.2.3.1: the constant the integrity check looks for.
  private val Initial: Long = 0xa6a6a6a6a6a6a6a6L

  private def be64(bytes: scala.Array[Byte], offset: Int, index: Int = 0, value: Long = 0L): Long =
    if index == 8 then value
    else be64(bytes, offset, index + 1, (value << 8) | (bytes(offset + index) & 0xffL))

  private def put64(bytes: scala.Array[Byte]^, offset: Int, value: Long, index: Int = 0): Unit =
    if index < 8 then
      bytes(offset + index) = (value >>> (56 - index*8)).toByte
      put64(bytes, offset, value, index + 1)

  // One AES block from two 64-bit halves.
  private def block(high: Long, low: Long): Data =
    Array.tabulate[Byte](16): index =>
      if index < 8 then (high >>> (56 - 8*index)).toByte else (low >>> (120 - 8*index)).toByte

  // The exclusive arrays are parameters rather than captures, as in `ChaCha`: separation
  // checking forbids a local function closing over a fresh exclusive array.
  private def load
    ( words: scala.Array[Long]^, input: scala.Array[Byte], index: Int, count: Int, shift: Int )
  :   Unit =
    if index < count then
      words(index + shift) = be64(input, 8*index)
      load(words, input, index + 1, count, shift)

  private def store
    ( words: scala.Array[Long]^, output: scala.Array[Byte]^, index: Int, count: Int, shift: Int )
  :   Unit =
    if index < count then
      put64(output, 8*index, words(index + shift))
      store(words, output, index + 1, count, shift)

  // `words(0)` is the register `A`; `words(1)` to `words(count)` are `R[1]` to `R[n]`. Step
  // `t - 1` of the `6n` works on `R[(t - 1) mod n + 1]`.
  private def wrapSteps
    ( aes: Crypto.SymmetricCipher, key: Data, words: scala.Array[Long]^, count: Int, step: Int )
  :   Unit =
    if step < 6*count then
      val index = step%count + 1
      val input = block(words(0), words(index))
      val output = Array.unsafeJvm(aes.encrypt(Transformation, key, Unset, input))
      words(0) = be64(output, 0) ^ (step + 1).toLong
      words(index) = be64(output, 8)
      wrapSteps(aes, key, words, count, step + 1)

  private def unwrapSteps
    ( aes: Crypto.SymmetricCipher, key: Data, words: scala.Array[Long]^, count: Int, step: Int )
  :   Unit =
    if step >= 0 then
      val index = step%count + 1
      val input = block(words(0) ^ (step + 1).toLong, words(index))
      val output = Array.unsafeJvm(aes.decrypt(Transformation, key, Unset, input))
      words(0) = be64(output, 0)
      words(index) = be64(output, 8)
      unwrapSteps(aes, key, words, count, step - 1)

  private[enigmatic] def wrap(aes: Crypto.SymmetricCipher, key: Data, material: Data): Data =
    val count = material.length/8
    val words: scala.Array[Long]^ = Array.scratch[Long](count + 1)
    words(0) = Initial
    load(words, Array.unsafeJvm(material), 0, count, 1)
    wrapSteps(aes, key, words, count, 0)
    val output = Array.allocate[Byte](8*(count + 1))
    store(words, output.raw, 0, count + 1, 0)
    Array.freeze(output)

  private[enigmatic] def unwrap(aes: Crypto.SymmetricCipher, key: Data, wrapped: Data)
  :   Optional[Data] =
    if wrapped.length%8 != 0 || wrapped.length < 24 then Unset else
      val count = wrapped.length/8 - 1
      val words: scala.Array[Long]^ = Array.scratch[Long](count + 1)
      load(words, Array.unsafeJvm(wrapped), 0, count + 1, 0)
      unwrapSteps(aes, key, words, count, 6*count - 1)

      // The comparison is of one machine word, so it takes the same time whatever the word is.
      if words(0) != Initial then Unset else
        val output = Array.allocate[Byte](8*count)
        store(words, output.raw, 0, count, 1)
        Array.freeze(output)

class AesKeyWrap[bits <: 128 | 192 | 256: ValueOf](aes: Crypto.SymmetricCipher)
extends Cipher, Symmetric, Wrapping:
  type Size = bits
  def keySize: bits = valueOf[bits]
  def genKey(): Data = aes.generateKey(keySize)
  def privateToPublic(key: Data): Data = key
  def wrap(key: Data, material: Data): Data = AesKeyWrap.wrap(aes, key, material)

  def unwrap(key: Data, wrapped: Data): Optional[Data] =
    if key.length*8 != keySize then Unset else AesKeyWrap.unwrap(aes, key, wrapped)
