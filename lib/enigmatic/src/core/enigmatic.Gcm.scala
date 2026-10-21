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

// AES-GCM (NIST SP 800-38D) over `Rijndael`, for the `SoundnessCrypto` provider: a 96-bit nonce
// (any other length is hashed into the initial counter block as the standard allows), a 128-bit
// tag appended to the ciphertext, and associated data authenticated but not encrypted. GHASH is
// the bit-by-bit multiplication in GF(2^128) of the standard's Algorithm 1 over two 64-bit halves
// — simple and table-free, so it leaks nothing through cache timing, at the cost of speed.
private[enigmatic] object Gcm:
  private val Block: Int = 16
  private val Tag: Int = 16
  private val R: Long = 0xe100000000000000L

  private def be64(bytes: scala.Array[Byte], offset: Int, index: Int = 0, value: Long = 0L): Long =
    if index == 8 then value
    else be64(bytes, offset, index + 1, (value << 8) | (bytes(offset + index) & 0xffL))

  private def put64(bytes: scala.Array[Byte]^, offset: Int, value: Long, index: Int = 0): Unit =
    if index < 8 then
      bytes(offset + index) = (value >>> (56 - index*8)).toByte
      put64(bytes, offset, value, index + 1)

  // The hash subkey `H = E(0^128)`, as (high, low) halves.
  private final class Subkey(cipher: Rijndael):
    private val halves: (Long, Long) =
      val zero: scala.Array[Byte]^ = Array.scratch[Byte](Block)
      val h: scala.Array[Byte]^ = Array.scratch[Byte](Block)
      cipher.encrypt(zero, 0, h, 0)
      (be64(h, 0), be64(h, 8))

    val h0: Long = halves(0)
    val h1: Long = halves(1)

  private object Ghash:
    val Empty: Ghash = Ghash(0L, 0L)

  // The GHASH state, `Y`, as (high, low) halves — immutable, each absorption yielding the next.
  private final case class Ghash(y0: Long, y1: Long):
    // `Y = (Y xor block) · H`, with the product by the standard's right-shifting Algorithm 1: for
    // each bit of `X` from the most significant, add `V` when set, then halve `V` modulo the
    // field polynomial.
    private def multiply(x0: Long, x1: Long, bit: Int, v0: Long, v1: Long, z0: Long, z1: Long)
    :   Ghash =

      if bit == 128 then Ghash(z0, z1) else
        val set = if bit < 64 then (x0 >>> (63 - bit)) & 1L else (x1 >>> (127 - bit)) & 1L
        val mask = -set
        val w0 = z0 ^ (v0 & mask)
        val w1 = z1 ^ (v1 & mask)
        val carry = -(v1 & 1L)
        val u1 = (v1 >>> 1) | (v0 << 63)
        val u0 = (v0 >>> 1) ^ (R & carry)
        multiply(x0, x1, bit + 1, u0, u1, w0, w1)

    def absorb(subkey: Subkey, x0: Long, x1: Long): Ghash =
      multiply(y0 ^ x0, y1 ^ x1, 0, subkey.h0, subkey.h1, 0L, 0L)

    def absorb(subkey: Subkey, bytes: scala.Array[Byte], offset: Int, count: Int): Ghash =
      val block: scala.Array[Byte]^ = Array.scratch[Byte](Block)
      java.lang.System.arraycopy(bytes, offset, block, 0, count)
      absorb(subkey, be64(block, 0), be64(block, 8))

    // Whole blocks then the zero-padded remainder of `count` bytes of `bytes`.
    def absorbAll(subkey: Subkey, bytes: scala.Array[Byte], offset: Int, count: Int): Ghash =
      if count - offset >= Block
      then absorb(subkey, bytes, offset, Block).absorbAll(subkey, bytes, offset + Block, count)
      else if count - offset > 0 then absorb(subkey, bytes, offset, count - offset)
      else this

    def lengths(subkey: Subkey, associated: Long, ciphertext: Long): Ghash =
      absorb(subkey, associated*8, ciphertext*8)

    def state(target: scala.Array[Byte]^): Unit =
      put64(target, 0, y0)
      put64(target, 8, y1)

  // The initial counter block: the nonce with a 32-bit counter of 1 when it is 96 bits, else
  // its GHASH with its bit length.
  private def initial(cipher: Rijndael, nonce: scala.Array[Byte]): scala.Array[Byte]^ =
    val j0: scala.Array[Byte]^ = Array.scratch[Byte](Block)

    if nonce.length == 12 then
      java.lang.System.arraycopy(nonce, 0, j0, 0, 12)
      j0(15) = 1
    else
      val subkey = Subkey(cipher)
      Ghash.Empty.absorbAll(subkey, nonce, 0, nonce.length).lengths(subkey, 0L, nonce.length.toLong)
      . state(j0)

    j0

  // The 32-bit big-endian increment of a counter block's last word.
  private def increment(counter: scala.Array[Byte]^, index: Int): Unit =
    if index >= 12 then
      val next = ((counter(index) & 0xff) + 1).toByte
      counter(index) = next
      if next == 0 then increment(counter, index - 1)

  // CTR over the message from `inc32(J0)`, into a fresh frozen array.
  private def counterMode(cipher: Rijndael, j0: scala.Array[Byte], input: scala.Array[Byte])
  :   Array[Byte]^ =

    val counter: scala.Array[Byte]^ = Array.scratch[Byte](Block)
    java.lang.System.arraycopy(j0, 0, counter, 0, Block)
    val keystream: scala.Array[Byte]^ = Array.scratch[Byte](Block)
    val output = Array.allocate[Byte](input.length)
    counterBlocks(cipher, counter, keystream, input, output, 0)
    output

  private def counterBlocks
    ( cipher:    Rijndael,
      counter:   scala.Array[Byte]^,
      keystream: scala.Array[Byte]^,
      input:     scala.Array[Byte],
      output:    Array[Byte]^,
      offset:    Int )
  :   Unit =

    if offset < input.length then
      increment(counter, Block - 1)
      cipher.encrypt(counter, 0, keystream, 0)
      xor(input, keystream, output, offset, 0, Math.min(Block, input.length - offset))
      counterBlocks(cipher, counter, keystream, input, output, offset + Block)

  private def xor
    ( input:     scala.Array[Byte],
      keystream: scala.Array[Byte],
      output:    Array[Byte]^,
      offset:    Int,
      index:     Int,
      count:     Int )
  :   Unit =

    if index < count then
      output(offset + index) = (input(offset + index) ^ keystream(index)).toByte
      xor(input, keystream, output, offset, index + 1, count)

  private def tag
    ( cipher:     Rijndael,
      j0:         scala.Array[Byte],
      associated: scala.Array[Byte],
      ciphertext: scala.Array[Byte],
      count:      Int )
  :   scala.Array[Byte]^ =

    val subkey = Subkey(cipher)

    val ghash =
      Ghash.Empty.absorbAll(subkey, associated, 0, associated.length)
      . absorbAll(subkey, ciphertext, 0, count)
      . lengths(subkey, associated.length.toLong, count.toLong)

    val s: scala.Array[Byte]^ = Array.scratch[Byte](Block)
    ghash.state(s)
    val masked: scala.Array[Byte]^ = Array.scratch[Byte](Block)
    cipher.encrypt(j0, 0, masked, 0)
    mask(s, masked, 0)
    s

  private def mask(s: scala.Array[Byte]^, masked: scala.Array[Byte], index: Int): Unit =
    if index < Block then
      s(index) = (s(index) ^ masked(index)).toByte
      mask(s, masked, index + 1)

  def seal(key: Data, nonce: Data, associated: Data, plaintext: Data): Data =
    val cipher = Rijndael(key)
    // [java-boundary] read-only views, never written
    val nonce0 = Array.unsafeJvm(nonce)
    val j0 = initial(cipher, nonce0)
    // [java-boundary] read-only view of the plaintext
    val ciphertext = counterMode(cipher, j0, Array.unsafeJvm(plaintext))
    // [java-boundary] read-only view of the associated data
    val t = tag(cipher, j0, Array.unsafeJvm(associated), ciphertext.raw, ciphertext.length)
    val output = Array.allocate[Byte](ciphertext.length + Tag)
    java.lang.System.arraycopy(ciphertext.raw, 0, output.raw, 0, ciphertext.length)
    java.lang.System.arraycopy(t, 0, output.raw, ciphertext.length, Tag)
    Array.freeze(output)

  def unseal(key: Data, nonce: Data, associated: Data, ciphertext: Data): Optional[Data] =
    if ciphertext.length < Tag then Unset else
      val cipher = Rijndael(key)
      // [java-boundary] read-only views, never written
      val nonce0 = Array.unsafeJvm(nonce)
      val input = Array.unsafeJvm(ciphertext)
      val count = input.length - Tag
      val j0 = initial(cipher, nonce0)
      // [java-boundary] read-only view of the associated data
      val expected = tag(cipher, j0, Array.unsafeJvm(associated), input, count)

      if !constantTimeEquals(expected, 0, input, count, Tag) then Unset else
        val body: scala.Array[Byte]^ = Array.scratch[Byte](count)
        java.lang.System.arraycopy(input, 0, body, 0, count)
        Array.freeze(counterMode(cipher, j0, body))

  // Equality of two byte ranges in time independent of where they differ.
  private[enigmatic] def constantTimeEquals
    ( left:        scala.Array[Byte],
      leftOffset:  Int,
      right:       scala.Array[Byte],
      rightOffset: Int,
      count:       Int )
  :   Boolean =

    def recur(index: Int, difference: Int): Int =
      if index == count then difference
      else recur(index + 1, difference | (left(leftOffset + index) ^ right(rightOffset + index)))

    recur(0, 0) == 0
