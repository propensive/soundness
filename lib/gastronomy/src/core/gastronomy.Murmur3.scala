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
package gastronomy

import java.lang as jl

import scala.annotation.tailrec
import scala.caps
import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import corpuscular.*
import gossamer.*
import prepositional.*
import proscenium.*

// MurmurHash3, the 128-bit x64 variant: the fast non-cryptographic hash behind Guava's Bloom
// filters and hash-keyed structures everywhere, for the places where the input is not adversarial
// and a cryptographic digest's rounds would be wasted. The output is the two 64-bit words, h₁
// then h₂, each little-endian, which is byte-for-byte what the reference implementation and
// Guava's `murmur3_128()` produce.
object Murmur3:
  given hash: (hashing: Hashing { def murmur3: Hashing.Function }) => Hash in Murmur3 =
    Hash(t"Murmur3-128", t"HMAC-Murmur3-128", hashing.murmur3)

  private val C1: Long = 0x87c37b91114253d5L
  private val C2: Long = 0x4cf5ad432745937fL

  private inline def mix1(k: Long): Long = jl.Long.rotateLeft(k*C1, 31)*C2
  private inline def mix2(k: Long): Long = jl.Long.rotateLeft(k*C2, 33)*C1

  private inline def fmix(k0: Long): Long =
    var k = k0
    k ^= k >>> 33
    k *= 0xff51afd7ed558ccdL
    k ^= k >>> 33
    k *= 0xc4ceb9fe1a85ec53L
    k ^ (k >>> 33)

  // A little-endian 64-bit word of a byte array, assembled by hand so the same code serves every
  // platform; the JIT recognises the shape.
  private inline def word(bytes: scala.Array[Byte]^{caps.any.rd}, offset: Int): Long =
    (bytes(offset) & 0xffL) |
      ((bytes(offset + 1) & 0xffL) << 8) |
      ((bytes(offset + 2) & 0xffL) << 16) |
      ((bytes(offset + 3) & 0xffL) << 24) |
      ((bytes(offset + 4) & 0xffL) << 32) |
      ((bytes(offset + 5) & 0xffL) << 40) |
      ((bytes(offset + 6) & 0xffL) << 48) |
      ((bytes(offset + 7) & 0xffL) << 56)

  def digestion(seed: Long = 0L): Digestion^ = Hasher(seed)

  // The running state is two words and a sixteen-byte block the digestion owns, filled by
  // `append` and consumed as it fills; nothing is allocated until `digest` writes the result
  // into fresh scratch and freezes it. Mutable until frozen, like the filters it serves.
  private final class Hasher(seed: Long) extends Digestion:
    private var h1: Long = seed
    private var h2: Long = seed
    private var length: Long = 0L
    private val block: scala.Array[Byte]^ = new scala.Array[Byte](16)
    private var filled: Int = 0

    update def append(bytes: Data): Unit = update(Array.unsafeJvm(bytes), 0, bytes.length)

    override update def append(array: Array[Byte]^{caps.any.rd}, start: Int, count: Int): Unit =
      update(Array.unsafeJvm(array), start, count)

    private update def absorb(k1: Long, k2: Long): Unit =
      h1 ^= mix1(k1)
      h1 = (jl.Long.rotateLeft(h1, 27) + h2)*5 + 0x52dce729L
      h2 ^= mix2(k2)
      h2 = (jl.Long.rotateLeft(h2, 31) + h1)*5 + 0x38495ab5L

    // Whole blocks are absorbed straight from the caller's array; only a partial block at either
    // end passes through `block`, which the loops take as a parameter rather than capture, since
    // a local function may not close over an exclusive array.
    update def update(buffer: scala.Array[Byte]^{caps.any.rd}, start: Int, count: Int): Unit =
      length += count
      val end = start + count

      if filled == 0 then tail(block, buffer, blocks(buffer, start, end), end)
      else
        val needed = (16 - filled).min(count)
        System.arraycopy(buffer, start, block, filled, needed)
        filled += needed

        if filled == 16 then
          absorb(word(block, 0), word(block, 8))
          filled = 0
          tail(block, buffer, blocks(buffer, start + needed, end), end)
        else
          tail(block, buffer, start + needed, end)

    @tailrec
    private update def blocks(buffer: scala.Array[Byte]^{caps.any.rd}, index: Int, end: Int): Int =
      if end - index >= 16 then
        absorb(word(buffer, index), word(buffer, index + 8))
        blocks(buffer, index + 16, end)
      else
        index

    @tailrec
    private update def tail
      ( target: scala.Array[Byte]^, buffer: scala.Array[Byte]^{caps.any.rd}, index: Int, end: Int )
    :   Unit =

      if index < end then
        target(filled) = buffer(index)
        filled += 1

        if filled == 16 then
          absorb(word(target, 0), word(target, 8))
          filled = 0

        tail(target, buffer, index + 1, end)

    // The bytes of the partial final block from `from` up to `until`, as one little-endian word.
    @tailrec
    private def gather(bytes: scala.Array[Byte]^{caps.any.rd}, from: Int, until: Int, k: Long)
    :   Long =

      if until > from then gather(bytes, from, until - 1, (k << 8) | (bytes(until - 1) & 0xffL))
      else k

    @tailrec
    private def write(target: scala.Array[Byte]^, index: Int, h1: Long, h2: Long): Unit =
      if index < 8 then
        target(index) = (h1 >>> (8*index)).toByte
        target(index + 8) = (h2 >>> (8*index)).toByte
        write(target, index + 1, h1, h2)

    update def digest(): Data =
      if filled > 8 then h2 ^= mix2(gather(block, 8, filled, 0L))
      if filled > 0 then h1 ^= mix1(gather(block, 0, filled.min(8), 0L))

      h1 ^= length
      h2 ^= length
      h1 += h2
      h2 += h1
      h1 = fmix(h1)
      h2 = fmix(h2)
      h1 += h2
      h2 += h1

      val result = Array.allocate[Byte](16)
      write(result.raw, 0, h1, h2)
      Array.freeze(result)

sealed trait Murmur3 extends Algorithm:
  type Bits = 128
