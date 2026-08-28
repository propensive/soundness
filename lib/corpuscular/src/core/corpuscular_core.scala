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
package corpuscular

import scala.caps

import anticipation.*

// The standard CRC-32 (polynomial 0xedb88320, reflected), shared by gzip, PNG, ZIP and XZ.
// Three copies of this table and loop existed — in pneumatic, hallucination and zeppelin — and
// this is the one they all delegate to.
// The standard CRC-32 (polynomial 0xedb88320, reflected), shared by gzip, PNG, ZIP and XZ.
// Three copies of this table and loop existed — in pneumatic, hallucination and zeppelin — and
// this is the one they all delegate to.
//
// Each checksum is a `sealed trait ... extends Algorithm`: a marker naming the algorithm in a
// type position, so a value can be digested with `.digest[Crc32]` exactly as with
// `.digest[Md5]`. The running state lives in the companion's `Accumulator`, which implements
// `Digestion` directly — its windowed `append` is the native loop, so a streaming consumer
// feeds a reusable window with no copy. `value` reads the checksum as the integer the
// compressed-container formats write into their trailers; `digest` reads it as the big-endian
// bytes the digest framework expects.
sealed trait Crc32 extends Algorithm:
  type Bits = 32

object Crc32:
  val table: Array[Int]^{} =
    val result = Array.allocate[Int](256)
    var n = 0

    while n < 256 do
      var c = n
      var k = 8
      while k > 0 do
        k -= 1
        c = if (c & 1) != 0 then 0xedb88320 ^ (c >>> 1) else c >>> 1

      result(n) = c
      n += 1

    Array.freeze(result)

  // One-shot over a sequence of segments, as PNG chunks and ZIP entries need.
  def checksum(segments: Data*): Int =
    var crc = 0xffffffff

    segments.foreach: segment =>
      var index = 0

      while index < segment.length do
        crc = table.readable((crc ^ segment.readable(index)) & 0xff) ^ (crc >>> 8)
        index += 1

    crc ^ 0xffffffff

  def apply(): Accumulator^ = Accumulator()

  // The running form, for streaming compressors. Mutable state, so each use makes a fresh one.
  final class Accumulator extends Digestion:
    private var v: Int = 0

    // `Digestion`'s two `append`s delegate to the native `update` loop below, which takes the
    // JVM array directly: `Array.unsafeJvm` is a view, not a copy, so the windowed path stays
    // allocation-free — the property a streaming consumer feeding a reusable window needs.
    update def append(bytes: Data): Unit = update(Array.unsafeJvm(bytes), 0, bytes.length)

    override update def append(array: Array[Byte]^{caps.any.rd}, start: Int, count: Int): Unit =
      update(Array.unsafeJvm(array), start, count)

    // The form pneumatic's `FlateChecksum` drives, kept as the primitive.
    update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index0: Int, length0: Int): Unit =

      var index = index0
      var length = length0
      var c = ~v

      while length > 0 do
        length -= 1
        c = table.readable((c ^ buffer(index)) & 0xff) ^ (c >>> 8)
        index += 1

      v = ~c

    update def reset(): Unit = v = 0
    def value: Long = v.toLong & 0xffffffffL

    update def digest(): Data =
      val v0 = v

      Array(((v0 >>> 24) & 0xff).toByte, ((v0 >>> 16) & 0xff).toByte,
            ((v0 >>> 8) & 0xff).toByte, (v0 & 0xff).toByte)

// CRC-64, in the ECMA-182 form XZ uses (polynomial 0xc96c5795d7870f42, reflected). The JDK has
// no CRC-64, so only the Soundness provider offers it.
sealed trait Crc64 extends Algorithm:
  type Bits = 64

object Crc64:
  val table: Array[Long]^{} =
    val poly = 0xc96c5795d7870f42L
    val result = Array.allocate[Long](256)
    var n = 0

    while n < 256 do
      var c = n.toLong
      var k = 8
      while k > 0 do
        k -= 1
        c = if (c & 1L) != 0 then (c >>> 1) ^ poly else c >>> 1

      result(n) = c
      n += 1

    Array.freeze(result)

  def apply(): Accumulator^ = Accumulator()

  final class Accumulator extends Digestion:
    private var v: Long = -1L

    // `Digestion`'s two `append`s delegate to the native `update` loop below, which takes the
    // JVM array directly: `Array.unsafeJvm` is a view, not a copy, so the windowed path stays
    // allocation-free — the property a streaming consumer feeding a reusable window needs.
    update def append(bytes: Data): Unit = update(Array.unsafeJvm(bytes), 0, bytes.length)

    override update def append(array: Array[Byte]^{caps.any.rd}, start: Int, count: Int): Unit =
      update(Array.unsafeJvm(array), start, count)

    // The form pneumatic's `FlateChecksum` drives, kept as the primitive.
    update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index0: Int, length0: Int): Unit =

      var index = index0
      var length = length0
      var c = v

      while length > 0 do
        length -= 1
        c = table.readable(((c ^ buffer(index)) & 0xff).toInt) ^ (c >>> 8)
        index += 1

      v = c

    update def reset(): Unit = v = -1L
    def value: Long = ~v

    update def digest(): Data =
      val v0 = ~v

      Array(((v0 >>> 56) & 0xff).toByte, ((v0 >>> 48) & 0xff).toByte,
            ((v0 >>> 40) & 0xff).toByte, ((v0 >>> 32) & 0xff).toByte,
            ((v0 >>> 24) & 0xff).toByte, ((v0 >>> 16) & 0xff).toByte,
            ((v0 >>> 8) & 0xff).toByte, (v0 & 0xff).toByte)

// Adler-32, the zlib wrapper's checksum.
sealed trait Adler32 extends Algorithm:
  type Bits = 32

object Adler32:
  private[corpuscular] final val Base = 65521 // largest prime smaller than 65536

  def apply(): Accumulator^ = Accumulator()

  final class Accumulator extends Digestion:
    private var s1: Long = 1L
    private var s2: Long = 0L

    // `Digestion`'s two `append`s delegate to the native `update` loop below, which takes the
    // JVM array directly: `Array.unsafeJvm` is a view, not a copy, so the windowed path stays
    // allocation-free — the property a streaming consumer feeding a reusable window needs.
    update def append(bytes: Data): Unit = update(Array.unsafeJvm(bytes), 0, bytes.length)

    override update def append(array: Array[Byte]^{caps.any.rd}, start: Int, count: Int): Unit =
      update(Array.unsafeJvm(array), start, count)

    // The form pneumatic's `FlateChecksum` drives, kept as the primitive.
    update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index0: Int, length0: Int): Unit =

      var index = index0
      var length = length0

      while length > 0 do
        length -= 1
        s1 = (s1 + (buffer(index) & 0xff)) % Base
        s2 = (s2 + s1) % Base
        index += 1

    update def reset(): Unit =
      s1 = 1L
      s2 = 0L

    def value: Long = (s2 << 16) | s1

    update def digest(): Data =
      val v0 = (s2 << 16) | s1

      Array(((v0 >>> 24) & 0xff).toByte, ((v0 >>> 16) & 0xff).toByte,
            ((v0 >>> 8) & 0xff).toByte, (v0 & 0xff).toByte)
