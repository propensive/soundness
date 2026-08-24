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

// The running form, for streaming compressors. Mutable state, so each use makes a fresh one.
final class Crc32 extends caps.Mutable:
  private var v: Int = 0

  update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index0: Int, length0: Int): Unit =
    var index = index0
    var length = length0
    var c = ~v

    while length > 0 do
      length -= 1
      c = Crc32.table.readable((c ^ buffer(index)) & 0xff) ^ (c >>> 8)
      index += 1

    v = ~c

  update def reset(): Unit = v = 0
  def value: Long = v.toLong & 0xffffffffL

// CRC-64, in the ECMA-182 form XZ uses (polynomial 0xc96c5795d7870f42, reflected).
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

final class Crc64 extends caps.Mutable:
  private var v: Long = -1L

  update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index0: Int, length0: Int): Unit =
    var index = index0
    var length = length0
    var c = v

    while length > 0 do
      length -= 1
      c = Crc64.table.readable(((c ^ buffer(index)) & 0xff).toInt) ^ (c >>> 8)
      index += 1

    v = c

  update def reset(): Unit = v = -1L
  def value: Long = ~v

// Adler-32, the zlib wrapper's checksum.
final class Adler32 extends caps.Mutable:
  private var s1: Long = 1L
  private var s2: Long = 0L

  update def reset(): Unit =
    s1 = 1L
    s2 = 0L

  def value: Long = (s2 << 16) | s1

  update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index0: Int, length0: Int): Unit =
    var index = index0
    var length = length0

    while length > 0 do
      length -= 1
      s1 = (s1 + (buffer(index) & 0xff)) % Adler32.Base
      s2 = (s2 + s1) % Adler32.Base
      index += 1

object Adler32:
  private[corpuscular] final val Base = 65521 // largest prime smaller than 65536
