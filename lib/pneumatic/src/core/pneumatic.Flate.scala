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
package pneumatic

// Shared definitions for the pure-Scala DEFLATE implementation, ported faithfully from JZlib
// (com.jcraft.jzlib, BSD 3-clause, Copyright (c) 2000-2011 ymnk, JCraft, Inc.), itself a port of
// zlib by Jean-loup Gailly and Mark Adler. Because this port is pure Scala, the `Deflate`, `Gzip`
// and `Zlib` formats run on every platform, not just the JVM.
private[pneumatic] object Flate:
  final val ZOk = 0
  final val ZStreamEnd = 1
  final val ZNeedDict = 2
  final val ZStreamError = -2
  final val ZDataError = -3
  final val ZMemError = -4
  final val ZBufError = -5

  final val ZNoFlush = 0
  final val ZPartialFlush = 1
  final val ZSyncFlush = 2
  final val ZFullFlush = 3
  final val ZFinish = 4

  final val MaxWbits = 15
  final val ZDeflated = 8
  final val PresetDict = 0x20

  // And-ing with inflateMask(n) masks the lower n bits.
  val inflateMask: Array[Int] = Array(
    0x00000000, 0x00000001, 0x00000003, 0x00000007, 0x0000000f,
    0x0000001f, 0x0000003f, 0x0000007f, 0x000000ff, 0x000001ff,
    0x000003ff, 0x000007ff, 0x00000fff, 0x00001fff, 0x00003fff,
    0x00007fff, 0x0000ffff)

  val empty: Array[Byte] = new Array[Byte](0)
  val emptyInts: Array[Int] = new Array[Int](0)
  val emptyShorts: Array[Short] = new Array[Short](0)

  def corrupt(message: String): Nothing =
    throw IllegalStateException("the compressed data is corrupt: "+message)

// The engine interfaces the `flate` formats compile against. Each platform supplies a
// `FlateBackend` returning its own implementations: `java.util.zip` on the JVM (native zlib),
// and the pure-Scala port below it everywhere else. The pure implementations are compiled on
// every platform, so the JVM test suite exercises them too.
private[pneumatic] trait DeflateEngine:
  def setInput(buffer: Array[Byte]): Unit
  def setInput(buffer: Array[Byte], offset: Int, length: Int): Unit
  def deflate(target: Array[Byte], offset: Int, space: Int): Int
  def deflate(target: Array[Byte], offset: Int, space: Int, flush: Int): Int
  def finish(): Unit
  def finished: Boolean
  def getBytesRead: Long
  def end(): Unit

private[pneumatic] trait InflateEngine:
  def setInput(buffer: Array[Byte]): Unit
  def setInput(buffer: Array[Byte], offset: Int, length: Int): Unit
  def inflate(target: Array[Byte]): Int
  def inflate(target: Array[Byte], offset: Int, space: Int): Int
  def getRemaining: Int
  def finished: Boolean
  def end(): Unit

// The running checksums of the two zlib framings: Adler-32 for the zlib wrapper and CRC-32 for
// gzip, ported from JZlib's `Adler32` and `CRC32`.
private[pneumatic] trait FlateChecksum:
  def update(buffer: Array[Byte], index: Int, length: Int): Unit
  def reset(): Unit
  def value: Long

private[pneumatic] final class Adler32 extends FlateChecksum:
  private final val Base = 65521 // largest prime smaller than 65536
  private final val NMax = 5552  // largest n with 255n(n+1)/2 + (n+1)(Base-1) <= 2^32-1

  private var s1: Long = 1L
  private var s2: Long = 0L

  def reset(): Unit =
    s1 = 1L
    s2 = 0L

  def value: Long = (s2 << 16) | s1

  def update(buffer: Array[Byte], index0: Int, length: Int): Unit =
    var index = index0

    if length == 1 then
      s1 += buffer(index) & 0xff
      s2 += s1
      s1 %= Base
      s2 %= Base
    else
      var len1 = length/NMax
      val len2 = length%NMax

      while len1 > 0 do
        len1 -= 1
        var k = NMax

        while k > 0 do
          k -= 1
          s1 += buffer(index) & 0xff
          s2 += s1
          index += 1

        s1 %= Base
        s2 %= Base

      var k = len2

      while k > 0 do
        k -= 1
        s1 += buffer(index) & 0xff
        s2 += s1
        index += 1

      s1 %= Base
      s2 %= Base

private[pneumatic] object Crc32:
  val table: Array[Int] =
    val result = new Array[Int](256)
    var n = 0

    while n < 256 do
      var c = n
      var k = 8

      while k > 0 do
        k -= 1
        c = if (c & 1) != 0 then 0xedb88320 ^ (c >>> 1) else c >>> 1

      result(n) = c
      n += 1

    result

private[pneumatic] final class Crc32 extends FlateChecksum:
  private var v: Int = 0

  def update(buffer: Array[Byte], index0: Int, length0: Int): Unit =
    var index = index0
    var length = length0
    var c = ~v

    while length > 0 do
      length -= 1
      c = Crc32.table((c ^ buffer(index)) & 0xff) ^ (c >>> 8)
      index += 1

    v = ~c

  def reset(): Unit = v = 0

  def value: Long = v.toLong & 0xffffffffL
