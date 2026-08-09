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

import scala.caps

import proscenium.compat.*
import vacuous.*

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
  val inflateMask: Array[Int]^{} =
    Array.unsafeFrozen:
      scala.Array(
      0x00000000, 0x00000001, 0x00000003, 0x00000007, 0x0000000f,
      0x0000001f, 0x0000003f, 0x0000007f, 0x000000ff, 0x000001ff,
      0x000003ff, 0x000007ff, 0x00000fff, 0x00001fff, 0x00003fff,
      0x00007fff, 0x0000ffff)

  def empty: scala.Array[Byte]^ = new scala.Array[Byte](0)
  def emptyInts: scala.Array[Int]^ = new scala.Array[Int](0)
  val emptyShorts: Array[Short]^{} = Array.unsafeFrozen(new scala.Array[Short](0))

  def corrupt(message: String): Nothing =
    throw IllegalStateException("the compressed data is corrupt: "+message)

// The engine interfaces the `flate` formats compile against. Each platform supplies a
// `FlateBackend` returning its own implementations: `java.util.zip` on the JVM (native zlib),
// and the pure-Scala port below it everywhere else. The pure implementations are compiled on
// every platform, so the JVM test suite exercises them too.
private[pneumatic] trait DeflateEngine extends caps.Mutable:
  update def setInput(buffer: scala.Array[Byte]^{caps.any.rd}): Unit
  update def setInput(buffer: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit
  update def deflate(target: scala.Array[Byte]^, offset: Int, space: Int): Int
  update def deflate(target: scala.Array[Byte]^, offset: Int, space: Int, flush: Int): Int
  update def finish(): Unit
  def finished: Boolean
  def getBytesRead: Long
  def end(): Unit

private[pneumatic] trait InflateEngine extends caps.Mutable:
  update def setInput(buffer: scala.Array[Byte]^{caps.any.rd}): Unit
  update def setInput(buffer: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit
  update def inflate(target: scala.Array[Byte]^): Int
  update def inflate(target: scala.Array[Byte]^, offset: Int, space: Int): Int
  def getRemaining: Int
  def finished: Boolean
  def end(): Unit

// The running checksums of the two zlib framings: Adler-32 for the zlib wrapper and CRC-32 for
// gzip, ported from JZlib's `Adler32` and `CRC32`.
private[pneumatic] trait FlateChecksum extends caps.Mutable:
  update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index: Int, length: Int): Unit
  update def reset(): Unit
  def value: Long

// `Adler32` and `Crc32` delegate to `corpuscular`'s implementations. The pure-Scala versions
// lived here, ported from JZlib, until they were shared; the JVM backend still supplies its own
// `java.util.zip`-backed `FlateChecksum`, which is faster than any of them.
private[pneumatic] final class Adler32 extends FlateChecksum:
  private val adler: corpuscular.Adler32^ = corpuscular.Adler32()

  update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index: Int, length: Int): Unit =
    adler.update(buffer, index, length)

  update def reset(): Unit = adler.reset()
  def value: Long = adler.value

private[pneumatic] final class Crc32 extends FlateChecksum:
  private val crc: corpuscular.Crc32^ = corpuscular.Crc32()

  update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index: Int, length: Int): Unit =
    crc.update(buffer, index, length)

  update def reset(): Unit = crc.reset()
  def value: Long = crc.value
