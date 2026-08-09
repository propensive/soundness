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

import java.util.zip as juz

import scala.caps

// The JVM backend for the `Deflate`/`Gzip`/`Zlib` formats: thin adapters over `java.util.zip`,
// whose native zlib and CRC-32 intrinsics outperform any pure implementation. The pure-Scala
// engines in `core` remain compiled (and tested) on the JVM; they are simply not selected here.
private[pneumatic] object FlateBackend:
  def deflater(level: Int, nowrap: Boolean): DeflateEngine^ = JavaDeflateEngine(level, nowrap)
  def inflater(nowrap: Boolean): InflateEngine^ = JavaInflateEngine(nowrap)
  def crc32(): FlateChecksum^ = JavaCrc32()

private final class JavaDeflateEngine(level: Int, nowrap: Boolean) extends DeflateEngine:
  private val deflater: juz.Deflater = juz.Deflater(level, nowrap)

  update def setInput(buffer: scala.Array[Byte]^{caps.any.rd}): Unit =
    deflater.setInput(buffer.asInstanceOf[scala.Array[Byte]])

  update def setInput(buffer: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    deflater.setInput(buffer.asInstanceOf[scala.Array[Byte]], offset, length)

  update def deflate(target: scala.Array[Byte]^, offset: Int, space: Int): Int =
    deflater.deflate(target.asInstanceOf[scala.Array[Byte]], offset, space)

  update def deflate(target: scala.Array[Byte]^, offset: Int, space: Int, flush: Int): Int =
    val flushMode =
      if flush == Flate.ZSyncFlush then juz.Deflater.SYNC_FLUSH else juz.Deflater.NO_FLUSH

    deflater.deflate(target.asInstanceOf[scala.Array[Byte]], offset, space, flushMode)

  update def finish(): Unit = deflater.finish()
  def finished: Boolean = deflater.finished
  def getBytesRead: Long = deflater.getBytesRead
  def end(): Unit = deflater.end()

private final class JavaInflateEngine(nowrap: Boolean) extends InflateEngine:
  private val inflater: juz.Inflater = juz.Inflater(nowrap)

  update def setInput(buffer: scala.Array[Byte]^{caps.any.rd}): Unit =
    inflater.setInput(buffer.asInstanceOf[scala.Array[Byte]])

  update def setInput(buffer: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    inflater.setInput(buffer.asInstanceOf[scala.Array[Byte]], offset, length)

  update def inflate(target: scala.Array[Byte]^): Int = inflate(target, 0, target.length)

  update def inflate(target: scala.Array[Byte]^, offset: Int, space: Int): Int =
    try inflater.inflate(target.asInstanceOf[scala.Array[Byte]], offset, space)
    catch case error: juz.DataFormatException => throw IllegalStateException(error)

  def getRemaining: Int = inflater.getRemaining
  def finished: Boolean = inflater.finished
  def end(): Unit = inflater.end()

private final class JavaCrc32 extends FlateChecksum:
  private val crc: juz.CRC32 = juz.CRC32()

  update def update(buffer: scala.Array[Byte]^{caps.any.rd}, index: Int, length: Int): Unit =
    crc.update(buffer.asInstanceOf[scala.Array[Byte]], index, length)

  update def reset(): Unit = crc.reset()
  def value: Long = crc.getValue
