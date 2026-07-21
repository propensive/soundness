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

// The JVM backend for the `Deflate`/`Gzip`/`Zlib` formats: thin adapters over `java.util.zip`,
// whose native zlib and CRC-32 intrinsics outperform any pure implementation. The pure-Scala
// engines in `core` remain compiled (and tested) on the JVM; they are simply not selected here.
private[pneumatic] object FlateBackend:
  def deflater(level: Int, nowrap: Boolean): DeflateEngine = JavaDeflateEngine(level, nowrap)
  def inflater(nowrap: Boolean): InflateEngine = JavaInflateEngine(nowrap)
  def crc32(): FlateChecksum = JavaCrc32()

private final class JavaDeflateEngine(level: Int, nowrap: Boolean) extends DeflateEngine:
  private val deflater: juz.Deflater = juz.Deflater(level, nowrap)

  def setInput(buffer: Array[Byte]): Unit = deflater.setInput(buffer)

  def setInput(buffer: Array[Byte], offset: Int, length: Int): Unit =
    deflater.setInput(buffer, offset, length)

  def deflate(target: Array[Byte], offset: Int, space: Int): Int =
    deflater.deflate(target, offset, space)

  def deflate(target: Array[Byte], offset: Int, space: Int, flush: Int): Int =
    val flushMode =
      if flush == Flate.ZSyncFlush then juz.Deflater.SYNC_FLUSH else juz.Deflater.NO_FLUSH

    deflater.deflate(target, offset, space, flushMode)

  def finish(): Unit = deflater.finish()
  def finished: Boolean = deflater.finished
  def getBytesRead: Long = deflater.getBytesRead
  def end(): Unit = deflater.end()

private final class JavaInflateEngine(nowrap: Boolean) extends InflateEngine:
  private val inflater: juz.Inflater = juz.Inflater(nowrap)

  def setInput(buffer: Array[Byte]): Unit = inflater.setInput(buffer)

  def setInput(buffer: Array[Byte], offset: Int, length: Int): Unit =
    inflater.setInput(buffer, offset, length)

  def inflate(target: Array[Byte]): Int = inflate(target, 0, target.length)

  def inflate(target: Array[Byte], offset: Int, space: Int): Int =
    try inflater.inflate(target, offset, space)
    catch case error: juz.DataFormatException => throw IllegalStateException(error)

  def getRemaining: Int = inflater.getRemaining
  def finished: Boolean = inflater.finished
  def end(): Unit = inflater.end()

private final class JavaCrc32 extends FlateChecksum:
  private val crc: juz.CRC32 = juz.CRC32()

  def update(buffer: Array[Byte], index: Int, length: Int): Unit = crc.update(buffer, index, length)
  def reset(): Unit = crc.reset()
  def value: Long = crc.getValue
