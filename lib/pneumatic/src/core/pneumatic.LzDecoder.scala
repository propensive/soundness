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

// The decoder's sliding-window dictionary: a flat buffer holding recently-decoded bytes, from which
// LZMA match copies read. `start..pos` is the not-yet-flushed run; `full` tracks the history
// available for distance references. `setLimit` bounds how far a single decode pass may advance
// `pos` before the caller flushes, which keeps match copies inside the buffer.
private[pneumatic] final class LzDecoder(dictSize: Int):
  private val buffer: Array[Byte] = new Array[Byte](dictSize)
  private var start: Int = 0
  private var position: Int = 0
  private var full: Int = 0
  private var limit: Int = 0
  private var pendingLength: Int = 0
  private var pendingDist: Int = 0

  reset()

  def reset(): Unit =
    start = 0
    position = 0
    full = 0
    limit = 0
    buffer(dictSize - 1) = 0 // so the very first back-reference reads a defined zero

  // After a dictionary reset LZMA2 may keep decoding into the same window; only the model resets.
  def resetDictionary(): Unit = reset()

  def setLimit(outMax: Int): Unit =
    limit = if dictSize - position <= outMax then dictSize else position + outMax

  def hasSpace: Boolean = position < limit
  def hasPending: Boolean = pendingLength > 0
  def pos: Int = position

  def getByte(dist: Int): Int =
    var offset = position - dist - 1
    if dist >= position then offset += dictSize
    buffer(offset) & 0xff

  def putByte(byte: Byte): Unit =
    buffer(position) = byte
    position += 1
    if full < position then full = position

  def repeat(dist: Int, len: Int): Unit =
    if dist < 0 || dist >= full then
      throw IllegalStateException("the LZMA data is corrupt: invalid match distance")

    var left = if limit - position < len then limit - position else len
    pendingLength = len - left
    pendingDist = dist

    var back = position - dist - 1
    if dist >= position then back += dictSize

    while left > 0 do
      buffer(position) = buffer(back)
      position += 1
      back += 1
      if back == dictSize then back = 0
      left -= 1

    if full < position then full = position

  def repeatPending(): Unit =
    if pendingLength > 0 then repeat(pendingDist, pendingLength)

  // Copy `len` literal bytes straight from a source array (an LZMA2 uncompressed chunk).
  def copyUncompressed(source: Array[Byte], sourceOffset: Int, len: Int): Unit =
    val copySize = if dictSize - position < len then dictSize - position else len
    System.arraycopy(source, sourceOffset, buffer, position, copySize)
    position += copySize
    if full < position then full = position

  // Emit everything decoded since the last flush, wrapping `pos` when it reaches the buffer end.
  def flush(out: Array[Byte], outOffset: Int): Int =
    val copySize = position - start
    if position == dictSize then position = 0
    System.arraycopy(buffer, start, out, outOffset, copySize)
    start = position
    copySize

  def flushSize: Int = position - start
