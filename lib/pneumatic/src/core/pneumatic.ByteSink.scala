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

// A growable flat byte buffer with a delivery cursor: the staging area shared by the LZMA2, XZ
// and LZW engines and the `.xz` container builders. It replaces `scm.ArrayBuffer[Byte]`, which is
// not specialized — its backing store is an `Object[]` of references to boxed bytes, four to eight
// bytes of pointer per byte of payload before doubling slack, filled by one boxed append per byte
// and read back by one unboxing subscript per byte (the envelope `BrotliEngine` shed in #1805).
// Here appends are indexed writes or `System.arraycopy`, and drains are `System.arraycopy`.
//
// The bytes at `delivered until length` are live. Once everything has been delivered the cursor
// rewinds, keeping the capacity for the next round; `take()` releases the store as well. The
// backing array is never exposed, so no caller can mistake its capacity for the length.
private[pneumatic] final class ByteSink(initial: Int = 8192) extends caps.Mutable:
  private var buffer: scala.Array[Byte]^ =
    new scala.Array[Byte](if initial < 16 then 16 else initial)

  private var length0: Int = 0
  private var delivered: Int = 0

  // The number of live (appended but not yet delivered) bytes.
  def length: Int = length0 - delivered

  private update def reserve(extra: Int): Unit =
    if length0 + extra > buffer.length then
      var size = buffer.length*2
      while size < length0 + extra do size *= 2
      val grown: scala.Array[Byte]^ = new scala.Array[Byte](size)
      System.arraycopy(buffer, delivered, grown, 0, length0 - delivered)
      length0 -= delivered
      delivered = 0
      buffer = grown

  update def append(byte: Byte): Unit =
    if length0 == buffer.length then reserve(1)
    buffer(length0) = byte
    length0 += 1

  // Reads its argument only, so it takes any array through a shared-read reference; the cast is
  // the launder `BrotliAccumulator.accumulate` uses for the same `arraycopy`.
  update def append(bytes: scala.Array[Byte]^{caps.any.rd}, offset: Int, count: Int): Unit =
    if count > 0 then
      reserve(count)
      System.arraycopy(bytes.asInstanceOf[scala.Array[Byte]], offset, buffer, length0, count)
      length0 += count

  update def clear(): Unit =
    length0 = 0
    delivered = 0

  // The live bytes, copied out to an exact-size array; the sink is unchanged.
  def toArray: scala.Array[Byte] =
    val count = length0 - delivered
    val result: scala.Array[Byte]^ = new scala.Array[Byte](count)
    System.arraycopy(buffer, delivered, result, 0, count)
    result

  // Copies up to `space` live bytes into `target`, advancing the cursor, and rewinds (keeping
  // the capacity) once everything has been delivered.
  update def drainInto(target: scala.Array[Byte]^, offset: Int, space: Int): Int =
    val count = (length0 - delivered).min(space)

    if count > 0 then
      System.arraycopy(buffer, delivered, target, offset, count)
      delivered += count
      if delivered == length0 then clear()

    count

  // The live bytes in one exact-size array, releasing the backing store.
  update def take(): scala.Array[Byte] =
    val result = toArray
    buffer = new scala.Array[Byte](16)
    clear()
    result
