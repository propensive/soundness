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
package galilei

import java.lang.foreign as jlf
import java.nio as jn

import anticipation.*
import zephyrine.u8

private[galilei] object SegmentMapped:
  private val littleEndian: jn.ByteOrder = jn.ByteOrder.LITTLE_ENDIAN.nn

  // Unaligned layouts: a format decides where its integers lie, not the platform's ABI.
  val Byte: jlf.ValueLayout.OfByte = jlf.ValueLayout.JAVA_BYTE.nn

  val Short: jlf.ValueLayout.OfShort =
    jlf.ValueLayout.JAVA_SHORT_UNALIGNED.nn.withOrder(littleEndian).nn

  val Int: jlf.ValueLayout.OfInt =
    jlf.ValueLayout.JAVA_INT_UNALIGNED.nn.withOrder(littleEndian).nn

  val Long: jlf.ValueLayout.OfLong =
    jlf.ValueLayout.JAVA_LONG_UNALIGNED.nn.withOrder(littleEndian).nn

// A `zephyrine.Mapped` over a `MemorySegment`: every load is the segment's own, so it is
// bounds-checked, and fails once the arena which owns the memory has been closed.
private[galilei] class SegmentMapped(segment: jlf.MemorySegment) extends zephyrine.Mapped:
  def size: Long = segment.byteSize

  def u8(offset: Long): Int = segment.get(SegmentMapped.Byte, offset) & 0xff
  def u16(offset: Long): Int = segment.get(SegmentMapped.Short, offset) & 0xffff
  def i32(offset: Long): Int = segment.get(SegmentMapped.Int, offset)
  def u32(offset: Long): Long = segment.get(SegmentMapped.Int, offset) & 0xffffffffL
  def u64(offset: Long): Long = segment.get(SegmentMapped.Long, offset)

  def slice(offset: Long, length: Long): zephyrine.Mapped =
    new SegmentMapped(segment.asSlice(offset, length).nn)

  def copy(offset: Long, length: Int): Data =
    val array = Array.allocate[Byte](length)
    jlf.MemorySegment.copy(segment, SegmentMapped.Byte, offset, array.raw, 0, length)
    Array.freeze(array)

  // [java-boundary] the heap side of a comparison is read, never written or retained
  private def heap(data: Data): jlf.MemorySegment =
    jlf.MemorySegment.ofArray(Array.unsafeJvm(data)).nn

  def same(offset: Long, data: Data): Boolean =
    val length = data.length.toLong

    offset >= 0L && offset + length <= size
    && jlf.MemorySegment.mismatch(segment, offset, offset + length, heap(data), 0L, length) == -1L

  def compare(offset: Long, length: Long, data: Data): Int =
    val other = data.length.toLong

    val differ =
      jlf.MemorySegment.mismatch(segment, offset, offset + length, heap(data), 0L, other)

    if differ == -1L then 0
    else if differ == length || differ == other then java.lang.Long.compare(length, other)
    else u8(offset + differ) - data.u8(differ.toInt)

// A whole-file, read-only mapping: the segment's memory belongs to `arena`, and is unmapped
// -- deterministically, rather than whenever the garbage collector notices -- by closing it.
private[galilei] class SegmentMapping(arena: jlf.Arena, segment: jlf.MemorySegment)
extends SegmentMapped(segment), FilesystemBackend.Mapping:
  def unmap(): Unit = arena.close()
