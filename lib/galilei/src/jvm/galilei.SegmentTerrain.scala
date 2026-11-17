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
import java.nio.channels as jnc
import java.util.concurrent.atomic as juca

import anticipation.*
import zephyrine.u8

private[galilei] object SegmentTerrain:
  private val littleEndian: jn.ByteOrder = jn.ByteOrder.LITTLE_ENDIAN.nn

  // Unaligned layouts: a format decides where its integers lie, not the platform's ABI.
  val Byte: jlf.ValueLayout.OfByte = jlf.ValueLayout.JAVA_BYTE.nn

  val Short: jlf.ValueLayout.OfShort =
    jlf.ValueLayout.JAVA_SHORT_UNALIGNED.nn.withOrder(littleEndian).nn

  val Int: jlf.ValueLayout.OfInt =
    jlf.ValueLayout.JAVA_INT_UNALIGNED.nn.withOrder(littleEndian).nn

  val Long: jlf.ValueLayout.OfLong =
    jlf.ValueLayout.JAVA_LONG_UNALIGNED.nn.withOrder(littleEndian).nn

// A `zephyrine.Terrain` over a `MemorySegment`: every load is the segment's own, so it is
// bounds-checked, and fails once the arena which owns the memory has been closed.
private[galilei] class SegmentTerrain(initial: jlf.MemorySegment) extends zephyrine.Terrain:
  // The segment which is read: the one this was made over, unless it is one which is remapped.
  protected def segment: jlf.MemorySegment = initial

  def size: Long = segment.byteSize

  def u8(offset: Long): Int = segment.get(SegmentTerrain.Byte, offset) & 0xff
  def u16(offset: Long): Int = segment.get(SegmentTerrain.Short, offset) & 0xffff
  def i32(offset: Long): Int = segment.get(SegmentTerrain.Int, offset)
  def u32(offset: Long): Long = segment.get(SegmentTerrain.Int, offset) & 0xffffffffL
  def u64(offset: Long): Long = segment.get(SegmentTerrain.Long, offset)

  def slice(offset: Long, length: Long): zephyrine.Terrain =
    new SegmentTerrain(segment.asSlice(offset, length).nn)

  def read(offset: Long, length: Int): Data =
    val array = Array.allocate[Byte](length)
    jlf.MemorySegment.copy(segment, SegmentTerrain.Byte, offset, array.raw, 0, length)
    Array.freeze(array)

  // [java-boundary] the heap side of a comparison is read, never written or retained
  protected def heap(data: Data): jlf.MemorySegment =
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

// A whole-file mapping for changing in place: the JVM's `FilesystemBackend.Tilling`. The
// channel stays open, and its lock held, for as long as the file is mapped; the arena and the
// segment are each in a cell, since `expand` extends the file and maps it again, in an arena
// of its own, before the old one is closed.
private[galilei] class SegmentTilling
  ( channel: jnc.FileChannel, lock: jnc.FileLock, first: jlf.Arena, initial: jlf.MemorySegment )
extends SegmentTerrain(initial), FilesystemBackend.Tilling:
  private val arena: juca.AtomicReference[jlf.Arena] = juca.AtomicReference(first)
  private val mapped: juca.AtomicReference[jlf.MemorySegment] = juca.AtomicReference(initial)

  override protected def segment: jlf.MemorySegment = mapped.get.nn

  override def slice(offset: Long, length: Long): zephyrine.Terrain =
    if offset < 0L || length < 0L || offset > size - length
    then throw IndexOutOfBoundsException(s"$length bytes at $offset of $size")

    zephyrine.Tilth.window(this, offset, length)

  def set8(offset: Long, value: Int): Unit = segment.set(SegmentTerrain.Byte, offset, value.toByte)

  def set16(offset: Long, value: Int): Unit =
    segment.set(SegmentTerrain.Short, offset, value.toShort)

  def set32(offset: Long, value: Long): Unit = segment.set(SegmentTerrain.Int, offset, value.toInt)
  def set64(offset: Long, value: Long): Unit = segment.set(SegmentTerrain.Long, offset, value)

  def write(offset: Long, data: Data): Unit =
    jlf.MemorySegment.copy(heap(data), 0L, segment, offset, data.length.toLong)

  def zero(offset: Long, length: Long): Unit = segment.asSlice(offset, length).nn.fill(0.toByte)

  def move(from: Long, to: Long, length: Long): Unit =
    jlf.MemorySegment.copy(segment, from, segment, to, length)

  // The file is extended by writing its last byte, and mapped afresh at its new size.
  def expand(size: Long): Unit = if size > this.size then
    segment.force()
    channel.write(jn.ByteBuffer.wrap(scala.Array[Byte](0)).nn, size - 1)
    val grown = jlf.Arena.ofShared().nn
    val mode = jnc.FileChannel.MapMode.READ_WRITE.nn
    mapped.set(channel.map(mode, 0L, size, grown).nn)
    arena.getAndSet(grown).nn.close()

  def flush(): Unit = segment.force()

  def close(): Unit =
    try segment.force() finally
      try arena.get.nn.close() finally
        try lock.release() finally channel.close()

// A whole-file, read-only mapping: the segment's memory belongs to `arena`, and is unmapped
// -- deterministically, rather than whenever the garbage collector notices -- by closing it.
private[galilei] class SegmentMapping(arena: jlf.Arena, segment: jlf.MemorySegment)
extends SegmentTerrain(segment), FilesystemBackend.Mapping:
  def unmap(): Unit = arena.close()
