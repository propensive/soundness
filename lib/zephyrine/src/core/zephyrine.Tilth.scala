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
package zephyrine

import java.nio as jn
import java.util.concurrent.atomic as juca

import anticipation.*

// A terrain which can be changed where it lies: worked ground. It is what a format which is
// kept and altered in place is altered through, as a `Terrain` is what one is read through --
// a file mapped for writing by the one process which owns it, or bytes on the heap -- and,
// like a terrain, it is a capability which belongs to a scope.
//
// Every store, like every load, is at an offset: a tilth hands out no address into its
// memory, because `expand` may move it. A view taken with `slice` reads through the tilth it
// came from, so it stays good across an expansion, and sees whatever is stored afterwards.
trait Tilth extends Terrain:
  def set8(offset: Long, value: Int): Unit
  def set16(offset: Long, value: Int): Unit
  def set32(offset: Long, value: Long): Unit
  def set64(offset: Long, value: Long): Unit

  // Stores the bytes of `data` from `offset`.
  def write(offset: Long, data: Data): Unit

  // Stores `length` zero bytes from `offset`.
  def zero(offset: Long, length: Long): Unit

  // Copies `length` bytes from `from` to `to`, which may overlap.
  def move(from: Long, to: Long, length: Long): Unit

  // Makes the tilth at least `size` bytes, the new ones zero; it never shrinks.
  def expand(size: Long): Unit

  // Makes what has been stored durable, where there is anything for it to be durable in.
  def flush(): Unit

object Tilth:
  // A tilth on the heap, of `size` zero bytes: for building, or altering, an image which is
  // not, or not yet, a file.
  def apply(size: Long = 0L): Tilth = Heap(size)

  // A tilth on the heap which begins as a copy of `data`.
  def apply(data: Data): Tilth =
    val tilth = Heap(data.length.toLong)
    tilth.write(0L, data)
    tilth

  // A view of part of a tilth, which reads through it.
  private[zephyrine] final class Window(tilth: Tilth^, start: Long, extent: Long) extends Terrain:
    def size: Long = extent

    private inline def at(offset: Long, width: Long): Long =
      if offset < 0L || width < 0L || offset > extent - width
      then throw IndexOutOfBoundsException(s"$width bytes at $offset of $extent")

      start + offset

    def u8(offset: Long): Int = tilth.u8(at(offset, 1L))
    def u16(offset: Long): Int = tilth.u16(at(offset, 2L))
    def u32(offset: Long): Long = tilth.u32(at(offset, 4L))
    def u64(offset: Long): Long = tilth.u64(at(offset, 8L))
    def i32(offset: Long): Int = tilth.i32(at(offset, 4L))
    def read(offset: Long, length: Int): Data = tilth.read(at(offset, length.toLong), length)
    def slice(offset: Long, length: Long): Terrain^{this} = Window(tilth, at(offset, length), length)

    def same(offset: Long, data: Data): Boolean =
      offset >= 0L && offset <= extent - data.length && tilth.same(start + offset, data)

    def compare(offset: Long, length: Long, data: Data): Int =
      tilth.compare(at(offset, length), length, data)

  // A view of part of a tilth, for whatever implements one.
  def window(tilth: Tilth^, offset: Long, length: Long): Terrain^{tilth} =
    Window(tilth, offset, length)

  private final class Heap(initial: Long) extends Tilth:
    private val order: jn.ByteOrder = jn.ByteOrder.LITTLE_ENDIAN.nn

    private def allocate(size: Long): jn.ByteBuffer =
      if size > Int.MaxValue then throw IndexOutOfBoundsException(s"$size bytes on the heap")
      jn.ByteBuffer.allocate(size.toInt).nn.order(order).nn

    // The bytes, and how many of them are the tilth: each in a cell, since `expand` replaces
    // the one and advances the other.
    private val held: juca.AtomicReference[jn.ByteBuffer] = juca.AtomicReference(allocate(initial))
    private val reached: juca.AtomicLong = juca.AtomicLong(initial)

    private def buffer: jn.ByteBuffer = held.get.nn
    private def extent: Long = reached.get

    def size: Long = extent

    private inline def at(offset: Long, width: Long): Int =
      if offset < 0L || width < 0L || offset > extent - width
      then throw IndexOutOfBoundsException(s"$width bytes at $offset of $extent")

      offset.toInt

    def u8(offset: Long): Int = buffer.get(at(offset, 1L)) & 0xff
    def u16(offset: Long): Int = buffer.getShort(at(offset, 2L)) & 0xffff
    def i32(offset: Long): Int = buffer.getInt(at(offset, 4L))
    def u32(offset: Long): Long = buffer.getInt(at(offset, 4L)) & 0xffffffffL
    def u64(offset: Long): Long = buffer.getLong(at(offset, 8L))

    def slice(offset: Long, length: Long): Terrain^{this} = Window(this, at(offset, length), length)

    def read(offset: Long, length: Int): Data =
      val base = at(offset, length.toLong)
      Array.tabulate[Byte](length) { index => buffer.get(base + index) }

    def same(offset: Long, data: Data): Boolean =
      offset >= 0L && offset <= extent - data.length
      && compare(offset, data.length.toLong, data) == 0

    def compare(offset: Long, length: Long, data: Data): Int =
      val base = at(offset, length)
      val common = length.toInt.min(data.length)

      @scala.annotation.tailrec
      def recur(index: Int): Int =
        if index == common then java.lang.Long.compare(length, data.length.toLong) else
          val left = buffer.get(base + index) & 0xff
          val right = data.u8(index)
          if left == right then recur(index + 1) else left - right

      recur(0)

    def set8(offset: Long, value: Int): Unit = buffer.put(at(offset, 1L), value.toByte)
    def set16(offset: Long, value: Int): Unit = buffer.putShort(at(offset, 2L), value.toShort)
    def set32(offset: Long, value: Long): Unit = buffer.putInt(at(offset, 4L), value.toInt)
    def set64(offset: Long, value: Long): Unit = buffer.putLong(at(offset, 8L), value)

    def write(offset: Long, data: Data): Unit =
      val base = at(offset, data.length.toLong)

      @scala.annotation.tailrec
      def recur(index: Int): Unit = if index < data.length then
        buffer.put(base + index, data.u8(index).toByte)
        recur(index + 1)

      recur(0)

    def zero(offset: Long, length: Long): Unit =
      val base = at(offset, length)

      @scala.annotation.tailrec
      def recur(index: Int): Unit = if index < length then
        buffer.put(base + index, 0.toByte)
        recur(index + 1)

      recur(0)

    // Forwards if the destination is below the source, and backwards if it is above, so that
    // nothing is overwritten before it is copied.
    def move(from: Long, to: Long, length: Long): Unit =
      val source = at(from, length)
      val target = at(to, length)
      val count = length.toInt

      @scala.annotation.tailrec
      def forwards(index: Int): Unit = if index < count then
        buffer.put(target + index, buffer.get(source + index))
        forwards(index + 1)

      @scala.annotation.tailrec
      def backwards(index: Int): Unit = if index >= 0 then
        buffer.put(target + index, buffer.get(source + index))
        backwards(index - 1)

      if target < source then forwards(0) else if target > source then backwards(count - 1)

    // At least doubled, so that growing a byte at a time costs no more than growing at once.
    def expand(size: Long): Unit = if size > extent then
      if size > buffer.capacity then
        val grown = allocate(size.max(2L*buffer.capacity).min(Int.MaxValue.toLong).max(size))
        grown.put(0, buffer, 0, extent.toInt)
        held.set(grown)

      reached.set(size)

    def flush(): Unit = ()
