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

import scala.scalanative.posix.fcntl
import scala.scalanative.posix.sys.mman
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import anticipation.*
import vacuous.*

// A `zephyrine.Mapped` over `extent` bytes of memory at `base`: the Scala Native twin of the
// JVM's `SegmentMapped`. Loads are composed bytewise, so they are little-endian and free of
// alignment requirements whatever the target; each is bounds-checked first, since a stray
// load here is a segmentation fault rather than an exception.
private[galilei] class PointerMapped(base: Ptr[Byte], extent: Long) extends zephyrine.Mapped:
  def size: Long = extent

  private inline def check(offset: Long, width: Long): Unit =
    if offset < 0L || width < 0L || offset > extent - width
    then throw IndexOutOfBoundsException(s"$width bytes at $offset of $extent")

  private inline def byte(offset: Long): Int = !(base + offset) & 0xff

  def u8(offset: Long): Int =
    check(offset, 1L)
    byte(offset)

  def u16(offset: Long): Int =
    check(offset, 2L)
    byte(offset) | byte(offset + 1L) << 8

  def i32(offset: Long): Int =
    check(offset, 4L)
    byte(offset) | byte(offset + 1L) << 8 | byte(offset + 2L) << 16 | byte(offset + 3L) << 24

  def u32(offset: Long): Long = i32(offset) & 0xffffffffL
  def u64(offset: Long): Long = u32(offset) | u32(offset + 4L) << 32

  def slice(offset: Long, length: Long): zephyrine.Mapped^{this} =
    check(offset, length)
    new PointerMapped(base + offset, length)

  def copy(offset: Long, length: Int): Data =
    check(offset, length.toLong)
    val array = Array.allocate[Byte](length)
    val raw = array.raw
    var index = 0

    while index < length do
      raw(index) = !(base + (offset + index))
      index += 1

    Array.freeze(array)

  def same(offset: Long, data: Data): Boolean =
    val length = data.length

    offset >= 0L && offset <= extent - length && {
      var index = 0
      while index < length && !(base + (offset + index)) == data.readUnchecked(index) do index += 1
      index == length
    }

// A whole-file, read-only mapping, from `mmap` until `munmap`.
private[galilei] class PointerMapping(base: Ptr[Byte], extent: Long)
extends PointerMapped(base, extent), FilesystemBackend.Mapping:
  def unmap(): Unit = if extent > 0L then mman.munmap(base, extent.toCSize)

private[galilei] object PointerMapping:
  // Maps all `extent` bytes of the named file, or answers `Unset` if the file cannot be opened
  // or mapped. The descriptor is needed only to establish the mapping, which outlives it. An
  // empty file is not mapped at all: `mmap` refuses a length of zero, and there is nothing to
  // load.
  def apply(name: String, extent: Long): Optional[PointerMapping] =
    val nowhere = null.asInstanceOf[Ptr[Byte]]
    val failed = mman.MAP_FAILED.asInstanceOf[Ptr[Byte]]

    if extent == 0L then new PointerMapping(nowhere, 0L) else
      // The mapping is made within the zone which holds the C string, but the capability over
      // it is made outside, so that it does not appear to depend on the zone.
      val base: Ptr[Byte] = Zone.acquire: zone =>
        given Zone = zone
        val descriptor = fcntl.open(toCString(name), fcntl.O_RDONLY, 0.toUInt)

        if descriptor < 0 then failed else
          val length = extent.toCSize
          try mman.mmap(nowhere, length, mman.PROT_READ, mman.MAP_SHARED, descriptor, 0.toSize)
          finally unistd.close(descriptor)

      if base == failed then Unset else new PointerMapping(base, extent)
