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

import scala.scalanative.libc.string
import scala.scalanative.posix.fcntl
import scala.scalanative.posix.fcntlOps.*
import scala.scalanative.posix.sys.mman
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import java.util.concurrent as juc
import java.util.concurrent.atomic as juca

import anticipation.*
import vacuous.*

// A `zephyrine.Terrain` over `extent` bytes of memory at `base`: the Scala Native twin of the
// JVM's `SegmentTerrain`. Loads are composed bytewise, so they are little-endian and free of
// alignment requirements whatever the target; each is bounds-checked first, since a stray
// load here is a segmentation fault rather than an exception.
private[galilei] class PointerTerrain(first: Ptr[Byte], initial: Long) extends zephyrine.Terrain:
  // Where the memory is, and how much of it there is: fixed for a view which is only read,
  // but a holding's memory moves when it is expanded.
  protected def origin: Ptr[Byte] = first
  def size: Long = initial

  protected inline def check(offset: Long, width: Long): Unit =
    if offset < 0L || width < 0L || offset > size - width
    then throw IndexOutOfBoundsException(s"$width bytes at $offset of $size")

  protected inline def byte(offset: Long): Int = !(origin + offset) & 0xff

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

  def slice(offset: Long, length: Long): zephyrine.Terrain^{this} =
    check(offset, length)
    new PointerTerrain(origin + offset, length)

  def read(offset: Long, length: Int): Data =
    check(offset, length.toLong)
    val array = Array.allocate[Byte](length)
    val raw = array.raw
    var index = 0

    while index < length do
      raw(index) = !(origin + (offset + index))
      index += 1

    Array.freeze(array)

  def same(offset: Long, data: Data): Boolean =
    val length = data.length

    offset >= 0L && offset <= size - length && {
      var index = 0

      while index < length && !(origin + (offset + index)) == data.readUnchecked(index)
      do index += 1

      index == length
    }

  def compare(offset: Long, length: Long, data: Data): Int =
    check(offset, length)
    val other = data.length
    val common = if length < other then length.toInt else other
    var index = 0
    var order = 0

    while order == 0 && index < common do
      order = byte(offset + index) - (data.readUnchecked(index) & 0xff)
      index += 1

    if order != 0 then order else java.lang.Long.compare(length, other.toLong)

// A whole-file, read-only mapping, from `mmap` until `munmap`.
private[galilei] class PointerMapping(base: Ptr[Byte], extent: Long)
extends PointerTerrain(base, extent), FilesystemBackend.Mapping:
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

// A whole file mapped for changing in place: the Scala Native twin of the JVM's
// `SegmentHolding`. The descriptor stays open for as long as the file is mapped, since the
// lock is released when it is closed; where the memory is, and how much there is, are each in
// a cell, since `expand` extends the file and maps it again.
private[galilei] class PointerHolding
  ( descriptor: Int, name: String, first: Ptr[Byte], initial: Long )
extends PointerTerrain(first, initial), FilesystemBackend.Hold:
  private val start: juca.AtomicReference[Ptr[Byte]] = juca.AtomicReference(first)
  private val extent: juca.AtomicLong = juca.AtomicLong(initial)

  // Not `.nn`: the origin of an empty holding, which is not mapped, is null.
  override protected def origin: Ptr[Byte] = start.get.asInstanceOf[Ptr[Byte]]
  override def size: Long = extent.get

  override def slice(offset: Long, length: Long): zephyrine.Terrain =
    check(offset, length)
    zephyrine.Holding.window(this, offset, length)

  private inline def store(offset: Long, value: Long): Unit = !(origin + offset) = value.toByte

  def set8(offset: Long, value: Int): Unit =
    check(offset, 1L)
    store(offset, value.toLong)

  def set16(offset: Long, value: Int): Unit =
    check(offset, 2L)
    store(offset, value.toLong)
    store(offset + 1L, (value >> 8).toLong)

  def set32(offset: Long, value: Long): Unit =
    check(offset, 4L)
    store(offset, value)
    store(offset + 1L, value >> 8)
    store(offset + 2L, value >> 16)
    store(offset + 3L, value >> 24)

  def set64(offset: Long, value: Long): Unit =
    check(offset, 8L)
    set32(offset, value)
    set32(offset + 4L, value >> 32)

  def write(offset: Long, data: Data): Unit =
    val length = data.length
    check(offset, length.toLong)
    var index = 0

    while index < length do
      !(origin + (offset + index)) = data.readUnchecked(index)
      index += 1

  def zero(offset: Long, length: Long): Unit =
    check(offset, length)
    if length > 0L then string.memset(origin + offset, 0, length.toCSize)

  // The two ranges may overlap, which `memmove` allows.
  def move(from: Long, to: Long, length: Long): Unit =
    check(from, length)
    check(to, length)
    if length > 0L then string.memmove(origin + to, origin + from, length.toCSize)

  // The file is extended, and mapped afresh at its new size; the old mapping is given up
  // only once the new one is made.
  def expand(size: Long): Unit = if size > this.size then
    flush()

    if unistd.ftruncate(descriptor, size.toSize) != 0
    then PointerHolding.fail(s"$name could not be extended to $size bytes")

    val grown = PointerHolding.map(descriptor, size)

    if grown == PointerHolding.failed
    then PointerHolding.fail(s"$name could not be mapped at $size bytes")

    val before = origin
    val length = this.size
    start.set(grown)
    extent.set(size)
    if length > 0L then mman.munmap(before, length.toCSize)

  def flush(): Unit = if size > 0L then mman.msync(origin, size.toCSize, mman.MS_SYNC)

  // Closing the descriptor releases the lock.
  def close(): Unit =
    try
      flush()
      if size > 0L then mman.munmap(origin, size.toCSize)
    finally
      try unistd.close(descriptor) finally PointerHolding.held.remove(name)

private[galilei] object PointerHolding:
  val nowhere: Ptr[Byte] = null.asInstanceOf[Ptr[Byte]]
  val failed: Ptr[Byte] = mman.MAP_FAILED.asInstanceOf[Ptr[Byte]]

  // The files this process holds, by name. The lock on a file is a POSIX record lock, as the
  // JVM's is, so that the two exclude each other; but such a lock belongs to the process, and
  // would be granted again to a second scope within it, which is refused here instead.
  val held: juc.ConcurrentHashMap[String, String] = juc.ConcurrentHashMap()

  def fail(message: String): Nothing =
    throw java.io.UncheckedIOException(java.io.IOException(message))

  inline val Denied = -1
  inline val Busy = -2

  // Opens the named file for reading and writing, creating it if it is not there, and locks
  // all of it; and extends it to `reserve` bytes if it is shorter, as `length` says it is.
  // Answers the descriptor, or `Denied` if the file cannot be opened or extended, or `Busy`
  // if it is held already, by this process or another.
  def open(name: String, length: Long, reserve: Long): Int =
    if held.putIfAbsent(name, name) != null then Busy else
      val descriptor: Int = Zone.acquire: zone =>
        given Zone = zone
        fcntl.open(toCString(name), fcntl.O_RDWR | fcntl.O_CREAT, 0x1b6.toUInt)

      val outcome: Int =
        if descriptor < 0 then Denied else
          val lock = stackalloc[fcntl.flock]()
          lock.l_type = fcntl.F_WRLCK
          lock.l_whence = 0
          lock.l_start = 0.toSize
          lock.l_len = 0.toSize
          lock.l_pid = 0

          if fcntl.fcntl(descriptor, fcntl.F_SETLK, lock) != 0 then Busy
          else if length < reserve && unistd.ftruncate(descriptor, reserve.toSize) != 0 then Denied
          else descriptor

      if outcome < 0 then
        if descriptor >= 0 then unistd.close(descriptor)
        held.remove(name)

      outcome

  def map(descriptor: Int, length: Long): Ptr[Byte] =
    if length == 0L then nowhere else
      val access = mman.PROT_READ | mman.PROT_WRITE
      mman.mmap(nowhere, length.toCSize, access, mman.MAP_SHARED, descriptor, 0.toSize)

  // The holding of a file which `open` has opened, all `length` bytes of it mapped; or
  // `Unset`, with the file closed, if it cannot be mapped.
  def apply(descriptor: Int, name: String, length: Long): Optional[PointerHolding] =
    val base = map(descriptor, length)

    if base == failed then
      unistd.close(descriptor)
      held.remove(name)
      Unset
    else new PointerHolding(descriptor, name, base, length)
