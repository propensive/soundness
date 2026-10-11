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
package octogenarian

import java.nio.{ByteBuffer, LongBuffer}

import anticipation.*
import contingency.*
import galilei.*
import gossamer.*
import pneumatic.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*
import zephyrine.*

import filesystemBackends.javaBaseFilesystem

// A packfile and its index, read positionally: the index is loaded once into arrays, and each
// object is inflated from its own slice of the pack, whose end is the next entry's offset, so no
// stream position has to be tracked across entries.
private[octogenarian] object Packfile:
  import Git.Error.Reason as Reason

  // The version-2 index: a 256-entry fanout, the sorted hashes, their CRCs and offsets, with
  // offsets beyond 2 GiB in a trailing table of eight-byte values.
  class Index(val hashes: ByteBuffer, val offsets: LongBuffer, val count: Int):
    private def hashAt(index: Int): Text =
      val bytes = new scala.Array[Byte](20)
      hashes.get(index*20, bytes)
      Packfile.hex(bytes, 0, 20)

    def offsetOf(hash: Git.Hash): Optional[Long] =
      val target = Packfile.unhex(hash.text)

      def compare(index: Int): Int =
        def recur(k: Int): Int =
          if k == 20 then 0 else
            val a = hashes.get(index*20 + k) & 0xff
            val b = target(k) & 0xff
            if a == b then recur(k + 1) else a - b

        recur(0)

      def search(low: Int, high: Int): Optional[Long] =
        if low > high then Unset else
          val mid = (low + high) >>> 1
          val order = compare(mid)

          if order == 0 then offsets.get(mid)
          else if order < 0 then search(mid + 1, high)
          else search(low, mid - 1)

      search(0, count - 1)

    // Every object's hash paired with its offset, for enumeration.
    def entries: List[(Git.Hash, Long)] =
      def recur(index: Int, acc: List[(Git.Hash, Long)]): List[(Git.Hash, Long)] =
        if index < 0 then acc
        else recur(index - 1, (Git.Hash.unsafe(hashAt(index)), offsets.get(index)) :: acc)

      recur(count - 1, Nil)

    // The offset of the entry following the one at `offset`, or the trailer's start.
    def end(offset: Long, packSize: Long): Long =
      def recur(index: Int, best: Long): Long =
        if index == count then best else
          val candidate = offsets.get(index)
          recur(index + 1, if candidate > offset && candidate < best then candidate else best)

      recur(0, packSize - 20)

  def loadIndex(data: Data)(using Tactic[Git.Error]): Index =
    val array = data.asInstanceOf[scala.Array[Byte]]
    val buffer = ByteBuffer.wrap(array).nn

    if buffer.getInt(0) != 0xff744f63 || buffer.getInt(4) != 2
    then abort(Git.Error(Reason.Unsupported(t"packfile index version")))

    val count = buffer.getInt(8 + 255*4)

    val hashesStart = 8 + 256*4
    val crcStart = hashesStart + count*20
    val offsetStart = crcStart + count*4
    val largeStart = offsetStart + count*4
    val hashes = ByteBuffer.wrap(array, hashesStart, count*20).nn.slice().nn
    val offsets = LongBuffer.allocate(count).nn

    def recur(index: Int): Unit = if index < count then
      val raw = buffer.getInt(offsetStart + index*4)

      val offset =
        if (raw & 0x80000000) == 0 then raw.toLong & 0xffffffffL
        else buffer.getLong(largeStart + (raw & 0x7fffffff)*8)

      offsets.put(index, offset)
      recur(index + 1)

    recur(0)
    Index(hashes, offsets, count)

  def hex(array: scala.Array[Byte], from: Int, length: Int): Text = Objects.hex(array, from, length)
  def unhex(text: Text): scala.Array[Byte] = Objects.unhex(text)

  // A pack entry's header: its type, its inflated size, and where its data begins. Types 6 and 7
  // are deltas, against the object at a relative offset or against a named base.
  enum Entry:
    case Whole(kind: Git.Object.Kind, size: Long, data: Long)
    case OffsetDelta(base: Long, size: Long, data: Long)
    case RefDelta(base: Git.Hash, size: Long, data: Long)

  // Reads an entry header from the pack through a positional view.
  def header(view: Expanse, offset: Long)(using Tactic[Git.Error]): Entry =
    val bytes = view.read(offset, 32).asInstanceOf[scala.Array[Byte]]
    val first = bytes(0) & 0xff
    val typeCode = (first >> 4) & 7

    // The size is variable-length: four bits in the first byte, then seven per continuation.
    def sizeFrom(index: Int, size: Long, shift: Int): (Long, Int) =
      if (bytes(index - 1) & 0x80) == 0 then (size, index)
      else sizeFrom(index + 1, size | ((bytes(index) & 0x7f).toLong << shift), shift + 7)

    val (size, index) = sizeFrom(1, (first & 0x0f).toLong, 4)

    typeCode match
      case 1 => Entry.Whole(Git.Object.Kind.Commit, size, offset + index)
      case 2 => Entry.Whole(Git.Object.Kind.Tree, size, offset + index)
      case 3 => Entry.Whole(Git.Object.Kind.Blob, size, offset + index)
      case 4 => Entry.Whole(Git.Object.Kind.Tag, size, offset + index)

      case 6 =>
        // The base offset is big-endian, with one added at each continuation.
        def distanceFrom(k: Int, distance: Long): (Long, Int) =
          if (bytes(k) & 0x80) == 0 then (distance, k)
          else distanceFrom(k + 1, ((distance + 1) << 7) | (bytes(k + 1) & 0x7f).toLong)

        val (distance, k) = distanceFrom(index, (bytes(index) & 0x7f).toLong)
        Entry.OffsetDelta(offset - distance, size, offset + k + 1)

      case 7 =>
        val hash = hex(bytes, index, 20)
        Entry.RefDelta(Git.Hash.unsafe(hash), size, offset + index + 20)

      case other =>
        abort(Git.Error(Reason.NoSuchObject(t"pack entry of type $other")))

  // Inflates the zlib stream lying between `from` and `until` in the pack: the slice is exactly
  // the entry's compressed data, since the next entry begins where it ends.
  def inflate(view: Expanse, from: Long, until: Long): Data =
    view.read(from, (until - from).toInt).decompress[Zlib]

  // Applies a delta to its base: a source size, a target size, then copy and insert instructions.
  def applyDelta(base: Data, delta: Data): Data =
    val source = base.asInstanceOf[scala.Array[Byte]]
    val instructions = delta.asInstanceOf[scala.Array[Byte]]

    // A little-endian base-128 size, returned with the position after it.
    def varint(position: Int, value: Long, shift: Int): (Long, Int) =
      val byte = instructions(position) & 0xff
      val next = value | ((byte & 0x7f).toLong << shift)
      if (byte & 0x80) == 0 then (next, position + 1) else varint(position + 1, next, shift + 7)

    val (_, afterSource) = varint(0, 0L, 0)
    val (targetSize, start) = varint(afterSource, 0L, 0)
    val target = java.io.ByteArrayOutputStream(targetSize.toInt)

    // A copy instruction's offset and length are assembled from the bytes its opcode bits select.
    def field
      ( position: Int, opcode: Int, bits: List[Int], value: Int, shift: Int )
    :   (Int, Int) =

      bits match
        case bit :: rest =>
          if (opcode & bit) != 0 then
            field
              ( position + 1,
                opcode,
                rest,
                value | ((instructions(position) & 0xff) << shift),
                shift + 8 )
          else
            field(position, opcode, rest, value, shift + 8)

        case _ => (value, position)

    def recur(position: Int): Unit = if position < instructions.length then
      val opcode = instructions(position) & 0xff

      if (opcode & 0x80) != 0 then
        val (offset, afterOffset) = field(position + 1, opcode, List(0x01, 0x02, 0x04, 0x08), 0, 0)
        val (rawLength, next) = field(afterOffset, opcode, List(0x10, 0x20, 0x40), 0, 0)
        val length = if rawLength == 0 then 0x10000 else rawLength
        target.write(source, offset, length)
        recur(next)
      else
        target.write(instructions, position + 1, opcode)
        recur(position + 1 + opcode)

    recur(start)
    target.toByteArray.nn.asInstanceOf[Data]

// One pack on disk, with its index loaded.
private[octogenarian] class Packfile(val pack: Path on Linux, val index: Packfile.Index):
  import Git.Error.Reason as Reason

  def contains(hash: Git.Hash): Boolean = index.offsetOf(hash).present

  // Reads and resolves the object at `offset`, following deltas within this pack; a delta whose
  // base lies in another pack is resolved through `external`.
  def read(hash: Git.Hash, external: Git.Hash => Optional[(Git.Object.Kind, Data)])
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Optional[(Git.Object.Kind, Data)] =

    index.offsetOf(hash).let: offset =>
      pack.expanse: view =>
        def at(offset: Long): (Git.Object.Kind, Data) =
          val end = index.end(offset, view.size)

          Packfile.header(view, offset) match
            case Packfile.Entry.Whole(kind, _, data) =>
              (kind, Packfile.inflate(view, data, end))

            case Packfile.Entry.OffsetDelta(baseOffset, _, data) =>
              val (kind, base) = at(baseOffset)
              (kind, Packfile.applyDelta(base, Packfile.inflate(view, data, end)))

            case Packfile.Entry.RefDelta(baseHash, _, data) =>
              val (kind, base) =
                index.offsetOf(baseHash).lay(external(baseHash)
                . or(abort(Git.Error(Reason.NoSuchObject(baseHash.text)))))(at(_))

              (kind, Packfile.applyDelta(base, Packfile.inflate(view, data, end)))

        at(offset)
