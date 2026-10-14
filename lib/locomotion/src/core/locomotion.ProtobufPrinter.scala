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
package locomotion

import scala.caps

import anticipation.*
import rudiments.*

// Writes Protocol Buffers wire bytes. Varints are LEB128; fixed32/fixed64 are little-endian; a
// field is its tag varint followed by the value (length-prefixed for the length-delimited wire
// type). A message's encoded children are already in hand, so its size is known before it is
// written (`fieldSize`), and `Protobuf` writes it into an exact-size array.
//
// The printer owns its buffer, in one of three modes: an array of exactly the measured size,
// returned by `data()` without a copy; a growable array, for bytes of unknown length; or a
// block, handed to `deliver` whenever the next write would not fit, so that a message is
// written as a stream of blocks rather than into one array. A write longer than a whole block
// goes to `deliver` directly. Holding its buffer and cursor, the printer is a stateful
// capability with update-classified writes.
@unexported
final class ProtobufPrinter private
  ( buffer0: scala.Array[Byte], growable: Boolean, deliver: (Data => Unit) | Null )
extends caps.ExclusiveCapability, caps.Stateful:

  // The raw JVM array appears here only, as the size table's scratch does.
  private var buffer: scala.Array[Byte] = buffer0
  private var position: Int = 0

  // The cast re-asserts the exclusivity the field's type forgets, as the size table's writes do:
  // the buffer is reached only through this (exclusive) printer.
  private inline def target: scala.Array[Byte]^ = buffer.asInstanceOf[scala.Array[Byte]^]

  // Room for `count` more bytes: in block mode the filled block is delivered first, and in
  // growable mode the buffer doubles. An exact-size printer is never short of room, since its
  // size was measured for exactly what it writes.
  // Inline, so that the common case — room enough — is one comparison at each write.
  private update inline def room(count: Int): Unit =
    if position + count > buffer.length then makeRoom(count)

  private update def makeRoom(count: Int): Unit =
    if deliver != null then flush()
    else if growable then
      var capacity = (buffer.length*2).max(16)
      while capacity < position + count do capacity *= 2
      val bigger = new scala.Array[Byte](capacity)
      System.arraycopy(buffer, 0, bigger, 0, position)
      buffer = bigger

  // In block mode, a write that a whole block could not hold.
  private update def oversized(count: Int): Boolean = deliver != null && count > buffer.length

  // In block mode, hands the bytes written so far to `deliver`, as a block of their own.
  update def flush(): Unit =
    if deliver != null && position > 0 then
      deliver.nn(ProtobufPrinter.copied(buffer, 0, position))
      position = 0

  // The bytes written, in exact-size or growable mode. An exact-size printer's array is
  // returned uncopied: nothing writes to it once its size is reached, as `Array.scribe`'s is.
  def data(): Data =
    if position == buffer.length then Array.unsafeFrozen(buffer)
    else ProtobufPrinter.copied(buffer, 0, position)

  update def byte(value: Int): Unit =
    room(1)
    target(position) = value.toByte
    position += 1

  // The common case — the bytes fit — is one comparison and a copy, kept small so that it
  // inlines where it is called; growing, flushing and oversized writes are out of line.
  update def raw(data: Data): Unit =
    val count = data.length

    if position + count <= buffer.length then
      System.arraycopy(Array.unsafeJvm(data), 0, target, position, count)
      position += count
    else rawOverflowing(data)

  private update def rawOverflowing(data: Data): Unit =
    if oversized(data.length) then
      flush()
      deliver.nn(data)
    else
      room(data.length)
      System.arraycopy(Array.unsafeJvm(data), 0, target, position, data.length)
      position += data.length

  // A length-delimited payload: its length prefix, then its bytes.
  update def chunk(data: Data): Unit =
    varint(data.length.toLong)
    raw(data)

  // A one-byte varint — a tag, a short length, a small number — is the common case, written as
  // the byte it is.
  // Kept small, so that it inlines where it is called; the longer encodings are out of line.
  update def varint(value: Long): Unit =
    if (value & ~0x7fL) == 0L then byte(value.toInt) else longVarint(value)

  private update def longVarint(value: Long): Unit =
    room(10)
    position = Scribe.varintInto(target, position, value)

  // The UTF-8 encoding of `string`, encoded straight into the output; its length, which a
  // printer measures first, is `ProtobufPrinter.utf8Length(string)`.
  update def utf8(string: String): Unit =
    val count = Scribe.utf8Length(string)

    if oversized(count) then
      flush()
      val bytes = Array.allocate[Byte](count)
      Scribe.utf8Into(bytes.raw, 0, string, 0)
      deliver.nn(Array.freeze(bytes))
    else
      room(count)
      position = Scribe.utf8Into(target, position, string, 0)

  // A string the measuring pass stashed (`ProtobufSizes.stashText`): its length prefix, then
  // its bytes copied from the table's scratch.
  update def stashedText(sizes: ProtobufSizes^): Unit =
    val offset = sizes.next()
    val length = sizes.next()
    varint(length.toLong)

    if oversized(length) then
      flush()
      deliver.nn(ProtobufPrinter.copied(sizes.scratchBytes, offset, length))
    else
      room(length)
      System.arraycopy(sizes.scratchBytes, offset, target, position, length)
      position += length

  update def fixed32(value: Int): Unit =
    var i = 0

    while i < 4 do
      byte((value >>> (i*8)) & 0xff)
      i += 1

  update def fixed64(value: Long): Unit =
    var i = 0

    while i < 8 do
      byte(((value >>> (i*8)) & 0xff).toInt)
      i += 1

  update def tag(number: Int, wireType: WireType): Unit = varint((number.toLong << 3) | wireType.id)

  update def field(number: Int, value: Protobuf): Unit = value match
    case Protobuf.Absent           => ()
    case Protobuf.Repeated(values) => values.each(field(number, _))

    case Protobuf.Wire(wireType, bytes) =>
      tag(number, wireType)
      if wireType == WireType.Len then varint(bytes.length)
      raw(bytes)

@unexported
object ProtobufPrinter:
  // `length` bytes of `source` from `offset`, copied out as a frozen array of their own.
  private[locomotion] def copied(source: scala.Array[Byte], offset: Int, length: Int): Data =
    val bytes = Array.allocate[Byte](length)
    System.arraycopy(source, offset, bytes.raw, 0, length)
    Array.freeze(bytes)

  // A printer of exactly `size` bytes, whose `data()` is that array, uncopied.
  def exact(size: Int): ProtobufPrinter^ = new ProtobufPrinter(new scala.Array[Byte](size), false, null)

  // A printer of bytes of unknown length, from a buffer of `hint` bytes.
  def growing(hint: Int): ProtobufPrinter^ =
    new ProtobufPrinter(new scala.Array[Byte](hint.max(16)), true, null)

  // A printer that hands each block of `size` bytes to `deliver` as it fills; `flush()` hands on
  // the last, partial one.
  def blocks(size: Int, consume deliver: Data => Unit): ProtobufPrinter^ =
    new ProtobufPrinter(new scala.Array[Byte](size.max(16)), false, deliver)

  // Reasserts the capability of a printer that travelled as a neutral carrier, for the
  // generated printers spliced into user modules; inline for the reason `ProtobufParser.of` is.
  inline def of(carrier: AnyRef): ProtobufPrinter^ = carrier.asInstanceOf[ProtobufPrinter^]

  // The LEB128 length of `value`, read as an unsigned 64-bit quantity: 1 to 10 bytes.
  def varintSize(value: Long): Int = Scribe.varintLength(value)

  // The length of `string`'s UTF-8 encoding, without encoding it.
  def utf8Length(string: String): Int = Scribe.utf8Length(string)

  // The length of a field's tag: one byte up to field number 15, as a varint thereafter.
  def tagSize(number: Int): Int = varintSize(number.toLong << 3)


  // The number of bytes `field(number, value)` writes, which it must mirror exactly: a scribe
  // of fixed size drops whatever is written past its end.
  def fieldSize(number: Int, value: Protobuf): Int = value match
    case Protobuf.Absent           => 0
    case Protobuf.Repeated(values) => values.fold(0)(_ + fieldSize(number, _))

    case Protobuf.Wire(wireType, bytes) =>
      val tag = varintSize((number.toLong << 3) | wireType.id)
      if wireType == WireType.Len then tag + varintSize(bytes.length) + bytes.length
      else tag + bytes.length

  // A scalar's wire bytes, written straight into an array of exactly their length.
  def varintData(value: Long): Data =
    val size = varintSize(value)
    val buffer = Array.allocate[Byte](size)
    var rest = value
    var index = 0

    repeat(size - 1):
      buffer(index) = ((rest & 0x7f) | 0x80).toByte
      rest = rest >>> 7
      index += 1

    buffer(index) = rest.toByte
    Array.freeze(buffer)

  // The low `width` bytes of `value`, little-endian.
  def fixedData(value: Long, width: Int): Data =
    val buffer = Array.allocate[Byte](width)
    var index = 0

    repeat(width):
      buffer(index) = (value >>> (index*8)).toByte
      index += 1

    Array.freeze(buffer)
