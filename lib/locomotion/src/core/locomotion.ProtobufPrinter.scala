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

// Writes Protocol Buffers wire bytes into a `Scribe[Byte]`. Varints are LEB128; fixed32/fixed64
// are little-endian; a field is its tag varint followed by the value (length-prefixed for the
// length-delimited wire type). A message's encoded children are already in hand, so its size is
// known before it is written (`fieldSize`), and `Protobuf` writes it into an exact-size array.
@unexported
// Holds the exclusive scribe it drives, so the printer is itself a stateful
// capability with update-classified writes.
class ProtobufPrinter(out: Scribe[Byte]) extends caps.ExclusiveCapability, caps.Stateful:
  update def byte(value: Int): Unit = out.append(value.toByte)

  update def raw(data: Data): Unit = out.append(data)

  update def varint(value: Long): Unit =
    var rest = value
    var continue = true

    while continue do
      val septet = (rest & 0x7f).toInt
      rest = rest >>> 7

      if rest != 0 then byte(septet | 0x80) else
        byte(septet)
        continue = false

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
  // The LEB128 length of `value`, read as an unsigned 64-bit quantity: 1 to 10 bytes.
  def varintSize(value: Long): Int =
    (63 - java.lang.Long.numberOfLeadingZeros(value | 1))/7 + 1

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
