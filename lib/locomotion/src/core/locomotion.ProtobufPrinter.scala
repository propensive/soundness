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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import zephyrine.*

// Writes Protocol Buffers wire bytes into a `Producer[Data]`. Varints are LEB128; fixed32/fixed64
// are little-endian; a field is its tag varint followed by the value (length-prefixed for the
// length-delimited wire type). Drive it through `Protobuf.printed` (synchronous) or `Protobuf.emit`
// (streaming).
@unexported
// Holds the exclusive producer it drives, so the printer is itself a stateful
// capability with update-classified writes.
class ProtobufPrinter(out: (Producer.Bytes)^) extends caps.ExclusiveCapability, caps.Stateful:
  update def byte(value: Int): Unit = out.push(value.toByte)

  update def raw(data: Data): Unit = out.put(data)

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
    case Protobuf.Repeated(values) => values.stdlib.foreach(field(number, _))

    case Protobuf.Wire(wireType, bytes) =>
      tag(number, wireType)
      if wireType == WireType.Len then varint(bytes.length)
      raw(bytes)
