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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.collection.mutable as scm

import anticipation.*
import rudiments.*
import vacuous.*

// Accumulates Protocol Buffers wire bytes. Varints are LEB128; fixed32/fixed64 are
// little-endian; a field is its tag varint followed by the value (length-prefixed
// for the length-delimited wire type).
class ProtobufPrinter():
  private val buffer = scm.ArrayBuilder.make[Byte]

  def byte(value: Int): Unit = buffer.addOne(value.toByte)

  def raw(data: Data): Unit =
    var i = 0

    while i < data.length do
      buffer.addOne(data(i))
      i += 1

  def varint(value: Long): Unit =
    var rest = value
    var continue = true

    while continue do
      val septet = (rest & 0x7f).toInt
      rest = rest >>> 7

      if rest != 0 then byte(septet | 0x80) else
        byte(septet)
        continue = false

  def fixed32(value: Int): Unit =
    var i = 0

    while i < 4 do
      byte((value >>> (i*8)) & 0xff)
      i += 1

  def fixed64(value: Long): Unit =
    var i = 0

    while i < 8 do
      byte(((value >>> (i*8)) & 0xff).toInt)
      i += 1

  def tag(number: Int, wireType: WireType): Unit = varint((number.toLong << 3) | wireType.id)

  def field(number: Int, value: Protobuf): Unit = value match
    case Protobuf.Absent           => ()
    case Protobuf.Repeated(values) => values.scala.foreach(field(number, _))

    case Protobuf.Wire(wireType, bytes) =>
      tag(number, wireType)
      if wireType == WireType.Len then varint(bytes.length)
      raw(bytes)

  def result: Data = buffer.result().immutable(using Unsafe)
