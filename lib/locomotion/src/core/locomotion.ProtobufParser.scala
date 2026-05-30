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
import contingency.*

import ProtobufError.Reason

// Reads Protocol Buffers wire bytes. `fields` parses a whole message payload into a
// number-keyed map of (one or more) raw wire values, preserving repeats and unknown
// fields — the structure the message decoder looks fields up in by number.
class ProtobufParser(data: Data):
  private var pos: Int = 0

  def atEnd: Boolean = pos >= data.length

  def varint(): Long raises ProtobufError =
    var result = 0L
    var shift = 0
    var continue = true

    while continue do
      if pos >= data.length then abort(ProtobufError(Reason.Truncated))
      val byte = data(pos) & 0xff
      pos += 1
      if shift < 64 then result |= (byte.toLong & 0x7f) << shift
      shift += 7
      if (byte & 0x80) == 0 then continue = false

    result

  def fixed32(): Int raises ProtobufError =
    if pos + 4 > data.length then abort(ProtobufError(Reason.Truncated))
    var result = 0
    var i = 0

    while i < 4 do
      result |= (data(pos + i) & 0xff) << (i*8)
      i += 1

    pos += 4
    result

  def fixed64(): Long raises ProtobufError =
    if pos + 8 > data.length then abort(ProtobufError(Reason.Truncated))
    var result = 0L
    var i = 0

    while i < 8 do
      result |= (data(pos + i).toLong & 0xff) << (i*8)
      i += 1

    pos += 8
    result

  def slice(length: Int): Data raises ProtobufError =
    if length < 0 || pos + length > data.length then abort(ProtobufError(Reason.Truncated))
    val result = data.slice(pos, pos + length)
    pos += length
    result

  def fields(): Map[Int, List[Protobuf]] raises ProtobufError =
    val accumulator = scm.LinkedHashMap.empty[Int, scm.ListBuffer[Protobuf]]

    while !atEnd do
      val tag = varint().toInt
      val number = tag >>> 3
      val code = tag & 0x7

      val wireType =
        WireType.fromId(code).lest(ProtobufError(Reason.UnexpectedWireType(code)))

      val value = wireType match
        case WireType.Varint =>
          val start = pos
          varint()
          Protobuf.Wire(WireType.Varint, data.slice(start, pos))

        case WireType.I64 => Protobuf.Wire(WireType.I64, slice(8))
        case WireType.I32 => Protobuf.Wire(WireType.I32, slice(4))
        case WireType.Len => Protobuf.Wire(WireType.Len, slice(varint().toInt))

      accumulator.getOrElseUpdate(number, scm.ListBuffer()).addOne(value)

    accumulator.view.mapValues(_.to(List)).to(Map)

  // Splits a packed `repeated` payload (a single length-delimited field holding
  // concatenated scalar values) into one wire value per element. `wireType` is the
  // element encoding, supplied by the element's `Packable` instance.
  def packed(wireType: WireType): List[Protobuf] raises ProtobufError =
    val builder = List.newBuilder[Protobuf]

    while !atEnd do
      val value = wireType match
        case WireType.Varint =>
          val start = pos
          varint()
          Protobuf.Wire(WireType.Varint, data.slice(start, pos))

        case WireType.I64 => Protobuf.Wire(WireType.I64, slice(8))
        case WireType.I32 => Protobuf.Wire(WireType.I32, slice(4))
        case WireType.Len => Protobuf.Wire(WireType.Len, slice(varint().toInt))

      builder += value

    builder.result()
