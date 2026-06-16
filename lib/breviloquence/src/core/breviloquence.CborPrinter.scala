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
package breviloquence

import anticipation.*
import parasite.*
import rudiments.*
import vacuous.*
import zephyrine.*

object CborPrinter:
  def encode(cbor: Cbor.Ast): Data =
    Producer.collect[Data](): producer =>
      write(producer, cbor)

  // Stream the encoded bytes incrementally; the producing code runs on a separate fiber.
  def emit(cbor: Cbor.Ast)(using Monitor, Probate): Iterator[Data] =
    val producer = Producer[Data](4096)

    async:
      write(producer, cbor)
      producer.finish()

    producer.iterator

  private def head(out: Producer.Bytes, major: Int, value: Long): Unit =
    val majorBits = major << 5

    if value < 0 then
      out.push((majorBits | 27).toByte)
      u64(out, value)
    else if value < 24 then
      out.push((majorBits | value.toInt).toByte)
    else if value < (1 << 8) then
      out.push((majorBits | 24).toByte)
      out.push(value.toByte)
    else if value < (1 << 16) then
      out.push((majorBits | 25).toByte)
      u16(out, value.toInt)
    else if value < (1L << 32) then
      out.push((majorBits | 26).toByte)
      u32(out, value)
    else
      out.push((majorBits | 27).toByte)
      u64(out, value)

  private def u16(out: Producer.Bytes, value: Int): Unit =
    out.push(((value >>> 8) & 0xFF).toByte)
    out.push((value & 0xFF).toByte)

  private def u32(out: Producer.Bytes, value: Long): Unit =
    out.push(((value >>> 24) & 0xFF).toByte)
    out.push(((value >>> 16) & 0xFF).toByte)
    out.push(((value >>> 8) & 0xFF).toByte)
    out.push((value & 0xFF).toByte)

  private def u64(out: Producer.Bytes, value: Long): Unit =
    out.push(((value >>> 56) & 0xFF).toByte)
    out.push(((value >>> 48) & 0xFF).toByte)
    out.push(((value >>> 40) & 0xFF).toByte)
    out.push(((value >>> 32) & 0xFF).toByte)
    out.push(((value >>> 24) & 0xFF).toByte)
    out.push(((value >>> 16) & 0xFF).toByte)
    out.push(((value >>> 8) & 0xFF).toByte)
    out.push((value & 0xFF).toByte)

  private def write(out: Producer.Bytes, cbor: Cbor.Ast): Unit =
    if cbor.isInteger then
      val long = cbor.asInstanceOf[Long]

      if long >= 0 then head(out, 0, long)
      else head(out, 1, -1L - long)

    else if cbor.isFloat then
      out.push((0xE0 | 27).toByte)
      u64(out, java.lang.Double.doubleToLongBits(cbor.asInstanceOf[Double]))

    else if cbor.isTextString then
      val text = cbor.asInstanceOf[String]
      val bytes = text.getBytes("UTF-8").nn
      head(out, 3, bytes.length.toLong)
      out.put(bytes.immutable(using Unsafe))

    else if cbor.isByteString then
      val bytes = cbor.asInstanceOf[Array[Byte]]
      head(out, 2, bytes.length.toLong)
      out.put(bytes.immutable(using Unsafe))

    else if cbor.isBoolean then
      out.push(if cbor.asInstanceOf[Boolean] then 0xF5.toByte else 0xF4.toByte)

    else if cbor.nullary then
      out.push(0xF6.toByte)
    else if cbor.unset then
      out.push(0xF7.toByte)

    else if cbor.isTag then
      val tag = cbor.asInstanceOf[Cbor.Tag]
      head(out, 6, tag.tag)
      write(out, tag.value.asInstanceOf[Cbor.Ast])

    else if cbor.isArray then
      val count = cbor.elements
      head(out, 4, count.toLong)
      var index = 0

      while index < count do
        write(out, cbor.element(index))
        index += 1

    else if cbor.isMap then
      val count = cbor.entries
      head(out, 5, count.toLong)
      var index = 0

      while index < count do
        write(out, cbor.key(index))
        write(out, cbor.value(index))
        index += 1

  def diagnostic(cbor: Cbor.Ast): String =
    val builder = new java.lang.StringBuilder
    append(builder, cbor)
    builder.toString

  private def append(builder: java.lang.StringBuilder, cbor: Cbor.Ast): Unit =
    if cbor.isInteger then builder.append(cbor.asInstanceOf[Long].toString)
    else if cbor.isFloat then
      val double = cbor.asInstanceOf[Double]

      if double.isNaN then builder.append("NaN")
      else if double == Double.PositiveInfinity then builder.append("Infinity")
      else if double == Double.NegativeInfinity then builder.append("-Infinity")
      else builder.append(double.toString)

    else if cbor.isTextString then
      builder.append('"')
      val text = cbor.asInstanceOf[String]
      var index = 0

      while index < text.length do builder.append:
        text.charAt(index) match
          case '"'                 => "\\\""
          case '\\'                => "\\\\"
          case '\n'                => "\\n"
          case '\r'                => "\\r"
          case '\t'                => "\\t"
          case char if char < 0x20 => f"\\u${char.toInt}%04x"
          case char                => char

        index += 1

      builder.append('"')

    else if cbor.isByteString then
      val bytes = cbor.asInstanceOf[Array[Byte]]
      builder.append("h'")
      var index = 0

      while index < bytes.length do
        builder.append(f"${bytes(index) & 0xFF}%02x")
        index += 1

      builder.append('\'')

    else if cbor.isBoolean then
      builder.append(cbor.asInstanceOf[Boolean].toString)
    else if cbor.nullary then
      builder.append("null")
    else if cbor.unset then
      builder.append("undefined")

    else if cbor.isTag then
      val tag = cbor.asInstanceOf[Cbor.Tag]
      builder.append(tag.tag.toString)
      builder.append('(')
      append(builder, tag.value.asInstanceOf[Cbor.Ast])
      builder.append(')')

    else if cbor.isArray then
      val count = cbor.elements
      builder.append('[')
      var index = 0

      while index < count do
        if index > 0 then builder.append(", ")
        append(builder, cbor.element(index))
        index += 1

      builder.append(']')

    else if cbor.isMap then
      val count = cbor.entries
      builder.append('{')
      var index = 0

      while index < count do
        if index > 0 then builder.append(", ")
        append(builder, cbor.key(index))
        builder.append(": ")
        append(builder, cbor.value(index))
        index += 1

      builder.append('}')
