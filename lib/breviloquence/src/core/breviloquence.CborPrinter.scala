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

import scala.collection.mutable.ArrayBuffer

object CborPrinter:

  // Canonical CBOR encoding (RFC 8949 §4.2): smallest representation, definite
  // lengths, deterministic head bytes. Map key ordering follows the AST's
  // insertion order (callers requiring lexicographic key sort must arrange it
  // before encoding).
  def encode(cbor: CborAst): IArray[Byte] =
    val buffer = ArrayBuffer.empty[Byte]
    write(buffer, cbor)
    val out = new Array[Byte](buffer.length)
    var i = 0
    while i < buffer.length do { out(i) = buffer(i); i += 1 }
    out.asInstanceOf[IArray[Byte]]

  private def writeHead(out: ArrayBuffer[Byte], major: Int, value: Long): Unit =
    val majorBits = major << 5
    if value < 0 then
      // Long-overflow case: emit as 8-byte payload using the raw bits, since
      // CBOR major types 0/1 allow values up to 2^64 - 1.
      out += (majorBits | 27).toByte
      writeUInt64(out, value)
    else if value < 24 then out += (majorBits | value.toInt).toByte
    else if value < (1 << 8) then
      out += (majorBits | 24).toByte
      out += value.toByte
    else if value < (1 << 16) then
      out += (majorBits | 25).toByte
      writeUInt16(out, value.toInt)
    else if value < (1L << 32) then
      out += (majorBits | 26).toByte
      writeUInt32(out, value)
    else
      out += (majorBits | 27).toByte
      writeUInt64(out, value)

  private def writeUInt16(out: ArrayBuffer[Byte], value: Int): Unit =
    out += ((value >>> 8) & 0xFF).toByte
    out += (value & 0xFF).toByte

  private def writeUInt32(out: ArrayBuffer[Byte], value: Long): Unit =
    out += ((value >>> 24) & 0xFF).toByte
    out += ((value >>> 16) & 0xFF).toByte
    out += ((value >>> 8) & 0xFF).toByte
    out += (value & 0xFF).toByte

  private def writeUInt64(out: ArrayBuffer[Byte], value: Long): Unit =
    out += ((value >>> 56) & 0xFF).toByte
    out += ((value >>> 48) & 0xFF).toByte
    out += ((value >>> 40) & 0xFF).toByte
    out += ((value >>> 32) & 0xFF).toByte
    out += ((value >>> 24) & 0xFF).toByte
    out += ((value >>> 16) & 0xFF).toByte
    out += ((value >>> 8) & 0xFF).toByte
    out += (value & 0xFF).toByte

  private def write(out: ArrayBuffer[Byte], cbor: CborAst): Unit =
    if cbor.isInteger then
      val v = cbor.asInstanceOf[Long]
      if v >= 0 then writeHead(out, 0, v)
      else writeHead(out, 1, -1L - v)
    else if cbor.isFloat then
      out += (0xE0 | 27).toByte // major 7, info 27 (double)
      writeUInt64(out, java.lang.Double.doubleToLongBits(cbor.asInstanceOf[Double]))
    else if cbor.isTextString then
      val s = cbor.asInstanceOf[String]
      val bytes = s.getBytes("UTF-8").nn
      writeHead(out, 3, bytes.length.toLong)
      var i = 0
      while i < bytes.length do { out += bytes(i); i += 1 }
    else if cbor.isByteString then
      val bytes = cbor.asInstanceOf[Array[Byte]]
      writeHead(out, 2, bytes.length.toLong)
      var i = 0
      while i < bytes.length do { out += bytes(i); i += 1 }
    else if cbor.isBoolean then
      out += (if cbor.asInstanceOf[Boolean] then 0xF5.toByte else 0xF4.toByte)
    else if cbor.isNull then out += 0xF6.toByte
    else if cbor.isAbsent then out += 0xF7.toByte
    else if cbor.isTag then
      val tag = cbor.asInstanceOf[CborTag]
      writeHead(out, 6, tag.tag)
      write(out, tag.value.asInstanceOf[CborAst])
    else if cbor.isArray then
      val n = cbor.arrayLength
      writeHead(out, 4, n.toLong)
      var i = 0
      while i < n do { write(out, cbor.arrayElement(i)); i += 1 }
    else if cbor.isMap then
      val n = cbor.mapSize
      writeHead(out, 5, n.toLong)
      var i = 0
      while i < n do
        write(out, cbor.mapKey(i))
        write(out, cbor.mapValue(i))
        i += 1

  // RFC 8949 §8 diagnostic notation. Byte strings render as h'…' (lowercase
  // hex), tags as `n(value)`, undefined as `undefined`. Maps and arrays use
  // brace/bracket-comma notation with no whitespace.
  def diagnostic(cbor: CborAst): String =
    val sb = new java.lang.StringBuilder
    appendDiagnostic(sb, cbor)
    sb.toString.nn

  private def appendDiagnostic(sb: java.lang.StringBuilder, cbor: CborAst): Unit =
    if cbor.isInteger then
      sb.append(cbor.asInstanceOf[Long].toString)
    else if cbor.isFloat then
      val d = cbor.asInstanceOf[Double]
      if d.isNaN then sb.append("NaN")
      else if d == Double.PositiveInfinity then sb.append("Infinity")
      else if d == Double.NegativeInfinity then sb.append("-Infinity")
      else sb.append(d.toString)
    else if cbor.isTextString then
      sb.append('"')
      val s = cbor.asInstanceOf[String]
      var i = 0
      while i < s.length do
        val c = s.charAt(i)
        c match
          case '"'  => sb.append("\\\"")
          case '\\' => sb.append("\\\\")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          case ch if ch < 0x20 => sb.append(f"\\u${ch.toInt}%04x")
          case ch   => sb.append(ch)
        i += 1
      sb.append('"')
    else if cbor.isByteString then
      val bytes = cbor.asInstanceOf[Array[Byte]]
      sb.append("h'")
      var i = 0
      while i < bytes.length do
        sb.append(f"${bytes(i) & 0xFF}%02x")
        i += 1
      sb.append('\'')
    else if cbor.isBoolean then
      sb.append(cbor.asInstanceOf[Boolean].toString)
    else if cbor.isNull then
      sb.append("null")
    else if cbor.isAbsent then
      sb.append("undefined")
    else if cbor.isTag then
      val tag = cbor.asInstanceOf[CborTag]
      sb.append(tag.tag.toString)
      sb.append('(')
      appendDiagnostic(sb, tag.value.asInstanceOf[CborAst])
      sb.append(')')
    else if cbor.isArray then
      val n = cbor.arrayLength
      sb.append('[')
      var i = 0
      while i < n do
        if i > 0 then sb.append(", ")
        appendDiagnostic(sb, cbor.arrayElement(i))
        i += 1
      sb.append(']')
    else if cbor.isMap then
      val n = cbor.mapSize
      sb.append('{')
      var i = 0
      while i < n do
        if i > 0 then sb.append(", ")
        appendDiagnostic(sb, cbor.mapKey(i))
        sb.append(": ")
        appendDiagnostic(sb, cbor.mapValue(i))
        i += 1
      sb.append('}')
