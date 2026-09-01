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
package facsimile

import anticipation.*
import contingency.*
import hieroglyph.*
import hypotenuse.*
import rudiments.*
import vacuous.*

// Serialises `Cos` values to the syntax the lexer reads (ISO 32000-2 §7.3), the inverse of
// `CosParser`. Strings are written as escaped literals; names re-escape the delimiter and
// whitespace bytes as `#xx`; reals drop trailing zeros and never use exponents. A `Cos.Body`
// is written as its dictionary alone — the caller writes the `stream … endstream` framing,
// since it owns the payload bytes and the recomputed `/Length`.
private[facsimile] object CosWriter:
  def write(cos: Cos): Data =
    val builder = DataBuilder()
    append(builder, cos)
    builder.result()

  def dictionaryBytes(entries: Map[Text, Cos]): Data =
    val builder = DataBuilder()
    dictionary(builder, entries)
    builder.result()

  private def bytes(builder: DataBuilder, text: String): Unit =
    var i = 0
    while i < text.length do
      builder += text.charAt(i).toByte
      i += 1

  private def append(builder: DataBuilder, cos: Cos): Unit =
    cos match
      case Cos.Nil          => bytes(builder, "null")
      case Cos.Truth(value) => bytes(builder, if value then "true" else "false")
      case Cos.Integral(n)  => bytes(builder, n.toString)
      case Cos.Real(n)      => bytes(builder, real(n))
      case Cos.Ref(n, g)    => bytes(builder, s"$n $g R")
      case Cos.Name(text)   => name(builder, text)
      case Cos.Chars(data)  => literal(builder, data)

      case Cos.Sequence(elements) =>
        builder += '['.toByte

        elements.each: element =>
          if ordinal.n0 > 0 then builder += ' '.toByte
          append(builder, element)

        builder += ']'.toByte

      case Cos.Dictionary(entries) =>
        dictionary(builder, entries)

      case Cos.Body(entries, _) =>
        dictionary(builder, entries)

  private[facsimile] def dictionary
    ( builder: DataBuilder, entries: Map[Text, Cos] )
  :   Unit =

    bytes(builder, "<<")

    entries.each: (key, value) =>
      builder += ' '.toByte
      name(builder, key)
      builder += ' '.toByte
      append(builder, value)

    bytes(builder, " >>")

  // A real with no trailing zeros and no exponent (PDF reals are plain decimals), by the
  // shortest round-tripping representation; a non-finite value (which PDF cannot express)
  // degrades to zero.
  private def real(value: Double): String =
    if value == value.toLong.toDouble then value.toLong.toString
    else safely(Decimal(value).text.s).or("0")

  private def name(builder: DataBuilder, text: Text): Unit =
    builder += '/'.toByte
    val raw = charEncoders.utf8Encoder.encoded(text)
    var i = 0

    while i < raw.length do
      val byte = raw.readUnchecked(i) & 0xff

      if byte < 0x21 || byte > 0x7e || CosLexer.delimiter(byte) || byte == '#' then
        builder += '#'.toByte
        builder += hexDigit(byte >> 4)
        builder += hexDigit(byte & 0xf)
      else builder += byte.toByte

      i += 1

  // A literal string with the mandatory escapes, and non-printable bytes as octal, so any
  // byte sequence round-trips.
  private def literal(builder: DataBuilder, data: Data): Unit =
    builder += '('.toByte
    var i = 0

    while i < data.length do
      val byte = data.readUnchecked(i) & 0xff

      byte match
        case '('  => bytes(builder, "\\(")
        case ')'  => bytes(builder, "\\)")
        case '\\' => bytes(builder, "\\\\")

        case _ =>
          if byte >= 0x20 && byte < 0x7f then builder += byte.toByte else
            builder += '\\'.toByte
            builder += ('0' + ((byte >> 6) & 0x7)).toByte
            builder += ('0' + ((byte >> 3) & 0x7)).toByte
            builder += ('0' + (byte & 0x7)).toByte

      i += 1

    builder += ')'.toByte

  private def hexDigit(value: Int): Byte =
    (if value < 10 then '0' + value else 'A' + value - 10).toByte
