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
package facsimile

import anticipation.*
import hieroglyph.*
import rudiments.*
import vacuous.*

object Cos:
  // The code points at which PDFDocEncoding (ISO 32000-2 Annex D.7) differs from Latin-1:
  // typographic accents at 0x18–0x1F and publishing characters at 0x80–0x9F, with the euro
  // sign at 0xA0.
  private val docEncodingLow: IArray[Char] = IArray
    ( '˘', 'ˇ', 'ˆ', '˙', '˝', '˛', '˚', '˜' )

  private val docEncodingHigh: IArray[Char] = IArray
    ( '•', '†', '‡', '…', '—', '–', 'ƒ', '⁄',
      '‹', '›', '−', '‰', '„', '“', '”', '‘',
      '’', '‚', '™', 'ﬁ', 'ﬂ', 'Ł', 'Œ', 'Š',
      'Ÿ', 'Ž', 'ı', 'ł', 'œ', 'š', 'ž', '�' )

  // Encodes text as a PDF text string: Latin-1 (a subset of PDFDocEncoding) when every
  // character fits, otherwise UTF-16BE with a byte-order mark, matching `decodeText`.
  private[facsimile] def encodeText(text: Text): Data =
    if text.s.forall(_ < 0x100) then
      val bytes = new Array[Byte](text.s.length)
      var i = 0

      while i < text.s.length do
        bytes(i) = text.s.charAt(i).toByte
        i += 1

      bytes.immutable(using Unsafe)
    else
      val body = charEncoders.utf16BeEncoder.encoded(text)
      val bytes = new Array[Byte](body.length + 2)
      bytes(0) = 0xfe.toByte
      bytes(1) = 0xff.toByte
      System.arraycopy(body.mutable(using Unsafe), 0, bytes, 2, body.length)
      bytes.immutable(using Unsafe)

  // A text string (ISO 32000-2 §7.9.2.2): UTF-16BE or UTF-8 by byte-order mark, otherwise
  // PDFDocEncoding.
  private[facsimile] def decodeText(bytes: Data): Text =
    if bytes.length >= 2 && (bytes(0) & 0xff) == 0xfe && (bytes(1) & 0xff) == 0xff
    then charDecoders.utf16BeDecoder.decoded(bytes.drop(2))
    else if bytes.length >= 3
            && (bytes(0) & 0xff) == 0xef && (bytes(1) & 0xff) == 0xbb && (bytes(2) & 0xff) == 0xbf
    then charDecoders.utf8Decoder.decoded(bytes.drop(3))
    else
      val chars = new Array[Char](bytes.length)
      var i = 0

      while i < bytes.length do
        val byte = bytes(i) & 0xff

        chars(i) =
          if byte >= 0x18 && byte <= 0x1f then docEncodingLow(byte - 0x18)
          else if byte >= 0x80 && byte <= 0x9f then docEncodingHigh(byte - 0x80)
          else if byte == 0xa0 then '€'
          else byte.toChar

        i += 1

      String(chars).tt

  extension (cos: Cos)
    def dictionary: Optional[Map[Text, Cos]] = cos match
      case Cos.Dictionary(entries) => entries
      case Cos.Body(entries, _)    => entries // a stream is its dictionary for lookup purposes
      case _                       => Unset

    // Dictionary lookup: a `null` value is equivalent to an absent key (ISO 32000-2 §7.3.9),
    // so internal code deals only in `Optional`.
    def apply(key: Text): Optional[Cos] = dictionary.let(_.at(key)).let:
      case Cos.Nil => Unset
      case other   => other

    def long: Optional[Long] = cos match
      case Cos.Integral(value) => value
      case _                   => Unset

    // Numeric reads accept either numeric type; the parsed distinction is kept for writing.
    def double: Optional[Double] = cos match
      case Cos.Integral(value) => value.toDouble
      case Cos.Real(value)     => value
      case _                   => Unset

    def name: Optional[Text] = cos match
      case Cos.Name(text) => text
      case _              => Unset

    def truth: Optional[Boolean] = cos match
      case Cos.Truth(value) => value
      case _                => Unset

    def chars: Optional[Data] = cos match
      case Cos.Chars(bytes) => bytes
      case _                => Unset

    // The string's content as text, interpreting its byte-order mark or PDFDocEncoding.
    def text: Optional[Text] = cos match
      case Cos.Chars(bytes) => decodeText(bytes)
      case _                => Unset

    def elements: Optional[List[Cos]] = cos match
      case Cos.Sequence(elements) => elements
      case _                      => Unset

// The COS ("Carousel Object System") object model: the eight basic object types of
// ISO 32000-2 §7.3, plus streams and indirect references. `Chars` keeps a string's raw bytes:
// interpreting them (PDFDocEncoding, UTF-16BE, dates) is a semantic decision deferred to the
// typed document layer. `Body` is a stream *locator* — its dictionary plus the file offset of
// the raw payload — not the payload itself, which can only be dereferenced through a `Pdf`.
enum Cos:
  case Nil
  case Truth(value: Boolean)
  case Integral(value: Long)
  case Real(value: Double)
  case Name(text: Text)
  case Chars(bytes: Data)
  case Sequence(elements: List[Cos])
  case Dictionary(entries: Map[Text, Cos])
  case Body(entries: Map[Text, Cos], start: Long)
  case Ref(number: Int, generation: Int)
