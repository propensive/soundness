/*
    Hieroglyph, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hieroglyph

import anticipation.*
import contingency.*
import vacuous.*

import language.experimental.captureChecking

extension (encoding: Encoding { type CanEncode = true }) def encoder: CharEncoder =
  CharEncoder(encoding)

package charDecoders:
  given (using TextSanitizer) => CharDecoder as utf8 = CharDecoder.unapply("UTF-8".tt).get
  given (using TextSanitizer) => CharDecoder as utf16 = CharDecoder.unapply("UTF-16".tt).get

  given (using TextSanitizer) => CharDecoder as utf16Le =
    CharDecoder.unapply("UTF-16LE".tt).get

  given (using TextSanitizer) => CharDecoder as utf16Be =
    CharDecoder.unapply("UTF-16BE".tt).get

  given (using TextSanitizer) => CharDecoder as ascii = CharDecoder.unapply("ASCII".tt).get

  given CharDecoder as iso88591 =
    CharDecoder.unapply("ISO-8859-1".tt)(using textSanitizers.skip).get

package charEncoders:
  given CharEncoder as utf8 = CharEncoder.unapply("UTF-8".tt).get
  given CharEncoder as utf16 = CharEncoder.unapply("UTF-16".tt).get
  given CharEncoder as utf16Le = CharEncoder.unapply("UTF-16LE".tt).get
  given CharEncoder as utf16Be = CharEncoder.unapply("UTF-16BE".tt).get
  given CharEncoder as ascii = CharEncoder.unapply("ASCII".tt).get
  given CharEncoder as iso88591 = CharEncoder.unapply("ISO-8859-1".tt).get

package textSanitizers:
  given strict(using charDecode: Tactic[CharDecodeError]): TextSanitizer =
    new TextSanitizer:
      def sanitize(pos: Int, encoding: Encoding): Char = raise(CharDecodeError(pos, encoding), '?')

  given TextSanitizer as skip:
    def sanitize(pos: Int, encoding: Encoding): Optional[Char] = Unset

  given TextSanitizer as substitute:
    def sanitize(pos: Int, encoding: Encoding): Optional[Char] = '?'

extension (inline context: StringContext)
  transparent inline def enc(): Encoding = ${Hieroglyph.encoding('context)}
  transparent inline def u(): Char | Text = ${Hieroglyph.char('context)}

package textMetrics:
  given TextMetrics as uniform:
    def width(text: Text): Int = text.s.length
    def width(char: Char): Int = 1

  given TextMetrics as eastAsianScripts:
    def width(text: Text): Int = text.s.foldLeft(0)(_ + width(_))
    def width(char: Char): Int = char.metrics

extension (char: Char)
  def metrics: Int = Unicode.eastAsianWidth(char).let(_.width).or(1)
  def superscript: Optional[Char] = Chars.superscript.applyOrElse(char, _ => Unset)
  def subscript: Optional[Char] = Chars.subscript.applyOrElse(char, _ => Unset)
  def description: Optional[Text] = Unicode.name(char)
  def lower: Char = char.toLower
  def upper: Char = char.toUpper
