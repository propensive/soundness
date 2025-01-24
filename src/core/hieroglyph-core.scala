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

import language.experimental.pureFunctions

extension (encoding: Encoding { type CanEncode = true }) def encoder: CharEncoder =
  CharEncoder(encoding)

package charDecoders:
  given utf8: TextSanitizer => CharDecoder = CharDecoder.unapply("UTF-8".tt).get
  given utf16: TextSanitizer => CharDecoder = CharDecoder.unapply("UTF-16".tt).get

  given utf16Le: TextSanitizer => CharDecoder =
    CharDecoder.unapply("UTF-16LE".tt).get

  given utf16Be: TextSanitizer => CharDecoder =
    CharDecoder.unapply("UTF-16BE".tt).get

  given ascii: TextSanitizer => CharDecoder = CharDecoder.unapply("ASCII".tt).get

  given iso88591: CharDecoder =
    CharDecoder.unapply("ISO-8859-1".tt)(using textSanitizers.skip).get

package charEncoders:
  given utf8: CharEncoder = CharEncoder.unapply("UTF-8".tt).get
  given utf16: CharEncoder = CharEncoder.unapply("UTF-16".tt).get
  given utf16Le: CharEncoder = CharEncoder.unapply("UTF-16LE".tt).get
  given utf16Be: CharEncoder = CharEncoder.unapply("UTF-16BE".tt).get
  given ascii: CharEncoder = CharEncoder.unapply("ASCII".tt).get
  given iso88591: CharEncoder = CharEncoder.unapply("ISO-8859-1".tt).get

package textSanitizers:
  given strict: Tactic[CharDecodeError] => TextSanitizer = (pos, encoding) =>
    raise(CharDecodeError(pos, encoding), '?')

  given skip: TextSanitizer = (pos, encoding) => Unset
  given substitute: TextSanitizer = (pos, encoding) => '?'

extension (inline context: StringContext)
  transparent inline def enc(): Encoding = ${Hieroglyph.encoding('context)}
  transparent inline def u(): Char | Text = ${Hieroglyph.char('context)}

package textMetrics:
  given uniform: TextMetrics:
    def width(text: Text): Int = text.s.length
    def width(char: Char): Int = 1

  given eastAsianScripts: TextMetrics:
    def width(text: Text): Int = text.s.foldLeft(0)(_ + width(_))
    def width(char: Char): Int = char.metrics

extension (char: Char)
  def metrics: Int = Unicode.eastAsianWidth(char).let(_.width).or(1)
  def superscript: Optional[Char] = Chars.superscript.applyOrElse(char, _ => Unset)
  def subscript: Optional[Char] = Chars.subscript.applyOrElse(char, _ => Unset)
  def description: Optional[Text] = Unicode.name(char)
  def lower: Char = char.toLower
  def upper: Char = char.toUpper
