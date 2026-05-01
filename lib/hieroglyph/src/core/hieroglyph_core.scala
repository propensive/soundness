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
package hieroglyph

import language.experimental.pureFunctions

import anticipation.*
import contingency.*
import fulminate.*
import rudiments.*
import vacuous.*

extension (char: Char)
  def whitespace: Boolean = Character.isWhitespace(char)
  def control: Boolean = Character.isISOControl(char)
  def printable: Boolean = !control && !whitespace
  def name: Optional[Text] = Unicode.name(char)

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
  given strict: Tactic[CharDecodeError] => TextSanitizer = (position, encoding) =>
    raise(CharDecodeError(position, encoding)) yet '?'

  given skip: TextSanitizer = (position, encoding) => Unset
  given substitute: TextSanitizer = (position, encoding) => '?'

package communication:
  given unicodeCharNames: Char is Communicable = char => Message:
    val name =
      char.name.let { text => Unicode.smallCaps(text.s.toLowerCase.nn.tt) }.or("unknown".tt)

    if char.printable then s"$name [$char]".tt else name

extension (inline context: StringContext)
  transparent inline def enc(): Encoding = ${hieroglyph.internal.encoding('context)}
  transparent inline def ucs(): Char | Text = ${hieroglyph.internal.char('context)}

package textMetrics:
  given uniform: Char is Measurable = _ => 1
  given eastAsianScripts: Char is Measurable = Unicode.eastAsianWidth(_).let(_.width).or(1)
  given wideCharacterWidth: Char is Measurable = char => WideCharacterWidth.width(char.toInt).max(0)

extension (char: Char)
  def superscript: Optional[Char] = Chars.superscript.applyOrElse(char, _ => Unset)
  def subscript: Optional[Char] = Chars.subscript.applyOrElse(char, _ => Unset)
  def description: Optional[Text] = Unicode.name(char)
  def minuscule: Char = char.toLower
  def majuscule: Char = char.toUpper

extension (int: Int)
  def unicode: Text = String(Character.toChars(int)).tt

extension [measurable: Measurable](element: measurable) def metrics: Int = measurable.width(element)

final val Nul: '\u0000' = '\u0000'
final val Bel: '\u0007' = '\u0007'
final val Ht: '\u0009' = '\u0009'
final val Lf: '\u000a' = '\u000a'
final val Ff: '\u000c' = '\u000c'
final val Cr: '\u000d' = '\u000d'
final val Esc: '\u001b' = '\u001b'
final val Sqt: '\'' = '\''
final val Dqt: '"' = '"'
final val Bsl: '\\' = '\\'
final val Bs: '\u0008' = '\u0008'
