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

import language.experimental.captureChecking

import scala.quoted.*

import anticipation.*
import fulminate.*
import rudiments.*

object Hieroglyph:
  given Realm = realm"hieroglyph"
  opaque type CharRange = Long

  object CharRange:
    def apply(from: Int, to: Int): CharRange = (from.toLong << 32) + to.toLong
    def apply(char: Char): CharRange = (char.toLong << 32) + char.toInt
    def apply(char: Int): CharRange = (char.toLong << 32) + char

    given CharRange is Textualizer = range => "${range.from}..${range.to}".tt

  given Ordering[CharRange] = Ordering.Long

  extension (range: CharRange)
    def from: Int = (range >> 32).toInt
    def to: Int = range.toInt
    def contains(char: Char): Boolean = char.toInt >= from && char.toInt <= to

  def encoding(contextExpr: Expr[StringContext])(using Quotes): Expr[Encoding] =
    import quotes.reflect.*

    val context: StringContext = contextExpr.valueOrAbort
    Encoding.unapply(context.parts.head.tt) match
      case None =>
        halt(m"the encoding ${context.parts.head.tt} was not available")

      case Some(encoding) =>
        if !encoding.charset.isRegistered
        then report.warning(
          s"hieroglyph: the encoding ${encoding.charset.displayName} is not an IANA-registered "+
              "encoding, and may not be universally available")

        val name = context.parts.head.toLowerCase.nn
        if encoding.charset.canEncode then '{Encoding.codecs(${Expr(name)}.tt)}
        else '{Encoding.decodeOnly(${Expr(name)}.tt)}

  def char(contextExpr: Expr[StringContext])(using Quotes): Expr[Char | Text] =
    val name: Text = contextExpr.valueOrAbort.parts.head.toUpperCase.nn.tt

    Unicode(name) match
      case char: Char => Expr(char)
      case text: Text => Expr(text)
      case _          => halt(m"the unicode character $name does not exist")
