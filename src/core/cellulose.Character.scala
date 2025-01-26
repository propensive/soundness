/*
    Cellulose, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import anticipation.*
import denominative.*
import gossamer.*
import kaleidoscope.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

opaque type Character = Long

object Character:
  val End: Character = Long.MaxValue

  def apply(int: Int, line: Int, col: Int): Character =
    int.toLong | ((line.toLong&0xffffff) << 48) | ((col.toLong&0xffffff) << 24)

  given Character is Encodable in Text = new Encodable:
    type Self = Character
    type Format = Text
    def encode(char: Character): Text =
      if char == End then t"[END]" else t"[${char.char}:${char.line}:${char.column}]"

  given Decoder[Character] with
    def decode(text: Text): Character = text match
      case r"\[$char(.):${As[Int](l)}([0-9]+):${As[Int](c)}([0-9]+)\]" =>
        Character(char.at(Prim).vouch.toInt, l, c)

      case _ =>
        End

  given Typeable[Character] with
    def unapply(value: Any): Option[value.type & Character] = value.matchable(using Unsafe) match
      case char: Char => Some(value.asInstanceOf[value.type & Character])
      case _          => None

  erased given CanEqual[Char, Character] = ###
  erased given CanEqual[Character, Char] = ###

  extension (char: Character)
    def char: Char = if char == -1 then '\u0000' else char.toChar
    def line: Int = ((char >> 48) & 0xffffff).toInt
    def column: Int = if char == End then 0 else ((char >> 24) & 0xffffff).toInt
