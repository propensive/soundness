/*
    Punctuation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package punctuation

import denominative.*
import escapade.*, escapes.*
import gossamer.*
import rudiments.*
import vacuous.*

case class BodyText(blocks: TextBlock*):
  def serialize(width: Int): Teletype = blocks.map(_.render(width)).join(e"\n\n")

case class TextBlock(indent: Int, text: Teletype):
  @targetName("add")
  infix def + (txt: Teletype): TextBlock = TextBlock(indent, text+txt)

  def render(width: Int): Teletype =
    def rest(text: Teletype, lines: List[Teletype]): List[Teletype] =
      if text.length == 0 then lines.reverse
      else
        try
          val pt = text.plain.where(_ == ' ', Ordinal.zerary(width - indent*2), Rtl).or(throw RangeError(0, 0, 0))
          rest(text.after(pt), text.before(pt) :: lines)
        catch case err: RangeError =>
          rest(text.skip(width - indent*2), text.keep(width - indent*2) :: lines)

    rest(text, Nil).map((e"  "*indent)+_).join(e"\n")
