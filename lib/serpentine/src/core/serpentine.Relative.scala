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
┃    Soundness, version 0.32.0.                                                                    ┃
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
package serpentine

import anticipation.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.compiletime.*

object Relative:
  given encodable: [relative <: Relative] => (navigable: Navigable)
        =>  relative is Encodable in Text = relative =>
    if relative.textDescent.isEmpty then
      if relative.ascent == 0 then navigable.selfText
      else List.fill(relative.ascent)(navigable.parentElement).join(navigable.separator)
    else
      relative
      . textDescent
      . reverse
      . join(navigable.ascent*relative.ascent, navigable.separator, t"")

  given showable: [element, root: Navigable by element] => (Relative by element) is Showable =
    encodable.encode(_)

  given decoder: [element] => (Navigable by element)
        => (Relative by element) is Decodable in Text =
    parse(_)

  def parse[element](using navigable: Navigable by element)(text: Text): Relative by element =
    def recur(start: Int, ascent: Int, elements: List[element]): Relative by element =
      if start >= text.length then Relative(ascent, elements)
      else
        val end = text.s.indexOf(navigable.separator.s, start).puncture(-1).or(text.length)
        val element = text.s.substring(start, end).nn.tt
        val start2 = end + navigable.separator.length

        if element == navigable.parentElement then
          if elements.isEmpty then recur(start2, ascent + 1, Nil)
          else recur(start2, ascent, elements.tail)
        else recur(start2, ascent, navigable.element(element) :: elements)

    if text == navigable.selfText then Relative(0, Nil) else recur(0, 0, Nil)

  def apply[element](using navigable: Navigable by element)(ascent0: Int, descent0: List[element])
  :     Relative by element =
    Relative.from[element](ascent0, descent0.map(navigable.makeElement(_)), navigable.separator)

  private def from[element](ascent0: Int, descent0: List[Text], separator: Text)
  :     Relative by element =
    new Relative(ascent0, descent0, separator):
      type Operand = element

  given addable: [element] => (Relative by element) is Addable by (Relative by element) into
          (Relative by element) =
    (left, right) =>
      def recur(ascent: Int, descent: List[Text], ascent2: Int): Relative by element =
        if ascent2 > 0 then
          if descent.isEmpty then recur(ascent + 1, Nil, ascent - 1)
          else recur(ascent, descent.tail, ascent - 1)
        else Relative.from(ascent, right.textDescent ++ descent, left.separator)

      recur(left.ascent, left.textDescent, right.ascent)

abstract class Relative(val ascent: Int, val textDescent: List[Text], val separator: Text)
extends Pathlike:
  type Operand

  def delta: Int = textDescent.length - ascent

  def parent: Relative =
    if textDescent.isEmpty then Relative.from(ascent + 1, Nil, separator)
    else Relative.from(ascent, textDescent.tail, separator)

  override def equals(that: Any): Boolean = that.asMatchable match
    case that: Relative => that.ascent == ascent && that.textDescent == textDescent
    case _              => false

  override def hashCode: Int = ascent*31 + textDescent.hashCode

  def on[platform: Navigable]: Relative by platform.Operand =
    Relative.parse(Relative.encodable.encode(this))

  @targetName("child")
  infix def / (element: Operand)(using navigable: Navigable by Operand): Relative by Operand =
    Relative.from(ascent, navigable.makeElement(element) :: textDescent, separator)
