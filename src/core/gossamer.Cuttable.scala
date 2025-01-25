/*
    Gossamer, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import anticipation.*
import denominative.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import vacuous.*

import scala.reflect.*

import java.util.regex.*

import language.experimental.pureFunctions
import language.experimental.into

object Cuttable:
  given [TextType: {Textual, Countable}] => TextType is Cuttable by Text =
    (text, delimiter, limit) =>
      val string = TextType.text(text).s
      val dLength = delimiter.s.length

      @tailrec
      def recur(start: Ordinal, results: List[TextType]): List[TextType] =
        TextType.indexOf
         (text, delimiter, start).lay(text.segment(start ~ (text.length - 1).z) :: results):
          index => recur(index + dLength, text.segment(start ~ (index - 1)) :: results)

      recur(Prim, Nil).reverse

  given [TextType: {Textual, Countable}] => TextType is Cuttable by Regex = (text, regex, limit) =>
    val string = TextType.text(text).s
    val matcher = Pattern.compile(regex.pattern.s).nn.matcher(string).nn

    @tailrec
    def recur(start: Ordinal, results: List[TextType]): List[TextType] =
      if matcher.find(start.n0)
      then
        val interval = Ordinal.zerary(matcher.start) ~ Ordinal.zerary(matcher.end)
        recur(Ordinal.zerary(matcher.end), text.segment(interval) :: results)
      else results

    recur(Prim, Nil).reverse

  given Text is Cuttable by Text = (text, delimiter, limit) =>
    text.s.split(Pattern.quote(delimiter.s), limit).nn.map(_.nn.tt).to(List)

  given Text is Cuttable by Regex = (text, regex, limit) =>
    text.s.split(regex.pattern.s, limit).nn.map(_.nn.tt).to(List)

  given [TextType] => (cuttable: TextType is Cuttable by Text) => TextType is Cuttable by Char =
    (text, delimiter, limit) =>
      cuttable.cut(text, delimiter.toString.tt, limit)

trait Cuttable:
  type Self
  type Operand
  def cut(value: Self, delimiter: Operand, limit: Int): List[Self]
