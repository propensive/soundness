/*
    Gossamer, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import vacuous.*
import anticipation.*
import spectacular.*
import kaleidoscope.*

import scala.reflect.*

import java.util.regex.*

import language.experimental.pureFunctions
import language.experimental.into

object Cuttable:
  given [TextType: Textual] => Cuttable[TextType, Text] = (text, delimiter, limit) =>
    val string = TextType.text(text).s
    val dLength = delimiter.s.length

    @tailrec
    def recur(start: Int, results: List[TextType]): List[TextType] =
      string.indexOf(delimiter.s, start) match
        case -1    => TextType.range(text, start, text.length) :: results
        case index => recur(index + dLength, TextType.range(text, start, index) :: results)

    IArray.from(recur(0, Nil).reverse)(using TextType.classTag)

  given [TextType: Textual] => Cuttable[TextType, Regex] = (text, regex, limit) =>
    val string = TextType.text(text).s
    val matcher = Pattern.compile(regex.pattern.s).nn.matcher(string).nn

    @tailrec
    def recur(start: Int, results: List[TextType]): List[TextType] =
      if matcher.find(start)
      then recur(matcher.end, TextType.range(text, matcher.start, matcher.end) :: results)
      else results

    IArray.from(recur(0, Nil).reverse)(using TextType.classTag)

  given Cuttable[Text, Text] = (text, delimiter, limit) =>
    text.s.split(Pattern.quote(delimiter.s), limit).nn.map(_.nn.tt).immutable(using Unsafe)

  given Cuttable[Text, Regex] = (text, regex, limit) =>
    text.s.split(regex.pattern.s, limit).nn.map(_.nn.tt).immutable(using Unsafe)

  given [TextType](using cuttable: Cuttable[TextType, Text]): Cuttable[TextType, Char] = (text, delimiter, limit) =>
    cuttable.cut(text, delimiter.show, limit)

trait Cuttable[TextType, DelimiterType]:
  def cut(value: TextType, delimiter: DelimiterType, limit: Int): IArray[TextType]
