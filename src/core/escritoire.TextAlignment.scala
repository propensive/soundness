/*
    Escritoire, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escritoire

import anticipation.*
import gossamer.*
import hieroglyph.*
import rudiments.*

object TextAlignment:
  object Right extends TextAlignment:
    def pad[TextType: Textual](text: TextType, width: Int, last: Boolean)(using TextMetrics)
    :     TextType =
      Textual(t" "*(width - text.metrics))+text

  object Left extends TextAlignment:
    def pad[TextType: Textual](text: TextType, width: Int, last: Boolean)(using TextMetrics)
    :     TextType =
      text+Textual(t" "*(width - text.metrics))

  object Center extends TextAlignment:
    def pad[TextType: Textual](text: TextType, width: Int, last: Boolean)(using TextMetrics)
    :     TextType =
      val space = width - text.metrics
      val before = Textual(t" "*(space/2))
      val after = Textual(t" "*(space - space/2))

      before+text+after

  object Justify extends TextAlignment:
    def pad[TextType: Textual](text: TextType, width: Int, last: Boolean)(using TextMetrics)
    :     TextType =
      if last then text+Textual(t" "*(width - text.metrics))
      else
        val words = text.cut(t" ")
        val wordCount = words.length
        val spare = width - words.sumBy(_.metrics)

        def recur(spare: Int, count: Int, done: TextType): TextType =
          if count == 0 then done+Textual(t" "*spare) else
            val space = spare/count
            recur(spare - space, count - 1, done+Textual(t" "*space)+words(wordCount - count))

        recur(spare, wordCount - 1, words.head)

trait TextAlignment:
  def pad[TextType: Textual](text: TextType, width: Int, last: Boolean)(using TextMetrics): TextType
