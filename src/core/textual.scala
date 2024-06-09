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
import anticipation.*

import language.experimental.captureChecking

object Presentational:
  given Text is Textual as text:
    type Show[-ValueType] = spectacular.Show[ValueType]
    val classTag: ClassTag[Text] = summon[ClassTag[Text]]
    def show[ValueType](value: ValueType)(using show: Show[ValueType]): Text = show.text(value)
    def text(text: Text): Text = text
    def length(text: Text): Int = text.s.length
    def apply(text: Text): Text = text
    def map(text: Text, lambda: Char => Char): Text = Text(text.s.map(lambda))
    def range(text: Text, start: Int, end: Int): Text = Text(text.s.substring(start, end).nn)
    def empty: Text = Text("")
    def concat(left: Text, right: Text): Text = Text(left.s+right.s)
    def unsafeChar(text: Text, index: Int): Char = text.s.charAt(index)
    def indexOf(text: Text, sub: Text): Int = text.s.indexOf(sub.s)

trait Presentational:
  type Self
  type Show[-ValueType]
  def show[ValueType](value: ValueType)(using show: Show[ValueType]): Self
  def apply(text: Text): Self

trait Textual extends Presentational:
  type Self
  def classTag: ClassTag[Self]
  def length(text: Self): Int
  def text(text: Self): Text
  def map(text: Self, lambda: Char => Char): Self
  def range(text: Self, start: Int, end: Int): Self
  def empty: Self
  def concat(left: Self, right: Self): Self
  def unsafeChar(text: Self, index: Int): Char
  def indexOf(text: Self, sub: Text): Int

  extension (left: Self)
    @targetName("mul")
    infix def * (right: Int): Self =
      def recur(text: Self, n: Int, acc: Self): Self =
        if n <= 0 then acc else recur(text, n - 1, concat(acc, text))

      recur(left, right.max(0), empty)

    @targetName("add")
    infix def + (right: Self): Self = concat(left, right)

object Textual:
  def apply[TextType: Textual](text: Text): TextType = TextType(text)

  // given String is Textual as string:
  //   type Show[-ValueType] = spectacular.Show[ValueType]
  //   val classTag: ClassTag[String] = summon[ClassTag[String]]
  //   def show[ValueType](value: ValueType)(using show: Show[ValueType]): String = show.text(value).s
  //   def text(string: String): Text = string.tt
  //   def length(string: String): Int = string.length
  //   def apply(text: Text): String = text.s
  //   def map(string: String, lambda: Char => Char): String = string.map(lambda)
  //   def range(string: String, start: Int, end: Int): String = string.substring(start, end).nn
  //   def empty: String = ""
  //   def concat(left: String, right: String): String = left+right
  //   def unsafeChar(string: String, index: Int): Char = string.charAt(index)
  //   def indexOf(string: String, sub: Text): Int = string.indexOf(sub.s)
