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
import vacuous.*
import denominative.*

import language.experimental.captureChecking

object Presentational:
  given Text is Textual as text:
    type Show[ValueType] = ValueType is spectacular.Showable
    val classTag: ClassTag[Text] = summon[ClassTag[Text]]
    def show[ValueType](value: ValueType)(using show: Show[ValueType]): Text = show.text(value)
    def text(text: Text): Text = text
    def length(text: Text): Int = text.s.length
    def apply(text: Text): Text = text
    def map(text: Text, lambda: Char => Char): Text = Text(text.s.map(lambda))

    def range(text: Text, interval: Interval): Text =
      text.s.substring(interval.start.n0.max(0), interval.end.n0.min(length(text))).nn.tt

    def empty: Text = Text("")
    def concat(left: Text, right: Text): Text = Text(left.s+right.s)
    def unsafeChar(text: Text, index: Ordinal): Char = text.s.charAt(index.n0)

    def indexOf(text: Text, sub: Text, start: Ordinal): Optional[Ordinal] =
      text.s.indexOf(sub.s, start.n0).puncture(-1).let(Ordinal.zerary(_))

    def buffer(size: Optional[Int]): Buffer[Text] = TextBuffer(size)
    def size(text: Self): Int = text.length

trait Presentational:
  type Self
  type Show[ValueType]
  def show[ValueType](value: ValueType)(using show: Show[ValueType]): Self
  def apply(text: Text): Self
