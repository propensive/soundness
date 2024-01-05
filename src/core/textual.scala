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
import spectacular.*
import anticipation.*
import symbolism.*

import language.experimental.captureChecking

erased trait DefaultTextType:
  type TextType

package defaultTextTypes:
  erased given text: DefaultTextType { type TextType = Text } = ###

trait Textual[TextType]:
  type ShowType[-ValueType]
  def length(text: TextType): Int
  def string(text: TextType): String
  def make(string: String): TextType
  def map(text: TextType, lambda: Char => Char): TextType
  def slice(text: TextType, start: Int, end: Int): TextType
  def empty: TextType
  def concat(left: TextType, right: TextType): TextType
  def unsafeChar(text: TextType, index: Int): Char
  def indexOf(text: TextType, sub: Text): Int
  def show[ValueType](value: ValueType)(using ShowType[ValueType]): TextType

  given times: Operator["*", TextType, Int] with
    type Result = TextType
    
    private def recur(text: TextType, n: Int, acc: TextType): TextType =
      if n == 0 then acc else recur(text, n - 1, concat(acc, text))

    inline def apply(left: TextType, right: Int): TextType =
      recur(left, right.max(0), empty)
  
  given add: ClosedOperator["+", TextType] = concat(_, _)

extension [TextType](left: TextType)(using textual: Textual[TextType])
  @targetName("times")
  def *(right: Int): TextType = textual.times(left, right)
  
  @targetName("plus")
  def +(right: TextType): TextType = textual.concat(left, right)

object Textual:
  given text: Textual[Text] with
    type ShowType[-ValueType] = Show[ValueType]
    def string(text: Text): String = text.s
    def length(text: Text): Int = text.s.length
    def make(string: String): Text = Text(string)
    def map(text: Text, lambda: Char => Char): Text = Text(text.s.map(lambda))
    def slice(text: Text, start: Int, end: Int): Text = Text(text.s.substring(start, end).nn)
    def empty: Text = Text("")
    def concat(left: Text, right: Text): Text = Text(left.s+right.s)
    def unsafeChar(text: Text, index: Int): Char = text.s.charAt(index)
    def indexOf(text: Text, sub: Text): Int = text.s.indexOf(sub.s)
    
    def show[ValueType](value: ValueType)(using show: Show[ValueType]): Text = show(value)
    
  // given string: Textual[String] with
  //   def string(string: String): String = string
  //   def length(string: String): Int = string.length
  //   def make(string: String): String = string
  //   def map(string: String, lambda: Char -> Char): String = string.map(lambda)
  //   def slice(string: String, start: Int, end: Int): String = string.substring(start, end).nn
  //   def empty: String = ""
  //   def concat(left: String, right: String): String = left+right
  //   def unsafeChar(string: String, index: Int): Char = string.charAt(index)
  //   def indexOf(string: String, sub: Text): Int = string.indexOf(sub.s)
