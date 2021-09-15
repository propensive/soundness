/*
    Gossamer, version 0.5.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

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
import contextual.*

import java.util.regex.*
import java.net.{URLEncoder, URLDecoder}

import language.implicitConversions

opaque type Text = String

object Text:
  def apply(str: String): Text = str
  given Conversion[String, Text] = identity(_)
  given Conversion[Text, String] = identity(_)

  extension (text: Text)
    def +(other: Text) = text+other
    def slice(index: Int) = text.substring(index)
    def slice(from: Int, to: Int) = text.substring(from, to)
    def str: String = text
    def length = text.length
    
    def apply(idx: Int): Char exposes IndexExceedsRangeError =
      if idx >= 0 && idx < text.length then text(idx)
      else throw IndexExceedsRangeError(idx, 0, text.length)
    
    def populated: Option[String] = if text.isEmpty then None else Some(text)
    def cut(delimiter: String): IArray[String] = cut(delimiter, 0)
    
    def cut(delimiter: String, limit: Int): IArray[String] =
      IArray.from(text.split(Pattern.quote(delimiter), limit).nn.map(_.nn))
    
    def bytes: IArray[Byte] = IArray.from(text.getBytes("UTF-8").nn)
    def chars: IArray[Char] = IArray.from(text.toCharArray.nn)
    def urlEncode: Text = URLEncoder.encode(text, "UTF-8").nn
    def urlDecode: Text = URLDecoder.decode(text, "UTF-8").nn
    def lower: Text = text.toLowerCase.nn
    def upper: Text = text.toUpperCase.nn
  
    def padRight(length: Int, char: Char = ' '): String = 
      if text.length < length then text+char.toString*(length - text.length) else text
    
    def padLeft(length: Int, char: Char = ' '): String =
      if text.length < length then char.toString*(length - text.length)+text else text

extension (values: Iterable[Text])
  def join: Text = values.mkString
  def join(separator: Text): Text = values.mkString(separator)
  
  def join(left: Text, separator: Text, right: Text): Text =
    values.mkString(left, separator, right)
  
  def join(separator: Text, last: Text): Text = values.size match
    case 0 => ""
    case 1 => values.head
    case _ => values.init.mkString(separator)+last+values.last

case class IndexExceedsRangeError(idx: Int, from: Int, to: Int)
extends Exception(s"gossamer: the index $idx exceeds the range $from-$to")

trait Show[T]:
  def show(value: T): Text

object Show:
  given Show[Int] = num => Text(num.toString)
  given Show[Short] = num => Text(num.toString)
  given Show[Long] = num => Text(num.toString)
  given Show[Byte] = num => Text(num.toString)
  given Show[Boolean] = if _ then Text("true") else Text("false")

object Txt:
  case class Input(str: String)

  given [T: Show]: Insertion[Input, T] = value => Input(summon[Show[T]].show(value))

  object TxtInterpolator extends Interpolator[Input, String, Text]:
    def initial: String = ""
    def parse(state: String, next: String): String = state+next
    def skip(state: String): String = state
    def insert(state: String, input: Input): String = state+input

    def complete(state: String): Text =
      Text(state.split("\\n\\s*\\n").map(_.replaceAll("\\s\\s*", " ").trim).mkString("\n"))