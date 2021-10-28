/*
    Gossamer, version 0.5.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

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
import wisteria.*

import annotation.*

import scala.reflect.*
import java.util.regex.*
import java.net.{URLEncoder, URLDecoder}

opaque type Text = String

extension (value: Bytes)
  def uString: Text = Text(String(value.to(Array), "UTF-8"))
  def hex: Text = Text(value.map { b => String.format("\\u%04x", b.toInt).nn }.mkString)

extension (text: Text)
  def s: String = text
  def bytes: IArray[Byte] = IArray.from(s.getBytes("UTF-8").nn)
  def length: Int = s.size
  def populated: Option[Text] = if s.size == 0 then None else Some(s)
  def lower: Text = s.toLowerCase.nn
  def upper: Text = s.toUpperCase.nn
  def urlEncode: Text = URLEncoder.encode(s, "UTF-8").nn
  def urlDecode: Text = URLDecoder.decode(s, "UTF-8").nn
  def punycode: Text = java.net.IDN.toASCII(s).nn
  def drop(n: Int): Text = s.substring(n).nn
  def dropRight(n: Int): Text = s.substring(0, 0 max (s.size - n)).nn
  def take(n: Int): Text = s.substring(0, n).nn
  def trim: Text = text.trim.nn
  def slice(index: Int): Text = s.substring(index).nn
  def slice(from: Int, to: Int): Text = s.substring(from, to).nn
  def chars: IArray[Char] = IArray.from(s.toCharArray.nn)
  def flatMap(fn: Char => Text): Text = String(s.toCharArray.nn.flatMap(fn(_).toCharArray.nn))
  def map(fn: Char => Char): Text = String(s.toCharArray.nn.map(fn))
  def isEmpty: Boolean = s.isEmpty
  def cut(delimiter: Text): List[Text] = cut(delimiter, 0)
  def sub(from: Text, to: Text): Text = text.replaceAll(Pattern.quote(from), to).nn
  def rsub(from: Text, to: Text): Text = text.replaceAll(from, to).nn
  def startsWith(str: Text): Boolean = text.startsWith(str)
  def endsWith(str: Text): Boolean = text.endsWith(str)
  def sub(from: Char, to: Char): Text = text.replace(from, to).nn
  def cut(delimiter: Text, limit: Int): List[Text] =
    s.split(Pattern.quote(delimiter), limit).nn.map(_.nn).to(List)

  def fit(width: Int, char: Char = ' '): Text = (text + char.show*(width - text.length)).take(width)

  @targetName("add")
  infix def +(other: Text): Text = s+other

  @targetName("times")
  infix def *(n: Int): Text = IArray.fill(n)(s).mkString
  
  def apply(idx: Int): Char throws OutOfRangeError =
    if idx >= 0 && idx < s.size then s.charAt(idx)
    else throw OutOfRangeError(idx, 0, s.size)

  def padRight(length: Int, char: Char = ' '): Text = 
    if s.size < length then s+char.toString*(length - s.size) else s
  
  def padLeft(length: Int, char: Char = ' '): Text =
    if s.size < length then char.toString*(length - s.size)+s else s

  def contains(substring: Text): Boolean = text.contains(substring)
  def contains(char: Char): Boolean = text.indexOf(char) != -1

  @tailrec
  def indexWhere(pred: Char => Boolean, idx: Int = 0): Int throws OutOfRangeError =
    if idx >= text.length then throw OutOfRangeError(idx, 0, s.size)
    if pred(text.charAt(idx)) then idx else indexWhere(pred, idx + 1)

  def takeWhile(pred: Char => Boolean): Text =
    try text.substring(0, indexWhere(!pred(_))).nn
    catch case OutOfRangeError(_, _, _) => text

  def lev(other: Text): Int =
    val m = s.size
    val n = other.size
    val old = new Array[Int](n + 1)
    val dist = new Array[Int](n + 1)

    for j <- 1 to n do old(j) = old(j - 1) + 1
    
    for i <- 1 to m do
      dist(0) = old(0) + 1

      for j <- 1 to n do
        dist(j) = (old(j - 1) + (if s.charAt(i - 1) == other.charAt(j - 1) then 0 else 1))
          .min(old(j) + 1).min(dist(j - 1) + 1)

      for j <- 0 to n do old(j) = dist(j)
    
    dist(n)

extension (string: String)
  def text: Text = string

object Text:
  given Ordering[Text] = Ordering[String].on[Text](_.s)

  given typeTest: Typeable[Text] with
    def unapply(value: Any): Option[value.type & Text] = value match
      case str: String => Some(str.asInstanceOf[value.type & Text])
      case _           => None
    
  def apply(str: String): Text = str

object Joinable:
  given Joinable[Text] = xs => Text(xs.mkString)

trait Joinable[T]:
  def join(elements: Iterable[T]): T

extension [T](values: Iterable[T])(using joinable: Joinable[T])
  def join: T = joinable.join(values)
  
  def join(separator: T): T =
    joinable.join(values.flatMap(Iterable(separator, _)).drop(1))
  
  def join(left: T, separator: T, right: T): T =
    Iterable(left, join(separator), right).join
  
  def join(separator: T, penultimate: T): T = values.size match
    case 0 => Iterable().join
    case 1 => values.head
    case _ => Iterable(values.init.join(separator), penultimate, values.last).join
  
  def join(left: T, separator: T, penultimate: T, right: T): T =
    Iterable(left, join(separator, penultimate), right).join

case class OutOfRangeError(idx: Int, from: Int, to: Int)
extends Exception(s"gossamer: the index $idx exceeds the range $from-$to")

trait Shown[T](using Show[T]):
  this: T =>
    override def toString: String = summon[Show[T]].show(this).s

object Interpolation:
  case class Input(txt: Text)

  given [T: Show]: Insertion[Input, T] = value => Input(summon[Show[T]].show(value))

  private def escape(str: String): Text =
    val buf: StringBuffer = StringBuffer()
    
    def parseUnicode(chars: Text): Char =
      if chars.length < 4 then throw InterpolationError("the unicode escape is incomplete")
      else Integer.parseInt(chars.s, 16).toChar

    @tailrec
    def recur(cur: Int = 0, esc: Boolean = false): Unit =
      if cur < str.length
      then
        str.s.charAt(cur) match
          case '\\' if !esc => recur(cur + 1, true)
          case '\\'         => buf.append('\\')
                               recur(cur + 1, false)
          case 'n' if esc   => buf.append('\n')
                               recur(cur + 1)
          case 'r' if esc   => buf.append('\r')
                               recur(cur + 1)
          case 'f' if esc   => buf.append('\f')
                               recur(cur + 1)
          case 'b' if esc   => buf.append('\b')
                               recur(cur + 1)
          case 't' if esc   => buf.append('\t')
                               recur(cur + 1)
          case 'u' if esc   => buf.append(parseUnicode(str.slice(cur + 1, cur + 5)))
                               recur(cur + 4)
          case 'e' if esc   => buf.append('\u001b')
                               recur(cur + 1)
          case '"' if esc   => buf.append('"')
                               recur(cur + 1)
          case '\'' if esc  => buf.append('\'')
                               recur(cur + 1)
          case ch if esc    => throw InterpolationError(
                                   s"the character '$ch' does not need to be escaped".s)
          case ch           => buf.append(ch)
                               recur(cur + 1)
      else if esc then throw InterpolationError("the final character cannot be an escape")
    
    recur()
    
    gossamer.Text(buf.toString)
      

  object T extends Interpolator[Input, Text, Text]:
    def initial: Text = gossamer.Text("")
    def parse(state: Text, next: String): Text = state+escape(next)
    def skip(state: Text): Text = state
    def insert(state: Text, input: Input): Text = state+input.txt
    def complete(state: Text): Text = state
  
  object Text extends Interpolator[Input, Text, Text]:
    def initial: Text = gossamer.Text("")
    def parse(state: Text, next: String): Text = state+escape(next)
    def skip(state: Text): Text = state
    def insert(state: Text, input: Input): Text = state+input.txt

    def complete(state: Text): Text =
      gossamer.Text(state.s.split("\\n\\s*\\n").nn.map(_.nn.replaceAll("\\s\\s*", " ").nn.trim.nn
          ).mkString("\n").nn)
