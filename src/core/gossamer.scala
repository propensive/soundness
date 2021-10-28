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

opaque type Txt = String

extension (value: Bytes)
  def uString: Txt = Txt(String(value.to(Array), "UTF-8"))
  def hex: Txt = Txt(value.map { b => String.format("\\u%04x", b.toInt).nn }.mkString)

extension (text: Txt)
  def s: String = text
  def bytes: IArray[Byte] = IArray.from(s.getBytes("UTF-8").nn)
  def length: Int = s.size
  def populated: Option[String] = if s.size == 0 then None else Some(s)
  def lower: Txt = s.toLowerCase.nn
  def upper: Txt = s.toUpperCase.nn
  def urlEncode: Txt = URLEncoder.encode(s, "UTF-8").nn
  def urlDecode: Txt = URLDecoder.decode(s, "UTF-8").nn
  def punycode: Txt = java.net.IDN.toASCII(s).nn
  def drop(n: Int): Txt = s.substring(n).nn
  def dropRight(n: Int): Txt = s.substring(0, 0 max (s.size - n)).nn
  def take(n: Int): Txt = s.substring(0, n).nn
  def trim: Txt = text.trim.nn
  def slice(index: Int): Txt = s.substring(index).nn
  def slice(from: Int, to: Int): Txt = s.substring(from, to).nn
  def chars: IArray[Char] = IArray.from(s.toCharArray.nn)
  def flatMap(fn: Char => Txt): Txt = String(s.toCharArray.nn.flatMap(fn(_).toCharArray.nn))
  def map(fn: Char => Char): Txt = String(s.toCharArray.nn.map(fn))
  def isEmpty: Boolean = s.isEmpty
  def cut(delimiter: Txt): List[Txt] = cut(delimiter, 0)
  def sub(from: Txt, to: Txt): Txt = text.replaceAll(Pattern.quote(from), to).nn
  def rsub(from: Txt, to: Txt): Txt = text.replaceAll(from, to).nn
  def startsWith(str: Txt): Boolean = text.startsWith(str)
  def endsWith(str: Txt): Boolean = text.endsWith(str)
  def sub(from: Char, to: Char): Txt = text.replace(from, to).nn
  def cut(delimiter: Txt, limit: Int): List[Txt] =
    s.split(Pattern.quote(delimiter), limit).nn.map(_.nn).to(List)

  def fit(width: Int, char: Char = ' '): Txt = (text + char.show*(width - text.length)).take(width)

  @targetName("add")
  infix def +(other: Txt): Txt = s+other

  @targetName("times")
  infix def *(n: Int): Txt = IArray.fill(n)(s).mkString
  
  def apply(idx: Int): Char throws OutOfRangeError =
    if idx >= 0 && idx < s.size then s.charAt(idx)
    else throw OutOfRangeError(idx, 0, s.size)

  def padRight(length: Int, char: Char = ' '): Txt = 
    if s.size < length then s+char.toString*(length - s.size) else s
  
  def padLeft(length: Int, char: Char = ' '): Txt =
    if s.size < length then char.toString*(length - s.size)+s else s

  def contains(substring: Txt): Boolean = text.contains(substring)
  def contains(char: Char): Boolean = text.indexOf(char) != -1

  @tailrec
  def indexWhere(pred: Char => Boolean, idx: Int = 0): Int throws OutOfRangeError =
    if idx >= text.length then throw OutOfRangeError(idx, 0, s.size)
    if pred(text.charAt(idx)) then idx else indexWhere(pred, idx + 1)

  def takeWhile(pred: Char => Boolean): Txt =
    try text.substring(0, indexWhere(!pred(_))).nn
    catch case OutOfRangeError(_, _, _) => text

  def lev(other: Txt): Int =
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
  def text: Txt = string

object Txt:
  given Ordering[Txt] = Ordering[String].on[Txt](_.s)

  given typeTest: Typeable[Txt] with
    def unapply(value: Any): Option[value.type & Txt] = value match
      case str: String => Some(str.asInstanceOf[value.type & Txt])
      case _           => None
    
  def apply(str: String): Txt = str

object Joinable:
  given Joinable[Txt] = xs => Txt(xs.mkString)

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
  case class Input(txt: Txt)

  given [T: Show]: Insertion[Input, T] = value => Input(summon[Show[T]].show(value))

  private def escape(str: String): Txt =
    val buf: StringBuffer = StringBuffer()
    
    def parseUnicode(chars: Txt): Char =
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
    
    gossamer.Txt(buf.toString)
      

  object Str extends Interpolator[Input, Txt, Txt]:
    def initial: Txt = gossamer.Txt("")
    def parse(state: Txt, next: String): Txt = state+escape(next)
    def skip(state: Txt): Txt = state
    def insert(state: Txt, input: Input): Txt = state+input.txt
    def complete(state: Txt): Txt = state
  
  object Txt extends Interpolator[Input, Txt, Txt]:
    def initial: Txt = gossamer.Txt("")
    def parse(state: Txt, next: String): Txt = state+escape(next)
    def skip(state: Txt): Txt = state
    def insert(state: Txt, input: Input): Txt = state+input.txt

    def complete(state: Txt): Txt =
      gossamer.Txt(state.s.split("\\n\\s*\\n").nn.map(_.nn.replaceAll("\\s\\s*", " ").nn.trim.nn
          ).mkString("\n").nn)
