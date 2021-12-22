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

import scala.annotation.*
import scala.reflect.*
import scala.util.*

import java.util.regex.*
import java.net.{URLEncoder, URLDecoder}

opaque type Text = String
type TextStream = LazyList[Text throws StreamCutError]

extension (value: Bytes)
  def uString: Text = Text(String(value.to(Array), "UTF-8"))
  def hex: Text = Text(value.map { b => String.format("\\u%04x", b.toInt).nn }.mkString)
  def asString[Enc <: Encoding](using enc: Enc): String = Text(String(value.to(Array), enc.name))

extension (string: String)
  def text: Text = string

object Text:
  given CommandLineParser.FromString[Text] = identity(_)

  given (using Encoding): Readable[Text] with
    type E = ExcessDataError
    def read(value: DataStream) = value.slurp(1.mb).asString

  given Streamable[Text] = value => LazyList(value.bytes)

  given textReader(using enc: Encoding): Readable[LazyList[Text]] with
    type E = ExcessDataError
    def read(stream: DataStream) =
      
      def read(stream: DataStream, carried: Array[Byte] = Array.empty[Byte]): LazyList[Text] =
        if stream.isEmpty then LazyList()
        else
          // FIXME: constructing this new array may be unnecessarily costly.
          val buf = carried ++ stream.head.unsafeMutable
          val carry = enc.carry(buf)
          
          Text(String(buf, 0, buf.length - carry, enc.name.s)) #::
              read(stream.tail, buf.takeRight(carry))
      
      read(stream)

  extension (text: Text)
    def s: String = text
    def bytes: IArray[Byte] = s.getBytes("UTF-8").nn.unsafeImmutable
    def length: Int = s.length
    def populated: Option[Text] = if s.length == 0 then None else Some(s)
    def lower: Text = s.toLowerCase.nn
    def upper: Text = s.toUpperCase.nn
    def urlEncode: Text = URLEncoder.encode(s, "UTF-8").nn
    def urlDecode: Text = URLDecoder.decode(s, "UTF-8").nn
    def punycode: Text = java.net.IDN.toASCII(s).nn
    def drop(n: Int): Text = s.substring(n min length max 0).nn
    def dropRight(n: Int): Text = s.substring(0, 0 max (s.length - n) min length).nn
    def take(n: Int): Text = s.substring(0, n min length max 0).nn
    def takeRight(n: Int): Text = s.substring(0 max (s.length - n) min length, length).nn
    def trim: Text = text.trim.nn
    def slice(from: Int, to: Int): Text = s.substring(from max 0 min length, to min length max 0).nn
    def chars: IArray[Char] = s.toCharArray.nn.unsafeImmutable
    def map(fn: Char => Char): Text = String(s.toCharArray.nn.map(fn))
    def isEmpty: Boolean = s.isEmpty
    def cut(delimiter: Text): List[Text] = cut(delimiter, Int.MaxValue)
    def rsub(from: Text, to: Text): Text = text.replaceAll(from, to).nn
    def startsWith(str: Text): Boolean = text.startsWith(str)
    def endsWith(str: Text): Boolean = text.endsWith(str)
    def sub(from: Text, to: Text): Text = text.replaceAll(Pattern.quote(from), to).nn
    def tr(from: Char, to: Char): Text = text.replace(from, to).nn
    def dashed: Text = Text(camelCaseWords.mkString("-"))
    def capitalize: Text = take(1).upper+drop(1)
    def reverse: Text = Text(String(s.toCharArray.nn.reverse))

    def flatMap(fn: Char => Text): Text =
      String(s.toCharArray.nn.unsafeImmutable.flatMap(fn(_).s.toCharArray.nn.unsafeImmutable).asInstanceOf[Array[Char]])
    
    def dropWhile(pred: Char => Boolean): Text =
      try s.substring(0, where(!pred(_))).nn catch case err: OutOfRangeError => Text("")

    def snip(n: Int): (Text, Text) =
      (s.substring(0, n min s.length).nn, s.substring(n min s.length).nn)
    
    def snipWhere(pred: Char => Boolean, idx: Int = 0): (Text, Text) throws OutOfRangeError =
      snip(where(pred, idx))

    def camelCaseWords: List[Text] =
      (try text.where(_.isUpper, 1) catch case error: OutOfRangeError => -1) match
        case -1 => List(text.lower)
        case i  => text.take(i).lower :: text.drop(i).camelCaseWords


    def cut(delimiter: Text, limit: Int): List[Text] =
      List(s.split(Pattern.quote(delimiter), limit).nn.map(_.nn)*)

    def fit(width: Int, char: Char = ' '): Text =
      (text + Text(s"$char")*(width - text.length)).take(width)
    
    def fitRight(width: Int, char: Char = ' '): Text =
      (Text(s"$char")*(width - text.length) + text).takeRight(width)

    @targetName("add")
    infix def +(other: Text): Text = s+other

    @targetName("times")
    infix def *(n: Int): Text = IArray.fill(n)(s).mkString
    
    def apply(idx: Int): Char throws OutOfRangeError =
      if idx >= 0 && idx < s.length then s.charAt(idx)
      else throw OutOfRangeError(idx, 0, s.length)

    def padRight(length: Int, char: Char = ' '): Text = 
      if s.length < length then s+s"$char"*(length - s.length) else s
    
    def padLeft(length: Int, char: Char = ' '): Text =
      if s.length < length then s"$char"*(length - s.length)+s else s

    def contains(substring: Text): Boolean = text.contains(substring)
    def contains(char: Char): Boolean = text.indexOf(char) != -1

    @tailrec
    def where(pred: Char => Boolean, idx: Int = 0): Int throws OutOfRangeError =
      if idx >= text.length then throw OutOfRangeError(idx, 0, s.length)
      if pred(text.charAt(idx)) then idx else where(pred, idx + 1)

    def lastWhere(pred: Char => Boolean, idx: Int = text.length - 1): Int throws OutOfRangeError =
      if idx < 0 then throw OutOfRangeError(idx, 0, s.length)
      if pred(text.charAt(idx)) then idx else lastWhere(pred, idx - 1)

    def upto(pred: Char => Boolean): Text =
      try text.substring(0, where(!pred(_))).nn
      catch case e: OutOfRangeError => text

    def lev(other: Text): Int =
      val m = s.length
      val n = other.length
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
  
  given Ordering[Text] = Ordering.String.on[Text](_.s)

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

case class Showable[T](value: T):
  def show: Text = Text(value.toString)

trait Shown[T](using Show[T]):
  this: T =>
    override def toString(): String = summon[Show[T]].show(this).s

object Interpolation:
  case class Input(txt: Text)

  given [T: Show]: Insertion[Input, T] = value => Input(summon[Show[T]].show(value))

  private def escape(str: String): Text =
    val buf: StringBuilder = StringBuilder()
    
    def parseUnicode(chars: Text): Char =
      if chars.length < 4 then throw InterpolationError("the unicode escape is incomplete")
      else Integer.parseInt(chars.s, 16).toChar

    @tailrec
    def recur(cur: Int = 0, esc: Boolean = false): Unit =
      if cur < str.length
      then
        str.charAt(cur) match
          case '\\' if !esc => recur(cur + 1, true)
          case '\\'         => buf.add('\\')
                               recur(cur + 1, false)
          case 'n' if esc   => buf.add('\n')
                               recur(cur + 1)
          case 'r' if esc   => buf.add('\r')
                               recur(cur + 1)
          case 'f' if esc   => buf.add('\f')
                               recur(cur + 1)
          case 'b' if esc   => buf.add('\b')
                               recur(cur + 1)
          case 't' if esc   => buf.add('\t')
                               recur(cur + 1)
          case 'u' if esc   => buf.add(parseUnicode(gossamer.Text(str).slice(cur + 1, cur + 5)))
                               recur(cur + 4)
          case 'e' if esc   => buf.add('\u001b')
                               recur(cur + 1)
          case '"' if esc   => buf.add('"')
                               recur(cur + 1)
          case '\'' if esc  => buf.add('\'')
                               recur(cur + 1)
          case ch if esc    => throw InterpolationError(
                                   s"the character '$ch' does not need to be escaped")
          case ch           => buf.add(ch)
                               recur(cur + 1)
      else if esc then throw InterpolationError("the final character cannot be an escape")
    
    recur()
    
    buf.text
      
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
      val array = state.s.split("\\n\\s*\\n").nn.map(_.nn.replaceAll("\\s\\s*", " ").nn.trim.nn)
      gossamer.Text(String.join("\n", array*).nn)

extension (buf: StringBuilder)
  def add(text: Text): Unit = buf.append(text)
  def add(char: Char): Unit = buf.append(char)
  def text: Text = Showable(buf).show

object encodings:
  given Utf8: Encoding with
    def carry(arr: Array[Byte]): Int =
      val len = arr.length
      def last = arr(len - 1)
      def last2 = arr(len - 2)
      def last3 = arr(len - 3)
      
      if len > 0 && ((last & -32) == -64 || (last & -16) == -32 || (last & -8) == -16) then 1
      else if len > 1 && ((last2 & -16) == -32 || (last2 & -8) == -16) then 2
      else if len > 2 && ((last3 & -8) == -16) then 3
      else 0
    
    def name: Text = Text("UTF-8")
  
  given Ascii: Encoding with
    def carry(arr: Array[Byte]): Int = 0
    def name: Text = Text("ASCII")
  
  @targetName("ISO_8859_1")
  given `ISO-8859-1`: Encoding with
    def name: Text = Text("ISO-8859-1")
    def carry(arr: Array[Byte]): Int = 0

trait Encoding:
  def name: Text
  def carry(array: Array[Byte]): Int

case class Line(text: Text)

object Line:
  given lineReader(using enc: Encoding): Readable[LazyList[Line]] with
    type E = ExcessDataError
    def read(stream: DataStream) =
      def recur(stream: LazyList[Text], carry: Text = Text("")): LazyList[Line] =
        if stream.isEmpty then
          if carry.isEmpty then LazyList() else LazyList(Line(carry))
        else
          val parts = stream.head.s.split("\\r?\\n", Int.MaxValue).nn.map(_.nn)
          if parts.length == 1 then recur(stream.tail, carry + parts.head.show)
          else if parts.length == 2
          then Line(carry + parts.head.show) #:: recur(stream.tail, parts.last.show)
          else
            Line(carry + parts.head.show) #::
                LazyList(parts.tail.init.map(str => Line(str.show))*) #:::
                recur(stream.tail, parts.last.show)
      
      recur(summon[Readable[LazyList[Text]]].read(stream))
      
object DefaultSink:
  given DefaultSink[Stdout.type](Stdout)

case class DefaultSink[T](value: T)(using sink: Sink[T]):
  def write(msg: Text): Unit =
    try sink.write(value, LazyList(msg.bytes)) catch case e: Exception => ()

extension (obj: Boolean.type) def unapply(str: Text): Option[Boolean] =
  if str == Text("true") then Some(true) else if str == Text("false") then Some(false) else None

extension (obj: Byte.type) def unapply(str: Text): Option[Byte] =
  try Some(java.lang.Byte.parseByte(str)) catch NumberFormatException => None

extension (obj: Short.type) def unapply(str: Text): Option[Short] =
  try Some(java.lang.Short.parseShort(str)) catch NumberFormatException => None

extension (obj: Int.type) def unapply(str: Text): Option[Int] =
  try Some(java.lang.Integer.parseInt(str)) catch NumberFormatException => None

extension (obj: Long.type) def unapply(str: Text): Option[Long] =
  try Some(java.lang.Long.parseLong(str)) catch NumberFormatException => None

extension (obj: Float.type) def unapply(str: Text): Option[Float] =
  try Some(java.lang.Float.parseFloat(str)) catch NumberFormatException => None

extension (obj: Char.type) def unapply(str: Text): Option[Char] =
  if str.length == 1 then Some(str.charAt(0)) else None

extension (obj: Double.type) def unapply(str: Text): Option[Double] =
  try Some(java.lang.Double.parseDouble(str)) catch NumberFormatException => None

object Out:
  def print(msg: Text)(using defaultSink: DefaultSink[?]): Unit = defaultSink.write(msg)
  def println(msg: Text)(using DefaultSink[?]): Unit = print(Text(s"$msg\n"))