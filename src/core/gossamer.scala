/*
    Gossamer, version 0.4.0. Copyright 2021-23 Jon Pretty, Propensive OÜ.

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
import deviation.*
import turbulence.*
import contextual.*
import wisteria.*

import language.experimental.captureChecking

import scala.reflect.*
import scala.util.*

import java.util.regex.*
import java.net.{URLEncoder, URLDecoder}

type TextStream = LazyList[Text throws StreamCutError]

enum Direction:
  case Ltr, Rtl

export Direction.Ltr, Direction.Rtl

extension (value: Bytes)
  def uString: Text = Text(String(value.to(Array), "UTF-8"))
  def hex: Text = Text(value.map { b => String.format("\\u%04x", b.toInt).nn }.mkString)
  def text[Enc <: Encoding](using enc: Enc): Text = Text(String(value.to(Array), enc.name.s))

object Cuttable:
  given Cuttable[Text, Text] = (text, delimiter, limit) =>
    List(text.s.split(Pattern.quote(delimiter.s), limit).nn.map(_.nn).map(Text(_))*)

  given [T](using cuttable: Cuttable[T, Text]): Cuttable[T, Char] = (text, delimiter, limit) =>
    cuttable.cut(text, delimiter.show, limit)

trait Cuttable[V, D]:
  def cut(value: V, delimiter: D, limit: Int): List[V]

extension [V](value: V)
  def cut[D](delimiter: D, limit: Int = Int.MaxValue)(using cuttable: Cuttable[V, D]): List[V] =
    cuttable.cut(value, delimiter, limit)

extension (words: Iterable[Text])
  def pascal: Text = words.map(_.lower.capitalize).join
  def camel: Text = pascal.uncapitalize
  def snake: Text = words.join(Text("_"))
  def kebab: Text = words.join(Text("-"))

extension (text: Text)
  inline def bytes(using enc: Encoding): IArray[Byte] =
    text.s.getBytes(enc.name.s).nn.immutable(using Unsafe)
  
  inline def sysBytes: IArray[Byte] = text.s.getBytes().nn.immutable(using Unsafe)
  inline def length: Int = text.s.length
  inline def populated: Option[Text] = if text.s.length == 0 then None else Some(text)
  inline def lower: Text = Text(text.s.toLowerCase.nn)
  inline def upper: Text = Text(text.s.toUpperCase.nn)
  inline def urlEncode: Text = Text(URLEncoder.encode(text.s, "UTF-8").nn)
  inline def urlDecode: Text = Text(URLDecoder.decode(text.s, "UTF-8").nn)
  inline def punycode: Text = Text(java.net.IDN.toASCII(text.s).nn)
  
  def drop(n: Int, dir: Direction = Ltr): Text = dir match
    case Ltr => Text(text.s.substring(n min length max 0).nn)
    case Rtl => Text(text.s.substring(0, 0 max (text.s.length - n) min length).nn)
  
  def take(n: Int, dir: Direction = Ltr): Text = dir match
    case Ltr => Text(text.s.substring(0, n min length max 0).nn)
    case Rtl => Text(text.s.substring(0 max (text.s.length - n) min length, length).nn)

  inline def trim: Text = Text(text.s.trim.nn)
  
  def slice(from: Int, to: Int): Text =
    if to <= from then Text("")
    else Text(text.s.substring(from max 0 min length, to min length max 0).nn)
  
  inline def chars: IArray[Char] = text.s.toCharArray.nn.immutable(using Unsafe)
  def map(fn: Char => Char): Text = Text(String(text.s.toCharArray.nn.map(fn)))
  inline def empty: Boolean = text.s.isEmpty
  inline def rsub(from: Text, to: Text): Text = Text(text.s.replaceAll(from.s, to.s).nn)
  inline def starts(prefix: Text): Boolean = text.s.startsWith(prefix.s)
  inline def ends(suffix: Text): Boolean = text.s.endsWith(suffix.s)
  inline def sub(from: Text, to: Text): Text = Text(text.s.replaceAll(Pattern.quote(from.s), to.s).nn)
  inline def tr(from: Char, to: Char): Text = Text(text.s.replace(from, to).nn)
  inline def capitalize: Text = take(1).upper+drop(1)
  inline def uncapitalize: Text = take(1).lower+drop(1)
  inline def reverse: Text = Text(String(text.s.toCharArray.nn.reverse))
  inline def count(pred: Char -> Boolean): Int = text.s.toCharArray.nn.immutable(using Unsafe).count(pred)
  inline def head: Char = text.s.charAt(0)
  inline def last: Char = text.s.charAt(text.s.length - 1)
  inline def tail: Text = drop(1, Ltr)
  inline def init: Text = drop(1, Rtl)

  def flatMap(fn: Char => Text): Text =
    Text(String(text.s.toCharArray.nn.immutable(using Unsafe).flatMap(fn(_).s.toCharArray.nn
        .immutable(using Unsafe)).immutable(using Unsafe)))

  def dropWhile(pred: Char -> Boolean): Text =
    try Text(text.s.substring(0, where(!pred(_))).nn) catch case err: OutOfRangeError => Text("")

  def snip(n: Int): (Text, Text) =
    (Text(text.s.substring(0, n min text.s.length).nn), Text(text.s.substring(n min text.s.length)
        .nn))
  
  def snipWhere(pred: Char -> Boolean, idx: Int = 0): (Text, Text) throws OutOfRangeError =
    snip(where(pred, idx))

  def uncamel: List[Text] =
    (try text.where(_.isUpper, 1) catch case error: OutOfRangeError => -1) match
      case -1 => List(text.lower)
      case i  => text.take(i).lower :: text.drop(i).uncamel
  
  def unkebab: List[Text] = text.cut(Text("-"))
  def unsnake: List[Text] = text.cut(Text("_"))

  def fit(width: Int, dir: Direction = Ltr, char: Char = ' '): Text = dir match
    case Ltr => (text + Text(s"$char")*(width - length)).take(width, Ltr)
    case Rtl => (Text(s"$char")*(width - length) + text).take(width, Rtl)

  @targetName("add")
  infix def +(other: Text): Text = Text(text.s+other)

  @targetName("times")
  infix def *(n: Int): Text = Text(IArray.fill(n)(text.s).mkString)
  
  def apply(idx: Int): Char throws OutOfRangeError =
    if idx >= 0 && idx < text.s.length then text.s.charAt(idx)
    else throw OutOfRangeError(idx, 0, text.s.length)
  
  def char(idx: Int): Maybe[Char] = if idx >= 0 && idx < text.s.length then text.s.charAt(idx) else Unset

  def pad(length: Int, dir: Direction = Ltr, char: Char = ' '): Text = dir match
    case Ltr => if text.length < length then text+Text(s"$char")*(length - text.length) else text
    case Rtl => if text.length < length then Text(s"$char")*(length - text.length)+text else text

  def contains(substring: Text): Boolean = text.s.contains(substring.s)
  def contains(char: Char): Boolean = text.s.indexOf(char) != -1

  @tailrec
  def where(pred: Char -> Boolean, idx: Maybe[Int] = Unset, dir: Direction = Ltr)
            : Int throws OutOfRangeError = dir match
    case Ltr =>
      val index = idx.or(0)
      if index >= text.length || index < 0 then throw OutOfRangeError(index, 0, text.s.length)
      if pred(text.s.charAt(index)) then index else where(pred, index + 1, Ltr)
    
    case Rtl =>
      val index = idx.or(text.s.length - 1)
      if index < 0 || index >= text.length then throw OutOfRangeError(index, 0, text.s.length)
      if pred(text.s.charAt(index)) then index else where(pred, index - 1, Rtl)

  def upto(pred: Char -> Boolean): Text =
    try Text(text.s.substring(0, where(pred(_))).nn) catch case e: OutOfRangeError => text
  
  def whilst(pred: Char -> Boolean): Text =
    try Text(text.s.substring(0, where(!pred(_))).nn) catch case e: OutOfRangeError => text

  def lev(other: Text): Int =
    val m = text.s.length
    val n = other.length
    val old = new Array[Int](n + 1)
    val dist = new Array[Int](n + 1)

    for j <- 1 to n do old(j) = old(j - 1) + 1
    
    for i <- 1 to m do
      dist(0) = old(0) + 1

      for j <- 1 to n do
        dist(j) = (old(j - 1) + (if text.s.charAt(i - 1) == other.s.charAt(j - 1) then 0 else 1))
          .min(old(j) + 1).min(dist(j - 1) + 1)

      for j <- 0 to n do old(j) = dist(j)
    
    dist(n)

case class Numerous(word: Text, pluralEnd: Text = Text("s"), singularEnd: Text = Text("")):
  def apply(elements: Iterable[?]): Text = apply(elements.size)
  def apply(value: Int): Text = word+(if value == 1 then singularEnd else pluralEnd)

object Joinable:
  given Joinable[Text] = xs => Text(xs.mkString)

trait Joinable[T]:
  def join(elements: Iterable[T]): T

extension (iarray: IArray[Char]) def text: Text = Text(String(iarray.mutable(using Unsafe)))

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
extends Error(err"the index $idx is outside the range $from-$to")

case class Showable[T](value: T):
  def show: Text = Text(value.toString)

trait Shown[+T](using Show[T]):
  this: T =>
    override def toString(): String = summon[Show[T]].show(this).s

object Interpolation:
  case class Input(txt: Text)

  given [T: Show]: Insertion[Input, T] = value => Input(summon[Show[T]].show(value))

  private def escape(str: Text): Text throws InterpolationError =
    val buf: StringBuilder = StringBuilder()
    
    def parseUnicode(chars: Text): Char =
      if chars.length < 4 then throw InterpolationError(rudiments.Text("the unicode escape is incomplete"))
      else Integer.parseInt(chars.s, 16).toChar

    @tailrec
    def recur(cur: Int = 0, esc: Boolean = false): Unit =
      if cur < str.length
      then
        str.s.charAt(cur) match
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
          case 'u' if esc   => buf.add(parseUnicode(str.slice(cur + 1, cur + 5)))
                               recur(cur + 4)
          case 'e' if esc   => buf.add('\u001b')
                               recur(cur + 1)
          case '"' if esc   => buf.add('"')
                               recur(cur + 1)
          case '\'' if esc  => buf.add('\'')
                               recur(cur + 1)
          case ch if esc    => throw InterpolationError(
                                   rudiments.Text(s"the character '$ch' should not be escaped"))
          case ch           => buf.add(ch)
                               recur(cur + 1)
      else if esc then throw InterpolationError(rudiments.Text("the final character cannot be an escape"))
    
    recur()
    
    buf.text
      
  object T extends Interpolator[Input, Text, Text]:
    def initial: Text = rudiments.Text("")
    def parse(state: Text, next: Text): Text = state+escape(next)
    def skip(state: Text): Text = state
    def insert(state: Text, input: Input): Text = state+input.txt
    def complete(state: Text): Text = state
  
  object Text extends Interpolator[Input, Text, Text]:
    def initial: Text = rudiments.Text("")
    def parse(state: Text, next: Text): Text = state+escape(next)
    def skip(state: Text): Text = state
    def insert(state: Text, input: Input): Text = state+input.txt

    def complete(state: Text): Text =
      val array = state.s.split("\\n\\s*\\n").nn.map(_.nn.replaceAll("\\s\\s*", " ").nn.trim.nn)
      rudiments.Text(String.join("\n", array*).nn)

extension (buf: StringBuilder)
  def add(text: Text): Unit = buf.append(text.s)
  def add(char: Char): Unit = buf.append(char)
  def text: Text = Showable(buf).show

// object Line:
//   given lineReader(using enc: Encoding): Readable[LazyList[Line]] with
//     def read(stream: DataStream): LazyList[Line] throws StreamCutError =
//       def recur(stream: LazyList[Text], carry: Text = Text("")): LazyList[Line] =
//         if stream.isEmpty then
//           if carry.empty then LazyList() else LazyList(Line(carry))
//         else
//           val parts = stream.head.s.split("\\r?\\n", Int.MaxValue).nn.map(_.nn)
//           if parts.length == 1 then recur(stream.tail, carry + parts.head.show)
//           else if parts.length == 2
//           then Line(carry + parts.head.show) #:: recur(stream.tail, parts.last.show)
//           else
//             Line(carry + parts.head.show) #::
//                 LazyList(parts.tail.init.map(str => Line(str.show))*) #:::
//                 recur(stream.tail, parts.last.show)
      
//       recur(summon[Readable[LazyList[Text]]].read(stream))
      
// package stdouts:
//   given stdout: Stdout = txt =>
//     try summon[Appendable[SystemOut.type]].write(SystemOut, LazyList(txt.sysBytes))
//     catch case err: Exception => ()
  
//   given drain: Stdout = txt => ()

// object Stdout:
//   def apply[T](value: T)(using writable: Writable[T]): Stdout = txt =>
//     try writable.write(value, LazyList(txt.sysBytes)) catch case err: Exception => ()

// trait Stderr:
//   def write(msg: Text): Unit

// trait Stdout:
//   def write(msg: Text): Unit

// object Out:
//   def print[T: Show](msg: T)(using stdout: Stdout): Unit = stdout.write(msg.show)
//   def println[T: Show](msg: T)(using Stdout): Unit = print(Text(s"${msg.show}\n"))

object EncodingPrefix extends Verifier[Encoding]:
  def verify(value: Text): Encoding = value match
    case Encoding(enc) => enc
    case _             => throw InterpolationError(Text(s"$value is not a valid character encoding"))