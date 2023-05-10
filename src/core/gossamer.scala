/*
    Gossamer, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import digression.*
import lithography.*
import spectacular.*
import kaleidoscope.*
import contextual.*

import language.experimental.captureChecking

import scala.reflect.*

import java.util.regex.*
import java.net.{URLEncoder, URLDecoder}

type TextStream = LazyList[Text throws StreamCutError]

enum Bidi:
  case Ltr, Rtl

export Bidi.Ltr, Bidi.Rtl

extension (value: Bytes)
  def uString: Text = Text(String(value.to(Array), "UTF-8"))
  def hex: Text = Text(value.map { b => String.format("\\u%04x", b.toInt).nn }.mkString)
  def text[Enc <: Encoding](using enc: Enc): Text = Text(String(value.to(Array), enc.name.s))

object Cuttable:
  given [TextType: Textual](using textual: Textual[TextType]): Cuttable[TextType, Text] =
    (text, delimiter, limit) =>
      val string = textual.string(text)
      val dLength = delimiter.s.length
      
      @tailrec
      def recur(start: Int, results: List[TextType]): List[TextType] =
        string.indexOf(delimiter.s) match
          case -1    => Nil
          case index => recur(index + dLength, textual.slice(text, start, index) :: results)
    
      recur(0, Nil).reverse

  given [TextType: Textual](using textual: Textual[TextType]): Cuttable[TextType, Regex] =
    (text, regex, limit) =>
      val string = textual.string(text)
      val matcher = Pattern.compile(regex.pattern).nn.matcher(string).nn
      
      @tailrec
      def recur(start: Int, results: List[TextType]): List[TextType] =
        if matcher.find(start)
        then recur(matcher.end, textual.slice(text, matcher.start, matcher.end) :: results)
        else results
        
      recur(0, Nil).reverse

  given Cuttable[Text, Text] = (text, delimiter, limit) =>
    List(text.s.split(Pattern.quote(delimiter.s), limit).nn.map(_.nn).map(Text(_))*)
  
  given Cuttable[Text, Regex] = (text, regex, limit) =>
    List(text.s.split(regex.pattern, limit).nn.map(_.nn).map(Text(_))*)

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

extension [TextType](using textual: Textual[TextType])(text: TextType)
  inline def length: Int = textual.string(text).length
  inline def populated: Maybe[TextType] = if textual.string(text).length == 0 then Unset else text
  inline def lower: TextType = textual.map(text, _.toLower)
  inline def upper: TextType = textual.map(text, _.toUpper)
  def plain: Text = Text(textual.string(text))
  
  def drop(n: Int, bidi: Bidi = Ltr): TextType =
    val length = text.length
    bidi match
      case Ltr => textual.slice(text, n min length max 0, length)
      case Rtl => textual.slice(text, 0, 0 max (length - n) min length)
  
  def take(n: Int, bidi: Bidi = Ltr): TextType =
    val length = text.length
    bidi match
      case Ltr => textual.slice(text, 0, n min length max 0)
      case Rtl => textual.slice(text, 0 max (length - n) min length, length)
  
  def capitalize: TextType = textual.concat(text.take(1).upper, text.drop(1))
  def uncapitalize: TextType = textual.concat(text.take(1).lower, text.drop(1))

  inline def head: Char = textual.unsafeChar(text, 0)
  inline def last: Char = textual.unsafeChar(text, text.length - 1)
  inline def tail: TextType = text.drop(1, Ltr)
  inline def init: TextType = text.drop(1, Rtl)
  inline def empty: Boolean = text.length == 0
  def chars: IArray[Char] = textual.string(text).toCharArray.nn.immutable(using Unsafe)
  
  def slice(start: Int, end: Int): TextType =
    if end <= start then textual.empty
    else textual.slice(text, start max 0 min text.length, end min text.length max 0)
  
  def snip(n: Int): (TextType, TextType) =
    (text.slice(0, n min text.length), text.slice(n min text.length, text.length))
  
  def char(index: Int): Maybe[Char] =
    if index >= 0 && index < text.length then textual.unsafeChar(text, index) else Unset

  inline def reverse: TextType =
    val length = text.length
    
    def recur(index: Int, result: TextType): TextType =
      if index < length then recur(index + 1, textual.concat(text.slice(index, index + 1), result))
      else result
    
    recur(0, textual.empty)
  
  def contains(substring: Text): Boolean = textual.indexOf(text, substring) != -1
  def contains(char: Char): Boolean = textual.indexOf(text, char.show) != -1

  @targetName("add")
  infix def +(other: TextType): TextType = textual.concat(text, other)

  @targetName("times")
  infix def *(count: Int): TextType =
    def recur(n: Int, acc: TextType): TextType =
      if n == 0 then acc else recur(n - 1, textual.concat(acc, text))
    
    recur(count.max(0), textual.empty)
  
  def apply(index: Int): Char throws OutOfRangeError =
    if index >= 0 && index < text.length then textual.unsafeChar(text, index)
    else throw OutOfRangeError(index, 0, text.length)
  
  inline def trim: TextType =
    val start = text.where(_ != ' ').or(text.length)
    val end = text.where(_ != ' ', bidi = Rtl).or(0)
    text.slice(start, end + 1)
  
  def where(pred: Char -> Boolean, start: Maybe[Int] = Unset, bidi: Bidi = Ltr): Maybe[Int] =
    val length = text.length
    
    def recurLtr(i: Int): Maybe[Int] =
      if i >= length then Unset
      else if pred(textual.unsafeChar(text, i)) then i else recurLtr(i + 1)
    
    def recurRtl(i: Int): Maybe[Int] =
      if i < 0 then Unset else if pred(textual.unsafeChar(text, i)) then i else recurLtr(i - 1)
      
    bidi match
      case Ltr => recurLtr(start.or(0))
      case Rtl => recurRtl(start.or(length - 1))

  def upto(pred: Char -> Boolean): TextType =
    val end: Int = text.where(pred).or(text.length)
    text.slice(0, end)
  
  def dropWhile(pred: Char -> Boolean): TextType = text.where(!pred(_)) match
    case Unset  => textual.empty
    case i: Int => text.slice(i, text.length)

  def snipWhere(pred: Char -> Boolean, index: Int = 0): Maybe[(TextType, TextType)] =
    text.where(pred, index).mm(text.snip(_))

  def whilst(pred: Char -> Boolean): TextType = text.upto(!pred(_))
  def mapChars(fn: Char -> Char): TextType = textual.map(text, fn)

  inline def count(pred: Char -> Boolean): Int =
    val length: Int = text.length
    
    def recur(index: Int, sum: Int): Int = if index >= length then sum else
      val increment = if pred(textual.unsafeChar(text, index)) then 1 else 0
      recur(index + 1, sum + increment)
    
    recur(0, 0)
  
  def pad(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using TextWidthCalculator): TextType =
    val padding = textual.make(char.toString)*(length - text.length)
    
    bidi match
      case Ltr => textual.concat(text, padding)
      case Rtl => textual.concat(padding, text)
  
  def center(length: Int, char: Char = ' ')(using TextWidthCalculator): TextType =
    text.pad((length + text.length)/2, char = char).pad(length, Rtl, char = char)
  
  def fit(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using TextWidthCalculator): TextType =
    bidi match
      case Ltr => text.pad(length, bidi, char).take(length, Ltr)
      case Rtl => text.pad(length, bidi, char).take(length, Rtl)
  
  def uncamel: List[TextType] = text.where(_.isUpper, 1) match
    case Unset  => List(text.lower)
    case i: Int => text.take(i).lower :: text.drop(i).uncamel
  
  def unkebab: List[TextType] = text.cut(Text("-"))
  def unsnake: List[TextType] = text.cut(Text("_"))
  
  inline def starts(prefix: Text): Boolean =
    val length: Int = prefix.s.length
    
    def recur(index: Int): Boolean =
      index == length || textual.unsafeChar(text, index) == prefix.s.charAt(index) && recur(index + 1)

    length <= text.length && recur(0)
  
  inline def ends(suffix: Text): Boolean =
    val length: Int = suffix.s.length
    val offset: Int = text.length - length
    
    def recur(index: Int): Boolean =
      index == length || textual.unsafeChar(text, offset + index) == suffix.s.charAt(index) &&
          recur(index + 1)

    length <= text.length && recur(0)
  
  inline def tr(from: Char, to: Char): TextType =
    textual.map(text, char => if char == from then to else char)
  
extension (text: Text)
  inline def rsub(from: Text, to: Text): Text = Text(text.s.replaceAll(from.s, to.s).nn)
  
  inline def sub(from: Text, to: Text): Text =
    Text(text.s.replaceAll(Pattern.quote(from.s), to.s).nn)
  
  def flatMap(fn: Char => Text): Text =
    Text(String(text.s.toCharArray.nn.immutable(using Unsafe).flatMap(fn(_).s.toCharArray.nn
        .immutable(using Unsafe)).mutable(using Unsafe)))

  inline def urlEncode: Text = Text(URLEncoder.encode(text.s, "UTF-8").nn)
  inline def urlDecode: Text = Text(URLDecoder.decode(text.s, "UTF-8").nn)
  inline def punycode: Text = Text(java.net.IDN.toASCII(text.s).nn)
  
  inline def bytes(using enc: Encoding): IArray[Byte] =
    text.s.getBytes(enc.name.s).nn.immutable(using Unsafe)
  
  inline def sysBytes: IArray[Byte] = text.s.getBytes().nn.immutable(using Unsafe)
  
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
  given [TextType](using textual: Textual[TextType]): Joinable[TextType] = elements =>
    var acc: TextType = textual.empty
    for element <- elements do acc = textual.concat(acc, element)
    acc

trait Joinable[TextType]:
  def join(elements: Iterable[TextType]): TextType

extension (iarray: IArray[Char]) def text: Text = Text(String(iarray.mutable(using Unsafe)))

extension [TextType](values: Iterable[TextType])(using joinable: Joinable[TextType])
  def join: TextType = joinable.join(values)
  
  def join(separator: TextType): TextType =
    joinable.join(values.flatMap(Iterable(separator, _)).drop(1))
  
  def join(left: TextType, separator: TextType, right: TextType): TextType =
    Iterable(left, join(separator), right).join
  
  def join(separator: TextType, penultimate: TextType): TextType = values.size match
    case 0 => Iterable().join
    case 1 => values.head
    case _ => Iterable(values.init.join(separator), penultimate, values.last).join
  
  def join(left: TextType, separator: TextType, penultimate: TextType, right: TextType): TextType =
    Iterable(left, join(separator, penultimate), right).join

case class OutOfRangeError(index: Int, from: Int, to: Int)
extends Error(err"the index $index is outside the range $from-$to")

case class Showable[T](value: T):
  def show: Text = Text(value.toString)

trait Shown[+T](using Show[T]):
  this: T =>
    override def toString(): String = summon[Show[T]](this).s

object Interpolation:
  case class Input(txt: Text)

  given [ValueType](using display: Display[ValueType, EndUser]): Insertion[Input, ValueType] =
    value => Input(display(value))

  private def escape(str: Text): Text throws InterpolationError =
    val buf: StringBuilder = StringBuilder()
    
    def parseUnicode(chars: Text): Char =
      if chars.length < 4
      then throw InterpolationError(rudiments.Text("the unicode escape is incomplete"))
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
      else if esc then throw InterpolationError(
          rudiments.Text("the final character cannot be an escape"))
    
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
