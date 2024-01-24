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
import vacuous.*
import fulminate.*
import anticipation.*
import hieroglyph.*
import spectacular.*
import kaleidoscope.*
import contextual.*

import scala.reflect.*

import java.util.regex.*
import java.net.{URLEncoder, URLDecoder}

import language.experimental.pureFunctions

enum Bidi:
  case Ltr, Rtl

export Bidi.Ltr, Bidi.Rtl

extension (value: Bytes)
  def uString: Text = Text(String(value.to(Array), "UTF-8"))
  def hex: Text = Text(value.mutable(using Unsafe).map { b => String.format("\\u%04x", b.toInt).nn }.mkString)
  def text(using decoder: CharDecoder): Text = decoder.decode(value)
  
  // Printable Unicode Encoding
  def pue: Text =
    value.map: b =>
      val i = b&0xff
      (if i%0x80 <= 0x20 || i == 0x7f then i + 0x100 else i).toChar
    .mkString.tt

object Pue:
  def apply(text: into Text): Bytes =
    val length = text.length
    IArray.create[Byte](length): array =>
      var i = 0
      while i < length do
        array(i) = (text.s.charAt(i)%0x100).toByte
        i += 1

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
      val matcher = Pattern.compile(regex.pattern.s).nn.matcher(string).nn
      
      @tailrec
      def recur(start: Int, results: List[TextType]): List[TextType] =
        if matcher.find(start)
        then recur(matcher.end, textual.slice(text, matcher.start, matcher.end) :: results)
        else results
        
      recur(0, Nil).reverse

  given Cuttable[Text, Text] = (text, delimiter, limit) =>
    val parts = text.s.split(Pattern.quote(delimiter.s), limit).nn
    val nnParts = parts.map(_.nn)
    nnParts.map(Text(_)).to(List)
  
  given Cuttable[Text, Regex] = (text, regex, limit) =>
    text.s.split(regex.pattern.s, limit).nn.map(_.nn).map(Text(_)).to(List)

  given [TextType](using cuttable: Cuttable[TextType, Text]): Cuttable[TextType, Char] = (text, delimiter, limit) =>
    cuttable.cut(text, delimiter.show, limit)

trait Cuttable[TextType, DelimiterType]:
  def cut(value: TextType, delimiter: DelimiterType, limit: Int): List[TextType]

extension [TextType](value: TextType)
  def cut
      [DelimiterType]
      (delimiter: DelimiterType, limit: Int = Int.MaxValue)
      (using cuttable: Cuttable[TextType, DelimiterType])
      : List[TextType] =
    cuttable.cut(value, delimiter, limit)

extension (words: Iterable[Text])
  def pascal: Text = words.map(_.lower.capitalize).join
  def camel: Text = pascal.uncapitalize
  def snake: Text = words.join(Text("_"))
  def kebab: Text = words.join(Text("-"))

extension [TextType](text: TextType)(using textual: Textual[TextType])
  inline def length: Int = textual.string(text).length
  inline def populated: Optional[TextType] = if textual.string(text).length == 0 then Unset else text
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
  
  def char(index: Int): Optional[Char] =
    if index >= 0 && index < text.length then textual.unsafeChar(text, index) else Unset

  inline def reverse: TextType =
    val length = text.length
    
    def recur(index: Int, result: TextType): TextType =
      if index < length then recur(index + 1, textual.concat(text.slice(index, index + 1), result))
      else result
    
    recur(0, textual.empty)
  
  def contains(substring: into Text): Boolean = textual.indexOf(text, substring) != -1
  def contains(char: Char): Boolean = textual.indexOf(text, char.show) != -1

  def apply(index: Int): Char throws OutOfRangeError =
    if index >= 0 && index < text.length then textual.unsafeChar(text, index)
    else throw OutOfRangeError(index, 0, text.length)
  
  inline def trim: TextType =
    val start = text.where(!_.isWhitespace).or(text.length)
    val end = text.where(!_.isWhitespace, bidi = Rtl).or(0)
    text.slice(start, end + 1)
  
  def where(pred: Char -> Boolean, start: Optional[Int] = Unset, bidi: Bidi = Ltr): Optional[Int] =
    val length = text.length
    
    val step: Int = bidi match
      case Ltr => 1
      case Rtl => -1
    
    val first: Int = bidi match
      case Ltr => start.or(0)
      case Rtl => start.or((length - 1).max(0))

    def recur(i: Int): Optional[Int] =
      if i >= length || i < 0 then Unset else if pred(textual.unsafeChar(text, i)) then i
      else recur(i + step)
    
    recur(first)

  def upto(pred: Char -> Boolean): TextType =
    val end: Int = text.where(pred).or(text.length)
    text.slice(0, end)
  
  def dropWhile(pred: Char -> Boolean): TextType = text.where(!pred(_)) match
    case Unset  => textual.empty
    case i: Int => text.slice(i, text.length)

  def snipWhere(pred: Char -> Boolean, index: Int = 0): Optional[(TextType, TextType)] =
    text.where(pred, index).let(text.snip(_))

  def whilst(pred: Char -> Boolean): TextType = text.upto(!pred(_))
  def mapChars(lambda: Char -> Char): TextType = textual.map(text, lambda)

  inline def count(pred: Char -> Boolean): Int =
    val length: Int = text.length
    
    def recur(index: Int, sum: Int): Int = if index >= length then sum else
      val increment = if pred(textual.unsafeChar(text, index)) then 1 else 0
      recur(index + 1, sum + increment)
    
    recur(0, 0)
  
  def displayWidth(using calc: TextWidthCalculator) = calc.width(Text(textual.string(text)))
  
  def pad
      (length: Int, bidi: Bidi = Ltr, char: Char = ' ')
      (using TextWidthCalculator)
      : TextType =
    if text.displayWidth >= length then text else
      val padding = textual.make(char.toString)*(length - text.displayWidth)
    
      bidi match
        case Ltr => textual.concat(text, padding)
        case Rtl => textual.concat(padding, text)
  
  def center(length: Int, char: Char = ' ')(using TextWidthCalculator): TextType =
    text.pad((length + text.displayWidth)/2, char = char).pad(length, Rtl, char = char)
  
  def fit(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using TextWidthCalculator): TextType =
    bidi match
      case Ltr => text.pad(length, bidi, char).take(length, Ltr)
      case Rtl => text.pad(length, bidi, char).take(length, Rtl)
  
  def uncamel: List[TextType] = text.where(_.isUpper, 1) match
    case Unset  => List(text.lower)
    case i: Int => text.take(i).lower :: text.drop(i).uncamel
  
  def unkebab: List[TextType] = text.cut(Text("-"))
  def unsnake: List[TextType] = text.cut(Text("_"))
  
  inline def starts(prefix: into Text): Boolean =
    val length: Int = prefix.s.length
    
    def recur(index: Int): Boolean =
      index == length || textual.unsafeChar(text, index) == prefix.s.charAt(index) && recur(index + 1)

    length <= text.length && recur(0)
  
  inline def ends(suffix: into Text): Boolean =
    val length: Int = suffix.s.length
    val offset: Int = text.length - length
    
    def recur(index: Int): Boolean =
      index == length || textual.unsafeChar(text, offset + index) == suffix.s.charAt(index) &&
          recur(index + 1)

    length <= text.length && recur(0)
  
  inline def tr(from: Char, to: Char): TextType =
    textual.map(text, char => if char == from then to else char)

  // Extension method is applied explicitly because it appears ambiguous otherwise
  inline def subscript: TextType = textual.map(text, hieroglyph.subscript(_).or(' '))
  inline def superscript: TextType = textual.map(text, hieroglyph.superscript(_).or(' '))
  
extension (text: into Text)
  inline def rsub(from: into Text, to: into Text): Text = Text(text.s.replaceAll(from.s, to.s).nn)
  
  inline def sub(from: into Text, to: into Text): Text =
    Text(text.s.replaceAll(Pattern.quote(from.s), to.s).nn)
  
  def flatMap(lambda: Char => Text): Text =
    Text(String(text.s.toCharArray.nn.flatMap(lambda(_).s.toCharArray.nn.immutable(using Unsafe))))

  inline def urlEncode: Text = Text(URLEncoder.encode(text.s, "UTF-8").nn)
  inline def urlDecode: Text = Text(URLDecoder.decode(text.s, "UTF-8").nn)
  inline def punycode: Text = Text(java.net.IDN.toASCII(text.s).nn)
  inline def bytes(using encoder: CharEncoder): IArray[Byte] = encoder.encode(text)
  inline def sysBytes: IArray[Byte] = CharEncoder.system.encode(text)
  
  def lev(other: into Text): Int =
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
  def apply(value: Int): Text = Text(word.s+(if value == 1 then singularEnd.s else pluralEnd.s))

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
extends Error(msg"the index $index is outside the range $from-$to")

trait Shown[+T](using Show[T]):
  this: T =>
    override def toString(): String = summon[Show[T]](this).s

object Interpolation:
  case class Input(txt: Text)

  given [ValueType](using show: Show[ValueType]): Insertion[Input, ValueType] =
    value => Input(show(value))

  object T extends Interpolator[Input, Text, Text]:
    def initial: Text = anticipation.Text("")
    
    def parse(state: Text, next: Text): Text =
      try anticipation.Text(state.s+TextEscapes.escape(next).s)
      catch case error: EscapeError => error match
        case EscapeError(message) => throw InterpolationError(message)

    def skip(state: Text): Text = state
    def insert(state: Text, input: Input): Text = anticipation.Text(state.s+input.txt.s)
    def complete(state: Text): Text = state
  
  object Text extends Interpolator[Input, Text, Text]:
    def initial: Text = anticipation.Text("")
    
    def parse(state: Text, next: Text): Text =
      try anticipation.Text(state.s+TextEscapes.escape(next).s)
      catch case error: EscapeError => error match
        case EscapeError(message) => throw InterpolationError(message)
    
    def skip(state: Text): Text = state
    def insert(state: Text, input: Input): Text = anticipation.Text(state.s+input.txt.s)

    def complete(state: Text): Text =
      val array = state.s.split("\\n\\s*\\n").nn.map(_.nn.replaceAll("\\s\\s*", " ").nn.trim.nn)
      anticipation.Text(String.join("\n", array*).nn)

extension (buf: StringBuilder)
  def add(text: into Text): Unit = buf.append(text.s)
  def add(char: Char): Unit = buf.append(char)
  def text: Text = Text(buf.toString)
