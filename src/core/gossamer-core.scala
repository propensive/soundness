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

import scala.reflect.*

import java.util.regex.*
import java.net.{URLEncoder, URLDecoder}

import language.experimental.pureFunctions
import language.experimental.into

def append[TextType: Textual, ValueType](using buffer: Buffer[TextType])(value: ValueType)
    (using show: TextType.Show[ValueType])
        : Unit =
  buffer.append(TextType.show(value))

extension (textObject: Text.type)
  def construct(block: (buffer: TextBuffer) ?=> Unit): Text =
    val buffer = TextBuffer()
    block(using buffer)
    buffer()

  def fill(length: Int)(lambda: Int => Char): Text =
    val array = new Array[Char](length)
    (0 until length).each { index => array(index) = lambda(index) }

    String(array).tt

extension (inline ctx: StringContext)
  transparent inline def txt(inline parts: Any*): Text = ${Interpolation.Text.expand('ctx, 'parts)}
  transparent inline def t(inline parts: Any*): Text = ${Interpolation.T.expand('ctx, 'parts)}

extension (ctx: StringContext)
  def t = SimpleTExtractor(ctx.parts.head.show)

extension (value: Bytes)
  def utf8: Text = Text(String(value.to(Array), "UTF-8"))
  def utf16: Text = Text(String(value.to(Array), "UTF-16"))
  def ascii: Text = Text(String(value.to(Array), "ASCII"))
  def hex: Text = Text(value.mutable(using Unsafe).map { b => String.format("\\u%04x", b.toInt).nn }.mkString)
  def text(using CharDecoder): Text = summon[CharDecoder].decode(value)

  // Printable Unicode Encoding
  def pue: Text =
    value.map: b =>
      val i = b&0xff
      (if i%0x80 <= 0x20 || i == 0x7f then i + 0x100 else i).toChar
    .mkString.tt

extension [TextType](value: TextType)
  def cut[DelimiterType](delimiter: DelimiterType, limit: Int = Int.MaxValue)
      (using cuttable: Cuttable[TextType, DelimiterType])
          : IArray[TextType] =

    cuttable.cut(value, delimiter, limit)

extension (words: Iterable[Text])
  def pascal: Text = words.map(_.lower.capitalize).join
  def camel: Text = pascal.uncapitalize
  def snake: Text = words.join(Text("_"))
  def kebab: Text = words.join(Text("-"))

extension [TextType: Textual](text: TextType)
  inline def length: Int = TextType.length(text)

  inline def populated: Optional[TextType] =
    if TextType.text(text).length == 0 then Unset else text

  inline def lower: TextType = TextType.map(text, _.toLower)
  inline def upper: TextType = TextType.map(text, _.toUpper)
  def plain: Text = TextType.text(text)

  def breakable(predicate: (Char, Char) => Boolean, break: Char = '\u200b')
          : TextType =
    val breakText = TextType(break.toString.tt)
    def recur(from: Int = 0, index: Int = 1, current: TextType = TextType("".tt)): TextType =
      if index == length then current+text.drop(from) else
        if predicate(TextType.unsafeChar(text, index - 1), TextType.unsafeChar(text, index))
        then recur(index, index + 1, current+text.slice(from, index)+breakText)
        else recur(from, index + 1, current)

    recur()

  def justify(width: Int): TextType =
    val words = text.words
    val extra = width - text.length

    def recur(word: Int, spaces: Int, result: TextType): TextType =
      if word == 0 then result else
        val gap = ((spaces.toDouble/word) + 0.5).toInt
        recur(word - 1, spaces - gap, result+TextType(t" "*(gap + 1))+words(words.length - word))

    recur(words.length - 1, extra, words(0))

  def drop(n: Int, bidi: Bidi = Ltr): TextType =
    val length = text.length
    bidi match
      case Ltr => TextType.range(text, n min length max 0, length)
      case Rtl => TextType.range(text, 0, 0 max (length - n) min length)

  def take(n: Int, bidi: Bidi = Ltr): TextType =
    val length = text.length
    bidi match
      case Ltr => TextType.range(text, 0, n min length max 0)
      case Rtl => TextType.range(text, 0 max (length - n) min length, length)

  def capitalize: TextType = TextType.concat(text.take(1).upper, text.drop(1))
  def uncapitalize: TextType = TextType.concat(text.take(1).lower, text.drop(1))

  inline def head: Char = TextType.unsafeChar(text, 0)
  inline def last: Char = TextType.unsafeChar(text, text.length - 1)
  inline def tail: TextType = text.drop(1, Ltr)
  inline def init: TextType = text.drop(1, Rtl)
  inline def empty: Boolean = text.length == 0

  def chars: IArray[Char] = TextType.text(text).s.toCharArray.nn.immutable(using Unsafe)

  def slice(start: Int, end: Int): TextType =
    if end <= start then TextType.empty
    else TextType.range(text, start max 0 min text.length, end min text.length max 0)

  def snip(n: Int): (TextType, TextType) =
    (text.slice(0, n min text.length), text.slice(n min text.length, text.length))

  def char(index: Int): Optional[Char] =
    if index >= 0 && index < text.length then TextType.unsafeChar(text, index) else Unset

  inline def reverse: TextType =
    val length = text.length

    def recur(index: Int, result: TextType): TextType =
      if index < length then recur(index + 1, TextType.concat(text.slice(index, index + 1), result))
      else result

    recur(0, TextType.empty)

  def contains(substring: into Text): Boolean = TextType.indexOf(text, substring) != -1
  def has(char: Char): Boolean = TextType.indexOf(text, char.show) != -1

  inline def at(index: Int): Optional[Char] = optimizable[Char]: default =>
    if index < 0 || index >= length then default else TextType.unsafeChar(text, index)

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
      if i >= length || i < 0 then Unset else if pred(TextType.unsafeChar(text, i)) then i
      else recur(i + step)

    recur(first)

  def upto(pred: Char -> Boolean): TextType =
    val end: Int = text.where(pred).or(text.length)
    text.slice(0, end)

  def dropWhile(pred: Char -> Boolean): TextType = text.where(!pred(_)) match
    case Unset  => TextType.empty
    case i: Int => text.slice(i, text.length)

  def snipWhere(pred: Char -> Boolean, index: Int = 0): Optional[(TextType, TextType)] =
    text.where(pred, index).let(text.snip(_))

  def whilst(pred: Char -> Boolean): TextType = text.upto(!pred(_))
  def mapChars(lambda: Char -> Char): TextType = TextType.map(text, lambda)

  inline def count(pred: Char -> Boolean): Int =
    val length: Int = text.length

    def recur(index: Int, sum: Int): Int = if index >= length then sum else
      val increment = if pred(TextType.unsafeChar(text, index)) then 1 else 0
      recur(index + 1, sum + increment)

    recur(0, 0)

  def metrics(using TextMetrics) = summon[TextMetrics].width(TextType.text(text))

  def pad(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using TextMetrics): TextType =
    if text.metrics >= length then text else
      val padding = TextType(char.toString.tt)*(length - text.metrics)

      bidi match
        case Ltr => TextType.concat(text, padding)
        case Rtl => TextType.concat(padding, text)

  def center(length: Int, char: Char = ' ')(using TextMetrics): TextType =
    text.pad((length + text.metrics)/2, char = char).pad(length, Rtl, char = char)

  def fit(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using TextMetrics): TextType = bidi match
    case Ltr => text.pad(length, bidi, char).take(length, Ltr)
    case Rtl => text.pad(length, bidi, char).take(length, Rtl)

  def uncamel: IArray[TextType] =
    def recur(text: TextType): List[TextType] = text.where(_.isUpper, 1).lay(List(text.lower)): index =>
      text.take(index).lower :: recur(text.drop(index))

    IArray.from(recur(text))(using TextType.classTag)

  def words: IArray[TextType] = text.cut(Text(" "))
  def lines: IArray[TextType] = text.cut(Text("\n"))
  def unkebab: IArray[TextType] = text.cut(Text("-"))
  def unsnake: IArray[TextType] = text.cut(Text("_"))

  inline def starts(prefix: into Text): Boolean =
    val length: Int = prefix.s.length

    def recur(index: Int): Boolean =
      index == length || TextType.unsafeChar(text, index) == prefix.s.charAt(index) && recur(index + 1)

    length <= text.length && recur(0)

  inline def ends(suffix: into Text): Boolean =
    val length: Int = suffix.s.length
    val offset: Int = text.length - length

    def recur(index: Int): Boolean =
      index == length || TextType.unsafeChar(text, offset + index) == suffix.s.charAt(index) &&
          recur(index + 1)

    length <= text.length && recur(0)

  inline def tr(from: Char, to: Char): TextType =
    TextType.map(text, char => if char == from then to else char)

  // Extension method is applied explicitly because it appears ambiguous otherwise
  inline def subscript: TextType = TextType.map(text, hieroglyph.subscript(_).or(' '))
  inline def superscript: TextType = TextType.map(text, hieroglyph.superscript(_).or(' '))

extension (text: into Text)
  inline def rsub(from: into Text, to: into Text): Text = Text(text.s.replaceAll(from.s, to.s).nn)

  inline def sub(from: into Text, to: into Text): Text =
    text.s.replaceAll(Pattern.quote(from.s).nn, to.s).nn.tt

  def flatMap(lambda: Char => Text): Text =
    String(text.s.toCharArray.nn.flatMap(lambda(_).s.toCharArray.nn.immutable(using Unsafe))).tt

  inline def urlEncode: Text = URLEncoder.encode(text.s, "UTF-8").nn.tt
  inline def urlDecode: Text = URLDecoder.decode(text.s, "UTF-8").nn.tt
  inline def punycode: Text = java.net.IDN.toASCII(text.s).nn.tt
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

extension (iarray: IArray[Char]) def text: Text = Text(String(iarray.mutable(using Unsafe)))

extension [TextType: Joinable](values: Iterable[TextType])
  def join: TextType = TextType.join(values)

  def join(separator: TextType): TextType =
    TextType.join(values.flatMap(Iterable(separator, _)).drop(1))

  def join(left: TextType, separator: TextType, right: TextType): TextType =
    Iterable(left, join(separator), right).join

  def join(separator: TextType, penultimate: TextType): TextType = values.size match
    case 0 => Iterable().join
    case 1 => values.head
    case _ => Iterable(values.init.join(separator), penultimate, values.last).join

  def join(left: TextType, separator: TextType, penultimate: TextType, right: TextType): TextType =
    Iterable(left, join(separator, penultimate), right).join

extension (buf: StringBuilder)
  def add(text: into Text): Unit = buf.append(text.s)
  def add(char: Char): Unit = buf.append(char)
  def text: Text = Text(buf.toString)

package decimalFormatters:
  given DecimalConverter as java:
    def decimalize(double: Double): Text = double.toString.tt
