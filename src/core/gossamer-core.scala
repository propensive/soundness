/*
    Gossamer, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import denominative.*
import fulminate.*
import hieroglyph.*
import hypotenuse.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import scala.reflect.*
import scala.collection.mutable as scm

import java.util.regex as jur
import java.net.{URLEncoder, URLDecoder}

import language.experimental.pureFunctions
import language.experimental.into

export Gossamer2.opaques.Ascii

def append[TextType: Textual, ValueType](using buffer: Buffer[TextType])(value: ValueType)
   (using TextType.Show[ValueType])
        : Unit =
  buffer.append(TextType.show(value))

def appendln[TextType: Textual, ValueType](using buffer: Buffer[TextType])(value: ValueType)
   (using TextType.Show[ValueType])
        : Unit =
  buffer.append(TextType.show(value))
  buffer.append(TextType("\n".tt))

extension (textObject: Text.type)
  def construct(block: (buffer: TextBuffer) ?=> Unit): Text =
    val buffer = TextBuffer()
    block(using buffer)
    buffer()

  def ascii(bytes: Bytes): Text = new String(bytes.mutable(using Unsafe), "ASCII").tt

  def fill(length: Int)(lambda: Int => Char): Text =
    val array = new Array[Char](length)
    (0 until length).each { index => array(index) = lambda(index) }

    String(array).tt

extension (inline ctx: StringContext)
  transparent inline def txt(inline parts: Any*): Text = ${Interpolation.Text.expand('ctx, 'parts)}
  transparent inline def t(inline parts: Any*): Text = ${Interpolation.T.expand('ctx, 'parts)}

extension (ctx: StringContext)
  def t = SimpleTExtractor(ctx.parts.head.tt)

extension (bytes: Bytes)
  def utf8: Text = String(bytes.mutable(using Unsafe), "UTF-8").tt
  def utf16: Text = String(bytes.mutable(using Unsafe), "UTF-16").tt
  def ascii: Text = String(bytes.mutable(using Unsafe), "ASCII").tt

  def hex: Text =
    bytes.mutable(using Unsafe).map { byte => String.format("\\u%04x", byte.toInt).nn }
    . mkString.tt

  def text(using decoder: CharDecoder): Text = decoder.decode(bytes)

  // Printable Unicode Encoding
  def pue: Text =
    bytes.map: b =>
      val i = b&0xff
      (if i%0x80 <= 0x20 || i == 0x7f then i + 0x100 else i).toChar

    . mkString.tt

extension [TextType](text: TextType)
  def cut[DelimiterType](delimiter: DelimiterType, limit: Int = Int.MaxValue)
     (using cuttable: TextType is Cuttable by DelimiterType)
          : List[TextType] =

    cuttable.cut(text, delimiter, limit)

extension [TextType: Textual](words: Iterable[TextType])
  def pascal: TextType = words.map(_.lower.capitalize).join
  def camel: TextType = pascal.uncapitalize
  def snake: TextType = words.join(TextType("_".tt))
  def kebab: TextType = words.join(TextType("-".tt))
  def spaced: TextType = words.join(TextType(" ".tt))

extension [TextType: Textual](text: TextType)
  inline def length: Int = TextType.length(text)
  inline def populated: Optional[TextType] = if text == TextType.empty then Unset else text
  inline def lower: TextType = TextType.map(text, _.toLower)
  inline def upper: TextType = TextType.map(text, _.toUpper)
  def plain: Text = TextType.text(text)

  def broken(predicate: (Char, Char) => Boolean, break: Char = '\u200b'): TextType =
    val breakText = TextType(break.toString.tt)
    val buffer = TextType.buffer()

    @tailrec
    def recur(from: Ordinal = Prim, index: Ordinal = Sec): TextType =
        if index >= Ult.of(text) then
          buffer.append(text.from(from))
          buffer()
        else
          if !predicate(TextType.unsafeChar(text, index - 1), TextType.unsafeChar(text, index))
          then recur(from, index + 1)
          else
            buffer.append(text.segment(from ~ index.previous))
            buffer.append(breakText)
            recur(index, index + 1)

    recur()

  // FIXME
  def justify(width: Int): TextType =
    val words = text.words
    val extra = width - text.length

    def recur(word: Ordinal, spaces: Int, result: TextType): TextType =
      if word == Prim then result else
        val gap = ((spaces.toDouble/word.n0) + 0.5).toInt
        recur(word - 1, spaces - gap, result+TextType(t" "*(gap + 1))+words(words.length - word.n0))

    recur(Prim, extra, words(0))

  def before(ordinal: Ordinal): TextType = text.segment(Prim ~ (ordinal - 1))
  def after(ordinal: Ordinal): TextType = text.segment((ordinal + 1) ~ Ult.of(text))
  def upto(ordinal: Ordinal): TextType = text.segment(Prim ~ ordinal)
  def from(ordinal: Ordinal): TextType = text.segment(ordinal ~ Ult.of(text))

  def slices(size: Int): List[TextType] =
    val length = text.length
    List.tabulate[TextType]((length - 1)/size + 1): i =>
      text.segment((i*size).z ~ Ordinal.natural(((i + 1)*size).min(length)))

  def skip(count: Int, bidi: Bidi = Ltr): TextType = bidi match
    case Ltr => text.segment(count.z ~ Ult.of(text))
    case Rtl => text.segment(Prim ~ Countback(count).of(text))

  def keep(count: Int, bidi: Bidi = Ltr): TextType = bidi match
    case Ltr => text.segment(Interval.initial(count))
    case Rtl => text.segment(Countback(count - 1).of(text) ~ Ult.of(text))

  def capitalize: TextType = TextType.concat(text.keep(1).upper, text.after(Prim))
  def uncapitalize: TextType = TextType.concat(text.keep(1).lower, text.after(Prim))

  inline def tail: TextType = text.skip(1, Ltr)
  inline def init: TextType = text.skip(1, Rtl)
  inline def empty: Boolean = text.length == 0

  def chars: IArray[Char] = TextType.text(text).s.toCharArray.nn.immutable(using Unsafe)

  def snip(n: Int): (TextType, TextType) =
    (text.segment(Prim ~ (n - 1).z), text.segment(n.z ~ Ult.of(text)))

  def punch(n: Ordinal): (TextType, TextType) =
    (text.segment(Prim ~ (n - 1)), text.segment((n + 1) ~ Ult.of(text)))

  def reverse: TextType =
    def recur(index: Ordinal, result: TextType): TextType =
      if index <= Ult.of(text)
      then recur(index + 1, TextType.concat(text.segment(index ~ index), result))
      else result

    recur(Prim, TextType.empty)

  def contains(substring: into Text): Boolean = TextType.indexOf(text, substring).present
  def contains(char: Char): Boolean = TextType.indexOf(text, char.show).present

  def search(regex: Regex, overlap: Boolean = false): LazyList[TextType] =
    regex.search(TextType.text(text), overlap = overlap).map(text.segment(_))

  def extract[ValueType](start: Ordinal)(lambda: Matching ?=> TextType ~> ValueType)
          : LazyList[ValueType] =

    val input = TextType.text(text)
    if start.n0 < input.s.length then
      val matching = Matching(start.n0)
      lambda(using matching).lift(text) match
        case Some(head) => head #:: extract(matching.nextStart.or(0).z)(lambda)
        case _          => LazyList()

    else LazyList()

  def seek(regex: Regex): Optional[TextType] = regex.seek(TextType.text(text)).let(text.segment(_))

  inline def trim: TextType =
    val start = text.where(!_.isWhitespace).or(Ult.of(text))
    val end = text.where(!_.isWhitespace, bidi = Rtl).or(Prim)
    text.segment(start ~ end)

  def where(pred: Char => Boolean, start: Optional[Ordinal] = Unset, bidi: Bidi = Ltr)
          : Optional[Ordinal] =
    val step: Int = bidi match
      case Ltr => 1
      case Rtl => -1

    val first: Ordinal = bidi match
      case Ltr => start.or(Prim)
      case Rtl => start.or(Ult.of(length))

    def recur(ordinal: Ordinal): Optional[Ordinal] =
      if ordinal > Ult.of(text) || ordinal < Prim then Unset
      else if pred(TextType.unsafeChar(text, ordinal)) then ordinal
      else recur(ordinal + step)

    recur(first)

  def before(pred: Char => Boolean): TextType =
    val end: Ordinal = text.where(pred).or(Ult.of(text))
    text.before(end)

  def upto(pred: Char => Boolean): TextType =
    val end: Ordinal = text.where(pred).or(Ult.of(text))
    text.upto(end)

  def dropWhile(pred: Char => Boolean): TextType =
    text.where(!pred(_)).lay(TextType.empty): ordinal =>
      text.segment(ordinal ~ Ult.of(text))

  def whilst(pred: Char => Boolean): TextType =
    text.where(!pred(_)).lay(TextType.empty): ordinal =>
      text.before(ordinal)

  def snip(pred: Char => Boolean, index: Ordinal = Prim): Optional[(TextType, TextType)] =
    text.where(pred, index).let(_.n0).let(text.snip(_))

  def mapChars(lambda: Char => Char): TextType = TextType.map(text, lambda)

  inline def count(pred: Char => Boolean): Int =
    def recur(index: Ordinal, sum: Int): Int = if index > Ult.of(text) then sum else
      val increment = if pred(TextType.unsafeChar(text, index)) then 1 else 0
      recur(index + 1, sum + increment)

    recur(Prim, 0)

  def metrics(using metrics: TextMetrics) = metrics.width(TextType.text(text))

  def pad(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using TextMetrics): TextType =
    if text.metrics >= length then text else
      val padding = TextType(char.toString.tt)*(length - text.metrics + 1)

      bidi match
        case Ltr => TextType.concat(text, padding)
        case Rtl => TextType.concat(padding, text)

  def center(length: Int, char: Char = ' ')(using TextMetrics): TextType =
    text.pad((length + text.metrics)/2, char = char).pad(length, Rtl, char = char)

  def fit(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using TextMetrics): TextType =
    bidi match
      case Ltr => text.pad(length, bidi, char).keep(length, Ltr)
      case Rtl => text.pad(length, bidi, char).keep(length, Rtl)

  def uncamel: List[TextType] =
    def recur(text: TextType): List[TextType] =
      text.where(_.isUpper, Sec).lay(List(text.lower)): index =>
        text.before(index).lower :: recur(text.from(index))

    recur(text)

  def words: List[TextType] = text.cut(" ".tt)
  def lines: List[TextType] = text.cut("\n".tt)
  def unkebab: List[TextType] = text.cut("-".tt)
  def unsnake: List[TextType] = text.cut("_".tt)

  def starts(prefix: into Text): Boolean =
    def recur(index: Ordinal): Boolean =
      index > Ult.of(prefix) ||
          TextType.unsafeChar(text, index) == prefix.s.charAt(index.n0) && recur(index + 1)

    prefix.length <= text.length && recur(Prim)

  def ends(suffix: into Text): Boolean = text.keep(suffix.length, Rtl) == suffix

  inline def tr(from: Char, to: Char): TextType =
    TextType.map(text, char => if char == from then to else char)

  // Extension method is applied explicitly because it appears ambiguous otherwise
  inline def subscript: TextType = TextType.map(text, hieroglyph.subscript(_).or(' '))
  inline def superscript: TextType = TextType.map(text, hieroglyph.superscript(_).or(' '))

extension (text: into Text)
  inline def sub(from: into Text, to: into Text): Text =
    text.s.replaceAll(jur.Pattern.quote(from.s).nn, to.s).nn.tt

  inline def sub(from: Regex, to: into Text): Text = text.s.replaceAll(from.pattern.s, to.s).nn.tt

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

        . min(old(j) + 1).min(dist(j - 1) + 1)

      for j <- 0 to n do old(j) = dist(j)

    dist(n)

  def prefixMatch(other: Text): Int =
    val limit = text.length.min(other.length)

    def recur(index: Int = 0): Int = if index >= limit then index else
      if text.s.charAt(index) == other.s.charAt(index) then recur(index + 1) else index

    recur()

  def jaroWinklerDistance(other: Text, scale: Double = 0.1): Double =
    val distance = jaroDistance(other)
    distance + scale*prefixMatch(other).min(4)*(1 - distance)

  def jaroDistance(other: Text): Double = if text == other then 1.0 else
    val maxDist: Int = text.length.max(other.length)/2 - 1
    val found1 = new scm.BitSet(text.length)
    val found2 = new scm.BitSet(other.length)

    @tailrec
    def recur(i: Int, j: Int, matches: Int): Int =
      if i >= text.length then matches else
        if j >= (i + maxDist + 1).min(other.length)
        then recur(i + 1, (i + 1 - maxDist).max(0), matches)
        else if text.s.charAt(i) == other.s.charAt(j) && !found2(j) then
          found1(i) = true
          found2(j) = true
          recur(i + 1, (i + 1 - maxDist).max(0), matches + 1)
        else recur(i, j + 1, matches)

    val matches = recur(0, 0, 0)

    def trans(i: Int, j: Int, count: Int): Int =
      if i >= text.length then count else if found1(i) then
        def next(j: Int): Int = if found2(j) then j else next(j + 1)
        val j2 = next(j)
        trans(i + 1, j2 + 1, if text.s.charAt(i) == other.s.charAt(j2) then count else count + 1)
      else trans(i + 1, j, count)

    val count = trans(0, 0, 0)

    if matches == 0 then 0.0
    else (matches.toDouble/text.length + matches.toDouble/other.length +
        (matches - count/2.0)/matches)/3

extension (iarray: IArray[Char]) def text: Text = String(iarray.mutable(using Unsafe)).tt

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
  def text: Text = buf.toString.tt

package decimalFormatters:
  given DecimalConverter as java:
    def decimalize(double: Double): Text = double.toString.tt

package enumIdentification:
  given [EnumType <: reflect.Enum] => EnumType is Identifiable as kebabCase =
    Identifiable(_.uncamel.kebab, _.unkebab.pascal)

  given [EnumType <: reflect.Enum] => EnumType is Identifiable as snakeCase =
    Identifiable(_.uncamel.snake, _.unsnake.pascal)

  given [EnumType <: reflect.Enum] => EnumType is Identifiable as pascalCase =
    Identifiable(identity(_), identity(_))

  given [EnumType <: reflect.Enum] => EnumType is Identifiable as camelCase =
    Identifiable(_.uncamel.camel, _.unsnake.pascal)
