                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.36.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package gossamer

import anticipation.*
import denominative.*
import distillate.*
import fulminate.*
import hieroglyph.*
import hypotenuse.*
import kaleidoscope.*
import prepositional.*
import proscenium.*
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

export Gossamer.opaques.Ascii

inline def append[textual: Textual, value](using builder: Builder[textual])(value: value): Unit =
  inline value match
    case text: Text => builder.append(textual(text))
    case char: Char => builder.append(textual(char))
    case other      => provide[textual.Show[value]](builder.append(textual.show(value)))

inline def appendln[textual: Textual, value](using builder: Builder[textual])(value: value): Unit =
  append[textual, value](value)
  builder.append(textual('\n'))


extension (textObject: Text.type)
  def construct(block: (builder: TextBuilder) ?=> Unit): Text =
    val builder = TextBuilder()
    block(using builder)
    builder()

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

  def text(using decoder: CharDecoder): Text = decoder.decoded(bytes)

  // Printable Unicode Encoding
  def pue: Text =
    bytes.map: b =>
      val i = b&0xff
      (if i%0x80 <= 0x20 || i == 0x7f then i + 0x100 else i).toChar

    . mkString.tt

extension [textual](text: textual)
  def cut[delimiter](delimiter: delimiter, limit: Int = Int.MaxValue)
       (using cuttable: textual is Cuttable by delimiter)
  : List[textual] =

    cuttable.cut(text, delimiter, limit)

extension [textual: Textual](words: Iterable[textual])
  def pascal: textual = words.map(_.lower.capitalize).join
  def camel: textual = pascal.uncapitalize
  def snake: textual = words.join(textual("_".tt))
  def kebab: textual = words.join(textual("-".tt))
  def spaced: textual = words.join(textual(" ".tt))

extension [textual: Textual](text: textual)
  inline def length: Int = textual.length(text)
  inline def populated: Optional[textual] = if text == textual.empty then Unset else text
  inline def lower: textual = textual.map(text)(_.toLower)
  inline def upper: textual = textual.map(text)(_.toUpper)
  def plain: Text = textual.text(text)

  def broken(predicate: (Char, Char) => Boolean, break: Char = '\u200b'): textual =
    val breakText = textual(break.toString.tt)
    val builder = textual.builder()

    @tailrec
    def recur(from: Ordinal = Prim, index: Ordinal = Sec): textual =
        if index >= Ult.of(text) then
          builder.append(text.from(from))
          builder()
        else
          if !predicate(textual.unsafeChar(text, index - 1), textual.unsafeChar(text, index))
          then recur(from, index + 1)
          else
            builder.append(text.segment(from ~ index.previous))
            builder.append(breakText)
            recur(index, index + 1)

    recur()

  // FIXME
  def justify(width: Int): textual =
    val words = text.words
    val extra = width - text.length

    def recur(word: Ordinal, spaces: Int, result: textual): textual =
      if word == Prim then result else
        val gap = ((spaces.toDouble/word.n0) + 0.5).toInt
        recur(word - 1, spaces - gap, result+textual(t" "*(gap + 1))+words(words.length - word.n0))

    recur(Prim, extra, words(0))

  def before(ordinal: Ordinal): textual = text.segment(Prim ~ (ordinal - 1))
  def after(ordinal: Ordinal): textual = text.segment((ordinal + 1) ~ Ult.of(text))
  def upto(ordinal: Ordinal): textual = text.segment(Prim ~ ordinal)
  def from(ordinal: Ordinal): textual = text.segment(ordinal ~ Ult.of(text))

  def slices(size: Int): List[textual] =
    val length = text.length
    List.tabulate[textual]((length - 1)/size + 1): i =>
      text.segment((i*size).z ~ Ordinal.natural(((i + 1)*size).min(length)))

  def skip(count: Int, bidi: Bidi = Ltr): textual = bidi match
    case Ltr => text.segment(count.z ~ Ult.of(text))
    case Rtl => text.segment(Prim ~ Countback(count).of(text))

  def keep(count: Int, bidi: Bidi = Ltr): textual = bidi match
    case Ltr => text.segment(Interval.initial(count))
    case Rtl => text.segment(Countback(count - 1).of(text) ~ Ult.of(text))

  def capitalize: textual = textual.concat(text.keep(1).upper, text.after(Prim))
  def uncapitalize: textual = textual.concat(text.keep(1).lower, text.after(Prim))

  inline def tail: textual = text.skip(1, Ltr)
  inline def init: textual = text.skip(1, Rtl)
  inline def empty: Boolean = text.length == 0

  def chars: IArray[Char] = textual.text(text).s.toCharArray.nn.immutable(using Unsafe)

  def snip(n: Int): (textual, textual) =
    (text.segment(Prim ~ (n - 1).z), text.segment(n.z ~ Ult.of(text)))

  def punch(n: Ordinal): (textual, textual) =
    (text.segment(Prim ~ (n - 1)), text.segment((n + 1) ~ Ult.of(text)))

  def reverse: textual =
    def recur(index: Ordinal, result: textual): textual =
      if index <= Ult.of(text)
      then recur(index + 1, textual.concat(text.segment(index ~ index), result))
      else result

    recur(Prim, textual.empty)

  def contains(substring: into Text): Boolean = textual.indexOf(text, substring).present
  def contains(char: Char): Boolean = textual.indexOf(text, char.show).present

  def search(regex: Regex, overlap: Boolean = false): Stream[textual] =
    regex.search(textual.text(text), overlap = overlap).map(text.segment(_))

  def extract[value](start: Ordinal)(lambda: Scanner ?=> textual ~> value): Stream[value] =

    val input = textual.text(text)
    if start.n0 < input.s.length then
      val scanner = Scanner(start.n0)
      lambda(using scanner).lift(text) match
        case Some(head) => head #:: extract(scanner.nextStart.or(0).z)(lambda)
        case _          => Stream()

    else Stream()

  def seek(regex: Regex): Optional[textual] = regex.seek(textual.text(text)).let(text.segment(_))

  inline def trim: textual =
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
      else if pred(textual.unsafeChar(text, ordinal)) then ordinal
      else recur(ordinal + step)

    recur(first)

  def before(pred: Char => Boolean): textual =
    val end: Ordinal = text.where(pred).or(Ult.of(text))
    text.before(end)

  def upto(pred: Char => Boolean): textual =
    val end: Ordinal = text.where(pred).or(Ult.of(text))
    text.upto(end)

  def dropWhile(pred: Char => Boolean): textual =
    text.where(!pred(_)).lay(textual.empty): ordinal =>
      text.segment(ordinal ~ Ult.of(text))

  def whilst(pred: Char => Boolean): textual =
    text.where(!pred(_)).lay(textual.empty): ordinal =>
      text.before(ordinal)

  def snip(pred: Char => Boolean, index: Ordinal = Prim): Optional[(textual, textual)] =
    text.where(pred, index).let(_.n0).let(text.snip(_))

  def mapChars(lambda: Char => Char): textual = textual.map(text)(lambda)

  def erase(chars: Char*): textual =
    val set = chars.to(Set)
    textual.builder().build:
      textual.map(text): char =>
        if !set.contains(char) then append(char)
        char

  inline def count(pred: Char => Boolean): Int =
    def recur(index: Ordinal, sum: Int): Int = if index > Ult.of(text) then sum else
      val increment = if pred(textual.unsafeChar(text, index)) then 1 else 0
      recur(index + 1, sum + increment)

    recur(Prim, 0)

  def pad(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using Text is Measurable): textual =
    if text.plain.metrics >= length then text else
      val padding = textual(char.toString.tt)*(length - text.plain.metrics)

      bidi match
        case Ltr => textual.concat(text, padding)
        case Rtl => textual.concat(padding, text)

  def center(length: Int, char: Char = ' ')(using Text is Measurable): textual =
    text.pad((length + text.plain.metrics)/2, char = char).pad(length, Rtl, char = char)

  def fit(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using Text is Measurable): textual =
    bidi match
      case Ltr => text.pad(length, bidi, char).keep(length, Ltr)
      case Rtl => text.pad(length, bidi, char).keep(length, Rtl)

  def uncamel: List[textual] =
    def recur(text: textual): List[textual] =
      text.where(_.isUpper, Sec).lay(List(text.lower)): index =>
        text.before(index).lower :: recur(text.from(index))

    recur(text)

  def words: List[textual] = text.cut(" ".tt)
  def lines: List[textual] = text.cut("\n".tt)
  def unkebab: List[textual] = text.cut("-".tt)
  def unsnake: List[textual] = text.cut("_".tt)

  def starts(prefix: into Text): Boolean =
    def recur(index: Ordinal): Boolean =
      index > (prefix.length - 1).z
      || textual.unsafeChar(text, index) == prefix.s.charAt(index.n0) && recur(index + 1)

    prefix.length <= text.length && recur(Prim)

  def ends(suffix: into Text): Boolean = text.keep(suffix.length, Rtl) == suffix

  inline def tr(from: Char, to: Char): textual =
    textual.map(text)(char => if char == from then to else char)

  // Extension method is applied explicitly because it appears ambiguous otherwise
  inline def subscripts: textual = textual.map(text)(_.subscript.or(' '))
  inline def superscripts: textual = textual.map(text)(_.superscript.or(' '))

package proximityMeasures:
  given jaroDistance: Proximity = (left, right) =>
   if left == right then 1.0 else
     val maxDist: Int = left.length.max(right.length)/2 - 1
     val found1 = new scm.BitSet(left.length)
     val found2 = new scm.BitSet(right.length)

     @tailrec
     def recur(i: Int, j: Int, matches: Int): Int =
       if i >= left.length then matches else
         if j >= (i + maxDist + 1).min(right.length)
         then recur(i + 1, (i + 1 - maxDist).max(0), matches)
         else if left.s.charAt(i) == right.s.charAt(j) && !found2(j) then
           found1(i) = true
           found2(j) = true
           recur(i + 1, (i + 1 - maxDist).max(0), matches + 1)
         else recur(i, j + 1, matches)

     val matches = recur(0, 0, 0)

     def trans(i: Int, j: Int, count: Int): Int =
       if i >= left.length then count else if found1(i) then
         def next(j: Int): Int = if found2(j) then j else next(j + 1)
         val j2 = next(j)
         trans(i + 1, j2 + 1, if left.s.charAt(i) == right.s.charAt(j2) then count else count + 1)
       else trans(i + 1, j, count)

     val count = trans(0, 0, 0)

     if matches == 0 then 0.0
     else (matches.toDouble/left.length + matches.toDouble/right.length +
         (matches - count/2.0)/matches)/3

  given prefixMatch: Proximity = (left, right) =>
    val limit = left.length.min(right.length)

    def recur(index: Int = 0): Int = if index >= limit then index else
      if left.s.charAt(index) == right.s.charAt(index) then recur(index + 1) else index

    recur()

  given jaroWinklerDistance: Proximity = (left, right) =>
    val scale = 0.1
    val distance = jaroDistance.distance(left, right)
    distance + scale*prefixMatch.distance(left, right).min(4.0)*(1.0 - distance)

  given levenshteinDistance: Proximity = (left, right) =>
    val m = left.s.length
    val n = right.length
    val old = new Array[Int](n + 1)
    val dist = new Array[Int](n + 1)

    for j <- 1 to n do old(j) = old(j - 1) + 1

    for i <- 1 to m do
      dist(0) = old(0) + 1

      for j <- 1 to n do
        dist(j) = (old(j - 1) + (if left.s.charAt(i - 1) == right.s.charAt(j - 1) then 0 else 1))
        . min(old(j) + 1).min(dist(j - 1) + 1)

      for j <- 0 to n do old(j) = dist(j)

    dist(n)

  given normalizedLevenshteinDistance: Proximity = (left, right) =>
    levenshteinDistance.distance(left, right)/left.length.max(right.length)

extension (text: into Text)
  inline def sub(from: into Text, to: into Text): Text =
    text.s.replaceAll(jur.Pattern.quote(from.s).nn, to.s).nn.tt

  inline def sub(from: Regex, to: into Text): Text = text.s.replaceAll(from.pattern.s, to.s).nn.tt

  inline def urlEncode: Text = URLEncoder.encode(text.s, "UTF-8").nn.tt
  inline def urlDecode: Text = URLDecoder.decode(text.s, "UTF-8").nn.tt
  inline def punycode: Text = java.net.IDN.toASCII(text.s).nn.tt
  inline def bytes(using encoder: CharEncoder): IArray[Byte] = encoder.encode(text)
  inline def sysBytes: IArray[Byte] = CharEncoder.system.encode(text)

  def proximity(other: into Text)(using proximity: Proximity): Double =
    proximity.distance(text, other)

extension (iarray: IArray[Char]) def text: Text = String(iarray.mutable(using Unsafe)).tt

extension [textual: Joinable](values: Iterable[textual])
  def join: textual = textual.join(values)

  def join(separator: textual): textual =
    textual.join(values.flatMap(Iterable(separator, _)).drop(1))

  def join(left: textual, separator: textual, right: textual): textual =
    Iterable(left, join(separator), right).join

  def join(separator: textual, penultimate: textual): textual = values.size match
    case 0 => Iterable().join
    case 1 => values.head
    case _ => Iterable(values.init.join(separator), penultimate, values.last).join

  def join(left: textual, separator: textual, penultimate: textual, right: textual): textual =
    Iterable(left, join(separator, penultimate), right).join

extension (builder: StringBuilder)
  def add(text: into Text): Unit = builder.append(text.s)
  def add(char: Char): Unit = builder.append(char)
  def text: Text = builder.toString.tt

package decimalFormatters:
  given java: DecimalConverter:
    def decimalize(double: Double): Text = double.toString.tt

package enumIdentification:
  given kebabCase: [enumeration <: reflect.Enum] => enumeration is Identifiable =
    Identifiable(_.uncamel.kebab, _.unkebab.pascal)

  given snakeCase: [enumeration <: reflect.Enum] => enumeration is Identifiable =
    Identifiable(_.uncamel.snake, _.unsnake.pascal)

  given pascalCase: [enumeration <: reflect.Enum] => enumeration is Identifiable =
    Identifiable(identity(_), identity(_))

  given camelCase: [enumeration <: reflect.Enum] => enumeration is Identifiable =
    Identifiable(_.uncamel.camel, _.unsnake.pascal)
