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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import language.experimental.into
import language.experimental.pureFunctions

import java.lang as jl
import java.net.{URLEncoder, URLDecoder}
import java.util.regex as jur

import scala.collection.mutable as scm
import scala.reflect.*

import anticipation.*
import denominative.*
import distillate.*
import fulminate.*
import hieroglyph.*
import hypotenuse.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import Textual.concatenable

export gossamer.internal.opaques.{Ascii, Grapheme}

inline def append[textual: Textual, value](using builder: Builder[textual] aka "builder")
  ( value: value )
:   Unit =

  inline value match
    case text: Text => builder().append(textual(text))
    case char: Char => builder().append(char)
    case other      => provide[textual.Show[value]](builder().append(textual.show(value)))


inline def appendln[textual: Textual, value](using builder: Builder[textual] aka "builder")
  ( value: value )
:   Unit =

  append[textual, value](value)
  builder().append('\n')


inline def builder[value](using value: value aka "builder"): value = value()

extension (module: IArray.type)
  def build[element: ClassTag](size: Int)(lambda: Array[element] => Unit): IArray[element] =
    val array: Array[element] = new Array[element](size)
    lambda(array)
    array.immutable(using Unsafe)

extension (module: Text.type)
  def build(block: TextBuilder aka "builder" ?=> Unit): Text =
    val builder = TextBuilder()
    block(using builder.aka["builder"])
    builder()

  def ascii(bytes: Data): Text = new String(bytes.mutable(using Unsafe), "ASCII").tt

  def fill(length: Int)(lambda: Int => Char): Text =
    val array = new Array[Char](length)
    (0 until length).each: index => array(index) = lambda(index)

    String(array).tt

extension (inline context: StringContext)
  transparent inline def txt(inline parts: Any*): Text =
    ${gossamer.internal.txt('context, 'parts)}

  transparent inline def t(inline parts: Any*): Text =
    ${gossamer.internal.t('context, 'parts)}

extension (context: StringContext)
  def t = SimpleTExtractor(context.parts.head.tt)

extension (bytes: Data)
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
    ( using cuttable: textual is Cuttable by delimiter )
  :   List[textual] =

    cuttable.cut(text, delimiter, limit)

extension [textual: Textual { type Result = Char }](words: Iterable[textual])
  def pascal: textual = words.map(_.lower.capitalize).join
  def camel: textual = pascal.uncapitalize
  def snake: textual = words.join(textual("_".tt))
  def kebab: textual = words.join(textual("-".tt))
  def spaced: textual = words.join(textual(" ".tt))

extension [value: {Segmentable, Countable}](value: value)
  def before(ordinal: Ordinal): value = value.segment(Prim till ordinal)
  def upto(ordinal: Ordinal): value = value.segment(Prim thru ordinal)
  def from(ordinal: Ordinal): value = value.segment(ordinal thru value.limit)
  def after(ordinal: Ordinal): value = value.segment((ordinal + 1) till value.limit)

extension [textual: Textual](text: textual)
  inline def length: Int = textual.length(text)
  def plain: Text = textual.text(text)

  // FIXME
  def justify(width: Int): textual =
    val words = text.words
    val extra = width - text.length

    def recur(word: Ordinal, spaces: Int, result: textual): textual =
      if word == Prim then result else
        val gap = ((spaces.toDouble/word.n0) + 0.5).toInt
        recur(word - 1, spaces - gap, result+textual(t" "*(gap + 1))+words(words.length - word.n0))

    recur(Prim, extra, words(0))

  def slices(size: Int): List[textual] =
    val length = text.length

    List.tabulate[textual]((length - 1)/size + 1): i =>
      text.segment((i*size).z thru ((i + 1)*size).min(length).u)

  def skip(count: Int, bidi: Bidi = Ltr): textual = bidi match
    case Ltr => text.segment(count.z till text.limit)
    case Rtl => text.segment(Prim till text.limit - count)

  def keep(count: Int, bidi: Bidi = Ltr): textual = bidi match
    case Ltr => text.segment(Interval.initial(count))
    case Rtl => text.segment(text.limit - count till text.limit)

  inline def tail: textual = text.skip(1, Ltr)
  inline def init: textual = text.skip(1, Rtl)

  def chars: IArray[Char] = textual.text(text).s.toCharArray.nn.immutable(using Unsafe)

  def snip(n: Int): (textual, textual) =
    (text.segment(Prim till n.z), text.segment(n.z till text.limit))

  def punch(n: Ordinal): (textual, textual) =
    (text.segment(Prim till n), text.segment((n + 1) till text.limit))

  def reverse: textual =
    val n = text.length
    val builder = textual.builder(n)
    var index = n - 1

    while index >= 0 do
      builder.append(textual.single(textual.access(text, index.z)))
      index -= 1

    builder()

  def contains(substring: Text): Boolean = textual.indexOf(text, substring).present

  def search(regex: Regex, overlap: Boolean = false): Stream[textual] =
    regex.search(textual.text(text), overlap = overlap).map(text.segment(_))

  inline def extract[value](inline start: Ordinal = Prim)
    ( inline lambda: Scanner ?=> textual ~> value )
  :   Stream[value] =

    $ {
        gossamer.internal.extractMacro[textual, value]
          ( 'text, 'start, 'lambda, '{compiletime.summonInline[textual is Textual]} )
      }

  // `offsetOf` returns the index (`Ordinal`) at which `substring` first occurs. It
  // is distinct from the generic `Traversable` `seek`/`where`, which act on
  // individual elements via a predicate, and from `Textual.indexOf`, the codec-
  // level primitive it delegates to. For regex matches, use `search`.
  def offsetOf(substring: Text, bidi: Bidi = Ltr): Optional[Ordinal] = bidi match
    case Ltr => textual.indexOf(text, substring)
    case Rtl => if substring.nil then Unset else textual.lastIndexOf(text, substring)

  def count(substring: Text): Int =
    if substring.nil then 0 else
      def recur(start: Ordinal, total: Int): Int =
        textual.indexOf(text, substring, start).lay(total): found =>
          recur(found + substring.length, total + 1)

      recur(Prim, 0)

  def words: List[textual] = text.cut(" ".tt)
  def lines: List[textual] = text.cut("\n".tt)
  def unkebab: List[textual] = text.cut("-".tt)
  def unsnake: List[textual] = text.cut("_".tt)

  def starts(prefix: Text): Boolean = textual.text(text).s.startsWith(prefix.s)
  def ends(suffix: Text): Boolean = textual.text(text).s.endsWith(suffix.s)

  def strip(affix: Text, bidi: Bidi = Ltr): textual = bidi match
    case Ltr => if text.starts(affix) then text.skip(affix.length) else text
    case Rtl => if text.ends(affix) then text.skip(affix.length, Rtl) else text

extension [textual: Textual { type Result = Char }](text: textual)
  inline def lower: textual = textual.map(text)(_.toLower)
  inline def upper: textual = textual.map(text)(_.toUpper)

  def broken(predicate: (Char, Char) => Boolean, break: Char = '\u200b'): textual =
    val breakText = textual(break.toString.tt)
    val builder = textual.builder()

    @tailrec
    def recur(from: Ordinal = Prim, index: Ordinal = Sec): textual =
      if index >= text.limit - 1 then
        builder.append(text.from(from))
        builder()
      else
        if !predicate(textual.access(text, index - 1), textual.access(text, index))
        then recur(from, index + 1)
        else
          builder.append(text.segment(from till index))
          builder.append(breakText)
          recur(index, index + 1)

    recur()

  def skip(predicate: Char => Boolean): textual = text.skip(predicate, Ltr)

  def skip(predicate: Char => Boolean, bidi: Bidi): textual = bidi match
    case Ltr => text.pinpoint(!predicate(_)).lay(textual.empty)(text.from(_))
    case Rtl => text.pinpoint(!predicate(_), bidi = Rtl).lay(textual.empty)(text.upto(_))

  def keep(predicate: Char => Boolean): textual = text.keep(predicate, Ltr)

  def keep(predicate: Char => Boolean, bidi: Bidi): textual = bidi match
    case Ltr => text.pinpoint(!predicate(_)).lay(text)(text.before(_))
    case Rtl => text.pinpoint(!predicate(_), bidi = Rtl).lay(text)(text.after(_))

  def capitalize: textual = textual.concat(text.keep(1).upper, text.after(Prim))
  def uncapitalize: textual = textual.concat(text.keep(1).lower, text.after(Prim))

  def contains(char: Char): Boolean = textual.indexOf(text, char.show).present

  inline def trim: textual =
    val start = text.pinpoint(!_.isWhitespace).or(text.limit - 1)
    val end = text.pinpoint(!_.isWhitespace, bidi = Rtl).or(Prim)
    text.segment(start thru end)

  def trim(bidi: Bidi): textual = text.skip(_.isWhitespace, bidi)

  def pinpoint(predicate: Char => Boolean, start: Optional[Ordinal] = Unset, bidi: Bidi = Ltr)
  :   Optional[Ordinal] =

    val step: Int = bidi match
      case Ltr => 1
      case Rtl => -1

    val first: Ordinal = bidi match
      case Ltr => start.or(Prim)
      case Rtl => start.or(text.length.limit - 1)

    def recur(ordinal: Ordinal): Optional[Ordinal] =
      if ordinal >= text.limit || ordinal < Prim then Unset
      else if predicate(textual.access(text, ordinal)) then ordinal
      else recur(ordinal + step)

    recur(first)

  def before(predicate: Char => Boolean): textual =
    val end: Ordinal = text.pinpoint(predicate).or(text.limit - 1)
    text.before(end)

  def upto(predicate: Char => Boolean): textual =
    val end: Ordinal = text.pinpoint(predicate).or(text.limit - 1)
    text.upto(end)

  def snip(predicate: Char => Boolean, index: Ordinal = Prim): Optional[(textual, textual)] =
    text.pinpoint(predicate, index).let(_.n0).let(text.snip(_))

  def tr(lambda: Char => Char): textual = textual.map(text)(lambda)

  def erase(chars: Char*): textual =
    val set = chars.to(Set)

    textual.builder().build:
      textual.map(text): char =>
        if !set.contains(char) then append(char)
        char

  inline def count(predicate: Char => Boolean): Int =
    def recur(index: Ordinal, sum: Int): Int = if index >= text.limit then sum else
      val increment = if predicate(textual.access(text, index)) then 1 else 0
      recur(index + 1, sum + increment)

    recur(Prim, 0)

  def blank: Boolean = text.pinpoint(!_.isWhitespace).absent

  def pad(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using Text is Measurable): textual =
    val current = text.plain.metrics

    if current >= length then text else
      val padSize = length - current
      val builder = textual.builder(text.length + padSize)

      bidi match
        case Ltr =>
          builder.append(text)
          var i = 0

          while i < padSize do
            builder.append(char)
            i += 1

        case Rtl =>
          var i = 0

          while i < padSize do
            builder.append(char)
            i += 1

          builder.append(text)

      builder()

  def center(length: Int, char: Char = ' ')(using Text is Measurable): textual =
    text.pad((length + text.plain.metrics)/2, char = char).pad(length, Rtl, char = char)

  def fit(length: Int, bidi: Bidi = Ltr, char: Char = ' ')(using Text is Measurable): textual =
    bidi match
      case Ltr => text.pad(length, bidi, char).keep(length, Ltr)
      case Rtl => text.pad(length, bidi, char).keep(length, Rtl)

  def uncamel: List[textual] =
    def recur(text: textual): List[textual] =
      text.pinpoint(_.isUpper, Sec).lay(List(text.lower)): index =>
        text.before(index).lower :: recur(text.from(index))

    recur(text)

  inline def tr(from: Char, to: Char): textual =
    textual.map(text): char => if char == from then to else char

  inline def ossify: textual = text.tr(' ', ' ')

  // Extension method is applied explicitly because it appears ambiguous otherwise
  inline def subscripts: textual = textual.map(text)(_.subscript.or(' '))
  inline def superscripts: textual = textual.map(text)(_.superscript.or(' '))

package proximities:
  given jaroProximity: (sensitivity: CaseSensitivity) => Proximity by Double = (left, right) =>
    if left == right then 1.0 else
      val maxDist: Int = left.length.max(right.length)/2 - 1
      val found1 = new scm.BitSet(left.length)
      val found2 = new scm.BitSet(right.length)

      @tailrec
      def recur(i: Int, j: Int, matches: Int): Int =
        if i >= left.length then matches else
          if j >= (i + maxDist + 1).min(right.length)
          then recur(i + 1, (i + 1 - maxDist).max(0), matches)
          else if sensitivity.compare(left.s.charAt(i), right.s.charAt(j)) && !found2(j) then
            found1(i) = true
            found2(j) = true
            recur(i + 1, (i + 1 - maxDist).max(0), matches + 1)
          else
            recur(i, j + 1, matches)

      val matches = recur(0, 0, 0)

      def transform(i: Int, j: Int, count: Int): Int =
        if i >= left.length then count else if found1(i) then
          def next(j: Int): Int = if found2(j) then j else next(j + 1)
          val j2 = next(j)

          transform(i + 1,
                j2 + 1,
                if sensitivity.compare(left.s.charAt(i), right.s.charAt(j2))
                then count
                else count + 1)

        else
          transform(i + 1, j, count)

      val count = transform(0, 0, 0)

      if matches == 0 then 0.0 else
        ( matches.toDouble/left.length + matches.toDouble/right.length +
          (matches - count/2.0)/matches ) /
          3

  given prefixProximity: (sensitivity: CaseSensitivity) => Proximity by Int = (left, right) =>
    val limit = left.length.min(right.length)

    def recur(index: Int = 0): Int = if index >= limit then index else
      if sensitivity.compare(left.s.charAt(index), right.s.charAt(index))
      then recur(index + 1)
      else index

    recur()

  given jaroWinklerProximity: CaseSensitivity => Proximity by Double = (left, right) =>
    val scale = 0.1
    val distance = jaroProximity.distance(left, right)
    distance + scale*prefixProximity.distance(left, right).min(4)*(1.0 - distance)


  given levenshteinProximity: (sensitivity: CaseSensitivity)
  =>  (Proximity { type Triangulable = true }) by Int =

    (left, right) =>
      val m = left.s.length
      val n = right.length
      val old = new Array[Int](n + 1)
      val dist = new Array[Int](n + 1)

      for j <- 1 to n do old(j) = old(j - 1) + 1

      for i <- 1 to m do
        dist(0) = old(0) + 1

        for j <- 1 to n do
          val c =
            if sensitivity.compare(left.s.charAt(i - 1), right.s.charAt(j - 1)) then 0 else 1

          dist(j) = (old(j - 1) + c).min(old(j) + 1).min(dist(j - 1) + 1)

        for j <- 0 to n do old(j) = dist(j)

      if m == 0 then n else dist(n)


  given normalizedLevenshteinProximity: CaseSensitivity => Proximity by Double =
    (left, right) =>
      val span = left.length.max(right.length)
      if span == 0 then 0.0 else levenshteinProximity.distance(left, right).toDouble/span

extension (text: Text)
  def sub(from: Text, to: Text): Text =
    text.subPattern(jur.Pattern.compile(jur.Pattern.quote(from.s)).nn, to, Int.MaxValue)

  def sub(from: Text, to: Text, count: Int): Text =
    text.subPattern(jur.Pattern.compile(jur.Pattern.quote(from.s)).nn, to, count)

  def sub(from: Regex, to: Text): Text =
    text.subPattern(jur.Pattern.compile(from.pattern.s).nn, to, Int.MaxValue)

  def sub(from: Regex, to: Text, count: Int): Text =
    text.subPattern(jur.Pattern.compile(from.pattern.s).nn, to, count)

  private def subPattern(pattern: jur.Pattern, to: Text, count: Int): Text =
    if count <= 0 then text else
      val matcher = pattern.matcher(text.s).nn
      val builder = jl.StringBuilder()
      var n = 0

      while n < count && matcher.find() do
        matcher.appendReplacement(builder, to.s)
        n += 1

      matcher.appendTail(builder)
      builder.toString.nn.tt

  inline def urlEncode: Text = URLEncoder.encode(text.s, "UTF-8").nn.tt
  inline def urlDecode: Text = URLDecoder.decode(text.s, "UTF-8").nn.tt
  inline def punycode: Text = java.net.IDN.toASCII(text.s).nn.tt
  inline def data(using encoder: CharEncoder): IArray[Byte] = encoder.encode(text)
  inline def sysData: IArray[Byte] = CharEncoder.system.encode(text)

  inline def fuzzy[result]
    ( inline threshold: Double = Double.PositiveInfinity )
    ( inline cases: Text ~> result )
  :   result =

    $ {
        gossamer.internal.fuzzyMacro[result]
          ( 'text,
            'threshold,
            'cases,
            '{compiletime.summonInline[Proximity { type Operand = Double }]} )
      }

  def proximity(other: Text)(using proximity: Proximity): proximity.Operand =
    proximity.distance(text, other)

extension (iarray: IArray[Char]) def text: Text = String(iarray.mutable(using Unsafe)).tt

extension [textual: {Joinable, Textual}](values: Iterable[textual])
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
  def add(text: Text): Unit = builder.append(text.s)
  def add(char: Char): Unit = builder.append(char)
  def text: Text = builder.toString.tt

package decimalConverters:
  given javaDecimalConverter: DecimalConverter:
    def decimalize(double: Double): Text = double.toString.tt

package enumIdentification:
  given kebabCaseIdentifiable: [enumeration <: reflect.Enum] => enumeration is Identifiable =
    Identifiable(_.uncamel.kebab, _.unkebab.pascal)

  given snakeCaseIdentifiable: [enumeration <: reflect.Enum] => enumeration is Identifiable =
    Identifiable(_.uncamel.snake, _.unsnake.pascal)

  given pascalCaseIdentifiable: [enumeration <: reflect.Enum] => enumeration is Identifiable =
    Identifiable(identity(_), identity(_))

  given camelCaseIdentifiable: [enumeration <: reflect.Enum] => enumeration is Identifiable =
    Identifiable(_.uncamel.camel, _.unsnake.pascal)

package caseSensitivity:
  given caseSensitive: CaseSensitivity = _ == _
  given caseInsensitive: CaseSensitivity = _.majuscule == _.majuscule

  given smartCase: CaseSensitivity = (left, right) =>
    left == right || left.isLower && left.majuscule == right
