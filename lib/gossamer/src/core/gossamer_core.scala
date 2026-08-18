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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import proscenium.compat.*

import scala.reflect

import scala.compiletime

import scala.language.experimental.into
import scala.language.experimental.pureFunctions

import java.lang as jl
import java.nio.charset.StandardCharsets
import java.net.{URLEncoder, URLDecoder}
import java.util.regex as jur

import scala.collection.mutable as scm
import scala.reflect.*

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

import Textual.concatenable

export gossamer.internal.opaques.{Ascii, Grapheme}

inline def append[textual: Textual, value](using builder: Builder[textual] aka "builder")
  ( value: value )
:   Unit =

  // The explicit import outranks the deindexing `apply`, which would otherwise shadow the
  // `Tagged` unwrapping.

  inline value match
    case text: Text => builder().append(textual.apply(text))
    case char: Char => builder().append(char)
    case other      => provide[textual.Show[value]](builder().append(textual.show(value)))


inline def appendln[textual: Textual, value](using builder: Builder[textual] aka "builder")
  ( value: value )
:   Unit =

  append[textual, value](value)
  builder().append('\n')


inline def builder[value](using value: value aka "builder"): value =
  value()

extension (module: Array.type)
  def build[element: ClassTag](size: Int)(lambda: scala.Array[element]^ => Unit): Array[element]^{} =
    val array = Array[element](size)
    lambda(array.raw)
    Array.freeze(array)

extension (module: Text.type)
  def build(block: TextBuilder aka "builder" ?=> Unit): Text =
    val builder = TextBuilder()
    block(using builder.aka["builder"])
    builder()

  def ascii(bytes: Data): Text = new String(Array.unsafeJvm(bytes), StandardCharsets.US_ASCII).tt

  def fill(length: Int)(lambda: Int => Char): Text =
    val buffer = Array.scribe[Char](length): scribe =>
      _ => scribe.iterate { index => scribe(index) = lambda((index: Ordinal).n0) }

    String(Array.unsafeJvm(buffer)).tt

extension (inline context: StringContext)
  transparent inline def txt(inline parts: Any*): Text =
    ${gossamer.internal.txt('context, 'parts)}

  transparent inline def t(inline parts: Any*): Text =
    ${gossamer.internal.t('context, 'parts)}

extension (context: StringContext)
  def t = SimpleTExtractor(context.parts.head.tt)

extension (bytes: Data)
  def utf8: Text = String(Array.unsafeJvm(bytes), StandardCharsets.UTF_8).tt
  def utf16: Text = String(Array.unsafeJvm(bytes), StandardCharsets.UTF_16).tt
  def ascii: Text = String(Array.unsafeJvm(bytes), StandardCharsets.US_ASCII).tt

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

extension [textual: Textual { type Result = Char } as instance](words: Iterable[textual])
  def pascal: textual = words.map(_.lower.capitalize).join
  def camel: textual = pascal.uncapitalize
  def snake: textual = words.join(instance.apply("_".tt))
  def kebab: textual = words.join(instance.apply("-".tt))
  def spaced: textual = words.join(instance.apply(" ".tt))

extension [textual: Textual { type Result = Char }](words: List[textual])
  def pascal: textual = words.stdlib.pascal
  def camel: textual = words.stdlib.camel
  def snake: textual = words.stdlib.snake
  def kebab: textual = words.stdlib.kebab
  def spaced: textual = words.stdlib.spaced

// The ordinal-bounded `before`/`upto`/`from`/`after` now live in `rudiments`, alongside the
// other generic positional operations over `Segmentable` and `Countable`.

// A textual value reverses to its own type. Exposed as a factory rather than a blanket given because
// `Reversible`'s companion (in `rudiments`) cannot reference `Textual`, so a generic given would not
// be in implicit scope; each textual type instead publishes `given … is Reversible = reversibleTextual`
// in its own companion (e.g. `Teletype`), which keeps the single `rudiments` `reverse` serving both
// text and collections with no competing extension at the umbrella.
def reversibleTextual[textual](using textual0: textual is Textual)
:   textual is Reversible { type Result = textual } =
  new Reversible:
    type Self = textual
    type Result = textual

    def reverse(text: textual): textual =
      val n = textual0.length(text)
      val builder = textual0.builder(n)
      var index = n - 1

      while index >= 0 do
        builder.append(textual0.single(textual0.access(text, index.z)))
        index -= 1

      builder()

// A textual value traverses its own elements. A factory for the same reason as
// `reversibleTextual` above: each textual type publishes `given … is Traversable by <Result> =
// traversableTextual` in its own companion, which is what makes the generic predicate forms of
// `keep` and `skip` (in `rudiments`) serve it — traversal finds the boundary, and the rebuild
// goes through `segment`, so styled texts stay styled.
def traversableTextual[textual](using textual0: textual is Textual)
:   textual is Traversable { type Operand = textual0.Result } =
  new Traversable:
    type Self = textual
    type Operand = textual0.Result

    def traverse(text: textual): Iterator[Operand] =
      scala.Iterator.range(0, textual0.length(text)).map { index => textual0.access(text, index.z) }

extension [textual: Textual as instance](text: textual)
  inline def length: Int = textual.length(text)
  def plain: Text = textual.text(text)

  // FIXME
  def justify(width: Int): textual =
    val words = text.words.stdlib
    val extra = width - text.length

    def recur(word: Ordinal, spaces: Int, result: textual): textual =
      if word == Prim then result else
        val gap = ((spaces.toDouble/word.n0) + 0.5).toInt
        recur(word - 1, spaces - gap, result+instance.apply(t" "*(gap + 1))+words(words.length - word.n0))

    recur(Prim, extra, words(0))

  def slices(size: Int): List[textual] =
    val length = text.length

    List.tabulate[textual]((length - 1)/size + 1): i =>
      text.segment((i*size).z thru ((i + 1)*size).min(length).u)

  // `keep`, `skip` and `snip` are no longer defined here: the generic positional operations in
  // `rudiments`, over `Segmentable` and `Countable`, serve every textual type through the
  // instances `Textual` extends.
  inline def tail: textual = text.skip(1, Ltr)
  inline def init: textual = text.skip(1, Rtl)

  def chars: Array[Char]^{} = Array.unsafeFrozen(textual.text(text).s.toCharArray.nn)

  def punch(n: Ordinal): (textual, textual) =
    (text.segment(Prim till n), text.segment((n + 1) till text.limit))

  def contains(substring: Text): Boolean = textual.indexOf(text, substring).present

  def search(regex: Regex, overlap: Boolean = false): Chain[textual] =
    regex.search(textual.text(text), overlap = overlap).map(text.segment(_))

  inline def extract[value](inline start: Ordinal = Prim)
    ( inline lambda: Scanner ?=> textual ~> value )
  :   Chain[value] =

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

  def chomp(affix: Text, bidi: Bidi = Ltr): textual = bidi match
    case Ltr => if text.starts(affix) then text.skip(affix.length) else text
    case Rtl => if text.ends(affix) then text.skip(affix.length, Rtl) else text

extension [textual: Textual { type Result = Char } as instance](text: textual)
  inline def lower: textual = textual.map(text)(_.toLower)
  inline def upper: textual = textual.map(text)(_.toUpper)

  def broken(predicate: (Char, Char) => Boolean, break: Char = '\u200b'): textual =
    val breakText = instance.apply(break.toString.tt)
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

  def capitalize: textual = textual.concat(text.keep(1).upper, text.after(Prim))
  def uncapitalize: textual = textual.concat(text.keep(1).lower, text.after(Prim))

  def contains(char: Char): Boolean = textual.indexOf(text, char.show).present

  inline def trim: textual =
    val start = text.pinpoint(!_.isWhitespace).or(text.limit - 1)
    val end = text.pinpoint(!_.isWhitespace, bidi = Rtl).or(Prim)
    text.segment(start thru end)

  // Not `skip(_.isWhitespace, bidi)`: `pinpoint` reaches the boundary by direct access, where
  // the generic `skip` would traverse.
  def trim(bidi: Bidi): textual = bidi match
    case Ltr => text.pinpoint(!_.isWhitespace).lay(textual.empty)(text.from(_))
    case Rtl => text.pinpoint(!_.isWhitespace, bidi = Rtl).lay(textual.empty)(text.upto(_))

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

  // The predicate forms of `before`/`upto`/`snip` now live in `rudiments` with the other
  // generic positional operations, generalized from characters to any traversable element.
  def tr(lambda: Char => Char): textual = textual.map(text)(lambda)

  def erase(chars: Char*): textual =
    val set = chars.toSet

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
      val old = Array[Int](n + 1)
      val dist = Array[Int](n + 1)
      var j = 1

      while j <= n do
        old(j) = old(j - 1) + 1
        j += 1

      var i = 1

      while i <= m do
        dist(0) = old(0) + 1
        j = 1

        while j <= n do
          val c =
            if sensitivity.compare(left.s.charAt(i - 1), right.s.charAt(j - 1)) then 0 else 1

          dist(j) = (old(j - 1) + c).min(old(j) + 1).min(dist(j - 1) + 1)
          j += 1

        old.copyFrom(dist, 0, 0, n + 1)
        i += 1

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
  inline def sysData: Array[Byte]^{} = CharEncoder.system.encode(text)

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

extension (iarray: Array[Char]^{}) def text: Text = String(Array.unsafeJvm(iarray)).tt

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

extension [textual: {Joinable, Textual}](values: List[textual])
  def join: textual = values.stdlib.join
  def join(separator: textual): textual = values.stdlib.join(separator)

  def join(left: textual, separator: textual, right: textual): textual =
    values.stdlib.join(left, separator, right)

  def join(separator: textual, penultimate: textual): textual =
    values.stdlib.join(separator, penultimate)

  def join(left: textual, separator: textual, penultimate: textual, right: textual): textual =
    values.stdlib.join(left, separator, penultimate, right)

// Opaque `Chain` is not an `Iterable`, so it needs its own `join` block bridging via
// `stdlib` (joining forces the whole stream, which is the caller's intent here).
extension [textual: {Joinable, Textual}](values: Chain[textual])
  def join: textual = values.stdlib.join
  def join(separator: textual): textual = values.stdlib.join(separator)

  def join(left: textual, separator: textual, right: textual): textual =
    values.stdlib.join(left, separator, right)

  def join(separator: textual, penultimate: textual): textual =
    values.stdlib.join(separator, penultimate)

  def join(left: textual, separator: textual, penultimate: textual, right: textual): textual =
    values.stdlib.join(left, separator, penultimate, right)

extension (builder: StringBuilder)
  def add(text: Text): Unit = builder.append(text.s)
  def add(char: Char): Unit = builder.append(char)
  def text: Text = builder.toString.tt

package decimalConverters:
  given javaDecimalConverter: DecimalConverter:
    def decimalize(double: Double): Text = double.toString.tt

package caseSensitivity:
  given caseSensitive: CaseSensitivity = _ == _
  given caseInsensitive: CaseSensitivity = _.majuscule == _.majuscule

  given smartCase: CaseSensitivity = (left, right) =>
    left == right || left.isLower && left.majuscule == right
