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
package escapade

import language.experimental.pureFunctions

import scala.util.*

import anticipation.*
import denominative.*
import gossamer.*
import mercator.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

object Teletype:
  given add: NotGiven[Teletype is Textual] => Teletype is Addable:
    type Operand = Teletype
    type Result = Teletype

    inline def add(left: Teletype, right: Teletype): Teletype = left.append(right)

  given concatenable: Teletype is Concatenable:
    type Operand = Teletype
    def concat(left: Teletype, right: Teletype): Teletype = left.append(right)

  given out: Stdio => Out.type is Writable by Teletype = new Writable:
    type Self = Out.type
    type Operand = Teletype

    def write(target: Self, stream: Stream[Teletype]): Unit =
      stream.flow(())(Out.print(next) yet write(target, more))

  given err: Stdio => Err.type is Writable by Teletype = new Writable:
    type Self = Err.type
    type Operand = Teletype

    def write(target: Self, stream: Stream[Teletype]): Unit =
      stream.flow(())(Err.print(next) yet write(target, more))

  given textual: Teletype is Textual:
    type Operand = Char
    type Show[value] = value is Teletypeable

    def classTag: ClassTag[Teletype] = summon[ClassTag[Teletype]]
    def size(text: Teletype): Int = text.plain.length
    def text(teletype: Teletype): Text = teletype.plain
    def length(text: Teletype): Int = text.plain.length
    def apply(text: Text): Teletype = Teletype(text)
    def single(operand: Char): Teletype = Teletype(operand.show)
    def fromChar(char: Char): Char = char

    def map(text: Teletype)(lambda: Char => Char): Teletype =
      val array = text.plain.s.toCharArray.nn

      array.indices.each: index =>
        array(index) = lambda(array(index))

      Teletype(new String(array).tt, text.styles, text.hyperlinks, text.insertions)

    def segment(text: Teletype, interval: Interval): Teletype =
      text.dropChars(interval.start.n0).takeChars(interval.size)

    val empty: Teletype = Teletype.empty

    def concat(left: Teletype, right: Teletype): Teletype = left.append(right)
    def at(text: Teletype, index: Ordinal): Char = text.plain.s.charAt(index.n0)

    def indexOf(text: Teletype, sub: Text, start: Ordinal): Optional[Ordinal] =
      text.plain.s.indexOf(sub.s, start.n0).puncture(-1).let(_.z)

    def show[value: Teletypeable](value: value) = value.teletype
    def builder(size: Optional[Int] = Unset): TeletypeBuilder = TeletypeBuilder(size)

  val empty: Teletype = new Teletype(t"", IArray(0L), Map.empty, TreeMap.empty)

  given joinable: Teletype is Joinable = _.fold(empty)(_ + _)
  given printable: Teletype is Printable = _.render(_)

  given cuttable: Teletype is Cuttable by Text = (text, delimiter, limit) =>
    import java.util.regex.*

    val pattern = Pattern.compile(t"(.*)${Pattern.quote(delimiter.s).nn}(.*)".s).nn

    @tailrec
    def recur(source: Teletype, limit: Int, acc: List[Teletype]): List[Teletype] =
      if limit <= 0 then acc
      else
        val matcher = pattern.matcher(source.plain.s).nn

        if !matcher.matches then source :: acc else
          val output = source.keep(matcher.group(2).nn.length, Rtl)
          recur(source.keep(matcher.group(1).nn.length), limit - 1, output :: acc)

    recur(text, limit, Nil)

  given ordering: Ordering[Teletype] = Ordering.by(_.plain)

  def apply(text: Text): Teletype =
    val styles = IArray.fill(text.length + 1)(0L)
    new Teletype(text, styles, Map.empty, TreeMap.empty)

  def styled[value: Showable](value: value)(transform: Ansi.Transform): Teletype =
    val text: Text = value.show
    val styled: Long = transform(TextStyle()).styleWord
    val styles = IArray.tabulate(text.length + 1) { i => if i < text.length then styled else 0L }
    new Teletype(text, styles, Map.empty, TreeMap.empty)

case class Teletype
  ( plain:      Text,
    styles:     IArray[Long],
    hyperlinks: Map[Int, Text]            = Map.empty,
    insertions: TreeMap[Int, Text]        = TreeMap.empty ):

  def explicit: Text = render(termcapDefinitions.xtermTrueColor).bind: char =>
    if char.toInt == 27 then t"\\e" else char.show

  @targetName("add")
  def append(text: Text): Teletype =
    if text.length == 0 then this else
      val tail = styles(plain.length)
      val newLength = plain.length + text.length + 1
      val arr = new Array[Long](newLength)
      var i = 0
      while i < plain.length do
        arr(i) = styles(i)
        i += 1
      while i < newLength do
        arr(i) = tail
        i += 1
      Teletype(t"$plain$text", IArray.unsafeFromArray(arr), hyperlinks, insertions)

  @targetName("add2")
  def append(that: Teletype): Teletype =
    if that.plain.length == 0 then this else if plain.length == 0 then that else
      val n = plain.length
      val newLength = n + that.plain.length + 1
      val arr = new Array[Long](newLength)
      var i = 0
      while i < n do
        arr(i) = styles(i)
        i += 1
      var j = 0
      while j < that.styles.length do
        arr(i) = that.styles(j)
        i += 1
        j += 1

      val shiftedLinks = if that.hyperlinks.isEmpty then hyperlinks else
        hyperlinks ++ that.hyperlinks.map((k, v) => (k + n) -> v)

      val shiftedInsertions = if that.insertions.isEmpty then insertions else
        insertions ++ that.insertions.map((k, v) => (k + n) -> v)

      Teletype(plain+that.plain, IArray.unsafeFromArray(arr), shiftedLinks, shiftedInsertions)

  def dropChars(n: Int, dir: Bidi = Ltr): Teletype = dir match
    case Rtl => takeChars(plain.length - n)

    case Ltr =>
      val keepLength = plain.length - n
      if keepLength <= 0 then Teletype.empty
      else
        val arr = new Array[Long](keepLength + 1)
        var i = 0
        while i <= keepLength do
          arr(i) = styles(n + i)
          i += 1
        val newHyperlinks = hyperlinks.collect { case (k, v) if k >= n => (k - n) -> v }
        val newInsertions = insertions.collect { case (k, v) if k >= n => (k - n) -> v }.to(TreeMap)
        Teletype(plain.skip(n), IArray.unsafeFromArray(arr), newHyperlinks, newInsertions)

  def takeChars(n: Int, dir: Bidi = Ltr): Teletype = dir match
    case Rtl => dropChars(plain.length - n)

    case Ltr =>
      if n <= 0 then Teletype.empty
      else if n >= plain.length then this
      else
        val arr = new Array[Long](n + 1)
        var i = 0
        while i < n do
          arr(i) = styles(i)
          i += 1
        arr(n) = 0L
        val newHyperlinks = hyperlinks.filter((k, _) => k < n)
        val newInsertions = insertions.rangeUntil(n)
        Teletype(plain.keep(n), IArray.unsafeFromArray(arr), newHyperlinks, newInsertions)

  def render(termcap: Termcap): Text =
    if !termcap.ansi then plain else
      val buffer = StringBuilder()
      val depth = termcap.color
      var prev: Long = 0L
      var i = 0
      val n = plain.length

      while i < n do
        val s = styles(i)
        if s != prev then StyleWord.emitDiff(buffer, prev, s, depth)
        insertions.get(i).foreach { content => buffer.add(content) }
        if (s & StyleWord.HyperlinkChange) != 0 then
          hyperlinks.get(i) match
            case Some(url) => buffer.add(t"\e]8;;$url\e\\")
            case None      => buffer.add(t"\e]8;;\e\\")
        buffer.add(plain.s.charAt(i))
        prev = s
        i += 1

      val tail = styles(n)
      if tail != prev then StyleWord.emitDiff(buffer, prev, tail, depth)
      insertions.rangeFrom(n).values.each { content => buffer.add(content) }

      buffer.text
