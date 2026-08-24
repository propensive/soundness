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

import scala.collection.immutable.IndexedSeq

import java.lang as jl

import scala.reflect.*

import anticipation.*

import denominative.*
import rudiments.*
import hieroglyph.*
import spectacular.*
import symbolism.*
import vacuous.*

object Writing:
  def apply(text: Text): Writing = new Writing(text, GraphemeBreak.boundaries(text))

  val empty: Writing = new Writing(Text(""), Array.of(0))

  given showable: Writing is Showable = _.text

  given concatenable: Writing is Concatenable:
    type Result = Writing
    type Operand = Writing
    def concat(left: Writing, right: Writing): Writing = textual.concat(left, right)

  // The display width of a Writing is the sum of its grapheme widths. This is more accurate
  // than `Text is Measurable` (which sums per-codepoint widths), since complex graphemes
  // — RI flag pairs, ZWJ-joined emoji, Hangul jamo syllables — occupy a single "wide" cell
  // rather than the over-counted sum of their constituent codepoints.
  given measurable: (graphemeM: Grapheme is Measurable) => Writing is Measurable = writing =>
    val s = writing.text.s
    val boundaries = writing.boundaries
    var total = 0

    boundaries.adjacent: (start, limit) =>
      total += graphemeM.width(Grapheme(s.substring(start, limit).nn))

    total

  given textual: Writing is Textual:
    type Result = Grapheme
    type Show[value] = value is Showable

    val classTag: ClassTag[Writing] = summon[ClassTag[Writing]]

    def show[value](value: value)(using show: Show[value]): Writing = Writing(show.text(value))
    def apply(text: Text): Writing = Writing(text)
    def single(operand: Grapheme): Writing = Writing(operand.text)
    def fromChar(char: Char): Grapheme = Grapheme(char.toString)
    def length(writing: Writing): Int = writing.boundaries.readable.length - 1
    def size(writing: Writing): Int = writing.boundaries.readable.length - 1
    def text(writing: Writing): Text = writing.text

    def map(writing: Writing)(lambda: Grapheme => Grapheme): Writing =
      val builder = jl.StringBuilder()

      writing.boundaries.adjacent: (start, limit) =>
        val piece = writing.text.s.substring(start, limit).nn
        builder.append(lambda(Grapheme(piece)).text.s)

      Writing(builder.toString.nn.tt)

    def empty: Writing = Writing.empty
    def concat(left: Writing, right: Writing): Writing = Writing(Text(left.text.s+right.text.s))

    def access(writing: Writing, index: Ordinal): Grapheme =
      Grapheme:
        writing.text.s.substring(writing.boundaries.readUnchecked(index.n0), writing.boundaries.readUnchecked(index.n0 + 1)).nn

    def indexOf(writing: Writing, sub: Text, start: Ordinal): Optional[Ordinal] =
      val charStart = writing.boundaries.at(Ordinal.zerary(start.n0.min(writing.boundaries.readable.length - 1))).or(0)
      val foundChar = writing.text.s.indexOf(sub.s, charStart)

      if foundChar < 0 then Unset else
        writing.boundaries.where(_ == foundChar)

    def builder(size: Optional[Int]): Builder[Writing] = WritingBuilder(size)

    def segment(writing: Writing, interval: Interval): Writing =
      val limit = length(writing)
      val s = interval.start.n0.max(0).min(limit)
      val e = interval.limit.n0.max(s).min(limit)
      val slice = writing.text.s.substring(writing.boundaries.readable(s), writing.boundaries.readable(e)).nn
      Writing(slice.tt)

  extension (writing: Writing)
    def graphemes: IndexedSeq[Grapheme] =
      val n = writing.boundaries.readable.length - 1

      IndexedSeq.tabulate(n): i =>
        Grapheme(writing.text.s.substring(writing.boundaries.readable(i), writing.boundaries.readable(i + 1)).nn)

    def graphemeCount: Int = writing.boundaries.readable.length - 1

case class Writing(text: Text, boundaries: Array[Int]^{})

class WritingBuilder(size: Optional[Int] = Unset) extends Builder[Writing](size):
  private val builder: jl.StringBuilder = size.lay(jl.StringBuilder())(jl.StringBuilder(_))

  protected def put(writing: Writing): Unit = builder.append(writing.text.s)
  protected def putChar(char: Char): Unit = builder.append(char)
  protected def wipe(): Unit = builder.setLength(0)
  protected def result(): Writing = Writing(builder.toString.nn.tt)
  def length: Int = builder.length
