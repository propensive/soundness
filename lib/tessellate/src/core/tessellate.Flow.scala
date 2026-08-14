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
package tessellate

import scala.collection.immutable.Vector

import anticipation.*
import denominative.*
import gossamer.*
import gossamer.Textual.concatenable
import hieroglyph.*
import polysyllabic.*
import rudiments.*
import symbolism.*

// Arranges a line of textual content within a fixed width: `wrap` flows a paragraph into
// width-bounded lines (breaking preferentially at hyphenation points, then at spaces, and
// honoring embedded hard line breaks), while `fit` confines a single line (truncating with an
// ellipsis or padding by alignment). All measurement is by grapheme cluster, so joiner-glued
// content (ZWJ emoji, flags, combining marks) and East Asian wide characters occupy their true
// display width under the contextual metric.
//
// Positions are char offsets into `content.plain`, sliced back out with `Textual.segment`, so
// the textual's element indexing must coincide with `plain`'s chars (true of `Text`, `Ascii`
// and `Teletype`; not of `Writing`, which indexes by grapheme).
object Flow:
  // Cumulative display width by grapheme cluster: entry `k` is the width of the first `k`
  // clusters. `boundaries` is `GraphemeBreak.boundaries(plain)`: each cluster's starting char
  // offset, with a final entry of `plain.length`.
  private def prefixWidths(plain: String, boundaries: Array[Int]^{})
    ( using measurable: Char is Measurable )
  :   Array[Int]^{} =

    val clusters = boundaries.readable.length - 1
    val buffer = Array[Int](clusters + 1)
    var total = 0
    var i = 0
    buffer(0) = 0

    while i < clusters do
      total += clusterWidth(plain, boundaries.readable(i), boundaries.readable(i + 1))
      buffer(i + 1) = total
      i += 1

    Array.freeze(buffer)

  // Width of one grapheme cluster: the maximum width of its constituent codepoints, as in
  // `Grapheme.measurable` — combining marks ride on their base character, and joiner-glued
  // clusters (RI flag pairs, ZWJ emoji families) count once rather than per codepoint.
  private def clusterWidth(plain: String, start: Int, limit: Int)
    ( using measurable: Char is Measurable )
  :   Int =

    var max = 0
    var i = start

    while i < limit do
      val codepoint = plain.codePointAt(i)

      val width =
        if Character.charCount(codepoint) == 2
        then measurable.width(plain.charAt(i)) + measurable.width(plain.charAt(i + 1))
        else measurable.width(plain.charAt(i))

      if width > max then max = width
      i += Character.charCount(codepoint)

    max

  // The index of the cluster starting at char offset `position`, or `-1` if `position` is not
  // a cluster boundary.
  private def clusterAt(boundaries: Array[Int]^{}, position: Int): Int =
    var low = 0
    var high = boundaries.readable.length - 1

    while low <= high do
      val mid = (low + high)/2
      val value = boundaries.readable(mid)

      if value == position then return mid
      else if value < position then low = mid + 1
      else high = mid - 1

    -1

  private def charMetric(using metric: Text is Measurable): Char is Measurable =
    char => metric.width(char.toString.tt)

  // Flow `content` into lines no wider than `width`. Hard breaks (`\n`, `\r\n`) always force a
  // new line. When a soft break is needed, the latest hyphenation point of the overflowing
  // word that still fits (with `hyphen` appended) is preferred; failing that, the line wraps
  // at the last space; failing that, the over-long word runs on beyond `width`.
  def wrap[textual: Textual { type Result = Char }]
    ( content: textual, width: Int, hyphen: Text = t"-" )
    ( using metric: Text is Measurable, hyphenation: Hyphenation )
  :   Sequence[textual] =

    given Char is Measurable = charMetric
    val plain = content.plain.s
    val boundaries = GraphemeBreak.boundaries(content.plain)
    val widths = prefixWidths(plain, boundaries)
    val clusters = boundaries.readable.length - 1
    val hyphenText = textual(hyphen)
    val hyphenWidth = hyphen.metrics
    val leftMin = hyphenation.leftMin
    val rightMin = hyphenation.rightMin

    def charStart(cluster: Int): Int = boundaries.readable(cluster)

    def segment(fromCluster: Int, toCluster: Int): textual =
      if fromCluster == toCluster then textual(t"")
      else content.segment(charStart(fromCluster).z thru charStart(toCluster).u)

    def hardBreak(cluster: Int): Boolean =
      val char = plain.charAt(charStart(cluster))
      char == '\n' || char == '\r'

    // The cluster after the end of the word containing `cluster` (the next space or hard
    // break, or the end of the content).
    def wordEnd(cluster: Int): Int =
      var end = cluster
      while end < clusters && plain.charAt(charStart(end)) != ' ' && !hardBreak(end) do end += 1
      end

    // The latest hyphenation point of the word spanning clusters [wordStart, wordEnd0) which,
    // measured from `lineStart` and with the hyphen appended, still fits in `width`; `-1` if
    // none does. Break offsets are char positions, admitted only on cluster boundaries.
    def hyphenationBreak(lineStart: Int, wordStart: Int, wordEnd0: Int): Int =
      val start = charStart(wordStart)

      val breaks =
        Hyphenation.breakPoints
          ( plain, start, charStart(wordEnd0) - start, hyphenation, leftMin, rightMin )

      var best = -1
      var index = 0

      while index < breaks.readable.length do
        val candidate = clusterAt(boundaries, start + breaks.readable(index))

        if candidate > 0 then
          val breakWidth = widths.readable(candidate) - widths.readable(lineStart) + hyphenWidth
          if breakWidth <= width && candidate > best then best = candidate

        index += 1

      best

    // Walk the clusters, accumulating display width since `lineStart`; `lastSpace` is the most
    // recent space cluster on the current line. Lines accumulate in reverse in `acc`.
    def recur(cluster: Int, lineStart: Int, lastSpace: Int, acc: List[textual]): List[textual] =
      if cluster >= clusters then
        if lineStart == cluster then acc else segment(lineStart, cluster) :: acc
      else if hardBreak(cluster) then
        recur(cluster + 1, cluster + 1, cluster + 1, segment(lineStart, cluster) :: acc)
      else if plain.charAt(charStart(cluster)) == ' ' then
        recur(cluster + 1, lineStart, cluster, acc)
      else
        val widthSoFar = widths.readable(cluster + 1) - widths.readable(lineStart)

        if widthSoFar > width then
          val wordStart = if lastSpace > lineStart then lastSpace + 1 else lineStart
          val wordEnd0 = wordEnd(cluster)
          val breakAt = hyphenationBreak(lineStart, wordStart, wordEnd0)

          if breakAt > lineStart then
            recur(breakAt, breakAt, breakAt, (segment(lineStart, breakAt) + hyphenText) :: acc)
          else if lastSpace > lineStart then
            recur(lastSpace + 1, lastSpace + 1, lastSpace + 1, segment(lineStart, lastSpace) :: acc)
          else
            recur(cluster + 1, lineStart, lastSpace, acc)
        else
          recur(cluster + 1, lineStart, lastSpace, acc)

    if width < 1 then Sequence.of(Vector(content))
    else Sequence.of(recur(0, 0, 0, Nil).stdlib.reverse.toVector)

  // Confine a single line of content to exactly `width` cells: content wider than `width` is
  // truncated (by cluster, so a wide character never straddles the cut) and suffixed with
  // `ellipsis`; anything narrower is padded per `alignment`.
  def fit[textual: Textual { type Result = Char }]
    ( content:   textual,
      width:     Int,
      alignment: Alignment = Alignment.Left,
      ellipsis:  Text = t"…",
      last:      Boolean = true )
    ( using metric: Text is Measurable )
  :   textual =

    if content.plain.metrics <= width then alignment.pad(content, width, last) else
      given Char is Measurable = charMetric
      val plain = content.plain.s
      val boundaries = GraphemeBreak.boundaries(content.plain)
      val widths = prefixWidths(plain, boundaries)
      val room = (width - ellipsis.metrics).max(0)
      var keep = 0

      while keep < boundaries.readable.length - 1 && widths.readable(keep + 1) <= room do keep += 1

      val kept =
        if keep == 0 then textual(t"") else content.segment(0.z thru boundaries.readable(keep).u)

      alignment.pad(kept + textual(ellipsis), width, last)

  // The narrowest width into which `content` can wrap without overflow: the display width of
  // its widest space-delimited word (hard breaks also delimit).
  def minContent[textual: Textual { type Result = Char }](content: textual)
    ( using Text is Measurable )
  :   Int =

    given Char is Measurable = charMetric
    val plain = content.plain.s
    val boundaries = GraphemeBreak.boundaries(content.plain)
    val widths = prefixWidths(plain, boundaries)
    val clusters = boundaries.readable.length - 1
    var max = 0
    var wordStart = 0
    var i = 0

    while i < clusters do
      val char = plain.charAt(boundaries.readable(i))

      if char == ' ' || char == '\n' || char == '\r' then
        val wordWidth = widths.readable(i) - widths.readable(wordStart)
        if wordWidth > max then max = wordWidth
        wordStart = i + 1

      i += 1

    val tailWidth = widths.readable(clusters) - widths.readable(wordStart)
    if tailWidth > max then max = tailWidth
    max

  // The width `content` occupies unwrapped: the display width of its widest hard line.
  def natural[textual: Textual { type Result = Char }](content: textual)
    ( using Text is Measurable )
  :   Int =

    given Char is Measurable = charMetric
    val plain = content.plain.s
    val boundaries = GraphemeBreak.boundaries(content.plain)
    val widths = prefixWidths(plain, boundaries)
    val clusters = boundaries.readable.length - 1
    var max = 0
    var lineStart = 0
    var i = 0

    while i < clusters do
      val char = plain.charAt(boundaries.readable(i))

      if char == '\n' || char == '\r' then
        val lineWidth = widths.readable(i) - widths.readable(lineStart)
        if lineWidth > max then max = lineWidth
        lineStart = i + 1

      i += 1

    val tailWidth = widths.readable(clusters) - widths.readable(lineStart)
    if tailWidth > max then max = tailWidth
    max

  // Both intrinsic widths in one pass over the content.
  def metrics[textual: Textual { type Result = Char }](content: textual)
    ( using Text is Measurable )
  :   Metrics =

    Metrics(minContent(content), natural(content))
