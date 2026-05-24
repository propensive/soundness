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
package polysyllabic

import scala.collection.mutable.ArrayBuilder

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

object Hyphenation:
  given fallback: Hyphenation = Unhyphenated

  // Build a `Hyphenation` from raw TeX-format pattern and exception strings.
  // Patterns look like `t"hy3ph"`; exceptions look like `t"as-so-ciate"`.
  def apply
    ( patterns:   Iterable[Text],
      exceptions: Iterable[Text] = Nil,
      leftMin:    Int            = 2,
      rightMin:   Int            = 3 )
  :   Hyphenation =

    val dict = patterns.foldLeft(Dictionary.Empty: Dictionary[IArray[Byte]]): (d, raw) =>
      val (key, scores) = TexPatterns.parsePattern(raw)
      d.add(key, scores, 0)

    val excMap = exceptions.foldLeft(Map.empty[Text, IArray[Int]]): (m, raw) =>
      val (word, offsets) = TexPatterns.parseException(raw)
      m.updated(word, offsets)

    make(dict, excMap, leftMin, rightMin)

  // Parse a full TeX hyphenation pattern file (the format used by CTAN
  // `hyph-utf8`): finds the `\patterns{…}` and `\hyphenation{…}` blocks plus
  // optional `\lefthyphenmin` / `\righthyphenmin` directives.
  def fromTex(content: Text): Hyphenation =
    val parsed = TexPatterns.parseFile(content)
    apply(parsed.patterns, parsed.exceptions, parsed.leftMin, parsed.rightMin)

  private[polysyllabic] def make
    ( patterns0:   Dictionary[IArray[Byte]],
      exceptions0: Map[Text, IArray[Int]],
      leftMin0:    Int,
      rightMin0:   Int )
  :   Hyphenation =

    new Hyphenation:
      val patterns = patterns0
      val exceptions = exceptions0
      val leftMin = leftMin0
      val rightMin = rightMin0

  // Liang's hyphenation algorithm. Pads the lowercased word with `.` sentinels,
  // walks the pattern dictionary from every starting position, merges each
  // matched pattern's per-gap scores into a buffer via `max`, then reports the
  // positions whose final score is odd — subject to the `leftMin`/`rightMin`
  // envelope. Exact exception-list hits short-circuit the trie walk.
  //
  // Returns positions in the original (un-padded) word at which a hyphen may
  // be inserted: position `p` means "insert before letter `p`".
  //
  // TODO: swap for an Aho–Corasick automaton once profiling shows wrapping
  // dominated by hyphenation. Same public interface, much smaller constant.
  def breakPoints(word: Text, hyphenation: Hyphenation, leftMin: Int, rightMin: Int)
  :   IArray[Int] =

    val source = word.s
    val length = source.length
    val lowered = new Array[Char](length)
    var i = 0

    while i < length do
      val c = source.charAt(i)
      lowered(i) = if c >= 'A' && c <= 'Z' then (c + 32).toChar else c
      i += 1

    val loweredText = new String(lowered).nn.tt

    hyphenation.exceptions.get(loweredText) match
      case Some(offsets) =>
        // Re-apply the leftMin/rightMin envelope to honoured exceptions too —
        // a user-tightened minimum should suppress otherwise-listed breaks.
        val filtered = ArrayBuilder.make[Int]
        var k = 0

        while k < offsets.length do
          val p = offsets(k)
          if p >= leftMin && p <= length - rightMin then filtered += p
          k += 1

        filtered.result().immutable(using Unsafe)

      case None =>
        // Pad the word with `.` boundary sentinels matching the convention
        // used inside the TeX patterns themselves.
        val paddedLength = length + 2
        val padded = new Array[Char](paddedLength)
        padded(0) = '.'
        i = 0

        while i < length do
          padded(i + 1) = lowered(i)
          i += 1

        padded(paddedLength - 1) = '.'

        // `scores(g)` is the running maximum for the gap immediately before
        // `padded(g)` (or after the last character when `g == paddedLength`).
        val scores = new Array[Byte](paddedLength + 1)
        var startPos = 0

        while startPos < paddedLength do
          var node: Dictionary[IArray[Byte]] = hyphenation.patterns
          var j = startPos
          var live = true

          while live && j < paddedLength do
            val next = node(padded(j))

            if next eq Dictionary.Empty then live = false else
              node = next

              next.element.let: patternScores =>
                var k = 0
                val max = patternScores.length

                while k < max do
                  val value = patternScores(k)
                  if value > scores(startPos + k) then scores(startPos + k) = value
                  k += 1

              j += 1

          startPos += 1

        // Break position `p` in the original word corresponds to padded gap
        // `p + 1` (the leading `.` shifts the indices by one).
        val breaks = ArrayBuilder.make[Int]
        var p = if leftMin > 1 then leftMin else 1
        val lastBreak = length - (if rightMin > 1 then rightMin else 1)

        while p <= lastBreak do
          if (scores(p + 1) & 1) == 1 then breaks += p
          p += 1

        breaks.result().immutable(using Unsafe)

trait Hyphenation:
  def patterns: Dictionary[IArray[Byte]]
  def exceptions: Map[Text, IArray[Int]]
  def leftMin: Int
  def rightMin: Int

  def extending
    ( patterns:   Iterable[Text] = Nil,
      exceptions: Iterable[Text] = Nil,
      leftMin:    Optional[Int]  = Unset,
      rightMin:   Optional[Int]  = Unset )
  :   Hyphenation =

    val newPatterns = patterns.foldLeft(this.patterns): (dict, raw) =>
      val (key, scores) = TexPatterns.parsePattern(raw)
      dict.add(key, scores, 0)

    val newExceptions = exceptions.foldLeft(this.exceptions): (map, raw) =>
      val (word, offsets) = TexPatterns.parseException(raw)
      map.updated(word, offsets)

    val effectiveLeft = leftMin.or(this.leftMin)
    val effectiveRight = rightMin.or(this.rightMin)
    Hyphenation.make(newPatterns, newExceptions, effectiveLeft, effectiveRight)

private[polysyllabic] object Unhyphenated extends Hyphenation:
  val patterns: Dictionary[IArray[Byte]] = Dictionary.Empty
  val exceptions: Map[Text, IArray[Int]] = Map.empty
  val leftMin: Int = Int.MaxValue
  val rightMin: Int = Int.MaxValue
