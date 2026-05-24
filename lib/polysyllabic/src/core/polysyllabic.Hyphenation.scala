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

    val excDict = exceptions.foldLeft(Dictionary.Empty: Dictionary[IArray[Int]]): (d, raw) =>
      val (word, offsets) = TexPatterns.parseException(raw)
      d.add(word, offsets, 0)

    make(dict, excDict, leftMin, rightMin)

  // Parse a full TeX hyphenation pattern file (the format used by CTAN
  // `hyph-utf8`): finds the `\patterns{…}` and `\hyphenation{…}` blocks plus
  // optional `\lefthyphenmin` / `\righthyphenmin` directives.
  def fromTex(content: Text): Hyphenation =
    val parsed = TexPatterns.parseFile(content)
    apply(parsed.patterns, parsed.exceptions, parsed.leftMin, parsed.rightMin)

  private[polysyllabic] def make
    ( patterns0:   Dictionary[IArray[Byte]],
      exceptions0: Dictionary[IArray[Int]],
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

    breakPoints(word.s, 0, word.s.length, hyphenation, leftMin, rightMin)

  // Slice variant: operate on `source[offset, offset + length)` without
  // allocating a substring. Callers iterating words inside a larger text
  // (e.g. `text.hyphenate`) hit this path one allocation cheaper per word.
  def breakPoints
    ( source:       String,
      offset:       Int,
      length:       Int,
      hyphenation:  Hyphenation,
      leftMin:      Int,
      rightMin:     Int )
  :   IArray[Int] =

    // Single Char buffer: `.` at the boundaries and lowercased word chars in
    // between. Used by both the exception lookup (via `lookupAscii` on slots
    // `[1, length)`) and the trie walk that follows.
    val paddedLength = length + 2
    val padded = new Array[Char](paddedLength)
    padded(0) = '.'
    padded(paddedLength - 1) = '.'
    var i = 0

    while i < length do
      val c = source.charAt(offset + i)
      padded(i + 1) = if c >= 'A' && c <= 'Z' then (c + 32).toChar else c
      i += 1

    val exception = hyphenation.exceptions.lookupAscii(padded, 1, length)

    if exception.absent then trieWalk(padded, paddedLength, length, hyphenation, leftMin, rightMin)
    else
      // Re-apply the leftMin/rightMin envelope to honoured exceptions too — a
      // user-tightened minimum should suppress otherwise-listed breaks.
      val offsets: IArray[Int] = exception.vouch
      val filtered = ArrayBuilder.make[Int]
      var k = 0

      while k < offsets.length do
        val p = offsets(k)
        if p >= leftMin && p <= length - rightMin then filtered += p
        k += 1

      filtered.result().immutable(using Unsafe)

  private inline def mergePattern
    ( scores: Array[Byte], base: Int, pattern: IArray[Byte] )
  :   Unit =

    var k = 0
    val n = pattern.length

    while k < n do
      val v = pattern(k)
      if v > scores(base + k) then scores(base + k) = v
      k += 1

  private def trieWalk
    ( padded:       Array[Char],
      paddedLength: Int,
      length:       Int,
      hyphenation:  Hyphenation,
      leftMin:      Int,
      rightMin:     Int )
  :   IArray[Int] =

    // `scores(g)` is the running maximum for the gap immediately before
    // `padded(g)` (or after the last character when `g == paddedLength`).
    val scores = new Array[Byte](paddedLength + 1)
    var startPos = 0

    while startPos < paddedLength do
      // We always walk from a Branch node (the root is a Branch). When we
      // step into a `Just` we match its entire stored tail in one
      // comparison loop and stop — avoiding the per-character `Just`
      // allocations the previous version paid inside `Dictionary#apply`.
      var node: Dictionary[IArray[Byte]] = hyphenation.patterns
      var j = startPos
      var live = true

      while live && j < paddedLength do
        node match
          case branch: Dictionary.Branch[IArray[Byte]] @unchecked =>
            val char = padded(j)

            branch.map.at(char) match
              case nextBranch: Dictionary.Branch[IArray[Byte]] @unchecked =>
                nextBranch.value.let(mergePattern(scores, startPos, _))
                node = nextBranch
                j += 1

              case nextJust: Dictionary.Just[IArray[Byte]] @unchecked =>
                // The first char of the Just's text matched (it was the map
                // key); the remaining tail is `text[offset, text.length)`.
                val text = nextJust.text.s
                val tailOffset = nextJust.offset
                val tailRemaining = text.length - tailOffset
                j += 1

                if j + tailRemaining > paddedLength then live = false else
                  var k = 0

                  while k < tailRemaining && padded(j + k) == text.charAt(tailOffset + k)
                  do k += 1

                  if k == tailRemaining then mergePattern(scores, startPos, nextJust.value)

                  live = false

              case _ =>
                live = false

          case _ =>
            live = false

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
  def exceptions: Dictionary[IArray[Int]]
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

    val newExceptions = exceptions.foldLeft(this.exceptions): (dict, raw) =>
      val (word, offsets) = TexPatterns.parseException(raw)
      dict.add(word, offsets, 0)

    val effectiveLeft = leftMin.or(this.leftMin)
    val effectiveRight = rightMin.or(this.rightMin)
    Hyphenation.make(newPatterns, newExceptions, effectiveLeft, effectiveRight)

private[polysyllabic] object Unhyphenated extends Hyphenation:
  val patterns: Dictionary[IArray[Byte]] = Dictionary.Empty
  val exceptions: Dictionary[IArray[Int]] = Dictionary.Empty
  val leftMin: Int = Int.MaxValue
  val rightMin: Int = Int.MaxValue
