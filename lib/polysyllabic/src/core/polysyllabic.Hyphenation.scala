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
package polysyllabic

import proscenium.compat.*

import scala.collection.mutable.ArrayBuilder

import anticipation.*
import denominative.*
import gossamer.*
import rudiments.*
import vacuous.*
import fulminate.*

object Hyphenation:
  given fallback: Hyphenation = Unhyphenated

  // Alphabet used by the pattern trie: lowercase ASCII letters in their
  // natural order, followed by the `.` word-boundary sentinel that TeX
  // hyphenation patterns and the Liang algorithm both rely on.
  val alphabet: Dictionary.Alphabet =
    Dictionary.Alphabet.of("abcdefghijklmnopqrstuvwxyz.")

  // Build a `Hyphenation` from raw TeX-format pattern and exception strings.
  // Patterns look like `t"hy3ph"`; exceptions look like `t"as-so-ciate"`.
  def apply
    ( patterns:   Iterable[Text],
      exceptions: Iterable[Text] = Iterable(),
      leftMin:    Int            = 2,
      rightMin:   Int            = 3 )
  :   Hyphenation =

    val patternPairs    = patterns.map(TexPatterns.parsePattern).toSeq
    val exceptionPairs  = exceptions.map(TexPatterns.parseException).toSeq
    val patternDict     = Dictionary.aho[Array[Byte]^{}](alphabet, patternPairs*)
    val exceptionDict   = Dictionary[Array[Int]^{}](exceptionPairs*)

    make(patternDict, exceptionDict, leftMin, rightMin)

  // Parse a full TeX hyphenation pattern file (the format used by CTAN
  // `hyph-utf8`): finds the `\patterns{…}` and `\hyphenation{…}` blocks plus
  // optional `\lefthyphenmin` / `\righthyphenmin` directives.
  def fromTex(content: Text): Hyphenation =
    val parsed = TexPatterns.parseFile(content)
    apply(parsed.patterns.readable.toSeq, parsed.exceptions.readable.toSeq, parsed.leftMin, parsed.rightMin)

  private[polysyllabic] def make
    ( patterns0:   Dictionary[Array[Byte]^{}],
      exceptions0: Dictionary[Array[Int]^{}],
      leftMin0:    Int,
      rightMin0:   Int )
  :   Hyphenation =

    new Hyphenation:
      val patterns = patterns0
      val exceptions = exceptions0
      val leftMin = leftMin0
      val rightMin = rightMin0

  // Liang's hyphenation algorithm. Pads the lowercased word with `.` sentinels,
  // walks the `CompactTrie` form of the pattern dictionary via Aho-Corasick in
  // a single forward pass, merging each matched pattern's per-gap scores into
  // a buffer via `max`, then reports the positions whose final score is odd —
  // subject to the `leftMin`/`rightMin` envelope. Exact exception-list hits
  // short-circuit the trie walk.
  //
  // Returns positions in the original (un-padded) word at which a hyphen may
  // be inserted: position `p` means "insert before letter `p`".
  def breakPoints(word: Text, hyphenation: Hyphenation, leftMin: Int, rightMin: Int)
  :   Array[Int]^{} =

    breakPoints(word.s, 0, word.s.length, hyphenation, leftMin, rightMin)

  // Slice variant: operate on `source[offset, offset + length)` without
  // allocating a substring. Allocates fresh scratch buffers internally; hot
  // callers iterating many words should prefer `breakPointsInto`, which
  // takes pre-allocated buffers and reuses them across calls.
  def breakPoints
    ( source:       String,
      offset:       Int,
      length:       Int,
      hyphenation:  Hyphenation,
      leftMin:      Int,
      rightMin:     Int )
  :   Array[Int]^{} =

    val paddedLength = length + 2
    val padded = Array[Char](paddedLength)
    padded(0) = '.'
    padded(paddedLength - 1) = '.'
    var i = 0

    while i < length do
      val c = source.charAt(offset + i)
      padded(i + 1) = if c >= 'A' && c <= 'Z' then (c + 32).toChar else c
      i += 1

    hyphenation.exceptions(padded.raw, 1, length).let: offsets =>
      val filtered = Array[Int](offsets.length)
      var count = 0

      offsets.iterate: index =>
        val p = offsets.at(index)

        if p >= leftMin && p <= length - rightMin then
          filtered(count) = p
          count += 1

      exactCopy(filtered, count)

    . or:
        val scores = Array[Byte](paddedLength + 1)
        walkCompact(padded.raw, paddedLength, hyphenation.patterns, scores.raw)
        val breaks = Array[Int](length)
        var count = 0
        var p = if leftMin > 1 then leftMin else 1
        val lastBreak = length - (if rightMin > 1 then rightMin else 1)

        while p <= lastBreak do
          if (scores(p + 1) & 1) == 1 then
            breaks(count) = p
            count += 1

          p += 1

        exactCopy(breaks, count)

  // Walk the compact pattern trie from every starting position in `padded`,
  // merging each matched pattern's score array into `scores` via `max`. Uses
  // Aho-Corasick: a single forward pass through `padded` with failure-link
  // and dictionary-suffix-link traversal at each step. The outer loop visits
  // each padded character exactly once instead of `paddedLength` times.
  // The first `count` elements of `source`, as an immutable array.
  private def exactCopy(source: Array[Int]^, count: Int): Array[Int]^{} =
    val result = Array[Int](count)
    result.copyFrom(source, 0, 0, count)
    Array.freeze(result)

  private def walkCompact
    ( padded:       scala.Array[Char],
      paddedLength: Int,
      trie:         Dictionary[Array[Byte]^{}],
      scores:       scala.Array[Byte]^ )
  :   Unit =

    val children = trie.children
    val values = trie.values
    val fail = trie.fail
    val dictLink = trie.dictLink
    val depth = trie.depth
    val alpha = trie.alphabet.size
    var node = 0
    var j = 0

    // Slot computation is hardcoded to the hyphenation alphabet
    // ('a'..'z' → 0..25, '.' → 26) rather than going through the trie's
    // generic `Alphabet#slot` virtual method. Saves ~15-25% on the
    // hot-path benchmark vs the virtual-dispatch version, at the cost of
    // coupling this walk to the specific alphabet `Hyphenation.alphabet`
    // is built with — kept in sync with that constant.
    while j < paddedLength do
      val char = padded(j)

      val sl =
        if char >= 'a' && char <= 'z' then char - 'a' else if char == '.' then 26 else -1

      if sl < 0 then node = 0 else
        // Follow failure links until we find a node whose child slot is set.
        while node != 0 && children(node*alpha + sl) < 0 do node = fail(node)
        val next = children(node*alpha + sl)
        if next >= 0 then node = next

        // Emit every value reachable via the dictionary suffix chain — these
        // are all patterns that terminate at position `j`. The pattern of
        // depth `d` starts at gap `j - d + 1`. Values in `Dictionary` are
        // stored as `Array[AnyRef | Null]` and cast at access; the JIT
        // typically folds the checkcast away.
        var emit = node

        while emit > 0 do
          val v = values(emit)

          if v != null then
            mergePattern(scores, j - depth(emit) + 1, v.asInstanceOf[Array[Byte]^{}])

          emit = dictLink(emit)

      j += 1

  // Array-reusing variant. Writes break offsets into `breaks` and returns
  // the count of breaks written. `padded`/`scores` are scratch space that
  // can grow over the lifetime of a `text.hyphenate` call so per-word
  // allocations collapse to zero.
  //
  // Required sizes:
  //   padded.length >= length + 2
  //   scores.length >= length + 3
  //   breaks.length >= length
  def breakPointsInto
    ( source:       String,
      offset:       Int,
      length:       Int,
      hyphenation:  Hyphenation,
      leftMin:      Int,
      rightMin:     Int,
      padded:       scala.Array[Char]^,
      scores:       scala.Array[Byte]^,
      breaks:       scala.Array[Int]^ )
  :   Int =

    // Fill `padded` with `.` sentinels + lowercased word chars.
    val paddedLength = length + 2
    padded(0) = '.'
    padded(paddedLength - 1) = '.'
    var i = 0

    while i < length do
      val c = source.charAt(offset + i)
      padded(i + 1) = if c >= 'A' && c <= 'Z' then (c + 32).toChar else c
      i += 1

    hyphenation.exceptions(padded, 1, length).let: offsets =>
      var count = 0

      offsets.iterate: index =>
        val p = offsets.at(index)

        if p >= leftMin && p <= length - rightMin then
          breaks(count) = p
          count += 1

      count

    . or:
        // Zero the active prefix of the scores buffer so the running maxima
        // start from 0 each call. `Arrays.fill` is a JIT intrinsic.
        java.util.Arrays.fill(scores, 0, paddedLength + 1, 0.toByte)
        trieWalkInto(padded, paddedLength, length, hyphenation, leftMin, rightMin, scores, breaks)

  private inline def mergePattern
    ( scores: scala.Array[Byte]^, base: Int, pattern: Array[Byte]^{} )
  :   Unit =

    var k = 0
    val n = pattern.length

    while k < n do
      val v = pattern.readUnchecked(k)
      if v > scores(base + k) then scores(base + k) = v
      k += 1

  private def trieWalkInto
    ( padded:       scala.Array[Char],
      paddedLength: Int,
      length:       Int,
      hyphenation:  Hyphenation,
      leftMin:      Int,
      rightMin:     Int,
      scores:       scala.Array[Byte]^,
      breaks:       scala.Array[Int]^ )
  :   Int =

    // `scores(g)` is the running maximum for the gap immediately before
    // `padded(g)` (or after the last character when `g == paddedLength`).
    // The caller has zeroed `scores[0, paddedLength + 1)`.
    walkCompact(padded, paddedLength, hyphenation.patterns, scores)

    // Break position `p` in the original word corresponds to padded gap
    // `p + 1` (the leading `.` shifts the indices by one).
    var count = 0
    var p = if leftMin > 1 then leftMin else 1
    val lastBreak = length - (if rightMin > 1 then rightMin else 1)

    while p <= lastBreak do
      if (scores(p + 1) & 1) == 1 then
        breaks(count) = p
        count += 1

      p += 1

    count

  // HyphenationError → Hyphenation.Error
  case class Error(resource: Text)(using Diagnostics)
  extends fulminate.Error(m"the hyphenation resource $resource could not be loaded from the classpath")

trait Hyphenation:
  // `patterns` is built via `Dictionary.aho(alphabet, …)` so the algorithm's
  // single-pass walk has the failure and dictionary-suffix links it needs.
  def patterns: Dictionary[Array[Byte]^{}]
  def exceptions: Dictionary[Array[Int]^{}]
  def leftMin: Int
  def rightMin: Int

  def extending
    ( patterns:   Iterable[Text] = Iterable(),
      exceptions: Iterable[Text] = Iterable(),
      leftMin:    Optional[Int]  = Unset,
      rightMin:   Optional[Int]  = Unset )
  :   Hyphenation =

    val newPatternPairs = patterns.map(TexPatterns.parsePattern).toSeq
    val newExceptionPairs = exceptions.map(TexPatterns.parseException).toSeq

    val newPatterns =
      Dictionary.aho[Array[Byte]^{}](Hyphenation.alphabet, (this.patterns.entries.toSeq ++ newPatternPairs)*)

    val newExceptions = this.exceptions ++ newExceptionPairs
    val effectiveLeft = leftMin.or(this.leftMin)
    val effectiveRight = rightMin.or(this.rightMin)
    Hyphenation.make(newPatterns, newExceptions, effectiveLeft, effectiveRight)

private[polysyllabic] object Unhyphenated extends Hyphenation:
  // Empty dictionaries, but built with the hyphenation alphabet so the
  // algorithm's hardcoded slot indexing finds a 27-wide children array
  // (all `-1`) instead of an empty one.
  val patterns: Dictionary[Array[Byte]^{}] = Dictionary.aho[Array[Byte]^{}](Hyphenation.alphabet)

  val exceptions: Dictionary[Array[Int]^{}] =
    Dictionary.withAlphabet[Array[Int]^{}](Hyphenation.alphabet)

  val leftMin: Int = Int.MaxValue
  val rightMin: Int = Int.MaxValue
