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

import scala.collection.mutable.ArrayBuffer

import anticipation.*
import fulminate.*
import vacuous.*

extension (text: Text)
  // Hyphen-insertion offsets for a single word: position `p` means a hyphen
  // may be inserted between letters `p - 1` and `p`. Uses the ambient
  // `Hyphenation`'s default `leftMin` / `rightMin`.
  def breakPoints(using hyphenation: Hyphenation): Array[Int]^{} =
    Hyphenation.breakPoints(text, hyphenation, hyphenation.leftMin, hyphenation.rightMin)

  // Insert `hyphen` (default: U+00AD SOFT HYPHEN) at every admissible break
  // in every letter-run of `text`. Non-letter characters pass through
  // unchanged, so `t"hello, world!".hyphenate(hyphen='-')` does not break at
  // the space or punctuation.
  def hyphenate
    ( hyphen:   Char          = '­',
      leftMin:  Optional[Int] = Unset,
      rightMin: Optional[Int] = Unset )
    ( using hyphenation: Hyphenation )
  :   Text =

    val source = text.s
    val length = source.length
    val effectiveLeft = leftMin.or(hyphenation.leftMin)
    val effectiveRight = rightMin.or(hyphenation.rightMin)
    val out = new java.lang.StringBuilder(length + (length >> 3))

    // Reusable scratch buffers, grown lazily by doubling. Many words share
    // the same allocation across the entire `text.hyphenate` call. A `var`
    // cannot hold an exclusive buffer, so growth recurses instead: each
    // `walk` frame scans until a word outgrows the scratch, then consumes
    // the buffers into `Array.grow` and continues from the same position —
    // recursion depth is the number of growths, not the number of words.
    def walk
      ( from:           Int,
        consume padded: Array[Char]^,
        consume scores: Array[Byte]^,
        consume breaks: Array[Int]^ )
    :   Unit =

      var i = from
      var needed = 0

      while i < length && needed == 0 do
        val c = source.charAt(i)

        if Character.isLetter(c) then
          var end = i
          while end < length && Character.isLetter(source.charAt(end)) do end += 1
          val wordOffset = i
          val wordLength = end - i

          if wordLength + 2 > padded.length then needed = wordLength + 2
          else
            val nBreaks =
              Hyphenation.breakPointsInto
                ( source, wordOffset, wordLength, hyphenation, effectiveLeft,
                  effectiveRight, padded.raw, scores.raw, breaks.raw )

            var prev = 0
            var k = 0

            while k < nBreaks do
              out.append(source, wordOffset + prev, wordOffset + breaks(k))
              out.append(hyphen)
              prev = breaks(k)
              k += 1

            out.append(source, wordOffset + prev, end)
            i = end
        else
          out.append(c)
          i += 1

      if needed > 0 then
        var newSize = padded.length
        while newSize < needed do newSize *= 2

        walk
          ( i,
            Array.grow(padded, newSize),
            Array.grow(scores, newSize + 1),
            Array.grow(breaks, newSize) )

    walk(0, Array[Char](32), Array[Byte](33), Array[Int](32))
    out.toString.tt

  // Split a single word at every admissible break point. For multi-word
  // input, non-letter regions and letter regions are treated as opaque
  // segments — only letter regions are split into syllables.
  def syllables(using hyphenation: Hyphenation): Sequence[Text] =
    val source = text.s
    val length = source.length
    val parts = new ArrayBuffer[Text]
    var i = 0

    while i < length do
      val c = source.charAt(i)

      if Character.isLetter(c) then
        var end = i
        while end < length && Character.isLetter(source.charAt(end)) do end += 1
        val word = source.substring(i, end).nn.tt

        val breaks =
          Hyphenation.breakPoints(word, hyphenation, hyphenation.leftMin, hyphenation.rightMin)

        if breaks.length == 0 then parts += word
        else
          var prev = 0
          var k = 0

          while k < breaks.length do
            parts += word.s.substring(prev, breaks(k)).nn.tt
            prev = breaks(k)
            k += 1

          parts += word.s.substring(prev).nn.tt

        i = end
      else
        var end = i
        while end < length && !Character.isLetter(source.charAt(end)) do end += 1
        parts += source.substring(i, end).nn.tt
        i = end

    Sequence.from(parts)

package hyphenations:
  // The CTAN hyph-utf8 American-English pattern file. Lazy: the patterns are
  // parsed the first time this given is referenced. The pattern file is
  // bundled at `polysyllabic/hyph-en-us.tex` on the classpath and is
  // redistributed verbatim under the FSF all-permissive licence shown in its
  // comment header.
  given englishHyphenation: Hyphenation =
    val resource = "/polysyllabic/hyph-en-us.tex"
    val stream = Hyphenation.getClass.getResourceAsStream(resource)

    if stream == null then panic(m"could not find $resource on the classpath")
    else
      val safe = stream.nn

      try Hyphenation.fromTex(scala.io.Source.fromInputStream(safe, "UTF-8").mkString.nn.tt)
      finally safe.close()
