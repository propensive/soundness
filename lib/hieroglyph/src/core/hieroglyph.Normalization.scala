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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package hieroglyph

import java.io as ji

import scala.collection.immutable as sci
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import rudiments.*
import vacuous.*

// Canonical decomposition (NFD), from `UnicodeData.txt` version 16.0.0: combining classes
// (field 3) and canonical decomposition mappings (field 5), with Hangul syllables decomposed
// arithmetically. Composition (NFC) is not provided.
object Normalization:
  private val HangulSBase: Int = 0xac00
  private val HangulLBase: Int = 0x1100
  private val HangulVBase: Int = 0x1161
  private val HangulTBase: Int = 0x11a7
  private val HangulVCount: Int = 21
  private val HangulTCount: Int = 28
  private val HangulSCount: Int = 11172

  private case class Tables
    ( cccCodepoints:    Array[Int]^{},
      cccValues:        Array[Byte]^{},
      decompCodepoints: Array[Int]^{},
      decompOffsets:    Array[Int]^{},
      decompData:       Array[Int]^{} )

  private lazy val tables: Tables =
    val in: ji.InputStream =
      Optional(getClass.getResourceAsStream("/hieroglyph/UnicodeData.txt"))
      . or(remoteUnicodeData("UnicodeData.txt".tt))
      . or(panic(m"could not find hieroglyph/UnicodeData.txt on the classpath"))

    val cccPairs = scm.ArrayBuffer[(Int, Int)]()
    val rawDecompositions = scm.HashMap[Int, sci.Vector[Int]]()

    scala.io.Source.fromInputStream(in).getLines().foreach: line =>
      val fields = line.split(";", -1).nn
      if fields.length > 5 then
        val codepoint = Integer.parseInt(fields(0).nn, 16)
        val ccc = Integer.parseInt(fields(3).nn)
        if ccc != 0 then cccPairs += ((codepoint, ccc))

        val mapping = fields(5).nn
        if !mapping.isEmpty && !mapping.startsWith("<") then
          val decomposition =
            mapping.split(" ").nn.iterator.map { part => Integer.parseInt(part.nn, 16) }
            . to(sci.Vector)

          rawDecompositions(codepoint) = decomposition

    // Close the mappings recursively (canonical decompositions are acyclic and shallow) so that
    // runtime decomposition is a single table hit.
    val closed = scm.HashMap[Int, sci.Vector[Int]]()

    def close(codepoint: Int): sci.Vector[Int] = closed.getOrElseUpdate(
      codepoint,
      rawDecompositions.get(codepoint) match
        case Some(decomposition) => decomposition.flatMap(close)
        case None                => sci.Vector(codepoint))

    val sortedCcc = cccPairs.sortBy(_(0))
    val cccCodepoints = Array.allocate[Int](sortedCcc.length)
    val cccValues = Array.allocate[Byte](sortedCcc.length)

    var index = 0

    while index < sortedCcc.length do
      cccCodepoints(index) = sortedCcc(index)(0)
      cccValues(index) = sortedCcc(index)(1).toByte
      index += 1

    val sortedDecompositions =
      rawDecompositions.keysIterator.to(sci.Vector).sorted.map { cp => (cp, close(cp)) }

    val decompCodepoints = Array.allocate[Int](sortedDecompositions.length)
    val decompOffsets = Array.allocate[Int](sortedDecompositions.length + 1)
    val dataLength = sortedDecompositions.iterator.map(_(1).length).sum
    val decompData = Array.allocate[Int](dataLength)

    index = 0
    var offset = 0

    sortedDecompositions.foreach: (codepoint, decomposition) =>
      decompCodepoints(index) = codepoint
      decompOffsets(index) = offset

      decomposition.foreach: element =>
        decompData(offset) = element
        offset += 1

      index += 1

    decompOffsets(index) = offset

    Tables
      ( Array.freeze(cccCodepoints),
        Array.freeze(cccValues),
        Array.freeze(decompCodepoints),
        Array.freeze(decompOffsets),
        Array.freeze(decompData) )

  // Index of `codepoint` in the sorted `array`, or -1 if absent.
  private def search(array: Array[Int]^{}, codepoint: Int): Int =
    var low = 0
    var high = array.length - 1
    var found = -1

    while low <= high do
      val mid = (low + high) >>> 1
      val value = array.at(Ordinal.zerary(mid)).or(Int.MaxValue)

      if value == codepoint then
        found = mid
        low = high + 1
      else if value > codepoint then high = mid - 1
      else low = mid + 1

    found

  def combiningClass(codepoint: Int): Int =
    val index = search(tables.cccCodepoints, codepoint)

    // Combining classes reach 240, so the byte is read back unsigned.
    if index < 0 then 0 else tables.cccValues.at(Ordinal.zerary(index)).lay(0)(_.toInt & 0xff)

  def decompose(text: Text): Array[Int]^{} =
    val s = text.s
    val n = s.length
    val raw = Array.scratch[Int](n)
    var count = 0
    var index = 0

    while index < n do
      val codepoint = Character.codePointAt(s, index)
      raw(count) = codepoint
      count += 1
      index += Character.charCount(codepoint)

    val trimmed = Array.scratch[Int](count)
    java.lang.System.arraycopy(raw, 0, trimmed, 0, count)
    decompose(Array.unsafeFrozen(trimmed))

  def decompose(codepoints: Array[Int]^{}): Array[Int]^{} =
    val cps = codepoints.readable
    val n = cps.length

    // Maximum canonical expansion is 4 codepoints per input codepoint (Hangul LVT = 3; Greek
    // with multiple marks = 4), so `n*4` bounds the output. A bare scratch array rather than
    // an exclusive `Array`: the reordering pass below both reads and writes interior elements.
    val buffer = Array.scratch[Int](n*4 + 1)
    var size = 0

    var index = 0

    while index < n do
      val codepoint = cps(index)

      if codepoint < 0xc0 then
        buffer(size) = codepoint
        size += 1
      else if codepoint >= HangulSBase && codepoint < HangulSBase + HangulSCount then
        val sIndex = codepoint - HangulSBase
        buffer(size) = HangulLBase + sIndex/(HangulVCount*HangulTCount)
        buffer(size + 1) = HangulVBase + (sIndex%(HangulVCount*HangulTCount))/HangulTCount
        size += 2
        val trailing = sIndex%HangulTCount

        if trailing != 0 then
          buffer(size) = HangulTBase + trailing
          size += 1
      else
        val found = search(tables.decompCodepoints, codepoint)

        if found < 0 then
          buffer(size) = codepoint
          size += 1
        else
          var offset = tables.decompOffsets.at(Ordinal.zerary(found)).or(0)
          val end = tables.decompOffsets.at(Ordinal.zerary(found + 1)).or(0)

          while offset < end do
            buffer(size) = tables.decompData.at(Ordinal.zerary(offset)).or(0)
            size += 1
            offset += 1

      index += 1

    // Canonical reordering: stable insertion sort of each maximal run of combining marks
    // (ccc > 0) by combining class; runs are rarely longer than a few codepoints.
    var position = 1

    while position < size do
      val ccc = combiningClass(buffer(position))

      if ccc != 0 then
        var cursor = position

        while cursor > 0 && combiningClass(buffer(cursor - 1)) > ccc do
          val previous = buffer(cursor - 1)
          buffer(cursor - 1) = buffer(cursor)
          buffer(cursor) = previous
          cursor -= 1

      position += 1

    val result = Array.scratch[Int](size)
    java.lang.System.arraycopy(buffer, 0, result, 0, size)
    Array.unsafeFrozen(result)
