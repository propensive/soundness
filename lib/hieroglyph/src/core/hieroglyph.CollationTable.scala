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
package hieroglyph

import java.io as ji

import scala.collection.immutable as sci
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import fulminate.*
import vacuous.*

// The Unicode Collation Algorithm (UTS #10), version 16.0.0, over the Default Unicode
// Collation Element Table (`allkeys.txt`). Comparison is non-ignorable (variable elements
// keep their weights); sort keys have three levels. Tailored tables are derived with
// `tailor`, which installs each `CollationRule`'s target between its base and the base's
// root successor.
object CollationTable:
  lazy val root: CollationTable =
    val in: ji.InputStream =
      Optional(getClass.getResourceAsStream("/hieroglyph/allkeys.txt")).or:
        safely:
          val uri = new java.net.URI("https://www.unicode.org/Public/UCA/16.0.0/allkeys.txt")
          uri.toURL().nn.openStream().nn: ji.InputStream

      . or(panic(m"could not find hieroglyph/allkeys.txt on the classpath"))

    val entries = scm.HashMap[sci.Vector[Int], sci.Vector[Long]]()
    val directives = scm.ArrayBuffer[(Int, Int, Int)]()

    scala.io.Source.fromInputStream(in).getLines().foreach: line =>
      val hash = line.indexOf('#')
      val data = if hash >= 0 then line.substring(0, hash).nn else line

      if data.startsWith("@implicitweights") then
        val semicolon = data.indexOf(';')
        val rangePart = data.substring("@implicitweights".length, semicolon).nn.trim.nn
        val dots = rangePart.indexOf("..")
        val start = Integer.parseInt(rangePart.substring(0, dots).nn, 16)
        val end = Integer.parseInt(rangePart.substring(dots + 2).nn, 16)
        val base = Integer.parseInt(data.substring(semicolon + 1).nn.trim.nn, 16)
        directives += ((start, end, base))
      else if !data.isEmpty && !data.startsWith("@") && data.indexOf(';') > 0 then
        val semicolon = data.indexOf(';')
        val keyPart = data.substring(0, semicolon).nn.trim.nn

        val key =
          keyPart.split(" ").nn.iterator.filterNot(_.nn.isEmpty).map: part =>
            Integer.parseInt(part.nn, 16)

          . to(sci.Vector)

        val elements = scm.ArrayBuffer[Long]()
        var open = data.indexOf('[', semicolon)

        while open >= 0 do
          val close = data.indexOf(']', open)
          val content = data.substring(open + 1, close).nn
          val variable = content.charAt(0) == '*'
          val fields = content.substring(1).nn.split("\\.").nn
          val primary = Integer.parseInt(fields(0).nn, 16)
          val secondary = Integer.parseInt(fields(1).nn, 16)
          val tertiary = Integer.parseInt(fields(2).nn, 16)
          elements += pack(primary << 8, secondary << 8, tertiary << 8, variable)
          open = data.indexOf('[', close)

        if !key.isEmpty then entries(key) = elements.to(sci.Vector)

    // Each directive's subtrahend is the range start of the *first* directive sharing its
    // base primary: the Tangut Supplement shares Tangut's base and subtrahend (UTS #10
    // §10.1.3).
    val firstStarts = scm.HashMap[Int, Int]()

    directives.foreach: (start, _, base) =>
      if !firstStarts.contains(base) then firstStarts(base) = start

    val implicitStarts = Array.allocate[Int](directives.length)
    val implicitEnds = Array.allocate[Int](directives.length)
    val implicitBases = Array.allocate[Int](directives.length)
    val implicitSubtrahends = Array.allocate[Int](directives.length)

    var index = 0

    while index < directives.length do
      implicitStarts(index) = directives(index)(0)
      implicitEnds(index) = directives(index)(1)
      implicitBases(index) = directives(index)(2)
      implicitSubtrahends(index) = firstStarts(directives(index)(2))
      index += 1

    build
      ( entries,
        Array.freeze(implicitStarts),
        Array.freeze(implicitEnds),
        Array.freeze(implicitBases),
        Array.freeze(implicitSubtrahends) )

  private def pack(primary: Int, secondary: Int, tertiary: Int, variable: Boolean): Long =
    val variableBit = if variable then 1L << 19 else 0L
    (primary.toLong << 40) | (secondary.toLong << 20) | variableBit | tertiary.toLong

  private def primaryOf(element: Long): Int = (element >>> 40).toInt
  private def secondaryOf(element: Long): Int = ((element >>> 20) & 0xfffff).toInt
  private def tertiaryOf(element: Long): Int = (element & 0x7ffff).toInt

  private def packKey(codepoints: sci.Vector[Int]): Long =
    var key = codepoints(0).toLong << 42
    if codepoints.length > 1 then key |= codepoints(1).toLong << 21
    if codepoints.length > 2 then key |= codepoints(2).toLong
    key

  private def build
    ( entries:              scm.HashMap[sci.Vector[Int], sci.Vector[Long]],
      implicitStarts:       Array[Int]^{},
      implicitEnds:         Array[Int]^{},
      implicitBases:        Array[Int]^{},
      implicitSubtrahends:  Array[Int]^{} )
  :   CollationTable =

    entries.keysIterator.foreach: key =>
      if key.length > 3 then panic(m"a collation table key has more than three codepoints")

    val singles =
      entries.iterator.filter(_(0).length == 1).map: entry => (entry(0)(0), entry(1))
      . to(sci.Vector).sortBy(_(0))

    val contractions =
      entries.iterator.filter(_(0).length > 1).map: entry => (packKey(entry(0)), entry(1))
      . to(sci.Vector).sortBy(_(0))

    val singleCodepoints = Array.allocate[Int](singles.length)
    val singleOffsets = Array.allocate[Int](singles.length + 1)
    val singleElements = Array.allocate[Long](singles.iterator.map(_(1).length).sum)

    var index = 0
    var offset = 0

    singles.foreach: (codepoint, elements) =>
      singleCodepoints(index) = codepoint
      singleOffsets(index) = offset

      elements.foreach: element =>
        singleElements(offset) = element
        offset += 1

      index += 1

    singleOffsets(index) = offset

    val contractionKeys = Array.allocate[Long](contractions.length)
    val contractionOffsets = Array.allocate[Int](contractions.length + 1)
    val contractionElements = Array.allocate[Long](contractions.iterator.map(_(1).length).sum)

    index = 0
    offset = 0

    contractions.foreach: (key, elements) =>
      contractionKeys(index) = key
      contractionOffsets(index) = offset

      elements.foreach: element =>
        contractionElements(offset) = element
        offset += 1

      index += 1

    contractionOffsets(index) = offset

    val starts = contractions.map { entry => (entry(0) >>> 42).toInt }.distinct.sorted
    val contractionStarts = Array.allocate[Int](starts.length)

    index = 0

    while index < starts.length do
      contractionStarts(index) = starts(index)
      index += 1

    val maxExpansion = entries.valuesIterator.map(_.length).maxOption.getOrElse(1).max(2)

    CollationTable
      ( Array.freeze(singleCodepoints),
        Array.freeze(singleOffsets),
        Array.freeze(singleElements),
        Array.freeze(contractionKeys),
        Array.freeze(contractionOffsets),
        Array.freeze(contractionElements),
        Array.freeze(contractionStarts),
        implicitStarts,
        implicitEnds,
        implicitBases,
        implicitSubtrahends,
        maxExpansion )

  // Unified_Ideograph codepoints in the CJK Unified Ideographs and CJK Compatibility
  // Ideographs blocks take base FB40; other Unified_Ideograph codepoints (the extension
  // blocks) take FB80; everything else unmapped takes FBC0 (UTS #10 §10.1.2, ranges per
  // Unicode 16.0.0).
  private def hanCore(codepoint: Int): Boolean =
    (codepoint >= 0x4e00 && codepoint <= 0x9fff) ||
      codepoint == 0xfa0e || codepoint == 0xfa0f || codepoint == 0xfa11 ||
      codepoint == 0xfa13 || codepoint == 0xfa14 || codepoint == 0xfa1f ||
      codepoint == 0xfa21 || codepoint == 0xfa23 || codepoint == 0xfa24 ||
      (codepoint >= 0xfa27 && codepoint <= 0xfa29)

  private def hanExtension(codepoint: Int): Boolean =
    (codepoint >= 0x3400 && codepoint <= 0x4dbf) ||
      (codepoint >= 0x20000 && codepoint <= 0x2a6df) ||
      (codepoint >= 0x2a700 && codepoint <= 0x2b739) ||
      (codepoint >= 0x2b740 && codepoint <= 0x2b81d) ||
      (codepoint >= 0x2b820 && codepoint <= 0x2cea1) ||
      (codepoint >= 0x2ceb0 && codepoint <= 0x2ebe0) ||
      (codepoint >= 0x2ebf0 && codepoint <= 0x2ee5d) ||
      (codepoint >= 0x30000 && codepoint <= 0x3134a) ||
      (codepoint >= 0x31350 && codepoint <= 0x323af)

class CollationTable private[hieroglyph]
  ( singleCodepoints:     Array[Int]^{},
    singleOffsets:        Array[Int]^{},
    singleElements:       Array[Long]^{},
    contractionKeys:      Array[Long]^{},
    contractionOffsets:   Array[Int]^{},
    contractionElements:  Array[Long]^{},
    contractionStarts:    Array[Int]^{},
    implicitStarts:       Array[Int]^{},
    implicitEnds:         Array[Int]^{},
    implicitBases:        Array[Int]^{},
    implicitSubtrahends:  Array[Int]^{},
    maxExpansion:         Int ):

  import CollationTable.{pack, primaryOf, secondaryOf, tertiaryOf}

  private def searchInt(array: Array[Int]^{}, value: Int): Int =
    val readable = array.readable
    var low = 0
    var high = readable.length - 1
    var found = -1

    while low <= high do
      val mid = (low + high) >>> 1

      if readable(mid) == value then
        found = mid
        low = high + 1
      else if readable(mid) > value then
        high = mid - 1
      else
        low = mid + 1

    found

  private def searchLong(array: Array[Long]^{}, value: Long): Int =
    val readable = array.readable
    var low = 0
    var high = readable.length - 1
    var found = -1

    while low <= high do
      val mid = (low + high) >>> 1

      if readable(mid) == value then
        found = mid
        low = high + 1
      else if readable(mid) > value then
        high = mid - 1
      else
        low = mid + 1

    found

  def key(text: Text): Array[Int]^{} = key(Normalization.decompose(text))

  def key(codepoints: Array[Int]^{}): Array[Int]^{} =
    val cps = Normalization.decompose(codepoints).readable
    val n = cps.length
    val elements = Array.scratch[Long](n*maxExpansion + 1)
    var elementCount = 0
    val consumed = Array.scratch[Boolean](n)
    val singleElementsView = singleElements.readable
    val singleOffsetsView = singleOffsets.readable
    val contractionElementsView = contractionElements.readable
    val contractionOffsetsView = contractionOffsets.readable
    val implicitStartsView = implicitStarts.readable
    val implicitEndsView = implicitEnds.readable
    val implicitBasesView = implicitBases.readable
    val implicitSubtrahendsView = implicitSubtrahends.readable

    var index = 0

    while index < n do
      if !consumed(index) then
        val codepoint = cps(index)
        var matchKey = codepoint.toLong << 42
        var matchLength = 1

        if searchInt(contractionStarts, codepoint) >= 0 then
          // Longest contiguous match first (UCA S2.1): with keys of at most three
          // codepoints, that means trying length three, then two.
          if index + 2 < n && !consumed(index + 1) && !consumed(index + 2) then
            val key3 = matchKey | cps(index + 1).toLong << 21 | cps(index + 2).toLong

            if searchLong(contractionKeys, key3) >= 0 then
              matchKey = key3
              matchLength = 3
              consumed(index + 1) = true
              consumed(index + 2) = true

          if matchLength == 1 && index + 1 < n && !consumed(index + 1) then
            val key2 = matchKey | cps(index + 1).toLong << 21

            if searchLong(contractionKeys, key2) >= 0 then
              matchKey = key2
              matchLength = 2
              consumed(index + 1) = true

          // Discontiguous matching (S2.1.1-S2.1.3): a following non-starter, not blocked
          // by an intervening codepoint of equal or higher combining class, may extend the
          // match; an absorbed codepoint is consumed and no longer blocks.
          if matchLength < 3 then
            var scan = index + matchLength
            var lastCcc = 0
            var scanning = true

            while scanning && scan < n && matchLength < 3 do
              if !consumed(scan) then
                val ccc = Normalization.combiningClass(cps(scan))

                if ccc == 0 then scanning = false
                else if ccc > lastCcc then
                  val extended =
                    if matchLength == 1 then matchKey | cps(scan).toLong << 21
                    else matchKey | cps(scan).toLong

                  if searchLong(contractionKeys, extended) >= 0 then
                    matchKey = extended
                    matchLength += 1
                    consumed(scan) = true
                  else
                    lastCcc = ccc
                else
                  lastCcc = ccc

              scan += 1

        if matchLength > 1 then
          val found = searchLong(contractionKeys, matchKey)
          var offset = contractionOffsetsView(found)
          val end = contractionOffsetsView(found + 1)

          while offset < end do
            elements(elementCount) = contractionElementsView(offset)
            elementCount += 1
            offset += 1
        else
          val found = searchInt(singleCodepoints, codepoint)

          if found >= 0 then
            var offset = singleOffsetsView(found)
            val end = singleOffsetsView(found + 1)

            while offset < end do
              elements(elementCount) = singleElementsView(offset)
              elementCount += 1
              offset += 1
          else
            var base = -1
            var subtrahend = 0
            var directive = 0

            while directive < implicitStartsView.length do
              val start = implicitStartsView(directive)
              val end = implicitEndsView(directive)

              if codepoint >= start && codepoint <= end then
                base = implicitBasesView(directive)
                subtrahend = implicitSubtrahendsView(directive)

              directive += 1

            val aaaa =
              if base >= 0 then base
              else if CollationTable.hanCore(codepoint) then 0xfb40 + (codepoint >>> 15)
              else if CollationTable.hanExtension(codepoint) then 0xfb80 + (codepoint >>> 15)
              else 0xfbc0 + (codepoint >>> 15)

            val bbbb =
              if base >= 0 then (codepoint - subtrahend) | 0x8000 else (codepoint & 0x7fff) | 0x8000

            elements(elementCount) = pack(aaaa << 8, 0x20 << 8, 0x02 << 8, false)
            elements(elementCount + 1) = pack(bbbb << 8, 0, 0, false)
            elementCount += 2

      index += 1

    val keyBuffer = Array.scratch[Int](elementCount*3 + 2)
    var keySize = 0
    var element = 0

    while element < elementCount do
      val weight = primaryOf(elements(element))

      if weight != 0 then
        keyBuffer(keySize) = weight
        keySize += 1

      element += 1

    keyBuffer(keySize) = 0
    keySize += 1
    element = 0

    while element < elementCount do
      val weight = secondaryOf(elements(element))

      if weight != 0 then
        keyBuffer(keySize) = weight
        keySize += 1

      element += 1

    keyBuffer(keySize) = 0
    keySize += 1
    element = 0

    while element < elementCount do
      val weight = tertiaryOf(elements(element))

      if weight != 0 then
        keyBuffer(keySize) = weight
        keySize += 1

      element += 1

    val result = Array.scratch[Int](keySize)
    java.lang.System.arraycopy(keyBuffer, 0, result, 0, keySize)
    Array.unsafeFrozen(result)

  def compare(left: Text, right: Text): Int = compareKeys(key(left), key(right))

  def compareKeys(left: Array[Int]^{}, right: Array[Int]^{}): Int =
    val leftView = left.readable
    val rightView = right.readable
    val limit = leftView.length.min(rightView.length)
    var index = 0
    var result = 0

    while result == 0 && index < limit do
      if leftView(index) != rightView(index)
      then result = if leftView(index) < rightView(index) then -1 else 1

      index += 1

    if result == 0 then leftView.length.compare(rightView.length) else result

  def tailor(rules: List[CollationRule]): CollationTable =
    val entries = scm.HashMap[sci.Vector[Int], sci.Vector[Long]]()
    val singleCodepointsView = singleCodepoints.readable
    val singleOffsetsView = singleOffsets.readable
    val singleElementsView = singleElements.readable
    val contractionKeysView = contractionKeys.readable
    val contractionOffsetsView = contractionOffsets.readable
    val contractionElementsView = contractionElements.readable

    var index = 0

    while index < singleCodepointsView.length do
      val elements =
        (singleOffsetsView(index) until singleOffsetsView(index + 1))
        . map(singleElementsView(_)).to(sci.Vector)

      entries(sci.Vector(singleCodepointsView(index))) = elements
      index += 1

    index = 0

    while index < contractionKeysView.length do
      val packed = contractionKeysView(index)
      val first = (packed >>> 42).toInt
      val second = ((packed >>> 21) & 0x1fffff).toInt
      val third = (packed & 0x1fffff).toInt

      val key =
        if third != 0 then sci.Vector(first, second, third) else sci.Vector(first, second)

      val elements =
        (contractionOffsetsView(index) until contractionOffsetsView(index + 1))
        . map(contractionElementsView(_)).to(sci.Vector)

      entries(key) = elements
      index += 1

    val counters = scm.HashMap[(sci.Vector[Int], Int), Int]()

    rules.stdlib.foreach: rule =>
      val baseKey = Normalization.decompose(rule.base).readable.toVector
      val targetKey = Normalization.decompose(rule.target).readable.toVector

      val baseElements = entries.get(baseKey) match
        case Some(elements) => elements
        case None           => panic(m"the tailoring base has no collation mapping")

      val count = counters.getOrElse((baseKey, rule.level.ordinal), 0) + 1
      counters((baseKey, rule.level.ordinal)) = count
      if count > 0xff then panic(m"too many tailoring rules against one base")

      val first = baseElements(0)

      val bumped = rule.level match
        case CollationLevel.Primary =>
          CollationTable.pack
            ( primaryOf(first) + count, secondaryOf(first), tertiaryOf(first), false )

        case CollationLevel.Secondary =>
          CollationTable.pack
            ( primaryOf(first), secondaryOf(first) + count, tertiaryOf(first), false )

        case CollationLevel.Tertiary =>
          CollationTable.pack
            ( primaryOf(first), secondaryOf(first), tertiaryOf(first) + count, false )

      entries(targetKey) = bumped +: baseElements.tail

    CollationTable.build
      ( entries, implicitStarts, implicitEnds, implicitBases, implicitSubtrahends )
