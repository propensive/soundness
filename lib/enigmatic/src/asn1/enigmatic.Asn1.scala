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
package enigmatic

import scala.caps

import java.nio.charset as jnc

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

import Asn1.Error.Reason
import fulminate.*

// An ASN.1 value, in the subset of the universal types that PKIX uses, plus two escape hatches for
// everything else.
//
// `Tagged` and `Unknown` deserve explanation. An explicit context tag (`[0] EXPLICIT`) wraps a
// complete inner value, so it can be both written and read; an *implicit* context tag (`[0]
// IMPLICIT`) merely replaces the inner value's tag, which makes `[0] IMPLICIT INTEGER` and `[0]
// IMPLICIT OCTET STRING` byte-identical. Recovering the inner type needs the ASN.1 module's schema,
// which this layer does not have, so `Tagged(_, explicit = false, _)` is *write-only*: the decoder
// never produces it, and yields `Unknown` instead.
//
// `Unknown` carries the content octets verbatim, which is what makes decoding total (real
// certificates hold `T61String`, `BMPString` and `ENUMERATED` values that are not modelled here)
// and what makes `decode` followed by `encode` reproduce the original bytes exactly — the property
// that verifying a signature over a certificate's `TBSCertificate` depends on.
object Asn1:
  // Tag classes, in the two high bits of the identifier octet.
  val Universal: Int = 0
  val Application: Int = 1
  val Context: Int = 2
  val Private: Int = 3

  // Universal tag numbers.
  private val BooleanTag: Int = 0x01
  private val IntegerTag: Int = 0x02
  private val BitStringTag: Int = 0x03
  private val OctetStringTag: Int = 0x04
  private val NullTag: Int = 0x05
  private val ObjectIdTag: Int = 0x06
  private val Utf8StringTag: Int = 0x0c
  private val SequenceTag: Int = 0x10
  private val SetTag: Int = 0x11
  private val PrintableStringTag: Int = 0x13
  private val Ia5StringTag: Int = 0x16
  private val UtcTimeTag: Int = 0x17
  private val GeneralizedTimeTag: Int = 0x18

  private val SecondsPerDay: Long = 86400L

  given encodable: Asn1 is Encodable in Der = value => Der(render(value))

  given decodable: (tactic: Tactic[Asn1.Error]^) => ((Asn1 is Decodable in Der)^{tactic, caps.any}) =
    der => Parser.parse(der.data)

  given aggregable: (tactic: Tactic[Asn1.Error]^)
  =>  ( ((Asn1 in Der) is Aggregable by Data)^{tactic, caps.any} ) =
    bytes => Parser.parse(bytes.read[Data]).asInstanceOf[Asn1 in Der]

  // The DER encoding of a single value. Each node renders its own content into its own producer,
  // because a definite-length prefix must be written before the content it measures, and because
  // DER orders the elements of a `SET` by their *encodings* — both need the children's bytes in
  // hand first. That costs one copy per level of nesting, which is immaterial for the kilobyte-
  // scale structures (certificates, keys, requests) this exists to serve.
  private def render(value: Asn1): Data = Producer.collect[Data]()(write(_, value))

  private def write(out: (Producer.Bytes)^, value: Asn1): Unit =
    val content = contentOf(value)
    identifier(out, tagClassOf(value), constructedForm(value), tagOf(value))
    length(out, content.length)
    out.put(content)

  private def tagClassOf(value: Asn1): Int = value match
    case Asn1.Tagged(_, _, _)            => Context
    case Asn1.Unknown(tagClass, _, _, _) => tagClass
    case _                               => Universal

  private def tagOf(value: Asn1): Int = value match
    case Asn1.Boolean(_)            => BooleanTag
    case Asn1.Integer(_)            => IntegerTag
    case Asn1.BitString(_, _)       => BitStringTag
    case Asn1.OctetString(_)        => OctetStringTag
    case Asn1.Null                  => NullTag
    case Asn1.ObjectId(_)           => ObjectIdTag
    case Asn1.Utf8String(_)         => Utf8StringTag
    case Asn1.PrintableString(_)    => PrintableStringTag
    case Asn1.Ia5String(_)          => Ia5StringTag
    case Asn1.UtcTime(_)            => UtcTimeTag
    case Asn1.GeneralizedTime(_)    => GeneralizedTimeTag
    case Asn1.Sequence(_)           => SequenceTag
    case Asn1.Set(_)                => SetTag
    case Asn1.Tagged(tag, _, _)     => tag
    case Asn1.Unknown(_, tag, _, _) => tag

  private def constructedForm(value: Asn1): scala.Boolean = value match
    case Asn1.Sequence(_)                   => true
    case Asn1.Set(_)                        => true
    case Asn1.Tagged(_, true, _)            => true
    case Asn1.Tagged(_, false, inner)       => constructedForm(inner)
    case Asn1.Unknown(_, _, constructed, _) => constructed
    case _                                  => false

  // The content octets of a value: everything after its identifier and length.
  private def contentOf(value: Asn1): Data = value match
    case Asn1.Boolean(boolean)      => Array[Byte](if boolean then 0xff.toByte else 0.toByte)
    case Asn1.Integer(integer)      => integer.toByteArray.immutable(using Unsafe)
    case Asn1.OctetString(bytes)    => bytes
    case Asn1.Null                  => Array[Byte]()
    case Asn1.Utf8String(text)      => utf8(text)
    case Asn1.PrintableString(text) => utf8(text)
    case Asn1.Ia5String(text)       => utf8(text)

    case Asn1.BitString(bytes, unusedBits) =>
      Producer.collect[Data](): out =>
        out.push(unusedBits.toByte)
        out.put(bytes)

    case Asn1.ObjectId(arcs) => Producer.collect[Data]()(objectId(_, arcs))

    case Asn1.UtcTime(timestamp) =>
      Producer.collect[Data]()(time(_, timestamp, false))

    case Asn1.GeneralizedTime(timestamp) =>
      Producer.collect[Data]()(time(_, timestamp, true))

    case Asn1.Sequence(elements) =>
      Producer.collect[Data](): out =>
        elements.foreach: element =>
          out.put(render(element))

    case Asn1.Set(elements) =>
      // DER orders the members of a `SET` by their encodings, shorter-first when one is a prefix
      // of the other (X.690 §11.6, treating the shorter as zero-padded).
      given derOrdering: Ordering[Data] = Ordering.fromLessThan(precedes(_, _))
      val rendered: List[Data] = elements.map(render(_)).sort

      Producer.collect[Data](): out =>
        rendered.foreach: element =>
          out.put(element)

    case Asn1.Tagged(_, true, inner)    => render(inner)
    case Asn1.Tagged(_, false, inner)   => contentOf(inner)
    case Asn1.Unknown(_, _, _, content) => content

  private def precedes(left: Data, right: Data): scala.Boolean =
    var index = 0
    var difference = 0

    while difference == 0 && index < left.length && index < right.length do
      difference = (left.readUnchecked(index) & 0xff) - (right.readUnchecked(index) & 0xff)
      index += 1

    if difference != 0 then difference < 0 else left.length <= right.length

  private def utf8(text: Text): Data =
    text.s.getBytes(jnc.StandardCharsets.UTF_8).nn.immutable(using Unsafe)

  private def identifier
    ( out: (Producer.Bytes)^, tagClass: Int, constructed: scala.Boolean, tag: Int )
  :   Unit =

    val bits = (tagClass << 6) | (if constructed then 0x20 else 0)

    if tag < 0x1f then out.push((bits | tag).toByte) else
      out.push((bits | 0x1f).toByte)
      base128(out, tag)

  private def length(out: (Producer.Bytes)^, size: Int): Unit =
    if size < 0x80 then out.push(size.toByte) else
      var count = 0
      var remaining = size

      while remaining > 0 do
        count += 1
        remaining >>>= 8

      out.push((0x80 | count).toByte)
      var index = count - 1

      while index >= 0 do
        out.push(((size >>> (index*8)) & 0xff).toByte)
        index -= 1

  // A base-128 subidentifier: seven bits per octet, most significant first, with the high bit set
  // on every octet but the last, and no leading zero group.
  private def base128(out: (Producer.Bytes)^, value: Int): Unit =
    var shift = 0
    var remaining = value >>> 7

    while remaining != 0 do
      shift += 7
      remaining >>>= 7

    while shift > 0 do
      out.push((0x80 | ((value >>> shift) & 0x7f)).toByte)
      shift -= 7

    out.push((value & 0x7f).toByte)

  // The first two arcs of an object identifier share one subidentifier, as `40*first + second`.
  // An identifier with fewer than two arcs is not well-formed; its arcs are emitted as they are.
  private def objectId(out: (Producer.Bytes)^, arcs: List[Int]): Unit = arcs match
    case first :: second :: rest =>
      base128(out, first*40 + second)

      rest.foreach: arc =>
        base128(out, arc)

    case rest =>
      rest.foreach: arc =>
        base128(out, arc)

  private def time(out: (Producer.Bytes)^, timestamp: Long, generalized: scala.Boolean): Unit =
    val days = Math.floorDiv(timestamp, SecondsPerDay)
    val seconds = Math.floorMod(timestamp, SecondsPerDay).toInt
    val (year, month, day) = civil(days)

    if generalized then digits(out, year, 4) else digits(out, ((year%100) + 100)%100, 2)

    digits(out, month, 2)
    digits(out, day, 2)
    digits(out, seconds/3600, 2)
    digits(out, (seconds/60)%60, 2)
    digits(out, seconds%60, 2)
    out.push('Z'.toByte)

  private def digits(out: (Producer.Bytes)^, value: Int, count: Int): Unit =
    var scale = 1
    var index = 1

    while index < count do
      scale *= 10
      index += 1

    var remaining = value

    while scale > 0 do
      out.push((remaining/scale + '0').toByte)
      remaining = remaining%scale
      scale /= 10

  // Howard Hinnant's `civil_from_days`: the proleptic Gregorian date of a day number counted from
  // 1970-01-01. Rolling our own keeps this component free of `java.time` (absent from some
  // platforms) and of a dependency on aviation, which is not yet capture-checked; a later stage
  // can hand aviation the `Long`.
  private def civil(days: Long): (Int, Int, Int) =
    val shifted = days + 719468
    val era = (if shifted >= 0 then shifted else shifted - 146096)/146097
    val dayOfEra = shifted - era*146097
    val yearOfEra = (dayOfEra - dayOfEra/1460 + dayOfEra/36524 - dayOfEra/146096)/365
    val yearZero = yearOfEra + era*400
    val dayOfYear = dayOfEra - (365*yearOfEra + yearOfEra/4 - yearOfEra/100)
    val monthZero = (5*dayOfYear + 2)/153
    val day = (dayOfYear - (153*monthZero + 2)/5 + 1).toInt
    val month = (if monthZero < 10 then monthZero + 3 else monthZero - 9).toInt

    ((if month <= 2 then yearZero + 1 else yearZero).toInt, month, day)

  // Hinnant's `days_from_civil`, the inverse of `civil`.
  private def epochDay(year: Int, month: Int, day: Int): Long =
    val shifted: Long = year - (if month <= 2 then 1 else 0)
    val era = (if shifted >= 0 then shifted else shifted - 399)/400
    val yearOfEra = shifted - era*400
    val dayOfYear = (153*(month + (if month > 2 then -3 else 9)) + 2)/5 + day - 1
    val dayOfEra = yearOfEra*365 + yearOfEra/4 - yearOfEra/100 + dayOfYear

    era*146097 + dayOfEra - 719468

  private def monthLength(year: Int, month: Int): Int =
    val leap = (year%4 == 0 && year%100 != 0) || year%400 == 0

    month match
      case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
      case 4 | 6 | 9 | 11              => 30
      case _                           => if leap then 29 else 28

  private[enigmatic] object Parser:
    def parse(source: Data): Asn1 raises Asn1.Error =
      val parser = new Parser(source)
      val result = parser.value(parser.data.length)

      if parser.offset < parser.data.length
      then abort(Asn1.Error(Reason.Trailing(parser.offset.toLong)))

      result

  // A strict DER reader: every construct that BER allows but DER forbids — indefinite lengths,
  // overlong lengths and tags, non-minimal integers, constructed strings, unordered sets — is an
  // error, so that whatever this accepts re-encodes to the bytes it was read from.
  final class Parser private[enigmatic] (input: Data) extends caps.Mutable:
    private[enigmatic] val data: Data = input

    // Exposed to the `parse` entry point only, so that it can detect trailing bytes.
    var offset: Int = 0

    private inline def need(count: Int, limit: Int): Unit raises Asn1.Error =
      if limit - offset < count then abort(Asn1.Error(Reason.Truncated(offset.toLong)))

    private inline update def readByte(): Int = (data.readUnchecked(offset) & 0xff).also(offset += 1)

    private update def readRaw(end: Int): scala.Array[Byte] =
      val result = new scala.Array[Byte](end - offset)
      System.arraycopy(Array.unsafeJvm(data), offset, result, 0, end - offset)
      offset = end

      result

    private update def readBytes(end: Int): Data = Array.unsafeFrozen(readRaw(end))

    update def value(limit: Int)(using Tactic[Asn1.Error]): Asn1 =
      val start = offset
      need(1, limit)
      val head = readByte()
      val tagClass = (head >>> 6) & 0x03
      val constructed = (head & 0x20) != 0
      val tag = if (head & 0x1f) == 0x1f then readTag(limit) else head & 0x1f
      val size = readLength(limit)
      need(size, limit)
      val end = offset + size

      if tagClass == Universal then universal(start, end, tag, constructed, size)
      else if !constructed then Asn1.Unknown(tagClass, tag, false, readBytes(end))
      else if scan(offset, end) == end then Asn1.Tagged(tag, true, value(end))
      else Asn1.Unknown(tagClass, tag, true, readBytes(end))

    private update def universal
      ( start: Int, end: Int, tag: Int, constructed: scala.Boolean, size: Int )
      ( using Tactic[Asn1.Error] )
    :   Asn1 =

      inline def primitive(): Unit =
        if constructed then abort(Asn1.Error(Reason.NotPrimitive(start.toLong, tag)))

      inline def aggregate(): Unit =
        if !constructed then abort(Asn1.Error(Reason.NotConstructed(start.toLong, tag)))

      tag match
        case 0 => abort(Asn1.Error(Reason.ReservedTag(start.toLong)))

        case BooleanTag =>
          primitive()
          if size != 1 then abort(Asn1.Error(Reason.BadLength(start.toLong, tag, size)))
          val byte = readByte()

          if byte == 0x00 then Asn1.Boolean(false)
          else if byte == 0xff then Asn1.Boolean(true)
          else abort(Asn1.Error(Reason.BadBoolean(start.toLong, byte)))

        case IntegerTag =>
          primitive()
          if size == 0 then abort(Asn1.Error(Reason.EmptyInteger(start.toLong)))

          if size > 1 then
            val first = data.readUnchecked(offset) & 0xff
            val second = data.readUnchecked(offset + 1) & 0xff

            if (first == 0x00 && (second & 0x80) == 0) || (first == 0xff && (second & 0x80) != 0)
            then abort(Asn1.Error(Reason.NonMinimalInteger(start.toLong)))

          Asn1.Integer(BigInt(new java.math.BigInteger(Array.unsafeJvm(readBytes(end)))))

        case BitStringTag =>
          primitive()
          if size == 0 then abort(Asn1.Error(Reason.BadLength(start.toLong, tag, size)))
          val unusedBits = readByte()

          if unusedBits > 7 || (size == 1 && unusedBits != 0)
          then abort(Asn1.Error(Reason.BadUnusedBits(start.toLong, unusedBits)))

          if unusedBits > 0 && (data.readUnchecked(end - 1) & ((1 << unusedBits) - 1)) != 0
          then abort(Asn1.Error(Reason.BadUnusedBits(start.toLong, unusedBits)))

          Asn1.BitString(readBytes(end), unusedBits)

        case OctetStringTag =>
          primitive()
          Asn1.OctetString(readBytes(end))

        case NullTag =>
          primitive()
          if size != 0 then abort(Asn1.Error(Reason.BadLength(start.toLong, tag, size)))
          Asn1.Null

        case ObjectIdTag =>
          primitive()
          Asn1.ObjectId(objectId(start, end))

        case Utf8StringTag =>
          primitive()
          Asn1.Utf8String(text(end))

        case PrintableStringTag =>
          primitive()
          Asn1.PrintableString(text(end))

        case Ia5StringTag =>
          primitive()
          Asn1.Ia5String(text(end))

        case UtcTimeTag =>
          primitive()
          Asn1.UtcTime(timestamp(start, end, false))

        case GeneralizedTimeTag =>
          primitive()
          Asn1.GeneralizedTime(timestamp(start, end, true))

        case SequenceTag =>
          aggregate()
          Asn1.Sequence(elements(end))

        case SetTag =>
          aggregate()
          Asn1.Set(members(end))

        case _ => Asn1.Unknown(Universal, tag, constructed, readBytes(end))

    private update def elements(end: Int)(using Tactic[Asn1.Error]): List[Asn1] =
      val builder = scm.ListBuffer[Asn1]()
      while offset < end do builder += value(end)

      builder.to(List)

    private update def members(end: Int)(using Tactic[Asn1.Error]): List[Asn1] =
      val builder = scm.ListBuffer[Asn1]()
      var previous = -1
      var previousEnd = -1

      while offset < end do
        val start = offset
        builder += value(end)

        if previous >= 0 && !ordered(previous, previousEnd, start, offset)
        then abort(Asn1.Error(Reason.UnsortedSet(start.toLong)))

        previous = start
        previousEnd = offset

      builder.to(List)

    private update def ordered(from: Int, until: Int, from2: Int, until2: Int): scala.Boolean =
      val leftSize = until - from
      val rightSize = until2 - from2
      var index = 0
      var difference = 0

      while difference == 0 && index < leftSize && index < rightSize do
        difference = (data.readUnchecked(from + index) & 0xff) - (data.readUnchecked(from2 + index) & 0xff)
        index += 1

      if difference != 0 then difference < 0 else leftSize <= rightSize

    private update def text(end: Int): Text =
      val size = end - offset
      val result = new String(Array.unsafeJvm(data), offset, size, jnc.StandardCharsets.UTF_8)
      offset = end

      result.tt

    private update def readTag(limit: Int)(using Tactic[Asn1.Error]): Int =
      val start = offset
      need(1, limit)
      if (data.readUnchecked(offset) & 0xff) == 0x80 then abort(Asn1.Error(Reason.NonMinimalTag(start.toLong)))
      var result = 0
      var reading = true

      while reading do
        need(1, limit)
        val byte = readByte()

        if (result >>> 24) != 0 then abort(Asn1.Error(Reason.Overflow(start.toLong)))
        result = (result << 7) | (byte & 0x7f)
        if (byte & 0x80) == 0 then reading = false

      if result < 0x1f then abort(Asn1.Error(Reason.NonMinimalTag(start.toLong)))

      result

    private update def readLength(limit: Int)(using Tactic[Asn1.Error]): Int =
      val start = offset
      need(1, limit)
      val first = readByte()

      if first < 0x80 then first
      else if first == 0x80 then abort(Asn1.Error(Reason.IndefiniteLength(start.toLong)))
      else
        val count = first & 0x7f
        if count == 0x7f then abort(Asn1.Error(Reason.NonMinimalLength(start.toLong)))
        if count > 4 then abort(Asn1.Error(Reason.Overflow(start.toLong)))
        need(count, limit)
        var result = 0L
        var index = 0

        while index < count do
          val byte = readByte()
          if index == 0 && byte == 0 then abort(Asn1.Error(Reason.NonMinimalLength(start.toLong)))
          result = (result << 8) | byte
          index += 1

        if result < 0x80 then abort(Asn1.Error(Reason.NonMinimalLength(start.toLong)))
        if result > Int.MaxValue then abort(Asn1.Error(Reason.Overflow(start.toLong)))

        result.toInt

    private update def objectId(start: Int, end: Int)(using Tactic[Asn1.Error]): List[Int] =
      if offset >= end then abort(Asn1.Error(Reason.BadOid(start.toLong)))
      val builder = scm.ListBuffer[Int]()
      var first = true

      while offset < end do
        val subidentifier = offset
        if (data.readUnchecked(offset) & 0xff) == 0x80 then abort(Asn1.Error(Reason.BadOid(subidentifier.toLong)))
        var accumulated = 0
        var reading = true

        while reading do
          if offset >= end then abort(Asn1.Error(Reason.BadOid(subidentifier.toLong)))
          val byte = readByte()

          if (accumulated >>> 24) != 0
          then abort(Asn1.Error(Reason.OidArcOverflow(subidentifier.toLong)))

          accumulated = (accumulated << 7) | (byte & 0x7f)
          if (byte & 0x80) == 0 then reading = false

        if !first then builder += accumulated else
          first = false

          if accumulated < 40 then
            builder += 0
            builder += accumulated
          else if accumulated < 80 then
            builder += 1
            builder += accumulated - 40
          else
            builder += 2
            builder += accumulated - 80

      builder.to(List)

    // DER admits exactly one form for each of the two time types: `YYMMDDHHMMSSZ` and
    // `YYYYMMDDHHMMSSZ`, with no fractional seconds and no offset from UTC. `UTCTime`'s two-digit
    // year runs from 1950 to 2049 (RFC 5280 §4.1.2.5.1).
    private update def timestamp(start: Int, end: Int, generalized: scala.Boolean)(using Tactic[Asn1.Error])
    :   Long =

      val size = end - offset
      val expected = if generalized then 15 else 13
      if size != expected then abort(Asn1.Error(Reason.BadTime(start.toLong)))

      def digit(index: Int): Int =
        val byte = data.readUnchecked(offset + index) & 0xff
        if byte < '0' || byte > '9' then abort(Asn1.Error(Reason.BadTime(start.toLong)))

        byte - '0'

      def number(index: Int, count: Int): Int =
        var result = 0
        var position = 0

        while position < count do
          result = result*10 + digit(index + position)
          position += 1

        result

      if (data.readUnchecked(end - 1) & 0xff) != 'Z' then abort(Asn1.Error(Reason.BadTime(start.toLong)))

      val year =
        if generalized then number(0, 4)
        else
          val short = number(0, 2)
          if short < 50 then 2000 + short else 1900 + short

      val base = if generalized then 4 else 2
      val month = number(base, 2)
      val day = number(base + 2, 2)
      val hour = number(base + 4, 2)
      val minute = number(base + 6, 2)
      val second = number(base + 8, 2)

      if month < 1 || month > 12 || day < 1 || day > monthLength(year, month) || hour > 23 ||
        minute > 59 || second > 59
      then abort(Asn1.Error(Reason.BadTime(start.toLong)))

      offset = end

      epochDay(year, month, day)*SecondsPerDay + hour*3600 + minute*60 + second

    // The end offset of the single well-formed value starting at `from`, or `-1` if the bytes up to
    // `limit` do not hold one. Used to decide whether a constructed context tag is an explicit
    // wrapper (exactly one value) or opaque content; it inspects structure only, and never raises,
    // because failing this test is an ordinary outcome rather than an error.
    private update def scan(from: Int, limit: Int): Int = boundary:
      var position = from

      def next(): Int =
        if position >= limit then break(-1)
        val byte = data.readUnchecked(position) & 0xff
        position += 1

        byte

      val head = next()
      if (head & 0x1f) == 0x1f then while (next() & 0x80) != 0 do ()
      val first = next()

      if first < 0x80 then (if limit - position < first then -1 else position + first)
      else if first == 0x80 then -1
      else
        val count = first & 0x7f
        if count > 4 then break(-1)
        var size = 0L
        var index = 0

        while index < count do
          size = (size << 8) | next()
          index += 1

        if size > limit - position then -1 else position + size.toInt

  // Asn1Error → Asn1.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case Truncated(offset)         => m"the input was truncated at byte $offset"
        case IndefiniteLength(offset)  => m"an indefinite length was found at byte $offset"
        case NonMinimalLength(offset)  => m"an overlong length was found at byte $offset"
        case NonMinimalTag(offset)     => m"an overlong tag number was found at byte $offset"
        case NonMinimalInteger(offset) => m"an overlong integer was found at byte $offset"
        case EmptyInteger(offset)      => m"an integer with no content was found at byte $offset"
        case Overflow(offset)          => m"an unrepresentable length was found at byte $offset"
        case Trailing(offset)          => m"unexpected trailing bytes were found from byte $offset"
        case InvalidUtf8(offset)       => m"invalid UTF-8 was found at byte $offset"
        case ReservedTag(offset)       => m"the reserved tag number 0 was found at byte $offset"
        case BadOid(offset)            => m"a malformed object identifier was found at byte $offset"
        case OidArcOverflow(offset)    => m"an arc too large for Int was found at byte $offset"
        case UnsortedSet(offset)       => m"the set at byte $offset was not in ascending order"
        case BadTime(offset)           => m"a malformed time value was found at byte $offset"

        case BadBoolean(offset, byte) =>
          m"a boolean with the content byte ${byte.toString} was found at byte $offset"

        case BadLength(offset, tag, length) =>
          m"the tag ${tag.toString} had the invalid length ${length.toString} at byte $offset"

        case BadUnusedBits(offset, count) =>
          m"a bit string declaring ${count.toString} unused bits was found at byte $offset"

        case NotPrimitive(offset, tag) =>
          m"the tag ${tag.toString} was encoded in constructed form at byte $offset"

        case NotConstructed(offset, tag) =>
          m"the tag ${tag.toString} was encoded in primitive form at byte $offset"

    enum Reason(val number: Int) extends Clarification:
      case Truncated(offset: Long) extends Reason(1)
      case IndefiniteLength(offset: Long) extends Reason(2)
      case NonMinimalLength(offset: Long) extends Reason(3)
      case NonMinimalTag(offset: Long) extends Reason(4)
      case NonMinimalInteger(offset: Long) extends Reason(5)
      case EmptyInteger(offset: Long) extends Reason(6)
      case Overflow(offset: Long) extends Reason(7)
      case Trailing(offset: Long) extends Reason(8)
      case InvalidUtf8(offset: Long) extends Reason(9)
      case ReservedTag(offset: Long) extends Reason(10)
      case BadOid(offset: Long) extends Reason(11)
      case OidArcOverflow(offset: Long) extends Reason(12)
      case UnsortedSet(offset: Long) extends Reason(13)
      case BadTime(offset: Long) extends Reason(14)
      case BadBoolean(offset: Long, byte: Int) extends Reason(15)
      case BadLength(offset: Long, tag: Int, length: Int) extends Reason(16)
      case BadUnusedBits(offset: Long, count: Int) extends Reason(17)
      case NotPrimitive(offset: Long, tag: Int) extends Reason(18)
      case NotConstructed(offset: Long, tag: Int) extends Reason(19)

  case class Error(reason: Asn1.Error.Reason)(using Diagnostics)
  extends fulminate.Error(523, reason.number)(m"could not process the ASN.1 value because $reason")

enum Asn1 derives CanEqual:
  case Boolean(value: scala.Boolean)
  case Integer(value: BigInt)
  case BitString(bytes: Data, unusedBits: Int)
  case OctetString(bytes: Data)
  case Null
  case ObjectId(arcs: List[Int])
  case Utf8String(text: Text)
  case PrintableString(text: Text)
  case Ia5String(text: Text)
  case UtcTime(timestamp: Long)
  case GeneralizedTime(timestamp: Long)
  case Sequence(elements: List[Asn1])
  case Set(elements: List[Asn1])
  case Tagged(tag: Int, explicit: scala.Boolean, value: Asn1)
  case Unknown(tagClass: Int, tag: Int, constructed: scala.Boolean, content: Data)
