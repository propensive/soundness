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
package denominative

import scala.annotation.targetName

import prepositional.*
import symbolism.*
import vacuous.*

object internal:
  opaque type Ordinal = Int
  opaque type Interval = Long

  // A `Span` packs a located range of characters into a single `Long`, tagged by
  // a 3-bit mode in the top bits. Lines, columns and offsets are 0-based and fit
  // an `Ordinal` (a 32-bit `Int`). Accessors return `Unset` for fields a mode does
  // not carry. The five modes (3 reserved for the future) are:
  //
  //   Empty  (0)  all-zero — absence, in-band (no `Optional[Span]` boxing needed)
  //   Offset (1)  offset(31), length(30)                       — line-less sources
  //   Line   (2)  line(23), column(19), length(19)             — single-line spans
  //   Lines  (3)  startLine(31), lineCount(30)                 — whole-line ranges
  //   Area   (4)  startLine(22), startColumn(14), lineDelta(11), endColumn(14)
  //                                  — multi-line with columns; endLine = start+delta
  opaque type Span = Long

  extension (ordinal: Ordinal)
    inline infix def span (right: Int): Interval = Interval(ordinal, ordinal + right - 1)

  // Every use of the inline receiver (and of an inline `right`) is widened by an erased cast,
  // a no-op on the `Int` representation: a caller in another compilation unit inlines these
  // onto a brand-refined receiver (`Ordinal in xs.type`, reached through the `Ordinal` export
  // alias), where a bare `ordinal + 1` fails to retype against the refinement ("expression
  // does not take parameters"), and a bare `ordinal` — or an ascribed `(ordinal: Int)` — keeps
  // the refined type when the enclosing lambda also applies a function value ("Found:
  // Ordinal in xs.type, Required: Int"). Only the cast survives both (verified for #1755).
  extension (inline ordinal: Ordinal)
    inline def le(inline right: Ordinal): Boolean =
      ordinal.asInstanceOf[Int] <= right.asInstanceOf[Int]

    inline def lt(inline right: Ordinal): Boolean =
      ordinal.asInstanceOf[Int] < right.asInstanceOf[Int]

    inline def ge(inline right: Ordinal): Boolean =
      ordinal.asInstanceOf[Int] >= right.asInstanceOf[Int]

    inline def gt(inline right: Ordinal): Boolean =
      ordinal.asInstanceOf[Int] > right.asInstanceOf[Int]

    inline def next: Ordinal = ordinal.asInstanceOf[Int] + 1
    inline def previous: Ordinal = (ordinal.asInstanceOf[Int] - 1).max(0)

    inline def n0: Int = ordinal.asInstanceOf[Int]
    inline def n1: Int = ordinal.asInstanceOf[Int] + 1

    inline def subsequent(size: Int): Interval =
      Interval(ordinal.asInstanceOf[Int] + 1, ordinal.asInstanceOf[Int] + size)

    inline def preceding(size: Int): Interval =
      Interval((ordinal.asInstanceOf[Int] - size).max(0), ordinal.asInstanceOf[Int] - 1)

    inline def within[collection: Countable](value: collection): Optional[Ordinal in value.type] =
      if ordinal.asInstanceOf[Int] >= 0 && ordinal.asInstanceOf[Int] < collection.size(value)
      then ordinal.asInstanceOf[Ordinal in value.type]
      else Unset

  object Ordinal:
    inline def zerary(inline cardinal: Int): Ordinal = cardinal
    inline def uniary(inline cardinal: Int): Ordinal = cardinal - 1

    given addable: Ordinal is Addable by Int to Ordinal = Addable(_ + _)
    given subtractable: Ordinal is Subtractable by Ordinal to Int = Subtractable(_ - _)
    given subtractable2: Ordinal is Subtractable by Int to Ordinal = Subtractable(_ - _)
    given comparable: Ordinal is Comparable = Comparable.int

    // The brand-generic twins: a typeclass's `Self` is exact, so `ordinal + 1` on an
    // `Ordinal in xs.type` finds none of the above. These live here because the companion of a
    // refinement's parent is in its implicit scope, so no import is needed. The results are
    // unbranded: arithmetic leaves the proven range.
    given brandedAddable: [form] => (Ordinal in form) is Addable by Int to Ordinal =
      Addable(_ + _)

    given brandedSubtractable: [form] => (Ordinal in form) is Subtractable by Ordinal to Int =
      Subtractable(_ - _)

    given brandedSubtractable2: [form] => (Ordinal in form) is Subtractable by Int to Ordinal =
      Subtractable(_ - _)

  // See `denominative.coextent.scala` for the producer and the rationale.
  opaque type Coextent[form1, form2] = Int

  object Coextent:
    private[denominative] def mint[form1, form2](size: Int): Coextent[form1, form2] = size

    // In the companion (reached through implicit scope) rather than at package level: `each` and
    // `size` are already package-level names in `denominative_core.scala`. The brands are type
    // parameters here, not refinements of the opaque, so nothing collapses.
    extension [form1, form2](coextent: Coextent[form1, form2])
      // Transfer the proof: an index valid for the first value is valid for the second.
      inline def apply(ordinal: Ordinal in form1): Ordinal in form2 =
        ordinal.asInstanceOf[Ordinal in form2]

      inline def invert: Coextent[form2, form1] = coextent
      inline def size: Int = coextent

      // Iterate the shared extent with the index proven against both values. A `while`, not
      // the interval `each`: inside `internal` an `Interval` is transparently a `Long`, which
      // is what the inlined call would carry to the caller (loops.md's third sanctioned shape).
      inline def each(inline lambda: (Ordinal in form1, Ordinal in form2) => Unit): Unit =
        var index: Int = 0
        val size: Int = coextent

        while index < size do
          lambda(index.asInstanceOf[Ordinal in form1], index.asInstanceOf[Ordinal in form2])
          index += 1

  extension (interval: Interval)
    inline def start: Ordinal = ((interval >> 32) & 0xffffffff).toInt

    inline def size: Int = (interval & 0xffffffff).toInt
    inline def end: Ordinal = start + size - 1
    inline def limit: Ordinal = start + size
    inline def contains(ordinal: Ordinal): Boolean = start <= ordinal && ordinal < limit
    inline def next: Ordinal = limit
    inline def previous: Ordinal = start - 1
    inline def subsequent(size: Int): Interval = end.subsequent(size)
    inline def preceding(size: Int): Interval = start.preceding(size)

    inline def fuse[value](inline initial: value)
      ( inline lambda: (value aka "state", Ordinal aka "next") ?=> value )
    :   value =

      var i: Ordinal = start
      var acc: value = initial

      while i <= end do
        acc = lambda(using acc.aka["state"], i.aka["next"])
        i = i.next

      acc


    inline def nil: Boolean = size == 0

  object Interval:
    inline def sized(inline start: Int, inline count: Int): Interval =
      if count <= 0 then 0L else (start.toLong & 0xffffffffL) << 32 | (count.toLong & 0xffffffffL)

    inline def initial(size: Int): Interval = sized(0, size)
    inline def apply(): Interval = 0L

    inline def zerary(inline start: Int, inline end: Int): Interval = sized(start, end - start)

    inline def apply(inline start: Ordinal, inline end: Ordinal): Interval =
      sized(start.n0, end.n0 - start.n0 + 1)

  extension (span: Span)
    inline def bits: Long = span
    inline def vacant: Boolean = (span >>> 61) == 0L
    inline def exists: Boolean = (span >>> 61) != 0L

    def mode: Span.Mode = ((span >>> 61) & 0x7L).toInt match
      case 1 => Span.Mode.Offset
      case 2 => Span.Mode.Line
      case 3 => Span.Mode.Lines
      case 4 => Span.Mode.Area
      case _ => Span.Mode.Empty

    def startLine: Optional[Ordinal] = mode match
      case Span.Mode.Line   => Ordinal.zerary(((span >>> 38) & 0x7fffffL).toInt)
      case Span.Mode.Lines  => Ordinal.zerary(((span >>> 30) & 0x7fffffffL).toInt)
      case Span.Mode.Area   => Ordinal.zerary(((span >>> 39) & 0x3fffffL).toInt)
      case _                => Unset

    def startColumn: Optional[Ordinal] = mode match
      case Span.Mode.Line   => Ordinal.zerary(((span >>> 19) & 0x7ffffL).toInt)
      case Span.Mode.Area   => Ordinal.zerary(((span >>> 25) & 0x3fffL).toInt)
      case _                => Unset

    def endLine: Optional[Ordinal] = mode match
      case Span.Mode.Line => Ordinal.zerary(((span >>> 38) & 0x7fffffL).toInt)

      case Span.Mode.Lines =>
        Ordinal.zerary((((span >>> 30) & 0x7fffffffL) + (span & 0x3fffffffL) - 1).toInt)

      case Span.Mode.Area =>
        Ordinal.zerary((((span >>> 39) & 0x3fffffL) + ((span >>> 14) & 0x7ffL)).toInt)

      case _ => Unset

    def endColumn: Optional[Ordinal] = mode match
      case Span.Mode.Line =>
        Ordinal.zerary(((span >>> 19) & 0x7ffffL).toInt + (span & 0x7ffffL).toInt)

      case Span.Mode.Area   => Ordinal.zerary((span & 0x3fffL).toInt)
      case _                => Unset

    def length: Optional[Int] = mode match
      case Span.Mode.Offset => (span & 0x3fffffffL).toInt
      case Span.Mode.Line   => (span & 0x7ffffL).toInt
      case _                => Unset

    def offset: Optional[Ordinal] = mode match
      case Span.Mode.Offset => Ordinal.zerary(((span >>> 30) & 0x7fffffffL).toInt)
      case _                => Unset

    def lineCount: Optional[Int] = mode match
      case Span.Mode.Line   => 1
      case Span.Mode.Lines  => (span & 0x3fffffffL).toInt
      case Span.Mode.Area   => ((span >>> 14) & 0x7ffL).toInt + 1
      case _                => Unset

    def singleLine: Boolean = mode match
      case Span.Mode.Line   => true
      case Span.Mode.Area   => ((span >>> 14) & 0x7ffL) == 0L
      case _                => false

  object Span:
    enum Mode derives CanEqual:
      case Empty, Offset, Line, Lines, Area

    given equality: CanEqual[Span, Span] = CanEqual.derived

    final val empty: Span = 0L

    inline def offset(start: Ordinal, length: Int): Span =
      (1L << 61) | ((start.n0.toLong & 0x7fffffffL) << 30) | (length.toLong & 0x3fffffffL)

    inline def line(line: Ordinal, column: Ordinal, length: Int): Span =
      (2L << 61) |
        ((line.n0.toLong & 0x7fffffL) << 38) |
        ((column.n0.toLong & 0x7ffffL) << 19) |
        (length.toLong & 0x7ffffL)

    inline def lines(startLine: Ordinal, lineCount: Int): Span =
      (3L << 61) | ((startLine.n0.toLong & 0x7fffffffL) << 30) | (lineCount.toLong & 0x3fffffffL)

    inline def area
      ( startLine: Ordinal, startColumn: Ordinal, endLine: Ordinal, endColumn: Ordinal )
    :   Span =

      val delta = (endLine.n0 - startLine.n0).max(0)

      (4L << 61) |
        ((startLine.n0.toLong & 0x3fffffL) << 39) |
        ((startColumn.n0.toLong & 0x3fffL) << 25) |
        ((delta.toLong & 0x7ffL) << 14) |
        (endColumn.n0.toLong & 0x3fffL)
