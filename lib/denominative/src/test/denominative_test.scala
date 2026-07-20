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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.collection.immutable.IndexedSeq

import soundness.*

object Tests extends Suite(m"Denominative Tests"):
  def run(): Unit =
    suite(m"Interval-construction tests"):
      test(m"subsequent yields an Interval of the requested size"):
        Prim.subsequent(3).size
      . assert(_ == 3)

      test(m"span yields an Interval of the requested size"):
        (Prim span 3).size
      . assert(_ == 3)

      test(m"preceding yields an Interval of the requested size"):
        Sept.preceding(3).size
      . assert(_ == 3)

      test(m"subsequent and span agree on size"):
        val skip = Prim.subsequent(3).size
        val full = (Prim span 3).size
        skip == full
      . assert(identity(_))

    suite(m"Interval-semantics tests"):
      test(m"span preserves the start ordinal"):
        (Sec span 3).start
      . assert(_ == Sec)

      test(m"span sets the inclusive end ordinal"):
        (Sec span 3).end
      . assert(_ == Quat)

      test(m"span sets the exclusive limit ordinal"):
        (Sec span 3).limit
      . assert(_ == Quin)

      test(m"interval contains an interior ordinal"):
        (Sec span 3).contains(Quat)
      . assert(identity(_))

      test(m"interval does not contain the limit ordinal"):
        (Sec span 3).contains(Quin)
      . assert(_ == false)

      test(m"each iterates exactly size times"):
        var count = 0
        (Sec span 3).each: _ =>
          count += 1
        count
      . assert(_ == 3)

    suite(m"Empty-interval tests"):
      test(m"an empty interval has zero size"):
        (Sec till Sec).size
      . assert(_ == 0)

      test(m"an empty interval is nil"):
        (Sec till Sec).nil
      . assert(identity(_))

      test(m"an empty interval contains no ordinals"):
        (Quat till Quat).contains(Quat)
      . assert(_ == false)

      test(m"empty intervals at different positions are equal"):
        (Sec till Sec) == (Sept till Sept)
      . assert(identity(_))

      test(m"the canonical empty interval equals a degenerate range"):
        Interval() == (Quat till Quat)
      . assert(identity(_))

    suite(m"Ordinal-conversion tests"):
      test(m"Prim is the zeroth zerary ordinal"):
        Prim.n0
      . assert(_ == 0)

      test(m"Prim is the first uniary ordinal"):
        Prim.n1
      . assert(_ == 1)

      test(m"Sept has zerary index six"):
        Sept.n0
      . assert(_ == 6)

      test(m"Sept has uniary index seven"):
        Sept.n1
      . assert(_ == 7)

      test(m"the zerary constructor agrees with the constant"):
        5.z
      . assert(_ == Sen)

      test(m"the zerary constructor round-trips through n0"):
        5.z.n0
      . assert(_ == 5)

      test(m"the uniary constructor counts from one"):
        5.u
      . assert(_ == Quin)

      test(m"the uniary constructor maps one to Prim"):
        1.u
      . assert(_ == Prim)

      test(m"the uniary constructor round-trips through n1"):
        7.u.n1
      . assert(_ == 7)

      test(m"Ordinal.zerary matches the named constant"):
        Ordinal.zerary(3)
      . assert(_ == Quat)

      test(m"Ordinal.uniary maps one to Prim"):
        Ordinal.uniary(1)
      . assert(_ == Prim)

    suite(m"Ordinal-arithmetic tests"):
      test(m"a cardinal can be added to an ordinal"):
        Prim + 3
      . assert(_ == Quat)

      test(m"adding a cardinal advances the ordinal"):
        Sec + 4
      . assert(_ == Sen)

      test(m"subtracting two ordinals yields a cardinal"):
        Sept - Quat
      . assert(_ == 3)

      test(m"subtracting a cardinal from an ordinal yields an ordinal"):
        Sept - 2
      . assert(_ == Quin)

    suite(m"Ordinal-comparison tests"):
      test(m"an earlier ordinal is less than a later one"):
        Sec.lt(Ter)
      . assert(identity(_))

      test(m"a later ordinal is not less than an earlier one"):
        Ter.lt(Sec)
      . assert(_ == false)

      test(m"an ordinal is less-than-or-equal to itself"):
        Sec.le(Sec)
      . assert(identity(_))

      test(m"a later ordinal is greater than an earlier one"):
        Ter.gt(Sec)
      . assert(identity(_))

      test(m"an earlier ordinal is not greater-or-equal to a later one"):
        Sec.ge(Ter)
      . assert(_ == false)

    suite(m"Ordinal-navigation tests"):
      test(m"the ordinal after Prim is Sec"):
        Prim.next
      . assert(_ == Sec)

      test(m"the ordinal before Sec is Prim"):
        Sec.previous
      . assert(_ == Prim)

      test(m"the ordinal before Prim is clamped to Prim"):
        Prim.previous
      . assert(_ == Prim)

    suite(m"Interval-factory tests"):
      test(m"thru includes its final ordinal"):
        (Sec thru Quat).size
      . assert(_ == 3)

      test(m"thru sets the inclusive end ordinal"):
        (Sec thru Quat).end
      . assert(_ == Quat)

      test(m"till excludes its final ordinal"):
        (Sec till Quat).size
      . assert(_ == 2)

      test(m"till sets the inclusive end below its argument"):
        (Sec till Quat).end
      . assert(_ == Ter)

      test(m"initial starts at Prim"):
        Interval.initial(4).start
      . assert(_ == Prim)

      test(m"initial has the requested size"):
        Interval.initial(4).size
      . assert(_ == 4)

      test(m"a zero-sized initial interval is empty"):
        Interval.initial(0).nil
      . assert(identity(_))

      test(m"zerary starts at its first argument"):
        Interval.zerary(2, 5).start
      . assert(_ == Ter)

      test(m"zerary's size is the difference of its arguments"):
        Interval.zerary(2, 5).size
      . assert(_ == 3)

      test(m"zerary's exclusive limit is its second argument"):
        Interval.zerary(2, 5).limit
      . assert(_ == 5.z)

      test(m"subsequent starts after the ordinal"):
        Prim.subsequent(3).start
      . assert(_ == Sec)

      test(m"preceding starts a cardinal before the ordinal"):
        Sept.preceding(3).start
      . assert(_ == Quat)

      test(m"preceding ends just before the ordinal"):
        Sept.preceding(3).end
      . assert(_ == Sen)

    suite(m"Interval-navigation tests"):
      test(m"the ordinal after an interval is its limit"):
        (Sec span 3).next
      . assert(_ == Quin)

      test(m"the ordinal before an interval is just before its start"):
        (Sec span 3).previous
      . assert(_ == Prim)

      test(m"subsequent yields the following interval"):
        (Quin span 2).subsequent(2).start
      . assert(_ == Sept)

      test(m"the preceding interval ends just before the start"):
        (Quin span 2).preceding(2).start
      . assert(_ == Ter)

      test(m"the interval contains its starting ordinal"):
        (Sec span 3).contains(Sec)
      . assert(identity(_))

      test(m"the interval does not contain an ordinal before its start"):
        (Sec span 3).contains(Prim)
      . assert(_ == false)

    suite(m"Interval-fold tests"):
      test(m"each visits every ordinal in order"):
        var total = 0
        (Sec span 3).each: ordinal =>
          total += ordinal.n0
        total
      . assert(_ == 6)

      test(m"each over an empty interval does nothing"):
        var count = 0
        (Sec till Sec).each: _ =>
          count += 1
        count
      . assert(_ == 0)

      test(m"fuse counts the ordinals in an interval"):
        (Sec span 3).fuse(0)(state + 1)
      . assert(_ == 3)

      test(m"fuse accumulates over every ordinal"):
        (Sec span 3).fuse(0)(state + next.n0)
      . assert(_ == 6)

    suite(m"Countable tests"):
      // `List`'s `Countable` (hence `gamut`) is O(n), so it is gated behind `LinearSizeComplexity`,
      // enabled here for the whole suite by importing the acknowledgement.
      import asymptotics.linearSizeComplexity

      test(m"a list's gamut spans all its elements"):
        val list = List(1, 2, 3)
        list.gamut.size
      . assert(_ == 3)

      test(m"a list's gamut starts at Prim"):
        val list = List(1, 2, 3)
        list.gamut.start
      . assert(_ == Prim)

      test(m"a list's gamut ends at its last ordinal"):
        val list = List(1, 2, 3)
        list.gamut.end
      . assert(_ == Ter)

      // `nil` is O(1) and ungated (from `Populable.list`), so it needs no acknowledgement.
      test(m"an empty list is nil"):
        val list = List[Int]()
        list.nil
      . assert(identity(_))

      test(m"a non-empty list is not nil"):
        val list = List(1)
        list.nil
      . assert(_ == false)

      test(m"a set's gamut spans all its elements"):
        // Bound to a `val` first: the inline `gamut` proxy otherwise dealiases the opaque `Set`.
        val set = Set(1, 2, 3, 4)
        set.gamut.size
      . assert(_ == 4)

      test(m"an indexed sequence's gamut spans all its elements"):
        val sequence: IndexedSeq[Int] = Series(1, 2)
        sequence.gamut.size
      . assert(_ == 2)

      test(m"an immutable array's gamut spans all its elements"):
        IArray(1, 2, 3).gamut.size
      . assert(_ == 3)

      test(m"a present option has a gamut of size one"):
        Option(1).gamut.size
      . assert(_ == 1)

      test(m"an absent option is nil"):
        Option.empty[Int].nil
      . assert(identity(_))

      test(m"text's gamut spans its characters"):
        t"hello".gamut.size
      . assert(_ == 5)

      test(m"empty text is nil"):
        t"".nil
      . assert(identity(_))

      test(m"an integer counts itself"):
        5.gamut.size
      . assert(_ == 5)

    suite(m"Ordinal-showable tests"):
      test(m"nominal names a known ordinal"):
        import ordinalTextualizables.nominalOrdinal
        Quat.textual
      . assert(_ == t"quat")

      test(m"nominal falls back for an unnamed ordinal"):
        import ordinalTextualizables.nominalOrdinal
        7.z.textual
      . assert(_ == t"7.z")

      test(m"the uniary showable counts from one"):
        import ordinalTextualizables.uniaryOrdinal
        Prim.textual
      . assert(_ == t"1♭")

      test(m"the zerary showable counts from zero"):
        import ordinalTextualizables.zeraryOrdinal
        Prim.textual
      . assert(_ == t"0♯")

      test(m"the unmarked uniary showable counts from one"):
        import ordinalTextualizables.unmarkedUniaryOrdinal
        Sec.textual
      . assert(_ == t"2")

      test(m"the unmarked zerary showable counts from zero"):
        import ordinalTextualizables.unmarkedZeraryOrdinal
        Sec.textual
      . assert(_ == t"1")

      test(m"the intermediate showable shows both indices"):
        import ordinalTextualizables.intermediateOrdinal
        Prim.textual
      . assert(_ == t"⌞0⌟|⌞1⌟")

      test(m"english renders the first ordinal"):
        import ordinalTextualizables.englishOrdinal
        Prim.textual
      . assert(_ == t"1st")

      test(m"english renders the second ordinal"):
        import ordinalTextualizables.englishOrdinal
        Sec.textual
      . assert(_ == t"2nd")

      test(m"english renders the third ordinal"):
        import ordinalTextualizables.englishOrdinal
        Ter.textual
      . assert(_ == t"3rd")

      test(m"english renders a regular ordinal"):
        import ordinalTextualizables.englishOrdinal
        Quat.textual
      . assert(_ == t"4th")

      test(m"english renders the eleventh ordinal"):
        import ordinalTextualizables.englishOrdinal
        10.z.textual
      . assert(_ == t"11th")

      test(m"english renders the twenty-first ordinal"):
        import ordinalTextualizables.englishOrdinal
        20.z.textual
      . assert(_ == t"21st")

      test(m"english renders the thirteenth ordinal"):
        import ordinalTextualizables.englishOrdinal
        12.z.textual
      . assert(_ == t"13th")

      test(m"english superscript renders the first ordinal"):
        import ordinalTextualizables.englishSuperscriptOrdinal
        Prim.textual
      . assert(_ == t"1ˢᵗ")

      test(m"english superscript renders the second ordinal"):
        import ordinalTextualizables.englishSuperscriptOrdinal
        Sec.textual
      . assert(_ == t"2ⁿᵈ")

      test(m"french renders the first ordinal"):
        import ordinalTextualizables.frenchOrdinal
        Prim.textual
      . assert(_ == t"1ᵉʳ")

      test(m"french renders the second ordinal"):
        import ordinalTextualizables.frenchOrdinal
        Sec.textual
      . assert(_ == t"2ᵉ")

      test(m"italian renders the first ordinal"):
        import ordinalTextualizables.italianOrdinal
        Prim.textual
      . assert(_ == t"1ᵒ")

      test(m"spanish renders the second ordinal"):
        import ordinalTextualizables.spanishOrdinal
        Sec.textual
      . assert(_ == t"2.ᵒ")

      test(m"russian renders the second ordinal"):
        import ordinalTextualizables.russianOrdinal
        Sec.textual
      . assert(_ == t"2-й")

    suite(m"Span offset-mode tests"):
      val span = Span.offset(10.z, 5)

      test(m"offset mode reports Offset"):
        span.mode
      . assert(_ == Span.Mode.Offset)

      test(m"offset round-trips the start offset"):
        span.offset.vouch
      . assert(_ == 10.z)

      test(m"offset round-trips the length"):
        span.length.vouch
      . assert(_ == 5)

      test(m"an offset span has no line"):
        span.startLine
      . assert(_ == Unset)

    suite(m"Span line-mode tests"):
      val span = Span.line(3.z, 7.z, 4)

      test(m"line mode reports Line"):
        span.mode
      . assert(_ == Span.Mode.Line)

      test(m"line round-trips the line"):
        span.startLine.vouch
      . assert(_ == 3.z)

      test(m"line round-trips the column"):
        span.startColumn.vouch
      . assert(_ == 7.z)

      test(m"line round-trips the length"):
        span.length.vouch
      . assert(_ == 4)

      test(m"line end column is start column plus length"):
        span.endColumn.vouch
      . assert(_ == 11.z)

      test(m"a line span is single-line"):
        span.singleLine
      . assert(identity(_))

      test(m"a line span spans one line"):
        span.lineCount.vouch
      . assert(_ == 1)

    suite(m"Span lines-mode tests"):
      val span = Span.lines(5.z, 3)

      test(m"lines mode reports Lines"):
        span.mode
      . assert(_ == Span.Mode.Lines)

      test(m"lines round-trips the start line"):
        span.startLine.vouch
      . assert(_ == 5.z)

      test(m"lines reports the inclusive end line"):
        span.endLine.vouch
      . assert(_ == 7.z)

      test(m"lines round-trips the line count"):
        span.lineCount.vouch
      . assert(_ == 3)

      test(m"a lines span has no columns"):
        span.startColumn
      . assert(_ == Unset)

    suite(m"Span region-mode tests"):
      val span = Span.region(2.z, 4.z, 6.z, 9.z)

      test(m"region mode reports Region"):
        span.mode
      . assert(_ == Span.Mode.Region)

      test(m"region round-trips the start line"):
        span.startLine.vouch
      . assert(_ == 2.z)

      test(m"region round-trips the start column"):
        span.startColumn.vouch
      . assert(_ == 4.z)

      test(m"region round-trips the end line"):
        span.endLine.vouch
      . assert(_ == 6.z)

      test(m"region round-trips the end column"):
        span.endColumn.vouch
      . assert(_ == 9.z)

      test(m"a multi-line region is not single-line"):
        span.singleLine
      . assert(_ == false)

      test(m"region line count is inclusive"):
        Span.region(10.z, 0.z, 13.z, 0.z).lineCount.vouch
      . assert(_ == 4)

    suite(m"Span empty-mode tests"):
      test(m"the empty span is vacant"):
        Span.empty.vacant
      . assert(identity(_))

      test(m"the empty span does not exist"):
        Span.empty.exists
      . assert(_ == false)

      test(m"the empty span reports Empty mode"):
        Span.empty.mode
      . assert(_ == Span.Mode.Empty)

      test(m"a zero-valued line span still exists"):
        Span.line(Prim, Prim, 0).exists
      . assert(identity(_))

      test(m"a large line number round-trips in line mode"):
        Span.line(1000000.z, Prim, 0).startLine.vouch
      . assert(_ == 1000000.z)
