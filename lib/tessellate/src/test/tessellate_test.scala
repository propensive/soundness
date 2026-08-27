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

import scala.collection.immutable.List as SList

import soundness.*

import hyphenations.englishHyphenation
import textMetrics.wideCharacterWidthMetric

object Tests extends Suite(m"Tessellate tests"):
  def run(): Unit =
    suite(m"Flex.solve"):
      def widths(tracks: Flex*)(available: Int, gap: Int = 0): SList[Optional[Int]] =
        Flex.solve(Sequence.from(tracks.toVector), available, gap).stdlib.to(SList)

      test(m"equal weights split space equally"):
        widths(Flex(Metrics(0)), Flex(Metrics(0)), Flex(Metrics(0)))(9)

      . assert(_ == SList(3, 3, 3))

      test(m"largest-remainder rounding sums exactly to the available space"):
        widths(Flex(Metrics(0)), Flex(Metrics(0)), Flex(Metrics(0)))(10)

      . assert(_.map(_.or(0)).sum == 10)

      test(m"fractional weights apportion proportionally"):
        widths(Flex(Metrics(0), 0.5), Flex(Metrics(0), 0.25), Flex(Metrics(0), 0.25))(100)

      . assert(_ == SList(50, 25, 25))

      test(m"a track pinned at its minimum takes from the others"):
        widths(Flex(Metrics(6, 10)), Flex(Metrics(0, 10)))(8)

      . assert(_ == SList(6, 2))

      test(m"a track capped at its max releases space to the others"):
        widths(Flex(Metrics(0), max = 2), Flex(Metrics(0)))(10)

      . assert(_ == SList(2, 8))

      test(m"gaps are deducted from the distributable pool"):
        widths(Flex(Metrics(0)), Flex(Metrics(0)))(10, gap = 2)

      . assert(_ == SList(4, 4))

      test(m"a collapsible track drops when minima cannot fit"):
        widths(Flex(Metrics(5)), Flex(Metrics(5), collapsible = true))(6)

      . assert(_ == SList[Optional[Int]](6, Unset))

      test(m"the lowest-rank collapsible track drops first"):
        widths
          ( Flex(Metrics(5), rank = 1, collapsible = true),
            Flex(Metrics(5), rank = 0, collapsible = true) )
          ( 6 )

      . assert(_ == SList[Optional[Int]](6, Unset))

      test(m"uncollapsible minima overflow rather than drop"):
        widths(Flex(Metrics(5)), Flex(Metrics(5)))(6)

      . assert(_ == SList(5, 5))

      test(m"content tracks reach their natural widths when space allows"):
        widths(Flex.content(Metrics(3, 9)), Flex.content(Metrics(2, 4)))(20)

      . assert(_ == SList(9, 4))

      test(m"content tracks share scarce space in proportion to their stretch"):
        widths(Flex.content(Metrics(3, 9)), Flex.content(Metrics(2, 4)))(9)

      . assert(_ == SList(7, 2))

    suite(m"Flow.wrap"):
      def wrapped(content: Text, width: Int): SList[Text] =
        Flow.wrap(content, width).stdlib.to(SList)

      test(m"a paragraph wraps at spaces"):
        wrapped(t"the quick brown fox", 10)

      . assert(_ == SList(t"the quick", t"brown fox"))

      test(m"a hard line break always forces a new line"):
        wrapped(t"one\ntwo", 10)

      . assert(_ == SList(t"one", t"two"))

      test(m"consecutive hard breaks preserve the empty line"):
        wrapped(t"one\n\ntwo", 10)

      . assert(_ == SList(t"one", t"", t"two"))

      test(m"an overflowing word breaks at its latest fitting hyphenation point"):
        wrapped(t"hyphenation", 7)

      . assert(_ == SList(t"hyphen-", t"ation"))

      // Regression check for #1788: a pre-tessellate implementation dropped the character
      // before every hyphenation break (`artifact` became `art-`/`fact`), silently, at any
      // width. Reassembling the wrapped lines — stripping the inserted hyphens and the spaces
      // consumed at soft breaks — must reproduce the content's characters exactly.
      test(m"hyphenated wrapping preserves every character at every width"):
        val content = t"the realm to atomize a bare artifact in a derivative membership"
        val expected = content.s.replace(" ", "")

        (5 to 20).toList.map: width =>
          wrapped(content, width).map(_.s.stripSuffix("-")).mkString.replace(" ", "")
        . forall(_ == expected)

      . assert(_ == true)

      test(m"wide characters wrap by display width, not char count"):
        wrapped(t"日本語 テスト", 6)

      . assert(_ == SList(t"日本語", t"テスト"))

      test(m"an unbreakable over-long word runs on beyond the width"):
        import polysyllabic.Hyphenation.fallback
        Flow.wrap(t"abcdefghij", 4).stdlib.to(SList)

      . assert(_ == SList(t"abcdefghij"))

    suite(m"Flow.fit"):
      test(m"short content pads to the right under Left alignment"):
        Flow.fit(t"abc", 6)

      . assert(_ == t"abc   ")

      test(m"short content pads to the left under Right alignment"):
        Flow.fit(t"abc", 6, Alignment.Right)

      . assert(_ == t"   abc")

      test(m"short content centers under Center alignment"):
        Flow.fit(t"ab", 6, Alignment.Center)

      . assert(_ == t"  ab  ")

      test(m"Justify stretches word gaps to fill the width"):
        Alignment.Justify.pad(t"a b c", 9, last = false)

      . assert(_ == t"a   b   c")

      test(m"over-wide content truncates with an ellipsis"):
        Flow.fit(t"hello world", 8)

      . assert(_ == t"hello w…")

      test(m"truncation never cuts a wide character in half"):
        Flow.fit(t"日本語abc", 5)

      . assert(_ == t"日本…")

    suite(m"Intrinsic metrics"):
      test(m"minContent is the widest word; natural is the widest line"):
        Flow.metrics(t"the quick brown\nfox")

      . assert(_ == Metrics(5, 15))

      test(m"a ZWJ emoji family counts as one wide cluster"):
        Flow.metrics(t"👨‍👩‍👧 ab")

      . assert(_ == Metrics(2, 5))

    suite(m"Reflowable"):
      test(m"a Reflowable instance is summonable for Text"):
        summon[Text is Reflowable].metrics(t"one two")

      . assert(_ == Metrics(3, 7))

      test(m"flowed lines are padded to exactly the given width"):
        summon[Text is Reflowable].flow(t"the quick brown fox", 10, Alignment.Left)
          . stdlib.to(SList)

      . assert(_ == SList(t"the quick ", t"brown fox "))
