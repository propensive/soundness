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

import scala.collection.immutable.Seq

import soundness.*

import hyphenations.englishHyphenation

object Tests extends Suite(m"Polysyllabic tests"):
  def run(): Unit =
    suite(m"Pattern parser"):
      test(m"plain pattern: hy3ph decomposes to (hyph, [0,0,3,0,0])"):
        val (key, scores) = TexPatterns.parsePattern(t"hy3ph")
        (key, scores.stdlib.to(Seq))

      . assert(_ == ((t"hyph", Seq[Byte](0, 0, 3, 0, 0))))

      test(m"leading boundary pattern: .ach4 decomposes to (.ach, [0,0,0,0,4])"):
        val (key, scores) = TexPatterns.parsePattern(t".ach4")
        (key, scores.stdlib.to(Seq))

      . assert(_ == ((t".ach", Seq[Byte](0, 0, 0, 0, 4))))

      test(m"trailing boundary pattern: ion5. decomposes to (ion., [0,0,0,5,0])"):
        val (key, scores) = TexPatterns.parsePattern(t"ion5.")
        (key, scores.stdlib.to(Seq))

      . assert(_ == ((t"ion.", Seq[Byte](0, 0, 0, 5, 0))))

      test(m"interleaved digits: a1b2c3 decomposes to (abc, [0,1,2,3])"):
        val (key, scores) = TexPatterns.parsePattern(t"a1b2c3")
        (key, scores.stdlib.to(Seq))

      . assert(_ == ((t"abc", Seq[Byte](0, 1, 2, 3))))

      test(m"pure-letter pattern: abc decomposes to (abc, [0,0,0,0])"):
        val (key, scores) = TexPatterns.parsePattern(t"abc")
        (key, scores.stdlib.to(Seq))

      . assert(_ == ((t"abc", Seq[Byte](0, 0, 0, 0))))

    suite(m"Exception parser"):
      test(m"as-so-ciate decomposes to (associate, [2, 4])"):
        val (word, offsets) = TexPatterns.parseException(t"as-so-ciate")
        (word, offsets.to(Seq))

      . assert(_ == ((t"associate", Seq(2, 4))))

      test(m"single break: ta-ble decomposes to (table, [2])"):
        val (word, offsets) = TexPatterns.parseException(t"ta-ble")
        (word, offsets.to(Seq))

      . assert(_ == ((t"table", Seq(2))))

      test(m"no breaks: present decomposes to (present, [])"):
        val (word, offsets) = TexPatterns.parseException(t"present")
        (word, offsets.to(Seq))

      . assert(_ == ((t"present", Seq())))

    suite(m"Liang algorithm with English patterns"):
      test(m"hyphenation breaks as hy-phen-ation"):
        t"hyphenation".hyphenate(hyphen = '-')

      . assert(_ == t"hy-phen-ation")

      test(m"algorithm breaks as al-go-rithm"):
        t"algorithm".hyphenate(hyphen = '-')

      . assert(_ == t"al-go-rithm")

      test(m"computer breaks as com-puter"):
        t"computer".hyphenate(hyphen = '-')

      . assert(_ == t"com-puter")

      test(m"presentation breaks as pre-sen-ta-tion"):
        t"presentation".hyphenate(hyphen = '-')

      . assert(_ == t"pre-sen-ta-tion")

      test(m"supercalifragilisticexpialidocious is broken into many syllables"):
        t"supercalifragilisticexpialidocious".hyphenate(hyphen = '-')

      . assert(_.s.split('-').nn.length >= 10)

    suite(m"Exception list"):
      test(m"associate uses the exception entry as-so-ci-ate"):
        t"associate".hyphenate(hyphen = '-')

      . assert(_ == t"as-so-ciate")

      test(m"table uses the exception entry ta-ble"):
        t"table".hyphenate(hyphen = '-')

      . assert(_ == t"ta-ble")

    suite(m"leftMin / rightMin"):
      test(m"leftMin = 4 suppresses early breaks in hyphenation"):
        t"hyphenation".hyphenate(hyphen = '-', leftMin = 4)

      . assert(_ == t"hyphen-ation")

      test(m"rightMin = 6 suppresses late breaks in hyphenation"):
        t"hyphenation".hyphenate(hyphen = '-', rightMin = 6)

      . assert(_ == t"hy-phenation")

    suite(m"Soft-hyphen default"):
      test(m"default hyphen character is U+00AD"):
        val out = t"hyphenation".hyphenate()
        out.s.indexOf('­')

      . assert(_ >= 0)

    suite(m"Word splitting"):
      test(m"non-letter regions pass through unchanged"):
        t"hello, world!".hyphenate(hyphen = '-')

      . assert(_ == t"hello, world!")

      test(m"each letter run is hyphenated independently"):
        t"the algorithm runs".hyphenate(hyphen = '-')

      . assert(_ == t"the al-go-rithm runs")

      test(m"syllables breaks letter runs and keeps non-letters as segments"):
        t"the algorithm".syllables.to(Seq)

      . assert(_ == Seq(t"the", t" ", t"al", t"go", t"rithm"))

    suite(m"Extending an existing Hyphenation"):
      test(m"user-supplied pattern overrides English with higher odd score"):
        // English contains `4nop`, which forces an even (no-break) score 4 at
        // the gap before `nop`. A user-supplied pattern with an odd score of
        // 9 wins via the max-merge and reinstates the break.
        val extended = englishHyphenation.extending(patterns = Seq(t"klm9nop"))

        given Hyphenation = extended
        t"klmnop".hyphenate(hyphen = '-')

      . assert(_ == t"klm-nop")

      test(m"user-supplied exception overrides default behaviour"):
        val extended = englishHyphenation.extending(exceptions = Seq(t"hy-phenation"))

        given Hyphenation = extended
        t"hyphenation".hyphenate(hyphen = '-')

      . assert(_ == t"hy-phenation")

    suite(m"Fallback / Unhyphenated"):
      test(m"with no language given, words have no break points"):
        import polysyllabic.Hyphenation.fallback
        t"hyphenation".breakPoints.length

      . assert(_ == 0)

    suite(m"User-built Hyphenation"):
      test(m"Hyphenation.apply from raw TeX patterns produces the same behaviour"):
        val tiny = Hyphenation(patterns = Seq(t"hy3ph", t"he2n", t"hena4", t"hen5at"))

        given Hyphenation = tiny
        t"hyphenation".breakPoints.to(Seq)

      . assert(breaks => breaks.nonEmpty)
