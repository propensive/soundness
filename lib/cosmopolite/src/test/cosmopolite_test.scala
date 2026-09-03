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
package cosmopolite

import soundness.*
import soundness.collationComparable
import soundness.localeCollation
import soundness.sortingAlgorithms.timsort

object Tests extends Suite(m"Cosmopolite tests"):
  def run(): Unit =
    suite(m"Language sort orders"):
      test(m"English uses dictionary order: cafe < café < caff"):
        given Locale[en] = Locale(en)
        proscenium.List(t"caff", t"café", t"cafe").sort
      . assert(_ == proscenium.List(t"cafe", t"café", t"caff"))

      test(m"Polish ó sorts as a letter between o and p"):
        given Locale[pl] = Locale(pl)
        proscenium.List(t"pod", t"ó", t"oz").sort
      . assert(_ == proscenium.List(t"oz", t"ó", t"pod"))

      test(m"Polish ż chains after ź after z"):
        given Locale[pl] = Locale(pl)
        proscenium.List(t"ż", t"z", t"ź").sort
      . assert(_ == proscenium.List(t"z", t"ź", t"ż"))

      test(m"Polish uppercase is a tertiary difference: ó < Ó"):
        given Locale[pl] = Locale(pl)
        t"ó" < t"Ó"
      . assert(_ == true)

      test(m"Spanish ñ sorts as a letter between n and o"):
        given Locale[es] = Locale(es)
        proscenium.List(t"año", t"anzuelo", t"ano").sort
      . assert(_ == proscenium.List(t"ano", t"anzuelo", t"año"))

      test(m"English keeps ñ with n: año < anzuelo"):
        given Locale[en] = Locale(en)
        proscenium.List(t"año", t"anzuelo", t"ano").sort
      . assert(_ == proscenium.List(t"ano", t"año", t"anzuelo"))

      test(m"German umlauts differ at the secondary level"):
        given Locale[de] = Locale(de)
        (t"äb" > t"ab", t"äa" < t"ab")
      . assert(_ == (true, true))

      // Forward secondary accents: modern CLDR French. The reversed relative order of coté
      // and côte under traditional (now Canadian) French would need backward secondaries.
      test(m"French uses forward secondary accents: coté < côte"):
        given Locale[fr] = Locale(fr)
        proscenium.List(t"côté", t"coté", t"côte", t"cote").sort
      . assert(_ == proscenium.List(t"cote", t"coté", t"côte", t"côté"))

    suite(m"Locale integration"):
      test(m"a via-typed value sees its language's collation"):
        // `minimum` rather than sorting and taking the first: the smallest element is what the
        // test is after, and it needs no algorithm — nor a proof that the list is non-empty.
        def first: Text via pl = proscenium.List(t"ó", t"oz").minimum.or(t"")
        given Locale[pl] = Locale(pl)
        first
      . assert(_ == t"oz")
