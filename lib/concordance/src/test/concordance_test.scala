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

// The sources under test keep `package rudiments` (see the note in `build.mill`), but the build
// derives the suite's main class from the module directory, so the suite itself is
// `concordance.Tests`.
package concordance

import soundness.*

import proscenium.{List, Set, Map, Sequence}

// The grouping combinators are top-level in `package rudiments` and so are not re-exported
// through the umbrella; they need a by-name import.
import rudiments.{pairs, triples, quads, adjacent}

// Two separate cost gates, both needed to group a `List`: `linearSize` admits the O(n) size,
// and `linearAccess` the O(n) indexed read that the combinators do once per element. An indexed
// receiver such as `Sequence` needs neither.
import denominative.dysasymptotics.{linearSize, linearAccess}

object Tests extends Suite(m"Concordance tests"):
  def run(): Unit =
    suite(m"Bijection"):
      val bijection = Bijection(scala.collection.immutable.Map(1 -> t"one", 2 -> t"two"))

      test(m"a bijection looks up by key"):
        bijection.get(1)
      . assert(_ == Some(t"one"))

      test(m"a bijection has no value for an absent key"):
        bijection.get(9)
      . assert(_ == None)

      test(m"flipping exchanges keys and values"):
        bijection.flip.get(t"one")
      . assert(_ == Some(1))

      test(m"flipping twice restores the original"):
        bijection.flip.flip.get(1)
      . assert(_ == Some(t"one"))

      test(m"a flipped bijection returns the same object when flipped back"):
        bijection.flip.flip.map
      . assert(_ == bijection.map)

      test(m"removing a key removes its transposition too"):
        bijection.-(1).flip.get(t"one")
      . assert(_ == None)

      test(m"removing a key leaves the others"):
        bijection.-(1).get(2)
      . assert(_ == Some(t"two"))

      test(m"a bijection iterates as pairs"):
        bijection.iterator.toSet
      . assert(_ == scala.collection.immutable.Set((1, t"one"), (2, t"two")))

      test(m"a stdlib map converts to a bijection"):
        scala.collection.immutable.Map(1 -> t"a").bijection.flip.get(t"a")
      . assert(_ == Some(1))

      test(m"a bijection is applicable by key"):
        bijection.defines(2)
      . assert(_ == true)

      test(m"a bijection does not define an absent key"):
        bijection.defines(9)
      . assert(_ == false)

    suite(m"prim, sec and ter"):
      test(m"prim reads the first element"):
        List(t"a", t"b", t"c").prim
      . assert(_ == t"a")

      test(m"sec reads the second element"):
        List(t"a", t"b", t"c").sec
      . assert(_ == t"b")

      test(m"ter reads the third element"):
        List(t"a", t"b", t"c").ter
      . assert(_ == t"c")

      test(m"prim is Unset for an empty list"):
        List.empty[Text].prim
      . assert(_ == Unset)

      test(m"sec is Unset for a one-element list"):
        List(t"a").sec
      . assert(_ == Unset)

      test(m"ter is Unset for a two-element list"):
        List(t"a", t"b").ter
      . assert(_ == Unset)

    suite(m"Grouping"):
      test(m"pairs visits whole disjoint pairs"):
        val visited = scala.collection.mutable.ListBuffer[(Int, Int)]()
        List(1, 2, 3, 4).pairs { (a, b) => visited += ((a, b)) }
        visited.toList
      . assert(_ == scala.collection.immutable.List((1, 2), (3, 4)))

      test(m"pairs leaves an odd trailing element out"):
        val visited = scala.collection.mutable.ListBuffer[(Int, Int)]()
        List(1, 2, 3).pairs { (a, b) => visited += ((a, b)) }
        visited.toList
      . assert(_ == scala.collection.immutable.List((1, 2)))

      test(m"pairs visits nothing when there is no whole pair"):
        val visited = scala.collection.mutable.ListBuffer[(Int, Int)]()
        List(1).pairs { (a, b) => visited += ((a, b)) }
        visited.toList
      . assert(_ == scala.collection.immutable.List())

      test(m"triples visits whole disjoint triples"):
        val visited = scala.collection.mutable.ListBuffer[(Int, Int, Int)]()
        List(1, 2, 3, 4, 5, 6, 7).triples { (a, b, c) => visited += ((a, b, c)) }
        visited.toList
      . assert(_ == scala.collection.immutable.List((1, 2, 3), (4, 5, 6)))

      test(m"quads visits whole disjoint groups of four"):
        val visited = scala.collection.mutable.ListBuffer[Int]()
        List(1, 2, 3, 4, 5).quads { (a, b, c, d) => visited += a + b + c + d }
        visited.toList
      . assert(_ == scala.collection.immutable.List(10))

      // `adjacent` is the overlapping window, not a disjoint grouping: four elements give three
      // pairs, where `pairs` gives two.
      test(m"adjacent visits every overlapping pair"):
        val visited = scala.collection.mutable.ListBuffer[(Int, Int)]()
        List(1, 2, 3, 4).adjacent { (a, b) => visited += ((a, b)) }
        visited.toList
      . assert(_ == scala.collection.immutable.List((1, 2), (2, 3), (3, 4)))

      test(m"adjacent visits nothing for a single element"):
        val visited = scala.collection.mutable.ListBuffer[(Int, Int)]()
        List(1).adjacent { (a, b) => visited += ((a, b)) }
        visited.toList
      . assert(_ == scala.collection.immutable.List())

      test(m"adjacent visits nothing for an empty collection"):
        val visited = scala.collection.mutable.ListBuffer[(Int, Int)]()
        List.empty[Int].adjacent { (a, b) => visited += ((a, b)) }
        visited.toList
      . assert(_ == scala.collection.immutable.List())

      // A literal constructor brands its result `List[Int] & Populated`, and these combinators
      // reach it only because `Countable.list` and `Applicable.list` are polymorphic in the
      // subtype rather than pinned to `List[element]`; a typeclass's `Self` is invariant, so a
      // fixed `Self` would make every one of the tests above unreachable from a literal.
      test(m"a literal-branded receiver is countable"):
        demilitarize:
          summon[(List[Int] & Populated) is Countable]
      . assert(_ == Nil)

      test(m"a literal-branded receiver is applicable by ordinal"):
        demilitarize:
          summon[(List[Int] & Populated) is Applicable by Ordinal]
      . assert(_ == Nil)

      // `Sequence` is the natural receiver for these combinators — indexing is O(1), so neither
      // cost gate is needed — and its literal constructors brand their result too.
      test(m"a branded sequence is countable"):
        demilitarize:
          summon[(Sequence[Int] & Populated) is Countable]
      . assert(_ == Nil)

      test(m"a branded sequence is applicable by ordinal"):
        demilitarize:
          summon[(Sequence[Int] & Populated) is Applicable by Ordinal]
      . assert(_ == Nil)

      test(m"pairs visits an indexed sequence without a cost gate"):
        val visited = scala.collection.mutable.ListBuffer[(Int, Int)]()
        Sequence(1, 2, 3, 4).pairs { (a, b) => visited += ((a, b)) }
        visited.toList
      . assert(_ == scala.collection.immutable.List((1, 2), (3, 4)))

      test(m"adjacent visits an indexed sequence"):
        val visited = scala.collection.mutable.ListBuffer[(Int, Int)]()
        Sequence(1, 2, 3).adjacent { (a, b) => visited += ((a, b)) }
        visited.toList
      . assert(_ == scala.collection.immutable.List((1, 2), (2, 3)))

