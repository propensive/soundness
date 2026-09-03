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
package proscenium

import soundness.*

object Tests extends Suite(m"Proscenium Tests"):
  def run(): Unit =
    // Everything the opaque types hide from their users is transparent inside this package, so
    // the consumer's view is exercised from a subpackage instead.
    prosceniumtest.Tests()

    // Laziness probes for the opaque `Chain` (Phase 8 acceptance guards): the cons keeps
    // its tail unforced, pattern-matching forces only the head, and `take`/`continually` never
    // force beyond the demanded prefix (an eager implementation would throw or diverge here).
    test(m"Chain cons does not force its tail"):
      var forced = 0
      def rest: Chain[Int] = { forced += 1; Chain() }
      val stream: Chain[Int] = 1 #:: rest
      forced

    . assert(_ == 0)

    test(m"Chain pattern match forces only the head"):
      var forced = 0
      def rest: Chain[Int] = { forced += 1; Chain() }
      val stream: Chain[Int] = 42 #:: rest
      val head = stream match { case head #:: _ => head }
      (head, forced)

    . assert(_ == (42, 0))

    test(m"take on an infinite stream does not diverge"):
      Chain.continually(7).take(3).toList

    . assert(_ == List(7, 7, 7))

    test(m"#::: keeps its lazy suffix unforced"):
      var forced = 0
      def suffix: Chain[Int] = { forced += 1; Chain(9) }
      val stream: Chain[Int] = Chain(1, 2) #::: suffix
      forced

    . assert(_ == 0)

    suite(m"Array construction"):
      // Frozen arrays compare through `readable`: `==` on an array is reference identity.
      test(m"a literal array holds its elements"):
        Array(1, 2, 3).readable.toList
      . assert(_ == scala.List(1, 2, 3))

      test(m"an empty array has no elements"):
        Array.empty[Int].length
      . assert(_ == 0)

      test(m"an allocated array is zero-filled"):
        Array.allocate[Int](3).readable.toList
      . assert(_ == scala.List(0, 0, 0))

      test(m"an allocated array has the requested length"):
        Array.allocate[Int](5).length
      . assert(_ == 5)

      test(m"fill repeats its element"):
        Array.fill(4)(7).readable.toList
      . assert(_ == scala.List(7, 7, 7, 7))

      test(m"fill evaluates its element for each position"):
        var count = 0
        Array.fill(4)({ count += 1; count }).readable.toList
      . assert(_ == scala.List(1, 2, 3, 4))

      test(m"tabulate applies its lambda to each index"):
        Array.tabulate(4)(_*10).readable.toList
      . assert(_ == scala.List(0, 10, 20, 30))

      test(m"range is end-exclusive"):
        Array.range(2, 6).readable.toList
      . assert(_ == scala.List(2, 3, 4, 5))

      test(m"an empty range gives an empty array"):
        Array.range(3, 3).length
      . assert(_ == 0)

      test(m"from copies an iterable"):
        Array.from(scala.List(9, 8, 7)).readable.toList
      . assert(_ == scala.List(9, 8, 7))

    suite(m"Array access"):
      test(m"a frozen array reads through readUnchecked"):
        Array(4, 5, 6).readUnchecked(1)
      . assert(_ == 5)

      // Read back through `freeze`, which consumes the exclusive reference: reading the same
      // array through the shared `readUnchecked` while an exclusive reference is still live is
      // a separation failure, which is exactly the discipline the type is there to enforce.
      test(m"an exclusive array reads back what was written"):
        val buffer = Array.allocate[Int](3)
        buffer(0) = 11
        Array.freeze(buffer).readUnchecked(0)
      . assert(_ == 11)

      test(m"an exclusive array is updated in place"):
        val buffer = Array.allocate[Int](3)
        buffer(1) = 42
        buffer.readable.toList
      . assert(_ == scala.List(0, 42, 0))

      test(m"fill overwrites every position"):
        val buffer = Array.allocate[Int](3)
        buffer.fill(8)
        buffer.readable.toList
      . assert(_ == scala.List(8, 8, 8))

      test(m"an array destructures by pattern"):
        Array(1, 2, 3) match
          case Array(a, b, c) => (a, b, c)
          case _              => (0, 0, 0)

      . assert(_ == (1, 2, 3))

      test(m"a pattern of the wrong arity does not match"):
        Array(1, 2, 3) match
          case Array(a, b) => -1
          case _           => 0

      . assert(_ == 0)

    suite(m"Array transformation"):
      test(m"mapping a frozen array gives a frozen array"):
        Array(1, 2, 3).map(_*2).readable.toList
      . assert(_ == scala.List(2, 4, 6))

      test(m"mapping may change the element type"):
        Array(1, 2, 3).map(_.toString).readable.toList
      . assert(_ == scala.List("1", "2", "3"))

      test(m"copyFrom transfers a window"):
        val buffer = Array.allocate[Int](4)
        buffer.copyFrom(Array(1, 2, 3, 4), 1, 0, 2)
        buffer.readable.toList
      . assert(_ == scala.List(2, 3, 0, 0))

      test(m"growing preserves the existing content"):
        val buffer = Array.allocate[Int](2)
        buffer(0) = 5
        buffer(1) = 6
        Array.grow(buffer, 4).readable.toList
      . assert(_ == scala.List(5, 6, 0, 0))

      test(m"growing to a shorter size truncates"):
        val buffer = Array.allocate[Int](4)
        buffer.fill(3)
        Array.grow(buffer, 2).readable.toList
      . assert(_ == scala.List(3, 3))

      test(m"freezing preserves the content"):
        val buffer = Array.allocate[Int](2)
        buffer(0) = 1
        buffer(1) = 2
        Array.freeze(buffer).readable.toList
      . assert(_ == scala.List(1, 2))

    suite(m"Ledger"):
      // A `Ledger` is exactly a `Map` that promises insertion order; that promise is the
      // whole reason the type exists, and it is what these tests check.
      test(m"a ledger iterates in insertion order"):
        Ledger(t"c" -> 3, t"a" -> 1, t"b" -> 2).stdlib.keys.toList
      . assert(_ == scala.List(t"c", t"a", t"b"))

      test(m"insertion order survives many entries, where a Map's would not"):
        val pairs = (0 until 32).map { n => (n, n*n) }
        Ledger.from(pairs).stdlib.keys.toList
      . assert(_ == (0 until 32).toList)

      test(m"an empty ledger has no entries"):
        Ledger.empty[Text, Int].stdlib.size
      . assert(_ == 0)

      test(m"a ledger looks up by key"):
        Ledger(t"a" -> 1, t"b" -> 2).stdlib(t"b")
      . assert(_ == 2)

      test(m"a later pair for the same key wins"):
        Ledger(t"a" -> 1, t"a" -> 2).stdlib(t"a")
      . assert(_ == 2)

      test(m"a collection converts to a ledger"):
        scala.List((1, t"one"), (2, t"two")).to(Ledger).stdlib.keys.toList
      . assert(_ == scala.List(1, 2))

    suite(m"Set, Map and Sequence"):
      test(m"a set discards duplicates"):
        Set(1, 2, 2, 3).size
      . assert(_ == 3)

      test(m"an empty set has no elements"):
        Set.empty[Int].size
      . assert(_ == 0)

      test(m"a set is built from an iterable"):
        Set.from(scala.List(1, 1, 2))
      . assert(_ == scala.collection.immutable.Set(1, 2))

      test(m"a map looks up by key"):
        Map(t"a" -> 1, t"b" -> 2)(t"a")
      . assert(_ == 1)

      test(m"an empty map has no entries"):
        Map.empty[Text, Int].size
      . assert(_ == 0)

      test(m"a sequence keeps its order"):
        Sequence(3, 1, 2)
      . assert(_ == scala.collection.immutable.Vector(3, 1, 2))

      test(m"a sequence destructures by pattern"):
        Sequence(1, 2) match
          case Sequence(a, b) => (a, b)
          case _              => (0, 0)

      . assert(_ == (1, 2))

      test(m"an empty sequence has no elements"):
        Sequence.empty[Int].size
      . assert(_ == 0)


    suite(m"Comparison"):
      test(m"a negative sign is a Less comparison"):
        Comparison(-42)
      . assert(_ == Comparison.Less)

      test(m"a positive sign is a More comparison"):
        Comparison(42)
      . assert(_ == Comparison.More)

      test(m"a zero sign is a Same comparison"):
        Comparison(0)
      . assert(_ == Comparison.Same)

      test(m"a Less comparison answers only to less"):
        (Comparison.Less.less, Comparison.Less.same, Comparison.Less.more)
      . assert(_ == (true, false, false))

      test(m"a Same comparison answers only to same"):
        (Comparison.Same.less, Comparison.Same.same, Comparison.Same.more)
      . assert(_ == (false, true, false))

      test(m"a More comparison answers only to more"):
        (Comparison.More.less, Comparison.More.same, Comparison.More.more)
      . assert(_ == (false, false, true))

      test(m"flipping exchanges Less and More"):
        (Comparison.Less.flip, Comparison.Same.flip, Comparison.More.flip)
      . assert(_ == (Comparison.More, Comparison.Same, Comparison.Less))

      test(m"the signs of the three comparisons"):
        (Comparison.Less.sign, Comparison.Same.sign, Comparison.More.sign)
      . assert(_ == (-1, 0, 1))

      test(m"a decisive comparison ignores what follows it"):
        Comparison.More.also(Comparison.Less)
      . assert(_ == Comparison.More)

      test(m"a Same comparison defers to what follows it"):
        Comparison.Same.also(Comparison.Less)
      . assert(_ == Comparison.Less)

      test(m"the second comparison of a decisive chain is never evaluated"):
        var evaluated = false
        Comparison.Less.also { evaluated = true; Comparison.More }
        evaluated
      . assert(_ == false)
