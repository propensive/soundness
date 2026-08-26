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

package murmuration

import soundness.*

// By name, so that `Map` is Proscenium's and not the stdlib's: through `soundness.*` a stdlib
// `Map` matches `Mappable.Fallback.iterable`, whose operand is the `(key, value)` pair, and
// `map` would silently map entries instead of values.
import proscenium.{List, Set, Map, Sequence, Ledger}

object Tests extends Suite(m"Murmuration tests"):
  def run(): Unit =
    suite(m"has"):
      test(m"a list contains its element"):
        List(1, 2, 3).has(2)
      . assert(_ == true)

      test(m"a list does not contain an absent element"):
        List(1, 2, 3).has(4)
      . assert(_ == false)

      test(m"a set contains its element"):
        Set(t"a", t"b").has(t"a")
      . assert(_ == true)

      test(m"a sequence contains its element"):
        Sequence(1, 2).has(1)
      . assert(_ == true)

      test(m"text contains its character"):
        t"hello".has('e')
      . assert(_ == true)

      test(m"text does not contain an absent character"):
        t"hello".has('z')
      . assert(_ == false)

    suite(m"map"):
      test(m"a list maps elementwise"):
        List(1, 2, 3).map(_*2)
      . assert(_ == List(2, 4, 6))

      test(m"a set maps elementwise"):
        Set(1, 2, 3).map(_*2)
      . assert(_ == Set(2, 4, 6))

      test(m"mapping may change the element type"):
        List(1, 2).map(_.toString.tt)
      . assert(_ == List(t"1", t"2"))

      // A `Map` is `Mappable` over its *values*: mapping preserves the keys, which is the whole
      // reason `map` is driven by `Mappable` rather than by `Traversable` plus `Reshapable`.
      test(m"a map maps its values, keeping its keys"):
        Map(t"a" -> 1, t"b" -> 2).map(_*10)
      . assert(_ == Map(t"a" -> 10, t"b" -> 20))

    suite(m"remap"):
      // `remap` is the pairwise counterpart: it sees a `Map`'s entries as tuples.
      test(m"a map remaps its entries as pairs"):
        Map(t"a" -> 1).remap { (key, value) => (key, value + 1) }
      . assert(_ == Map(t"a" -> 2))

      test(m"a list remaps like map"):
        List(1, 2).remap(_ + 1)
      . assert(_ == List(2, 3))

    suite(m"subsumes"):
      test(m"a contiguous run is subsumed"):
        List(1, 2, 3, 4).subsumes(List(2, 3))
      . assert(_ == true)

      test(m"a non-contiguous selection is not subsumed"):
        List(1, 2, 3, 4).subsumes(List(2, 4))
      . assert(_ == false)

      // The receiver is bound unbranded: `subsumes` takes `self`, so a literal-constructed
      // receiver carries `& Populated` into the parameter type and cannot be asked about an
      // empty argument at all.
      test(m"the empty subsequence is always subsumed"):
        val pair: List[Int] = List(1, 2)
        pair.subsumes(List.empty[Int])
      . assert(_ == true)

      test(m"a longer subsequence is not subsumed"):
        List(1, 2).subsumes(List(1, 2, 3))
      . assert(_ == false)

      test(m"a run at the start is subsumed"):
        List(1, 2, 3).subsumes(List(1, 2))
      . assert(_ == true)

      test(m"a run at the end is subsumed"):
        List(1, 2, 3).subsumes(List(2, 3))
      . assert(_ == true)

      // Text takes an overload that goes through `String.indexOf`, skipping the traversal
      // entirely; it must agree with the generic definition.
      test(m"text subsumes a substring"):
        t"hello world".subsumes(t"lo wo")
      . assert(_ == true)

      test(m"text does not subsume a non-substring"):
        t"hello world".subsumes(t"lowo")
      . assert(_ == false)

      test(m"text subsumes the empty text"):
        t"hello".subsumes(t"")
      . assert(_ == true)

    suite(m"bind and flatMap"):
      test(m"binding flattens the results"):
        List(1, 2).bind { n => List(n, n*10) }
      . assert(_ == List(1, 10, 2, 20))

      test(m"flatMap is the same operation under the desugaring name"):
        List(1, 2).flatMap { n => List(n, n*10) }
      . assert(_ == List(1, 10, 2, 20))

      test(m"binding may cross shapes"):
        List(1, 2).bind { n => Set(n) }
      . assert(_ == List(1, 2))

      test(m"a for-comprehension desugars through flatMap and withFilter"):
        for
          a <- List(1, 2, 3)
          if a > 1
          b <- List(a*10)
        yield b

      . assert(_ == List(20, 30))

    suite(m"filter, exists, count and fold"):
      test(m"filtering keeps the matching elements"):
        List(1, 2, 3, 4).filter(_%2 == 0)
      . assert(_ == List(2, 4))

      test(m"exists finds a matching element"):
        List(1, 2, 3).exists(_ == 2)
      . assert(_ == true)

      test(m"exists reports no matching element"):
        List(1, 2, 3).exists(_ == 9)
      . assert(_ == false)

      test(m"count counts the matching elements"):
        List(1, 2, 3, 4).count(_%2 == 0)
      . assert(_ == 2)

      test(m"fold accumulates from the initial state"):
        List(1, 2, 3).fold(0)(_ + _)
      . assert(_ == 6)

      test(m"fold on an empty collection is the initial state"):
        List.empty[Int].fold(7)(_ + _)
      . assert(_ == 7)

      test(m"foreach visits every element"):
        var total = 0
        List(1, 2, 3).foreach { n => total += n }
        total
      . assert(_ == 6)

    suite(m"flat"):
      test(m"nesting is flattened one level"):
        List(List(1, 2), List(3)).flat
      . assert(_ == List(1, 2, 3))

      test(m"the inner shape may differ from the outer"):
        List(Set(1), Set(2)).flat
      . assert(_ == List(1, 2))

    suite(m"trace, excerpt and zip"):
      test(m"trace records every intermediate state, initial first"):
        List(1, 2, 3).trace(0)(_ + _)
      . assert(_ == List(0, 1, 3, 6))

      test(m"excerpt takes a run by position, end-exclusive"):
        List(1, 2, 3, 4, 5).excerpt(1, 3)
      . assert(_ == List(2, 3))

      test(m"excerpt is total when the bounds fall outside"):
        List(1, 2).excerpt(5, 9)
      . assert(_ == List())

      test(m"excerpt clamps a partially-outside range"):
        List(1, 2, 3).excerpt(2, 99)
      . assert(_ == List(3))

      test(m"zip pairs elements positionally"):
        List(1, 2, 3).zip(List(t"a", t"b", t"c"))
      . assert(_ == List((1, t"a"), (2, t"b"), (3, t"c")))

      test(m"zip stops at the shorter side"):
        List(1, 2, 3).zip(List(t"a"))
      . assert(_ == List((1, t"a")))

    suite(m"group, sort and distinct"):
      test(m"grouping keys the source's own shape"):
        List(1, 2, 3, 4).group(_%2)
      . assert(_ == Map(1 -> List(1, 3), 0 -> List(2, 4)))

      test(m"sorting by a key orders the elements"):
        List(3, 1, 2).sort(-_)
      . assert(_ == List(3, 2, 1))

      test(m"sorting without a key uses the elements' own order"):
        List(3, 1, 2).sort
      . assert(_ == List(1, 2, 3))

      test(m"distinct drops repeats, keeping the first"):
        List(1, 2, 1, 3, 2).distinct
      . assert(_ == List(1, 2, 3))

      // `sort` demands `Reshapable.Stable`, so an unordered shape cannot be sorted: the
      // alternative would be to sort and then silently drop the order again.
      test(m"a set cannot be sorted"):
        demilitarize:
          Set(3, 1, 2).sort
      . assert(_.nonEmpty)

      test(m"a set cannot be excerpted"):
        demilitarize:
          Set(3, 1, 2).excerpt(0, 1)
      . assert(_.nonEmpty)

      // `sweep` is deliberately not `Stable`: gathering from a set is meaningful.
      test(m"a set can be swept"):
        Set(1, 2, 3, 4).sweep { case n if n%2 == 0 => n*10 }
      . assert(_ == Set(20, 40))

    suite(m"span, sweep and batched"):
      test(m"span splits at the first failure"):
        List(1, 2, 3, 1).span(_ < 3)
      . assert(_ == (List(1, 2), List(3, 1)))

      test(m"span with no match gives an empty prefix"):
        List(3, 1).span(_ < 3)
      . assert(_ == (List(), List(3, 1)))

      test(m"span with a total match gives an empty remainder"):
        List(1, 2).span(_ < 3)
      . assert(_ == (List(1, 2), List()))

      test(m"sweep filters and maps in one pass"):
        List(1, 2, 3, 4).sweep { case n if n%2 == 0 => n*10 }
      . assert(_ == List(20, 40))

      test(m"batched splits into runs of at most the given size"):
        List(1, 2, 3, 4, 5).batched(2)
      . assert(_ == List(List(1, 2), List(3, 4), List(5)))

      test(m"a batch size larger than the source gives one batch"):
        List(1, 2).batched(9)
      . assert(_ == List(List(1, 2)))

    suite(m"Reshaping across shapes"):
      // The `Fallback`/`Fallback2` priority ladder decides what a `Map` or `Ledger` reshapes
      // into when the lambda's result is not itself a pair. Ambiguity here is the classic
      // failure mode, so each rung is pinned.
      test(m"a map whose entries map to pairs stays a map"):
        Map(1 -> t"a").remap { (key, value) => (key + 1, value) }
      . assert(_ == Map(2 -> t"a"))

      test(m"a map whose entries map to non-pairs becomes a list"):
        Map(1 -> t"a").remap { (key, value) => key }
      . assert(_ == List(1))

      test(m"a ledger whose entries map to non-pairs becomes a list"):
        Ledger(1 -> t"a", 2 -> t"b").remap { (key, value) => key }
      . assert(_ == List(1, 2))

      test(m"a ledger keeps insertion order when reshaped to a list"):
        Ledger(2 -> t"b", 1 -> t"a").remap { (key, value) => key }
      . assert(_ == List(2, 1))

      test(m"text reshapes to text"):
        t"hello".filter(_ != 'l')
      . assert(_ == t"heo")

      test(m"text traverses as characters"):
        t"abc".fold(0)((total, _) => total + 1)
      . assert(_ == 3)
