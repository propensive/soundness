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

import murmuration.subsumes

// By name, so that `Map` is Proscenium's and not the stdlib's: through `soundness.*` a stdlib
// `Map` matches `Mappable.Fallback.iterable`, whose operand is the `(key, value)` pair, and
// `map` would silently map entries instead of values.
import proscenium.{List, Set, Map, Sequence, Ledger}

// A type whose order is expressed only as a stdlib `Ordering`, for checking that the bridge in
// `Comparable`'s companion still makes it sortable.
case class Suit(rank: Int)

// A collection whose `Countable` lies about how many elements it has, so that the size `sort`
// takes from it is exercised as the hint it is rather than the promise it looks like.
case class Miscounted(elements: List[Int], claim: Int)

object Miscounted:
  given traversable: Miscounted is Traversable by Int = _.elements.stdlib.iterator

  given reshapable: Miscounted is Reshapable.Stable by Int to List[Int] =
    elements => List.from(elements)

  given countable: Miscounted is Countable:
    def size(self: Miscounted): Int = self.claim

// Deterministic pseudo-random values, so every run of the suite sorts exactly the same input.
// `modulus` fixes how many distinct keys there are, and so how many ties the sort must break.
def pseudorandom(count: Int, modulus: Int): List[Int] =
  val builder = scala.collection.immutable.Vector.newBuilder[Int]
  var state = 12345L
  var index = 0

  while index < count do
    state = state*6364136223846793005L + 1442695040888963407L
    builder += ((state >>> 33)%modulus).toInt
    index += 1

  List.from(builder.result())

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
        import sortingAlgorithms.timsort
        List(3, 1, 2).order(-_)
      . assert(_ == List(3, 2, 1))

      test(m"sorting without a key uses the elements' own order"):
        import sortingAlgorithms.timsort
        List(3, 1, 2).sort
      . assert(_ == List(1, 2, 3))

      test(m"distinct drops repeats, keeping the first"):
        List(1, 2, 1, 3, 2).distinct
      . assert(_ == List(1, 2, 3))

      test(m"deduplicate keeps the first element for each key"):
        List(10, 43, 22, 71, 52).deduplicate(_%10)
      . assert(_ == List(10, 43, 22, 71))

      // Like `distinct`, which occurrence survives is positional, so unordered shapes
      // cannot deduplicate.
      test(m"a set cannot be deduplicated"):
        demilitarize:
          Set(1, 2, 3).deduplicate(_%2)
      . assert(_.nonEmpty)

      // `sort` demands `Reshapable.Stable`, so an unordered shape cannot be sorted: the
      // alternative would be to sort and then silently drop the order again.
      test(m"a set cannot be sorted"):
        demilitarize:
          import sortingAlgorithms.timsort
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

      test(m"partition splits by the predicate, keeping order on both sides"):
        List(1, 2, 3, 4, 5).partition(_%2 == 0)
      . assert(_ == (List(2, 4), List(1, 3, 5)))

      // Unlike `span`, `partition` ignores position, so it is not `Stable`-gated.
      test(m"a set can be partitioned"):
        Set(1, 2, 3, 4).partition(_%2 == 0)
      . assert(_ == (Set(2, 4), Set(1, 3)))

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

    suite(m"Sorting algorithms"):
      // Deliberately no import at this level: the algorithm is chosen per test, and a
      // file-level import would leak into the `demilitarize` blocks below.
      val shuffled = List(5, 3, 9, 1, 7, 3, 8, 2, 6, 0, 4)
      val ascending = List(0, 1, 2, 3, 3, 4, 5, 6, 7, 8, 9)

      test(m"sorting needs an algorithm to be chosen"):
        demilitarize:
          List(3, 1, 2).sort
      . assert(_.nonEmpty)

      test(m"two algorithms in scope are ambiguous"):
        demilitarize:
          import sortingAlgorithms.{timsort, quicksort}
          List(3, 1, 2).sort
      . assert(_.nonEmpty)

      test(m"Timsort orders a list"):
        import sortingAlgorithms.timsort
        shuffled.sort
      . assert(_ == ascending)

      test(m"Powersort orders a list"):
        import sortingAlgorithms.powersort
        shuffled.sort
      . assert(_ == ascending)

      test(m"Quicksort orders a list"):
        import sortingAlgorithms.quicksort
        shuffled.sort
      . assert(_ == ascending)

      test(m"Heapsort orders a list"):
        import sortingAlgorithms.heapsort
        shuffled.sort
      . assert(_ == ascending)

      test(m"bubble sort orders a list"):
        import sortingAlgorithms.bubbleSort
        shuffled.sort
      . assert(_ == ascending)

      test(m"insertion sort orders a list"):
        import sortingAlgorithms.insertionSort
        shuffled.sort
      . assert(_ == ascending)

      // Long enough that Powersort's merge policy runs, and Quicksort recurses past its
      // insertion-sort cutoff, rather than every input being sorted by the small-case path.
      val long = List.from((0 until 500).map { n => (n*37)%500 })
      val longAscending = List.from(0 until 500)

      test(m"Powersort orders five hundred elements"):
        import sortingAlgorithms.powersort
        long.sort
      . assert(_ == longAscending)

      test(m"Quicksort orders five hundred elements"):
        import sortingAlgorithms.quicksort
        long.sort
      . assert(_ == longAscending)

      test(m"Heapsort orders five hundred elements"):
        import sortingAlgorithms.heapsort
        long.sort
      . assert(_ == longAscending)

      // Sorted, reverse-sorted and constant input are the cases the adaptive algorithms
      // shortcut, and the ones a mis-set loop bound corrupts.
      test(m"already-sorted input is left alone"):
        import sortingAlgorithms.powersort
        ascending.sort
      . assert(_ == ascending)

      test(m"reversed input is reordered"):
        import sortingAlgorithms.powersort
        ascending.reverse.sort
      . assert(_ == ascending)

      test(m"every element the same"):
        import sortingAlgorithms.quicksort
        List(7, 7, 7, 7, 7).sort
      . assert(_ == List(7, 7, 7, 7, 7))

      test(m"an empty list sorts to itself"):
        import sortingAlgorithms.heapsort
        List[Int]().sort
      . assert(_ == List[Int]())

      test(m"a single element sorts to itself"):
        import sortingAlgorithms.bubbleSort
        List(1).sort
      . assert(_ == List(1))

      test(m"a sequence sorts to a sequence"):
        import sortingAlgorithms.insertionSort
        Sequence(3, 1, 2).sort
      . assert(_ == Sequence(1, 2, 3))

      test(m"text sorts to text"):
        import sortingAlgorithms.quicksort
        t"soundness".sort
      . assert(_ == t"dennosssu")

      test(m"sorting by a projection under each algorithm"):
        import sortingAlgorithms.heapsort
        List(t"ccc", t"a", t"bb").order(_.s.length)
      . assert(_ == List(t"a", t"bb", t"ccc"))

      // The stable algorithms must leave equal-keyed elements in their source order; the pairs
      // share a key, so only their relative order can tell the difference.
      val pairs = List((1, t"a"), (0, t"b"), (1, t"c"), (0, t"d"), (1, t"e"))
      val stably = List((0, t"b"), (0, t"d"), (1, t"a"), (1, t"c"), (1, t"e"))

      test(m"Timsort is stable"):
        import sortingAlgorithms.timsort
        pairs.order(_(0))
      . assert(_ == stably)

      test(m"Powersort is stable"):
        import sortingAlgorithms.powersort
        pairs.order(_(0))
      . assert(_ == stably)

      test(m"insertion sort is stable"):
        import sortingAlgorithms.insertionSort
        pairs.order(_(0))
      . assert(_ == stably)

      test(m"bubble sort is stable"):
        import sortingAlgorithms.bubbleSort
        pairs.order(_(0))
      . assert(_ == stably)

      // Every receiver shape sorts through the same algorithm, so they agree exactly.
      test(m"every shape sorts the same way"):
        import sortingAlgorithms.quicksort
        val list = List(3, 1, 2).sort
        val sequence = Sequence(3, 1, 2).sort
        (list, sequence)
      . assert(_ == (List(1, 2, 3), Sequence(1, 2, 3)))

      // A type whose order is still expressed as a stdlib `Ordering` sorts through the bridge
      // in `Comparable`'s companion.
      test(m"a type with only an Ordering can be sorted"):
        import sortingAlgorithms.timsort
        given ordering: scala.math.Ordering[Suit] = scala.math.Ordering.by(_.rank)
        List(Suit(3), Suit(1), Suit(2)).sort
      . assert(_ == List(Suit(1), Suit(2), Suit(3)))

    // Galloping engages only after one run has won seven comparisons in a row, which none of
    // the inputs above is large or lopsided enough to reach. These are, and each is checked
    // against Timsort — the JDK's own, which is stable — element for element: every element
    // carries its source position, so a merge that got its tie-breaking backwards would put
    // some equal pair in the other order and the two results would differ.
    suite(m"Powersort merging"):
      def agreesWithTimsort(data: List[Int]): Boolean =
        val tagged: List[(Int, Int)] = List.from(data.stdlib.zipWithIndex)

        val byPowersort =
          import sortingAlgorithms.powersort
          tagged.order(_(0))

        val byTimsort =
          import sortingAlgorithms.timsort
          tagged.order(_(0))

        byPowersort == byTimsort

      test(m"five thousand values, fifty distinct keys"):
        agreesWithTimsort(pseudorandom(5000, 50))
      . assert(_ == true)

      test(m"five thousand values, all keys distinct"):
        agreesWithTimsort(pseudorandom(5000, 1000000))
      . assert(_ == true)

      test(m"five thousand values, two distinct keys"):
        agreesWithTimsort(pseudorandom(5000, 2))
      . assert(_ == true)

      // Two ascending runs of the same values: every comparison in the merge is a tie, which
      // is the case a merge that gallops in the wrong direction gets wrong.
      test(m"two identical ascending runs"):
        agreesWithTimsort(List.from((0 until 2500) ++ (0 until 2500)))
      . assert(_ == true)

      // A long ascending run, one element that belongs before all of it, then another long
      // ascending run: the merge takes the single element and then the whole of the left run
      // in one gallop.
      test(m"a merge won almost entirely by one run"):
        agreesWithTimsort(List.from((1000 until 3000) ++ scala.Seq(0) ++ (3000 until 5000)))
      . assert(_ == true)

      // The same, with the lopsidedness the other way about.
      test(m"a merge won almost entirely by the other run"):
        agreesWithTimsort(List.from((3000 until 5000) ++ scala.Seq(9999) ++ (1000 until 3000)))
      . assert(_ == true)

      test(m"eight ascending runs of ties"):
        agreesWithTimsort(List.from((0 until 4000).map { n => (n%500)/10 }))
      . assert(_ == true)

      test(m"descending, which each run reverses"):
        agreesWithTimsort(List.from((0 until 3000).reverse))
      . assert(_ == true)

      test(m"already ascending, which needs no merge at all"):
        agreesWithTimsort(List.from(0 until 3000))
      . assert(_ == true)

      test(m"ascending with every hundredth element displaced"):
        agreesWithTimsort:
          List.from((0 until 3000).map { n => if n%100 == 0 then (n + 1500)%3000 else n })
      . assert(_ == true)

    // The one shape that deserves a specialization: an array's elements are already in the
    // arrangement the algorithms work on, so `sort()` rearranges them where they lie. Arrays
    // are frozen unless explicitly allocated, so each test starts from a fresh exclusive copy.
    suite(m"Sorting an array in place"):
      def mutableCopy[element: ClassTag](values: List[element]): Array[element]^ =
        val array = Array.allocate[element](values.stdlib.length)
        val iterator = values.stdlib.iterator
        var index = 0

        while iterator.hasNext do
          array(index) = iterator.next()
          index += 1

        array

      test(m"a reference array sorts in place"):
        import sortingAlgorithms.quicksort
        import soundness.collationComparable
        import collations.codepointCollation
        val array = mutableCopy(List(t"c", t"a", t"b"))
        array.sort()
        List.from(array.readable)
      . assert(_ == List(t"a", t"b", t"c"))

      // A primitive array is not a reference array, so its elements take a different route —
      // boxed into scratch and written back — and must come out in the same order.
      test(m"a primitive array sorts in place"):
        import sortingAlgorithms.powersort
        val array = mutableCopy(List(5, 3, 9, 1, 7, 3, 8, 2, 6, 0, 4))
        array.sort()
        List.from(array.readable)
      . assert(_ == List(0, 1, 2, 3, 3, 4, 5, 6, 7, 8, 9))

      test(m"an empty array sorts to itself"):
        import sortingAlgorithms.heapsort
        val array = Array.allocate[Int](0)
        array.sort()
        List.from(array.readable)
      . assert(_ == List[Int]())

      test(m"a single-element array sorts to itself"):
        import sortingAlgorithms.bubbleSort
        val array = mutableCopy(List(7))
        array.sort()
        List.from(array.readable)
      . assert(_ == List(7))

      test(m"sorting in place agrees with sorting a list"):
        import sortingAlgorithms.timsort
        val values = pseudorandom(2000, 500)
        val array = mutableCopy(values)
        array.sort()
        List.from(array.readable) == values.sort
      . assert(_ == true)

      test(m"every algorithm sorts an array in place alike"):
        val values = pseudorandom(1000, 100)

        def inPlace(algorithm: SortAlgorithm): List[Int] =
          given SortAlgorithm = algorithm
          val array = mutableCopy(values)
          array.sort()
          List.from(array.readable)

        val results =
          scala.collection.immutable.List
           ( inPlace(sortingAlgorithms.timsort),
             inPlace(sortingAlgorithms.powersort),
             inPlace(sortingAlgorithms.quicksort),
             inPlace(sortingAlgorithms.heapsort),
             inPlace(sortingAlgorithms.insertionSort),
             inPlace(sortingAlgorithms.bubbleSort) )

        results.distinct.length
      . assert(_ == 1)

      // The two `sort`s are told apart by the receiver and the parens: a collection answers
      // with a new collection, an exclusive array rearranges itself and answers with nothing.
      test(m"the two sorts are distinguished by their receivers"):
        import sortingAlgorithms.timsort
        val array = mutableCopy(List(3, 1, 2))
        val sorted: List[Int] = List(3, 1, 2).sort
        val inPlace: Unit = array.sort()
        (sorted, List.from(array.readable))
      . assert(_ == (List(1, 2, 3), List(1, 2, 3)))

      // `sort` on an array means the in-place one, whatever the parens: an extension is chosen
      // by its receiver before its argument list is considered. A frozen array has no writer to
      // sort through, so it is rejected rather than silently sorted into a copy.
      test(m"a frozen array cannot be sorted in place"):
        demilitarize:
          import sortingAlgorithms.timsort
          Array(3, 1, 2).sort()
      . assert(_.nonEmpty)

      test(m"a frozen array does not take the collection sort either"):
        demilitarize:
          import sortingAlgorithms.timsort
          Array(3, 1, 2).sort
      . assert(_.nonEmpty)

      // `order` is not overloaded, so it still sorts a frozen array into a new one — which is
      // what to reach for when the array must not be written to.
      test(m"a frozen array orders into a new array"):
        import sortingAlgorithms.timsort
        val array: Array[Int]^{} = Array(3, 1, 2).order(identity)
        List.from(array.readable)
      . assert(_ == List(1, 2, 3))

      test(m"sorting in place needs an algorithm to be chosen"):
        demilitarize:
          val array = Array.allocate[Int](3)
          array.sort()
      . assert(_.nonEmpty)

    // Sizing the scratch array from `Countable` is an optimization, and one that must not be
    // able to lose or invent elements when the count is wrong.
    suite(m"Sorting with a known size"):
      test(m"a countable receiver sorts correctly"):
        import sortingAlgorithms.timsort
        Miscounted(List(3, 1, 2), 3).sort
      . assert(_ == List(1, 2, 3))

      test(m"an under-reported count loses nothing"):
        import sortingAlgorithms.timsort
        Miscounted(List(5, 3, 9, 1, 7), 2).sort
      . assert(_ == List(1, 3, 5, 7, 9))

      test(m"an over-reported count invents nothing"):
        import sortingAlgorithms.timsort
        Miscounted(List(5, 3, 9, 1, 7), 50).sort
      . assert(_ == List(1, 3, 5, 7, 9))

      test(m"a count of zero loses nothing"):
        import sortingAlgorithms.timsort
        Miscounted(List(2, 1), 0).sort
      . assert(_ == List(1, 2))

      test(m"an empty countable receiver sorts to nothing"):
        import sortingAlgorithms.timsort
        Miscounted(List(), 0).sort
      . assert(_ == List[Int]())

      // A `List`'s `Countable` is gated behind `linearSize`, since counting one is a traversal;
      // without that import the sort takes its elements from an iterator of unknown length, and
      // must of course still sort them.
      test(m"sorting a list works either side of the linearSize gate"):
        import sortingAlgorithms.timsort
        val counted =
          import dysasymptotics.linearSize
          List(3, 1, 2).sort

        (counted, List(3, 1, 2).sort)
      . assert(_ == (List(1, 2, 3), List(1, 2, 3)))

    // `order` sorts keys interleaved with their elements, through algorithms compiled a second
    // time to move both cells together — and, for Timsort, through an implementation of our own,
    // since `java.util.Arrays.sort` cannot move pairs. The standard library's `sortBy` is the
    // outside oracle here: it is stable, and it is nobody's code but the standard library's.
    suite(m"Ordering by an interleaved key"):
      def keyed(count: Int, modulus: Int): List[(Int, Int)] =
        List.from(pseudorandom(count, modulus).stdlib.zipWithIndex)

      // Sorting pairs by their first component, with `zipWithIndex` making every element
      // distinct: a stable sort must leave equal keys in their original order.
      def agreesWithStdlib(data: List[(Int, Int)]): Boolean =
        val expected = List.from(data.stdlib.sortBy(_(0)))

        val byTimsort =
          import sortingAlgorithms.timsort
          data.order(_(0))

        val byPowersort =
          import sortingAlgorithms.powersort
          data.order(_(0))

        val byInsertion =
          import sortingAlgorithms.insertionSort
          data.order(_(0))

        val byBubble =
          import sortingAlgorithms.bubbleSort
          data.order(_(0))

        byTimsort == expected && byPowersort == expected && byInsertion == expected
        && byBubble == expected

      test(m"five thousand elements, fifty distinct keys"):
        agreesWithStdlib(keyed(5000, 50))
      . assert(_ == true)

      test(m"five thousand elements, every key distinct"):
        agreesWithStdlib(keyed(5000, 1000000))
      . assert(_ == true)

      test(m"five thousand elements, two distinct keys"):
        agreesWithStdlib(keyed(5000, 2))
      . assert(_ == true)

      test(m"a hundred elements, so that no run is extended"):
        agreesWithStdlib(keyed(100, 10))
      . assert(_ == true)

      test(m"already ordered by the key"):
        agreesWithStdlib(List.from((0 until 3000).map { n => (n, n) }))
      . assert(_ == true)

      test(m"ordered against the key"):
        agreesWithStdlib(List.from((0 until 3000).map { n => (3000 - n, n) }))
      . assert(_ == true)

      test(m"one element, and none"):
        agreesWithStdlib(List((1, 0))) && agreesWithStdlib(List())
      . assert(_ == true)

      // The unstable algorithms cannot be held to the order of equal keys, only to the keys.
      test(m"the unstable algorithms order the keys alike"):
        val data = keyed(5000, 50)
        val expected = List.from(data.stdlib.sortBy(_(0)).map(_(0)))

        val byQuicksort =
          import sortingAlgorithms.quicksort
          data.order(_(0)).map(_(0))

        val byHeapsort =
          import sortingAlgorithms.heapsort
          data.order(_(0)).map(_(0))

        byQuicksort == expected && byHeapsort == expected
      . assert(_ == true)
