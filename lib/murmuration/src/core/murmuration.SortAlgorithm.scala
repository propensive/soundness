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

import java.util as ju

import scala.collection.immutable as sci
import scala.util.boundary

import prepositional.*

// A sorting algorithm: a way of putting a scratch array into order against a comparator, which
// is the whole of what an algorithm is. Every operation that sorts — `sorted` and `order` over a
// collection, `sort()` over a mutable array — is that plus the arrangements for getting the
// elements into an array and the results back out, so the algorithm is not parameterized by the
// collection it came from. It was, once: the measurements said that every implementation drained
// its receiver into an array anyway, and that the one implementation which did not was five to
// seven times slower for the trouble.
//
// An algorithm is chosen by importing one of the givens in `sortingAlgorithms`; there is
// deliberately no default, so a program says how it sorts, and two imports are an ambiguity
// rather than a silent preference.
//
// `Timsort`, `Powersort`, `BubbleSort` and `InsertionSort` are stable — equal elements keep
// their source order — while `Quicksort` and `Heapsort` are not. Stability is not part of the
// types: a marker would have no consumer yet, and would need a name that could not be confused
// with `Reshapable.Stable`, which is about a *shape* preserving order, not an algorithm.
// `sort` is an ordinary method, and the algorithms below ordinary methods, deliberately.
// Making them `inline`, so that each call site got its own copy of the loop with the comparison
// compiled into it, was tried and measured: sorting a hundred thousand elements went from
// 5.59 ms to 6.68 ms. One shared, compact algorithm is inlined *by the JIT* better than many
// large copies are, and the copies push the comparison call past its inlining budget. The only
// arrangement that beat the shared one replaced the comparison with a primitive `<` (5.30 ms),
// which needs an element type known at the call site and a `Comparable` able to expand inline —
// neither of which the general case has.
trait SortAlgorithm:
  def sort(array: scala.Array[AnyRef]^, comparator: ju.Comparator[AnyRef]): Unit

object SortAlgorithm:
  // The JDK's own sort for objects, which is Timsort: adaptive, stable, and the algorithm
  // behind the standard library's `sorted`. The casts are the price of `-Yexplicit-nulls`:
  // `Arrays.sort` is a Java method, so its loaded signature admits nulls in both the array and
  // the comparator.
  private[murmuration] def timsorted
    (array: scala.Array[AnyRef]^, comparator: ju.Comparator[AnyRef])
  :   Unit =

    ju.Arrays.sort[AnyRef | Null]
      ( array.asInstanceOf[scala.Array[AnyRef | Null]],
        comparator.asInstanceOf[ju.Comparator[AnyRef | Null]] )

  private[murmuration] def insertionSorted
    ( array: scala.Array[AnyRef]^, comparator: ju.Comparator[AnyRef] )
  :   Unit =

    var index = 1

    while index < array.length do
      val element = array(index)
      var hole = index

      while hole > 0 && comparator.compare(array(hole - 1), element) > 0 do
        array(hole) = array(hole - 1)
        hole -= 1

      array(hole) = element
      index += 1

  private[murmuration] def bubbleSorted(array: scala.Array[AnyRef]^, comparator: ju.Comparator[AnyRef]): Unit =
    var end = array.length - 1

    while end > 0 do
      var last = 0
      var index = 0

      while index < end do
        if comparator.compare(array(index), array(index + 1)) > 0 then
          swap(array, index, index + 1)
          last = index + 1

        index += 1

      // Everything beyond the final swap of this pass is already in place, so the next pass
      // can stop there; on already-sorted input no swap happens and the sort ends after one
      // pass.
      end = last - 1

  private[murmuration] def heapsorted(array: scala.Array[AnyRef]^, comparator: ju.Comparator[AnyRef]): Unit =
    var index = array.length/2 - 1

    while index >= 0 do
      sift(array, index, array.length, comparator)
      index -= 1

    var end = array.length - 1

    while end > 0 do
      swap(array, 0, end)
      sift(array, 0, end, comparator)
      end -= 1

  // Restores the heap property at `root`, given that the subtrees below it are already heaps.
  private def sift
    ( array: scala.Array[AnyRef]^, root: Int, size: Int, comparator: ju.Comparator[AnyRef] )
  :   Unit =

    var parent = root
    var settled = false

    while !settled do
      val left = 2*parent + 1
      val right = left + 1
      var largest = parent

      if left < size && comparator.compare(array(left), array(largest)) > 0 then largest = left
      if right < size && comparator.compare(array(right), array(largest)) > 0 then largest = right

      if largest == parent then settled = true else
        swap(array, parent, largest)
        parent = largest

  // Median-of-three pivot selection with Hoare partitioning, recursing into the smaller side
  // and looping on the larger, which bounds the recursion depth at log₂(n) frames.
  private[murmuration] def quicksorted(array: scala.Array[AnyRef]^, comparator: ju.Comparator[AnyRef]): Unit =
    def recur(from: Int, to: Int): Unit =
      var start = from
      var end = to

      while end - start > 1 do
        if end - start < 12 then
          insertionSortRange(array, start, end, comparator)
          start = end
        else
          val pivot = median(array, start, end, comparator)
          var left = start
          var right = end - 1

          while left <= right do
            while comparator.compare(array(left), pivot) < 0 do left += 1
            while comparator.compare(array(right), pivot) > 0 do right -= 1

            if left <= right then
              swap(array, left, right)
              left += 1
              right -= 1

          if right - start < end - left then
            recur(start, right + 1)
            start = left
          else
            recur(left, end)
            end = right + 1

    recur(0, array.length)

  private def median
    ( array: scala.Array[AnyRef]^, from: Int, to: Int, comparator: ju.Comparator[AnyRef] )
  :   AnyRef =

    val middle = from + (to - from)/2
    val last = to - 1

    if comparator.compare(array(middle), array(from)) < 0 then swap(array, from, middle)
    if comparator.compare(array(last), array(from)) < 0 then swap(array, from, last)
    if comparator.compare(array(last), array(middle)) < 0 then swap(array, middle, last)

    array(middle)

  private def insertionSortRange
    ( array: scala.Array[AnyRef]^, from: Int, to: Int, comparator: ju.Comparator[AnyRef] )
  :   Unit =

    var index = from + 1

    while index < to do
      val element = array(index)
      var hole = index

      while hole > from && comparator.compare(array(hole - 1), element) > 0 do
        array(hole) = array(hole - 1)
        hole -= 1

      array(hole) = element
      index += 1

  private def swap(array: scala.Array[AnyRef]^, left: Int, right: Int): Unit =
    val value = array(left)
    array(left) = array(right)
    array(right) = value

  private inline def minimumRun = 24

  // Powersort (Munro & Wild, 2018): Timsort's merge policy replaced by one that merges runs in
  // the order a nearly-optimal binary merge tree would, which the "power" of a boundary between
  // two adjacent runs identifies without building the tree. Like Timsort it is stable, adaptive
  // to existing runs, and merges by galloping (see `merge`).
  private[murmuration] def powersorted(array: scala.Array[AnyRef]^, comparator: ju.Comparator[AnyRef]): Unit =
    val size = array.length

    var start = 0
    var end = if size > 1 then run(array, 0, comparator) else size

    // Input that is already in order is one run, so it is sorted by the scan that discovered
    // that and nothing else happens — in particular the merge buffer, which is the size of the
    // array, is never allocated.
    if end < size then
      // Full-length, not half: `merge` copies its *left* run aside, and by the last merges that
      // run is the whole accumulated prefix, which can be nearly the entire array.
      val buffer = new scala.Array[AnyRef](size)

      // The stack of runs not yet merged, each with the power of its left boundary. Depth is
      // bounded by the number of bits in `size`, powers being at most log₂(size) + 1.
      val starts = new scala.Array[Int](64)
      val powers = new scala.Array[Int](64)
      var depth = 0

      // How one-sided a merge must become before it starts galloping. It belongs to the sort
      // rather than to a single merge: input that rewards galloping tends to do so in every
      // merge, and input that punishes it likewise, so each merge hands the tuned value on.
      var threshold = minimumGallop

      while end < size do
        val nextEnd = run(array, end, comparator)
        val power = boundaryPower(start, end, nextEnd, size)

        // Every run on the stack whose boundary is more deeply nested than the new one belongs
        // to the left subtree of this boundary, so it is merged before the new run is pushed.
        while depth > 0 && powers(depth - 1) > power do
          depth -= 1
          threshold = merge(array, buffer, starts(depth), start, end, comparator, threshold)
          start = starts(depth)

        starts(depth) = start
        powers(depth) = power
        depth += 1
        start = end
        end = nextEnd

      while depth > 0 do
        depth -= 1
        threshold = merge(array, buffer, starts(depth), start, end, comparator, threshold)
        start = starts(depth)

  // The length of the run beginning at `from`, extended by insertion sort to `minimumRun`
  // elements where the natural run is shorter. A strictly descending run is reversed in place,
  // which is what keeps the sort stable: equal neighbours end a descending run.
  private def run(array: scala.Array[AnyRef]^, from: Int, comparator: ju.Comparator[AnyRef])
  :   Int =

    val size = array.length
    var end = from + 1

    if end < size then
      if comparator.compare(array(end), array(end - 1)) < 0 then
        while end < size && comparator.compare(array(end), array(end - 1)) < 0 do end += 1
        reverse(array, from, end)
      else
        while end < size && comparator.compare(array(end), array(end - 1)) >= 0 do end += 1

    if end - from >= minimumRun || end == size then end else
      val extended = scala.math.min(from + minimumRun, size)
      insertionSortRange(array, from, extended, comparator)
      extended

  private def reverse(array: scala.Array[AnyRef]^, from: Int, to: Int): Unit =
    var left = from
    var right = to - 1

    while left < right do
      swap(array, left, right)
      left += 1
      right -= 1

  // The power of the boundary between the runs `[left, middle)` and `[middle, right)`: the
  // number of binary digits of the two runs' midpoints (as fractions of the whole array) which
  // agree, plus one. Computed by extracting one bit of each at a time, so it needs no division.
  private def boundaryPower(left: Int, middle: Int, right: Int, size: Int): Int =
    var lower = 2L*left + (middle - left)
    var upper = lower + (middle - left) + (right - middle)
    var power = 0
    var found = false

    while !found do
      power += 1

      if lower >= size then
        lower -= size
        upper -= size
      else if upper >= size then
        found = true

      if !found then
        lower *= 2
        upper *= 2

    power

  // How many times in a row one run must win before the merge stops comparing element by
  // element and starts searching for whole blocks instead. Seven is Timsort's figure: low
  // enough that a lopsided merge is caught early, high enough that an evenly interleaved one
  // never pays for a search it cannot use.
  private inline def minimumGallop = 7

  // Merges the adjacent sorted runs `[from, middle)` and `[middle, to)`, copying the left run
  // aside so that the merge can write back over the array in place, and returns the tuned
  // galloping threshold for the next merge to start from.
  //
  // A straight merge costs one comparison per element, which is optimal only when the runs
  // interleave evenly. When one run wins `threshold` comparisons in a row, this one switches to
  // galloping: rather than asking which of two elements is smaller, it asks how many of the
  // winning run's elements precede the other run's next element, and answers by probing at
  // exponentially growing offsets (1, 3, 7, 15, …) until it overshoots, then bisecting the gap.
  // A block of `k` elements is then found in about 2·log₂(k) comparisons and moved in one copy.
  // Galloping continues while the blocks it finds stay large; when they do not, the merge falls
  // back to comparing and raises the threshold, so input that does not reward the search pays
  // very little for having tried it.
  private def merge
    ( array:      scala.Array[AnyRef]^,
      buffer:     scala.Array[AnyRef]^,
      from:       Int,
      middle:     Int,
      to:         Int,
      comparator: ju.Comparator[AnyRef],
      threshold:  Int )
  :   Int =

    val disjoint = middle <= from || to <= middle
    val ordered = disjoint || comparator.compare(array(middle), array(middle - 1)) >= 0

    // Runs already in order need no merging at all, which is much of what makes an adaptive
    // sort adaptive.
    if ordered then threshold else
      // Neither end of the merge need be touched: the left run's elements that already precede
      // the right run's first element are in place, as are the right run's elements that already
      // follow the left run's last. Both boundaries are found by galloping, so trimming a merge
      // that turns out to be nearly disjoint costs a logarithmic number of comparisons.
      val start = from + gallopRight(array(middle), array, from, middle - from, 0, comparator)

      val end =
        middle + gallopLeft(array(middle - 1), array, middle, to - middle, to - middle - 1,
            comparator)

      var length1 = middle - start
      var length2 = end - middle

      if length1 == 0 || length2 == 0 then threshold else
        scala.Array.copy(array, start, buffer, 0, length1)

        var cursor1 = 0        // in `buffer`, over the left run
        var cursor2 = middle   // in `array`, over the right run
        var target = start     // in `array`, where the next element is written
        var gallop = threshold

        boundary: label ?=>
          while true do
            var wins1 = 0
            var wins2 = 0

            // Element by element, until one run has won often enough in a row to be worth
            // searching rather than comparing.
            var comparing = true

            while comparing do
              if comparator.compare(array(cursor2), buffer(cursor1)) < 0 then
                array(target) = array(cursor2)
                target += 1
                cursor2 += 1
                length2 -= 1
                wins2 += 1
                wins1 = 0
                if length2 == 0 then boundary.break()
              else
                array(target) = buffer(cursor1)
                target += 1
                cursor1 += 1
                length1 -= 1
                wins1 += 1
                wins2 = 0

                // One element of the left run is left deliberately: it is greater than every
                // remaining element of the right run (the trimming above established that), so
                // the merge finishes by moving the rest of the right run and then that element.
                if length1 == 1 then boundary.break()

              comparing = (wins1 | wins2) < gallop

            // Galloping, in both directions alternately, while it keeps paying.
            var galloping = true

            while galloping do
              // `gallopRight`, an upper bound: an element of the left run equal to the right
              // run's next element still precedes it, which is what keeps the merge stable.
              wins1 = gallopRight(array(cursor2), buffer, cursor1, length1, 0, comparator)

              if wins1 != 0 then
                scala.Array.copy(buffer, cursor1, array, target, wins1)
                target += wins1
                cursor1 += wins1
                length1 -= wins1
                if length1 <= 1 then boundary.break()

              array(target) = array(cursor2)
              target += 1
              cursor2 += 1
              length2 -= 1
              if length2 == 0 then boundary.break()

              // `gallopLeft`, a lower bound, for the same reason in the other direction: an
              // element of the right run equal to the left run's next element must follow it.
              wins2 = gallopLeft(buffer(cursor1), array, cursor2, length2, 0, comparator)

              if wins2 != 0 then
                // Source and destination are the same array, and may overlap; the copy is a
                // `System.arraycopy`, which is defined for that case.
                scala.Array.copy(array, cursor2, array, target, wins2)
                target += wins2
                cursor2 += wins2
                length2 -= wins2
                if length2 == 0 then boundary.break()

              array(target) = buffer(cursor1)
              target += 1
              cursor1 += 1
              length1 -= 1
              if length1 == 1 then boundary.break()

              // Each round of successful galloping makes the next one easier to enter.
              gallop -= 1
              galloping = wins1 >= minimumGallop || wins2 >= minimumGallop

            if gallop < 0 then gallop = 0

            // Leaving galloping mode is the signal that it was not paying, so re-entering it
            // is made harder.
            gallop += 2

        if length1 == 1 then
          scala.Array.copy(array, cursor2, array, target, length2)
          array(target + length2) = buffer(cursor1)
        else
          scala.Array.copy(buffer, cursor1, array, target, length1)

        if gallop < 1 then 1 else gallop

  // The position in `array[from until from + count)` at which `key` belongs, taking the
  // *leftmost* of the positions among equal elements: everything before it compares strictly
  // less than `key`. `hint` is where the search starts from, and is worth giving when the answer
  // is expected near one end.
  private def gallopLeft
    ( key:        AnyRef,
      array:      scala.Array[AnyRef]^,
      from:       Int,
      count:      Int,
      hint:       Int,
      comparator: ju.Comparator[AnyRef] )
  :   Int =

    var offset = 1
    var last = 0

    if comparator.compare(key, array(from + hint)) > 0 then
      // The answer is to the right of the hint: probe 1, 3, 7, … elements beyond it.
      val limit = count - hint

      while offset < limit && comparator.compare(key, array(from + hint + offset)) > 0 do
        last = offset
        offset = 2*offset + 1
        if offset <= 0 then offset = limit   // the doubling overflowed

      if offset > limit then offset = limit
      last += hint
      offset += hint
    else
      // The answer is at or to the left of the hint, so the same probing runs backwards.
      val limit = hint + 1

      while offset < limit && comparator.compare(key, array(from + hint - offset)) <= 0 do
        last = offset
        offset = 2*offset + 1
        if offset <= 0 then offset = limit

      if offset > limit then offset = limit
      val swap = last
      last = hint - offset
      offset = hint - swap

    // The probes bracket the answer in `(last, offset]`; bisection finds it exactly.
    last += 1

    while last < offset do
      val middle = last + (offset - last)/2

      if comparator.compare(key, array(from + middle)) > 0
      then last = middle + 1
      else offset = middle

    offset

  // As `gallopLeft`, but taking the *rightmost* of the positions among equal elements:
  // everything before it compares less than or equal to `key`.
  private def gallopRight
    ( key:        AnyRef,
      array:      scala.Array[AnyRef]^,
      from:       Int,
      count:      Int,
      hint:       Int,
      comparator: ju.Comparator[AnyRef] )
  :   Int =

    var offset = 1
    var last = 0

    if comparator.compare(key, array(from + hint)) < 0 then
      val limit = hint + 1

      while offset < limit && comparator.compare(key, array(from + hint - offset)) < 0 do
        last = offset
        offset = 2*offset + 1
        if offset <= 0 then offset = limit

      if offset > limit then offset = limit
      val swap = last
      last = hint - offset
      offset = hint - swap
    else
      val limit = count - hint

      while offset < limit && comparator.compare(key, array(from + hint + offset)) >= 0 do
        last = offset
        offset = 2*offset + 1
        if offset <= 0 then offset = limit

      if offset > limit then offset = limit
      last += hint
      offset += hint

    last += 1

    while last < offset do
      val middle = last + (offset - last)/2

      if comparator.compare(key, array(from + middle)) < 0
      then offset = middle
      else last = middle + 1

    offset
