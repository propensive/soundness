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

import scala.collection.mutable as scm
import scala.quoted.*

import ambience.*, environments.javaEnvironment, systems.javaSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hypotenuse.*
import hellenism.*, classloaders.threadContextClassloader
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import superlunary.embeddings.automatic
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*

// The shape of the input, which is the axis that tells the adaptive algorithms apart: an
// algorithm that exploits existing order can only show it on input that has some.
enum Shape:
  case Shuffled, Ascending, Descending, NearlyOrdered, Runs, FewDistinct

object Algorithm:
  // Not the phantom types themselves: those are type-level, and an axis needs values.
  enum Choice:
    case Timsort, Powersort, Quicksort, Heapsort, InsertionSort, BubbleSort

// An element type that is a reference, so that an array of them is an array of references —
// which is the arrangement the algorithms already sort, and so the case in which sorting in
// place copies nothing at all. An array of a primitive cannot be that, and boxes instead.
case class Ticket(number: Int)

object Ticket:
  given comparable: Ticket is Comparable = Comparable.int.on(_.number)

object Benchmarks extends Suite(m"Murmuration benchmarks"):
  given device: BenchmarkDevice = LocalhostDevice

  // ─── input data ───────────────────────────────────────────────────────────

  // Datasets are built once per (shape, size) and cached, so that what each measurement
  // times is the sort and not the construction of its input. The cache lookup happens
  // inside the timed region — a few tens of nanoseconds against sorts measured in
  // microseconds and up.
  private val cache: scm.HashMap[(Int, Int), List[Int]] = scm.HashMap()

  // The shape arrives as its ordinal rather than as itself: a benchmark body's values are
  // carried into the staged code as JSON, and an `Int` needs no codec of its own.
  //
  // Unsynchronized: a measurement runs one body in one thread, and every entry but the
  // first is a plain read. That read is inside the timed region — some tens of nanoseconds
  // against a smallest measurement of about a microsecond, and identical for every
  // algorithm, so it shifts every cell of a column by the same amount.
  def data(shape: Int, size: Int): List[Int] =
    cache.getOrElseUpdate((shape, size), build(Shape.fromOrdinal(shape), size))

  // A fixed multiplier rather than a random source: every run of the benchmark sorts
  // exactly the same values, so results are comparable between runs and between machines.
  private def build(shape: Shape, size: Int): List[Int] =
    val array = new scala.Array[Int](size)
    var index = 0

    while index < size do
      array(index) = shape match
        case Shape.Shuffled      => (index*2654435761L % size).toInt
        case Shape.Ascending     => index
        case Shape.Descending    => size - index
        case Shape.FewDistinct   => index%8

        // Ascending, with every hundredth element displaced far from its place: the case
        // for which "nearly sorted" is usually claimed.
        case Shape.NearlyOrdered => if index%100 == 0 then (index + size/2)%size else index

        // Eight ascending runs laid end to end. Timsort and Powersort should find them and
        // merge, rather than sorting from scratch.
        case Shape.Runs          => (index%(size/8 + 1))*8

      index += 1

    List.from(array)

  // ─── the sorts ────────────────────────────────────────────────────────────

  // One method per algorithm, each with its own selector imported: the choice is made here,
  // in ordinary code, so that a benchmark body is a plain call and the staged tree it
  // compiles contains nothing but the sort.
  def timsorted(data: List[Int]): List[Int] =
    import sortingAlgorithms.timsort
    data.sort

  // The same sort with the `List`'s `Countable` available — it is gated behind `linearSize`,
  // counting a list being a traversal of it — so the scratch array is allocated once at the
  // right size rather than grown by a builder.
  def timsortedCounted(data: List[Int]): List[Int] =
    import sortingAlgorithms.timsort
    import denominative.dysasymptotics.linearSize
    data.sort

  def powersorted(data: List[Int]): List[Int] =
    import sortingAlgorithms.powersort
    data.sort

  def quicksorted(data: List[Int]): List[Int] =
    import sortingAlgorithms.quicksort
    data.sort

  def heapsorted(data: List[Int]): List[Int] =
    import sortingAlgorithms.heapsort
    data.sort

  def insertionSorted(data: List[Int]): List[Int] =
    import sortingAlgorithms.insertionSort
    data.sort

  def bubbleSorted(data: List[Int]): List[Int] =
    import sortingAlgorithms.bubbleSort
    data.sort

  // The same data as a `Sequence`, for the question of whether the receiver's shape matters.
  private val sequences: scm.HashMap[(Int, Int), Sequence[Int]] = scm.HashMap()

  def sequence(shape: Int, size: Int): Sequence[Int] =
    sequences.getOrElseUpdate((shape, size), Sequence.from(data(shape, size).stdlib))

  def timsortedSequence(data: Sequence[Int]): Sequence[Int] =
    import sortingAlgorithms.timsort
    data.sort

  // Sorting an array in place: no copy into scratch, no rebuild afterwards, and no decoration
  // of elements with their keys — the whole of what a collection's sort pays extra for. The
  // fresh copy is the measurement's own cost, since sorting in place consumes its input: the
  // second benchmark below measures that copy alone, so the difference between them is the sort.
  def sortedInPlace(data: List[Int]): Int =
    import sortingAlgorithms.timsort
    val array = copyOf(data)
    array.sort()
    array.length

  def copyOnly(data: List[Int]): Int = copyOf(data).length

  // The same, over elements that are references rather than primitives.
  private val tickets: scm.HashMap[(Int, Int), List[Ticket]] = scm.HashMap()

  def ticketData(shape: Int, size: Int): List[Ticket] =
    tickets.getOrElseUpdate((shape, size), data(shape, size).map(Ticket(_)))

  def sortedTickets(data: List[Ticket]): List[Ticket] =
    import sortingAlgorithms.timsort
    data.sort

  def sortedTicketsInPlace(data: List[Ticket]): Int =
    import sortingAlgorithms.timsort
    val array = copyOfTickets(data)
    array.sort()
    array.length

  def copyTicketsOnly(data: List[Ticket]): Int = copyOfTickets(data).length

  private def copyOfTickets(data: List[Ticket]): Array[Ticket] =
    val array = Array.allocate[Ticket](data.stdlib.length)
    val iterator = data.stdlib.iterator
    var index = 0

    while iterator.hasNext do
      array(index) = iterator.next()
      index += 1

    array

  private def copyOf(data: List[Int]): Array[Int] =
    val array = Array.allocate[Int](data.stdlib.length)
    val iterator = data.stdlib.iterator
    var index = 0

    while iterator.hasNext do
      array(index) = iterator.next()
      index += 1

    array

  // What the inline comparison operators are worth. `Bytes` is opaque over `Long` and has both
  // an inline `Orderable` — through which `<` expands to a primitive comparison — and a runtime
  // `Comparable`, so the same comparison can be made both ways over the same data. An
  // `Array[Bytes]` is a `long[]`, so the operator path never leaves the stack, while the
  // typeclass path boxes both operands and answers with a `Comparison`.
  private val sizes0: scala.Array[Bytes] =
    val array = new scala.Array[Bytes](10000)
    var state = 12345L
    var index = 0

    while index < 10000 do
      state = state*6364136223846793005L + 1442695040888963407L
      array(index) = Bytes(state >>> 33)
      index += 1

    array

  def ascendingByOperator: Int =
    var count = 0
    var index = 1

    while index < sizes0.length do
      if sizes0(index - 1) < sizes0(index) then count += 1
      index += 1

    count

  def ascendingByComparable: Int =
    val comparable = summon[Bytes is Comparable]
    var count = 0
    var index = 1

    while index < sizes0.length do
      if comparable.less(sizes0(index - 1), sizes0(index)) then count += 1
      index += 1

    count

  // What boxing still costs. Sorting an array of a primitive in place boxes every element into
  // the scratch the algorithms work on and unboxes it back afterwards, since one algorithm body
  // serves every element type. The JDK's own primitive sort is the zero-boxing bound — a
  // different algorithm (dual-pivot quicksort), so ours is the quicksort here too.
  def quicksortIntArrayInPlace(data: List[Int]): Int =
    import sortingAlgorithms.quicksort
    val array = copyOf(data)
    array.sort()
    array.length

  def jdkPrimitiveSort(data: List[Int]): Int =
    val array = copyOf(data)
    ju.Arrays.sort(array.raw)
    array.length

  // And what the decoration costs: `order` pairs each element with its key in an `Entry`, which
  // `sort` does not, since there an element is its own key.
  def sortWithoutKeys(data: List[Int]): List[Int] =
    import sortingAlgorithms.timsort
    data.sort

  def orderByIdentity(data: List[Int]): List[Int] =
    import sortingAlgorithms.timsort
    data.order(x => x)

  // Three layouts for what `order` has to do — carry each element's key beside it while the
  // elements are permuted — sorted by one and the same quicksort, so that only the layout
  // differs. The elements are references and the keys primitive, which is the shape `order` is
  // usually asked for: sort these records by that field.
  //
  // The library's own `order` is measured beside them as the anchor; it uses the library's
  // quicksort rather than this one, so it answers "is this worth changing" while the three
  // answer "which layout is fastest".

  // 1. As it is today: one object per element, holding the key (boxed, being a field of type
  //    `Any`) and the element.
  private class Pair(val key: Any, val element: Any)

  def orderByPairs(data: List[Ticket]): List[Ticket] =
    val elements = data.stdlib
    val array = new scala.Array[AnyRef](elements.length)
    var index = 0
    var rest = elements

    while index < array.length do
      val ticket = rest.head
      array(index) = Pair(ticket.number, ticket)
      rest = rest.tail
      index += 1

    quicksortPairs(array, 0, array.length)
    List.from(array.iterator.map(_.asInstanceOf[Pair].element.asInstanceOf[Ticket]))

  // 2. One array of twice the length, the key in the even cell and the element in the odd one,
  //    moved together. No object per element, but the key is still boxed to live in an
  //    `Object[]`.
  def orderByInterleaving(data: List[Ticket]): List[Ticket] =
    val elements = data.stdlib
    val count = elements.length
    val array = new scala.Array[AnyRef](count*2)
    var index = 0
    var rest = elements

    while index < count do
      val ticket = rest.head
      array(index*2) = ticket.number.asInstanceOf[AnyRef]
      array(index*2 + 1) = ticket
      rest = rest.tail
      index += 1

    quicksortInterleaved(array, 0, count)

    List.from:
      scala.collection.immutable.Range(0, count).iterator.map: index =>
        array(index*2 + 1).asInstanceOf[Ticket]

  // 3. Two arrays, the keys in a primitive one, permuted in step. Nothing is boxed and nothing
  //    is allocated per element.
  def orderByParallel(data: List[Ticket]): List[Ticket] =
    val elements = data.stdlib
    val keys = new scala.Array[Int](elements.length)
    val values = new scala.Array[AnyRef](elements.length)
    var index = 0
    var rest = elements

    while index < keys.length do
      val ticket = rest.head
      keys(index) = ticket.number
      values(index) = ticket
      rest = rest.tail
      index += 1

    quicksortParallel(keys, values, 0, keys.length)
    List.from(values.iterator.map(_.asInstanceOf[Ticket]))

  // 4. No keys carried at all: the projection is applied inside the comparison instead, twice
  //    per comparison rather than once per element. Nothing is allocated and no algorithm needs
  //    to know anything — but a costly projection would be evaluated n log n times over.
  def orderByRecomputing(data: List[Ticket]): List[Ticket] =
    val elements = data.stdlib
    val count = elements.length
    val array = new scala.Array[AnyRef](count)
    var index = 0
    var rest = elements

    while index < count do
      array(index) = rest.head
      rest = rest.tail
      index += 1

    quicksortRecomputing(array, 0, count)
    List.from(array.iterator.map(_.asInstanceOf[Ticket]))

  private def quicksortRecomputing(array: scala.Array[AnyRef], from: Int, to: Int): Unit =
    inline def key(index: Int): Int = array(index).asInstanceOf[Ticket].number

    def swap(left: Int, right: Int): Unit =
      val value = array(left)
      array(left) = array(right)
      array(right) = value

    if to - from < 12 then
      var index = from + 1

      while index < to do
        val element = array(index)
        val elementKey = element.asInstanceOf[Ticket].number
        var hole = index

        while hole > from && key(hole - 1) > elementKey do
          array(hole) = array(hole - 1)
          hole -= 1

        array(hole) = element
        index += 1
    else
      val pivot = key(from + (to - from)/2)
      var left = from
      var right = to - 1

      while left <= right do
        while key(left) < pivot do left += 1
        while key(right) > pivot do right -= 1

        if left <= right then
          swap(left, right)
          left += 1
          right -= 1

      quicksortRecomputing(array, from, right + 1)
      quicksortRecomputing(array, left, to)

  // The library's `order`, for scale.
  def orderByLibrary(data: List[Ticket]): List[Ticket] =
    import sortingAlgorithms.quicksort
    data.order(_.number)

  private def quicksortPairs(array: scala.Array[AnyRef], from: Int, to: Int): Unit =
    inline def key(index: Int): Int = array(index).asInstanceOf[Pair].key.asInstanceOf[Int]

    def swap(left: Int, right: Int): Unit =
      val value = array(left)
      array(left) = array(right)
      array(right) = value

    if to - from < 12 then
      var index = from + 1

      while index < to do
        val element = array(index)
        val elementKey = element.asInstanceOf[Pair].key.asInstanceOf[Int]
        var hole = index

        while hole > from && key(hole - 1) > elementKey do
          array(hole) = array(hole - 1)
          hole -= 1

        array(hole) = element
        index += 1
    else
      val pivot = key(from + (to - from)/2)
      var left = from
      var right = to - 1

      while left <= right do
        while key(left) < pivot do left += 1
        while key(right) > pivot do right -= 1

        if left <= right then
          swap(left, right)
          left += 1
          right -= 1

      quicksortPairs(array, from, right + 1)
      quicksortPairs(array, left, to)

  private def quicksortInterleaved(array: scala.Array[AnyRef], from: Int, to: Int): Unit =
    inline def key(index: Int): Int = array(index*2).asInstanceOf[Int]

    def swap(left: Int, right: Int): Unit =
      val leftKey = array(left*2)
      val leftValue = array(left*2 + 1)
      array(left*2) = array(right*2)
      array(left*2 + 1) = array(right*2 + 1)
      array(right*2) = leftKey
      array(right*2 + 1) = leftValue

    if to - from < 12 then
      var index = from + 1

      while index < to do
        val elementKey = array(index*2)
        val element = array(index*2 + 1)
        var hole = index

        while hole > from && key(hole - 1) > elementKey.asInstanceOf[Int] do
          array(hole*2) = array((hole - 1)*2)
          array(hole*2 + 1) = array((hole - 1)*2 + 1)
          hole -= 1

        array(hole*2) = elementKey
        array(hole*2 + 1) = element
        index += 1
    else
      val pivot = key(from + (to - from)/2)
      var left = from
      var right = to - 1

      while left <= right do
        while key(left) < pivot do left += 1
        while key(right) > pivot do right -= 1

        if left <= right then
          swap(left, right)
          left += 1
          right -= 1

      quicksortInterleaved(array, from, right + 1)
      quicksortInterleaved(array, left, to)

  private def quicksortParallel
    (keys: scala.Array[Int], values: scala.Array[AnyRef], from: Int, to: Int)
  :   Unit =

    def swap(left: Int, right: Int): Unit =
      val key = keys(left)
      keys(left) = keys(right)
      keys(right) = key
      val value = values(left)
      values(left) = values(right)
      values(right) = value

    if to - from < 12 then
      var index = from + 1

      while index < to do
        val elementKey = keys(index)
        val element = values(index)
        var hole = index

        while hole > from && keys(hole - 1) > elementKey do
          keys(hole) = keys(hole - 1)
          values(hole) = values(hole - 1)
          hole -= 1

        keys(hole) = elementKey
        values(hole) = element
        index += 1
    else
      val pivot = keys(from + (to - from)/2)
      var left = from
      var right = to - 1

      while left <= right do
        while keys(left) < pivot do left += 1
        while keys(right) > pivot do right -= 1

        if left <= right then
          swap(left, right)
          left += 1
          right -= 1

      quicksortParallel(keys, values, from, right + 1)
      quicksortParallel(keys, values, left, to)

  // Everything the library's path does apart from the sort itself: drain the traversal into a
  // scratch array and rebuild a `List` from it. Subtracting this from the sort leaves the sort.
  def roundTrip(data: List[Int]): Int =
    val array =
      data.stdlib.iterator.map(_.asInstanceOf[AnyRef])
      . toArray(using scala.reflect.ClassTag.AnyRef)

    List.from(array.iterator.map(_.asInstanceOf[Int])).stdlib.length

  // The same for the standard library's `sorted`, which knows how many elements are coming: one
  // allocation of exactly the right size, filled by `copyToArray`, and a builder told the size.
  def stdlibRoundTrip(data: List[Int]): Int =
    val list = data.stdlib
    val array = new scala.Array[AnyRef](list.length)
    list.asInstanceOf[scala.collection.immutable.List[AnyRef]].copyToArray(array)
    val builder = scala.collection.immutable.List.newBuilder[Int]
    builder.sizeHint(array.length)
    var index = 0

    while index < array.length do
      builder += array(index).asInstanceOf[Int]
      index += 1

    builder.result().length

  // The stdlib's own sort over the same data, as the outside reference point: it is the
  // same algorithm as `Timsort` (the JDK's object sort), so the difference between them is
  // the cost of Soundness's typeclass machinery, not of sorting.
  def stdlibSorted(data: List[Int]): scala.collection.immutable.List[Int] =
    data.stdlib.sorted

  // ─── benchmarks ───────────────────────────────────────────────────────────

  // The sizes each algorithm is measured at. The quadratic sorts are omitted above a
  // thousand elements: at ten thousand a bubble sort is a hundred million comparisons, and
  // the cell would cost minutes to say what the smaller cells already say.
  private val sizes = scala.Seq(100, 1000, 10000, 100000)
  private val quadraticLimit = 1000

  def run(): Unit =
    // The three layouts are only worth timing if they agree, and with the library's `order`.
    val sample = ticketData(0, 5000)
    val byLibrary = orderByLibrary(sample)

    val agree =
      orderByPairs(sample) == byLibrary && orderByInterleaving(sample) == byLibrary
      && orderByParallel(sample) == byLibrary && orderByRecomputing(sample) == byLibrary

    java.lang.System.out.nn.println(s"layouts agree with the library: $agree")

    val bench = Bench()

    // Each suite fixes one input shape and crosstabs algorithm against size, so reading
    // down a column shows how an algorithm scales, and reading the same cell across suites
    // shows what the input's existing order is worth to it.
    def sweep(shape: Shape, name: Message): Unit =
      val kind = shape.ordinal

      bench(name)(target = 250*Milli(Second), baseline = Algorithm.Choice.Timsort)
      . over(Axis(Algorithm.Choice), Axis(t"size")(sizes*)):
          case (Algorithm.Choice.Timsort, size) =>
            '{ murmuration.Benchmarks.timsorted(murmuration.Benchmarks.data($kind, $size)) }

          case (Algorithm.Choice.Powersort, size) =>
            '{ murmuration.Benchmarks.powersorted(murmuration.Benchmarks.data($kind, $size)) }

          case (Algorithm.Choice.Quicksort, size) =>
            '{ murmuration.Benchmarks.quicksorted(murmuration.Benchmarks.data($kind, $size)) }

          case (Algorithm.Choice.Heapsort, size) =>
            '{ murmuration.Benchmarks.heapsorted(murmuration.Benchmarks.data($kind, $size)) }

          case (Algorithm.Choice.InsertionSort, size) if size <= quadraticLimit =>
            '{ murmuration.Benchmarks.insertionSorted(murmuration.Benchmarks.data($kind, $size)) }

          case (Algorithm.Choice.BubbleSort, size) if size <= quadraticLimit =>
            '{ murmuration.Benchmarks.bubbleSorted(murmuration.Benchmarks.data($kind, $size)) }

    suite(m"Sorting by input shape"):
      sweep(Shape.Shuffled, m"shuffled")
      sweep(Shape.Ascending, m"already in order")
      sweep(Shape.Descending, m"in reverse order")
      sweep(Shape.NearlyOrdered, m"nearly in order")
      sweep(Shape.Runs, m"eight ascending runs")
      sweep(Shape.FewDistinct, m"eight distinct values")

    // Every generic instance traverses its receiver into a scratch array and rebuilds the
    // result from it, so the receiver's shape ought to cost only that traversal and rebuild,
    // not anything about the sort itself. These two pairs measure whether that is so, and what
    // the one shape-specialized instance is worth.
    suite(m"Receiver shape"):
      bench(m"Timsort over a List")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          '{ murmuration.Benchmarks.timsorted(murmuration.Benchmarks.data(${Expr(0)}, $size)) }

      bench(m"Timsort over a Sequence")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.sequence(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.timsortedSequence($input) }

      bench(m"Timsort over an array, in place")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.data(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.sortedInPlace($input) }

      // The copy the benchmark above makes to have something to consume, and nothing else: what
      // it measures has to be subtracted from that one to leave the sort.
      bench(m"the copy that in-place sorting consumes")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.data(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.copyOnly($input) }

      // The same three over elements that are references: here sorting in place really is the
      // algorithm and nothing else, with no boxing, no scratch array and no rebuild.
      bench(m"Timsort over a List of references")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.ticketData(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.sortedTickets($input) }

      bench(m"Timsort over an array of references, in place")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.ticketData(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.sortedTicketsInPlace($input) }

      bench(m"the copy that consumes, for references")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.ticketData(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.copyTicketsOnly($input) }

    // What the typeclass machinery costs over calling the standard library directly, on the
    // algorithm the standard library uses.
    suite(m"Against the standard library"):
      bench(m"Timsort through Sortable")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          '{ murmuration.Benchmarks.timsorted(murmuration.Benchmarks.data(${Expr(0)}, $size)) }

      bench(m"Timsort through Sortable, counting first")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.data(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.timsortedCounted($input) }

      bench(m"the standard library's own sort")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          '{ murmuration.Benchmarks.stdlibSorted(murmuration.Benchmarks.data(${Expr(0)}, $size)) }

      bench(m"ten thousand comparisons through the inline operator")(target = 250*Milli(Second)):
        '{ murmuration.Benchmarks.ascendingByOperator }

      bench(m"ten thousand comparisons through Comparable")(target = 250*Milli(Second)):
        '{ murmuration.Benchmarks.ascendingByComparable }

      // What boxing and decoration still cost.
      bench(m"an Int array sorted in place, boxing into scratch")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.data(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.quicksortIntArrayInPlace($input) }

      bench(m"the JDK's primitive sort, boxing nothing")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.data(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.jdkPrimitiveSort($input) }

      bench(m"sorting by the elements themselves")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.data(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.sortWithoutKeys($input) }

      bench(m"ordering by a projection, which decorates")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.data(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.orderByIdentity($input) }

      // How `order` might carry its keys.
      bench(m"keys in an object beside each element")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.ticketData(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.orderByPairs($input) }

      bench(m"keys interleaved with the elements")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.ticketData(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.orderByInterleaving($input) }

      bench(m"keys in a primitive array beside the elements")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.ticketData(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.orderByParallel($input) }

      bench(m"no keys carried, the projection recomputed")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.ticketData(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.orderByRecomputing($input) }

      bench(m"the library's order, for scale")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.ticketData(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.orderByLibrary($input) }

      // Everything but the sort, on each side, so that the sort can be had by subtraction.
      bench(m"the library's path without the sort")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.data(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.roundTrip($input) }

      bench(m"the standard library's path without the sort")(target = 250*Milli(Second))
      . over(Axis(t"size")(sizes*)): size =>
          val input = '{ murmuration.Benchmarks.data(${Expr(0)}, $size) }
          '{ murmuration.Benchmarks.stdlibRoundTrip($input) }
