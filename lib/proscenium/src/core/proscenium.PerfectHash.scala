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
package proscenium

import scala.caps
import scala.collection.immutable as sci
import scala.quoted.*

// A perfect hash function for one fixed set of keys, held as data: a short straight-line
// program of cheap operations on the keys' bits, which sends every key of the set to a slot of
// its own below `capacity`. A key is `width` words of `Long`; what the words are is the
// caller's business — the packed bytes of a field name, a text's `hashCode`, an integer.
//
// This is a macro's tool. A macro which knows its keys `search`es while it expands, asks the
// program for each key's `slot` to lay out whatever it is building, and splices the program
// with `quoted`, as inline arithmetic on literals: no table of hashes, no probe loop. Running
// a program with `slot` interprets it step by step, which is what a search and a macro need
// and far too slow for a lookup; nothing should call it at runtime. Because the function is
// data, the search is free to improve, or to be exchanged for another, without any macro
// changing.
//
// A program says nothing about a word that is not a key: it lands on some slot below
// `capacity` (or, after a displacement, below its range), and the caller compares what it
// finds there. That one comparison is the whole cost of a miss.
//
// Only inputs that mean the same on every platform may be given to the search: the keys' own
// bits, and `String.hashCode`, which the language specifies. An identity hash is not
// one of them.
object PerfectHash extends caps.Pure:
  // One operation on the accumulator, a `Long`. `Load`, `Xor` and `Add` take a word of the
  // key; the rest transform what has been accumulated.
  enum Step:
    case Load(word: Int)
    case Xor(word: Int)
    case Add(word: Int)
    case Shift(bits: Int)        // logical, rightwards
    case Fold(bits: Int)         // xor with itself shifted rightwards
    case Rotate(bits: Int)       // leftwards
    case Mask(mask: Long)
    case Multiply(factor: Long)

    // The last step of a program which has one: the top `bits` bits choose an entry of the
    // table, which is added, modulo `range`, to the low half scaled onto `range`.
    case Displace(bits: Int, range: Int)

    // What a step costs to run, relative to the cheapest, for choosing between programs.
    def cost: Int = this match
      case Load(_) | Xor(_) | Add(_) | Shift(_) | Mask(_) => 1
      case Fold(_) | Rotate(_)                            => 2
      case Multiply(_)                                    => 4
      case Displace(_, _)                                 => 8

  // How many candidate programs each strategy may try before the search moves on. The search
  // is deterministic: the same keys and effort always give the same program.
  inline val Effort = 4096

  // [construction-fresh] an empty array, which nothing could write to
  private val untabled: scala.Array[Int]^{} = caps.unsafe.unsafeAssumePure(new scala.Array[Int](0))

  // The multipliers a search tries, in order: the golden ratio, then an unending sequence of
  // odd constants from a fixed generator.
  private def multiplier(index: Int): Long =
    if index == 0 then 0x9E3779B97F4A7C15L else
      var value = index*0x9E3779B97F4A7C15L
      value = (value ^ (value >>> 30))*0xBF58476D1CE4E5B9L
      value = (value ^ (value >>> 27))*0x94D049BB133111EBL
      (value ^ (value >>> 31)) | 1L

  // Mixes every word of a key into one: the first, and then each other xored onto the running
  // product.
  private def mixing(width: Int, factor: Long): sci.List[Step] =
    var steps: sci.List[Step] = sci.Nil
    var word = width - 1

    // Bounded by the width, counting down.
    while word > 0 do
      steps = Step.Multiply(factor) :: Step.Xor(word) :: steps
      word -= 1

    Step.Load(0) :: steps

  // A program which sends each of the keys to a slot of its own, or `null` if the search's
  // effort ran out, as it must when two keys are the same. The keys lie end to end in `keys`,
  // `width` words each.
  //
  // The smallest table is sought first, and for a table of one size the cheapest program:
  // a window of one word's bits; a window of a word folded onto itself, or of two words
  // rotated together; a multiplication. Each is tried for the smallest power of two that
  // holds the keys. Failing those, a displaced program fills exactly as many slots as there
  // are keys, at the price of a table half as long again; and failing that, the cheap programs
  // are tried again for tables of twice and four times the size.
  def search(keys: scala.Array[Long]^{caps.any.rd}, width: Int, effort: Int = Effort)
  :   PerfectHash | Null =

    val count = if width == 0 then 0 else keys.length/width

    if count <= 1
    then new PerfectHash(width, 1, sci.List(Step.Load(0), Step.Mask(0L)), untabled)
    else
      val bits = 32 - Integer.numberOfLeadingZeros(count - 1)
      var found: PerfectHash | Null = direct(keys, width, count, bits, effort)
      if found == null then found = displaced(keys, width, count, effort)
      if found == null then found = direct(keys, width, count, bits + 1, effort)
      if found == null then found = direct(keys, width, count, bits + 2, effort)
      found

  // Whether a program sends the keys to distinct slots below its capacity.
  private def injective
    ( hash: PerfectHash, keys: scala.Array[Long]^{caps.any.rd}, width: Int, count: Int )
  :   Boolean =

    val taken: scala.Array[Boolean]^ = new scala.Array[Boolean](hash.capacity)
    var index = 0
    var distinct = true

    // Bounded by the number of keys.
    while distinct && index < count do
      val slot = hash.slot(keys, index*width)
      val free = slot >= 0 && slot < hash.capacity && !taken(slot)
      if free then taken(slot) = true else distinct = false
      index += 1

    distinct

  // The cheapest program without a table that separates the keys into `1 << bits` slots.
  private def direct
    ( keys: scala.Array[Long]^{caps.any.rd}, width: Int, count: Int, bits: Int, effort: Int )
  :   PerfectHash | Null =

    val capacity = 1 << bits
    val mask = capacity - 1L
    var found: PerfectHash | Null = null
    var tried = 0

    def attempt(steps: sci.List[Step]): Unit =
      tried += 1
      val hash = new PerfectHash(width, capacity, steps, untabled)
      if injective(hash, keys, width, count) then found = hash

    def window(shift: Int): sci.List[Step] =
      if shift == 0 then sci.List(Step.Mask(mask)) else sci.List(Step.Shift(shift), Step.Mask(mask))

    // A window of one word's bits.
    var word = 0

    // Each loop below is bounded by its own counter, and by the effort.
    while found == null && word < width do
      var shift = 0

      while found == null && shift <= 64 - bits do
        attempt(Step.Load(word) :: window(shift))
        shift += 1

      word += 1

    // A window of one word folded onto itself.
    tried = 0
    word = 0

    while found == null && tried < effort && word < width do
      var fold = 3

      while found == null && tried < effort && fold < 64 do
        var shift = 0

        while found == null && tried < effort && shift <= 64 - bits do
          attempt(Step.Load(word) :: Step.Fold(fold) :: window(shift))
          shift += 1

        fold += 5

      word += 1

    // A window of the words rotated and xored together.
    tried = 0

    if width > 1 then
      var rotation = 1

      while found == null && tried < effort && rotation < 64 do
        var steps: sci.List[Step] = sci.List(Step.Load(0))
        var other = 1

        while other < width do
          steps = Step.Xor(other) :: Step.Rotate(rotation) :: steps
          other += 1

        var shift = 0

        while found == null && tried < effort && shift <= 64 - bits do
          attempt(steps.reverse ::: window(shift))
          shift += 1

        rotation += 1

    // A multiplication, keeping the product's top bits.
    tried = 0

    while found == null && tried < effort do
      val factor = multiplier(tried)
      attempt(mixing(width, factor) ::: sci.List(Step.Multiply(factor), Step.Shift(64 - bits)))

    found

  // A program which fills exactly as many slots as there are keys. The keys are mixed and
  // dealt into buckets by their top bits; then, largest bucket first, each bucket is given the
  // first displacement that moves all of its keys onto free slots.
  private def displaced
    ( keys: scala.Array[Long]^{caps.any.rd}, width: Int, count: Int, effort: Int )
  :   PerfectHash | Null =

    val bits = (31 - Integer.numberOfLeadingZeros(count)).max(1)
    val buckets = 1 << bits
    val mixed: scala.Array[Long]^ = new scala.Array[Long](count)
    val first: scala.Array[Int]^ = new scala.Array[Int](buckets)
    val next: scala.Array[Int]^ = new scala.Array[Int](count)
    val sizes: scala.Array[Int]^ = new scala.Array[Int](buckets)
    val taken: scala.Array[Boolean]^ = new scala.Array[Boolean](count)
    val claims: scala.Array[Int]^ = new scala.Array[Int](count)
    var found: PerfectHash | Null = null
    var tried = 0

    // Bounded by the effort; every loop within is bounded by the keys, the buckets or the
    // slots.
    while found == null && tried < effort do
      val factor = multiplier(tried)
      val steps = mixing(width, factor) ::: sci.List(Step.Multiply(factor))
      val mixer = new PerfectHash(width, count, steps, untabled)
      val table: scala.Array[Int]^ = new scala.Array[Int](buckets)
      var bucket = 0

      while bucket < buckets do
        first(bucket) = -1
        sizes(bucket) = 0
        bucket += 1

      var index = 0
      var largest = 0

      while index < count do
        taken(index) = false
        claims(index) = 0
        val value = mixer.mix(keys, index*width)
        val home = (value >>> (64 - bits)).toInt
        mixed(index) = value
        next(index) = first(home)
        first(home) = index
        sizes(home) += 1
        if sizes(home) > largest then largest = sizes(home)
        index += 1

      var size = largest
      var claim = 0
      var free = 0
      var placed = true

      while placed && size > 0 do
        bucket = 0

        while placed && bucket < buckets do
          if sizes(bucket) == size then
            var displacement = 0
            var settled = false

            // A bucket of one key fits any free slot, so it takes the next: the slots are
            // nearly full by now, and trying each displacement in turn would be quadratic.
            if size == 1 then
              while free < count && taken(free) do free += 1

              if free < count then
                val home = spread(mixed(first(bucket)), count, 0)
                displacement = if free >= home then free - home else free - home + count
                settled = true
              else
                displacement = count

            while !settled && displacement < count do
              claim += 1
              var key = first(bucket)
              var free = true

              while free && key >= 0 do
                val slot = spread(mixed(key), count, displacement)
                if taken(slot) || claims(slot) == claim then free = false else claims(slot) = claim
                key = next(key)

              if free then settled = true else displacement += 1

            if settled then
              table(bucket) = displacement
              var key = first(bucket)

              while key >= 0 do
                taken(spread(mixed(key), count, displacement)) = true
                key = next(key)
            else
              placed = false

          bucket += 1

        size -= 1

      if placed then
        // [transfer] freeze: the table is written only above, and never after this
        val frozen: scala.Array[Int]^{} = caps.unsafe.unsafeAssumePure(table)
        val displacing = steps ::: sci.List(Step.Displace(bits, count))
        found = new PerfectHash(width, count, displacing, frozen)

      tried += 1

    found

  // The low half of a mixed word scaled onto `range`, and moved round it by `displacement`.
  private inline def spread(value: Long, range: Int, displacement: Int): Int =
    val moved = (((value & 0xFFFFFFFFL)*range) >>> 32).toInt + displacement
    if moved >= range then moved - range else moved

// A program found by `PerfectHash.search`: `steps`, run over a key of `width` words, give a
// slot below `capacity`, and a different one for each key of the set it was found for.
// `table` is what a final `Displace` reads, and is empty without one.
final class PerfectHash private[proscenium]
  ( val width:    Int,
    val capacity: Int,
    program:      sci.List[PerfectHash.Step],
    val table:    scala.Array[Int]^{} ):

  import PerfectHash.Step

  def steps: List[Step] = List.of(program)

  // What the program costs to run, for comparing one with another.
  def cost: Int = program.foldLeft(0)(_ + _.cost)

  // The program run over a key whose words are read by `fetch`.
  private inline def run(inline fetch: Int => Long): Long =
    var value = 0L
    var rest = program

    // Bounded by the program's length.
    while !rest.isEmpty do
      value = rest.head match
        case Step.Load(word)       => fetch(word)
        case Step.Xor(word)        => value ^ fetch(word)
        case Step.Add(word)        => value + fetch(word)
        case Step.Shift(bits)      => value >>> bits
        case Step.Fold(bits)       => value ^ (value >>> bits)
        case Step.Rotate(bits)     => java.lang.Long.rotateLeft(value, bits)
        case Step.Mask(mask)       => value & mask
        case Step.Multiply(factor) => value*factor

        case Step.Displace(bits, range) =>
          val scaled = (((value & 0xFFFFFFFFL)*range) >>> 32).toInt
          val moved = scaled + table((value >>> (64 - bits)).toInt)

          (if moved >= range then moved - range else moved).toLong

      rest = rest.tail

    value

  private[proscenium] def mix(words: scala.Array[Long]^{caps.any.rd}, offset: Int): Long =
    run: word => words(offset + word)

  // The slot of the key whose words begin at `offset` in `words`: interpreted, for a macro
  // laying out its table and for the search, and not for a lookup at runtime.
  def slot(words: scala.Array[Long]^{caps.any.rd}, offset: Int): Int =
    run { word => words(offset + word) }.toInt

  // The table as a literal, for a macro to bind once and hand to `quoted`. A literal array is
  // built by the method it appears in, and the JVM limits a method's size, so this serves a
  // table of a few thousand entries and no more.
  // Typed as readable rather than frozen: a frozen array's `^{}` is re-freshened to `any.rd`
  // when it is bound inside a quote, and a program only ever reads its table.
  def quotedTable(using Quotes): Expr[scala.Array[Int]^{caps.any.rd}] =
    val entries = sci.List.tabulate(table.length): index => Expr(table(index))
    '{scala.Array[Int](${Varargs(entries)}*)}

  // The program as inline arithmetic over the key's words, every parameter a literal: what a
  // macro splices in place of a hash. `table` is consulted only by a program that displaces,
  // and should be a reference to a value bound once from `quotedTable`, not the literal
  // itself, which would be built anew on every evaluation.
  def quoted(words: List[Expr[Long]])(table: => Expr[scala.Array[Int]^{caps.any.rd}])
    ( using Quotes )
  :   Expr[Int] =

    val result = program.foldLeft('{0L}: Expr[Long]): (value, step) =>
      step match
        case Step.Load(word)       => List.at(words, word)
        case Step.Xor(word)        => '{$value ^ ${List.at(words, word)}}
        case Step.Add(word)        => '{$value + ${List.at(words, word)}}
        case Step.Shift(bits)      => '{$value >>> ${Expr(bits)}}
        case Step.Fold(bits)       => '{val folded = $value; folded ^ (folded >>> ${Expr(bits)})}
        case Step.Rotate(bits)     => '{java.lang.Long.rotateLeft($value, ${Expr(bits)})}
        case Step.Mask(mask)       => '{$value & ${Expr(mask)}}
        case Step.Multiply(factor) => '{$value*${Expr(factor)}}

        case Step.Displace(bits, range) =>
          ' {
              val mixed = $value

              val scaled = (((mixed & 0xFFFFFFFFL)*${Expr(range.toLong)}) >>> 32).toInt
              val moved = scaled + $table((mixed >>> ${Expr(64 - bits)}).toInt)

              (if moved >= ${Expr(range)} then moved - ${Expr(range)} else moved).toLong
            }

    '{$result.toInt}

  override def toString: String = program.mkString("PerfectHash(", ", ", s") into $capacity")
