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

import scala.collection.immutable as sci

// The third opaque collection alias: an immutable map backed by `sci.Map`. Same design as
// `Sequence` and `Set`: members invisible, API via typeclasses (`Applicable` provides `at`/`defines`/
// `confine`; `Traversable by (key, value)` the transforming surface), construction and the
// greppable `stdlib` bridge in the companion, casts at the boundary, and deliberately NO
// `Conversion` to a stdlib supertype.
// The companion is `Pure`: the umbrella forwards it as a `val`, and under capture checking a
// value of a non-pure object type is a tracked capability, so `xs.to(Map)` — which passes the
// companion as a factory — would otherwise capture the forwarder and freshen its result.
object Map extends scala.caps.Pure:
  // `of` is a plain method, not `inline`: inline expansion of the cast inside capturing
  // lambdas crashes the capture checker's boxer (boxDeeply assertion).
  private[proscenium] def of[key, value](map: sci.Map[key, value]): Map[key, value] =
    map.asInstanceOf[Map[key, value]]

  // A literal is filled in place like any other map made all at once: five entries or more
  // are a flat hash table, fewer the standard library's small maps in the order written.
  def apply[key, value](pairs: (key, value)*): Map[key, value] = filled(pairs)
  def empty[key, value]: Map[key, value] = of(sci.Map.empty[key, value])

  // What is already a map is handed back as it is: a built one recognised here first, since
  // the standard library's `from` passes only its own maps through and would copy a table into
  // a tree. Anything else is filled in place.
  def from[key, value](pairs: IterableOnce[(key, value)]^): Map[key, value] =
    if pairs.isInstanceOf[FlatMap[?, ?]] then of(pairs.asInstanceOf[FlatMap[key, value]])
    else if pairs.isInstanceOf[sci.Map[?, ?]] then of(sci.Map.from(pairs))
    else filled(pairs)

  private def filled[key, value](pairs: IterableOnce[(key, value)]^): Map[key, value] =
    val builder: Builder[key, value]^ = new Builder[key, value]()
    val iterator = pairs.iterator

    // Terminated by the source's own state.
    while iterator.hasNext do
      val pair = iterator.next()
      builder(pair._1) = pair._2

    of(builder.result())

  // Allocate → mutate → freeze, for a map: the block is lent an exclusive `Builder` to fill in
  // place, and what it has filled comes back as an ordinary immutable `Map` without being
  // copied —
  //     Map.build[Int, Text]: map =>
  //       map(8) = t"eight"
  // The builder is a capability the block cannot let escape, so nothing can write to the table
  // once it is a `Map`. Five entries or more are a flat hash table (see `Hashtable`), which
  // builds and reads several times faster than the persistent tree, and grows one only if the
  // map is later updated; fewer are the standard library's compact small maps.
  def build[key, value](lambda: Builder[key, value]^ => Unit): Map[key, value] =
    val builder: Builder[key, value]^ = new Builder[key, value]()
    lambda(builder)
    of(builder.result())

  // The map while it is being filled. Only an exclusive reference can write to it, and the
  // only one there is belongs to the block `build` runs. Up to `Hashtable.Compact` entries are
  // kept packed at the front of `slots` and scanned; `hashes` stays empty until one more
  // arrives, when the table is allocated and they are spread into it.
  final class Builder[key, value] private[proscenium] () extends scala.caps.Mutable:
    private var slots: scala.Array[Any]^ = new scala.Array[Any](Hashtable.Compact << 1)
    private var hashes: scala.Array[Int]^ = new scala.Array[Int](0)
    private var shift: Int = Hashtable.Shift
    private var count: Int = 0

    def size: Int = count

    def defines(key: key): Boolean =
      val marked = Hashtable.marked(key)

      if hashes.length == 0 then Hashtable.scan(slots, count, marked) >= 0
      else Hashtable.find(slots, hashes, shift, marked) >= 0

    update def update(key: key, value: value): Unit =
      val marked = Hashtable.marked(key)

      if hashes.length > 0 then insert(marked, value) else
        val index = Hashtable.scan(slots, count, marked)

        if index >= 0 then slots((index << 1) + 1) = value
        else if count < Hashtable.Compact then
          slots(count << 1) = marked
          slots((count << 1) + 1) = value
          count += 1
        else
          val slots2 = new scala.Array[Any](Hashtable.Capacity << 1)
          val hashes2 = new scala.Array[Int](Hashtable.Capacity)
          Hashtable.spread(slots, count, slots2, hashes2, shift)
          slots = slots2
          hashes = hashes2
          insert(marked, value)

    // The probe is written here rather than in `Hashtable`: both arrays belong to this
    // builder, and two exclusive arguments may not share an owner.
    private update def insert(marked: Any, value: value): Unit =
      val wanted = Hashtable.hash(marked)
      val universal = Hashtable.numeric(marked)
      val mask = hashes.length - 1
      var index = (wanted*0x9E3779B9) >>> shift
      var done = false

      // Bounded by the load: at most half the slots are occupied, so the probe reaches an
      // empty one.
      while !done do
        val found = slots(index << 1)

        if found == null then
          slots(index << 1) = marked
          slots((index << 1) + 1) = value
          hashes(index) = wanted
          count += 1
          if (count << 1) > hashes.length then grow()
          done = true
        else if hashes(index) == wanted && Hashtable.same(universal, marked, found) then
          slots((index << 1) + 1) = value
          done = true
        else
          index = (index + 1) & mask

    private update def grow(): Unit =
      val capacity = hashes.length << 1
      val slots2 = new scala.Array[Any](capacity << 1)
      val hashes2 = new scala.Array[Int](capacity)
      Hashtable.rehash(slots, hashes, slots2, hashes2, shift - 1)
      slots = slots2
      hashes = hashes2
      shift -= 1

    private[proscenium] def result(): sci.Map[key, value] =
      if hashes.length == 0 then Hashtable.compact[key, value](slots, count) else
        // The block that held the only exclusive reference has returned, so the table's
        // writers are gone, and its arrays can be handed over frozen and uncopied.
        // [transfer] freeze: the builder's block has returned, retiring every writer
        val table = scala.caps.unsafe.unsafeAssumePure(slots)
        // [transfer] freeze: as for the slots
        val stored = scala.caps.unsafe.unsafeAssumePure(hashes)
        new FlatMap[key, value](table, stored, shift, count)

  // `.to[Map]` support (see `List`): the conversion is on `Map.type` only, so it cannot
  // expose members of `Map` values.
  given factory: [key, value] => Conversion[Map.type, scala.collection.Factory[(key, value), Map[key, value]]] =
    _ =>
      new scala.collection.Factory[(key, value), Map[key, value]]:
        def fromSpecific(elements: IterableOnce[(key, value)]^): Map[key, value] =
          Map.from(elements)

        def newBuilder: scala.collection.mutable.Builder[(key, value), Map[key, value]] =
          sci.Map.newBuilder[key, value].mapResult(of(_))

  extension [key, value](map: Map[key, value])
    inline def stdlib: sci.Map[key, value] = map.asInstanceOf[sci.Map[key, value]]

  // Lifting and unlifting for macros; see `List`'s companion for the full rationale
  // (public `from` splice, backticked pattern types, unlift limited to produced shapes).
  given toExpr: [key: {scala.quoted.Type, scala.quoted.ToExpr},
                 value: {scala.quoted.Type, scala.quoted.ToExpr}]
  =>  scala.quoted.ToExpr[Map[key, value]]:
    def apply(map: Map[key, value])(using scala.quoted.Quotes)
    :   scala.quoted.Expr[Map[key, value]] =
      '{Map.from(${scala.quoted.Expr.ofList(map.toList.map(scala.quoted.Expr(_)))})}

  given fromExpr: [key: {scala.quoted.Type, scala.quoted.FromExpr},
                   value: {scala.quoted.Type, scala.quoted.FromExpr}]
  =>  scala.quoted.FromExpr[Map[key, value]]:
    def unapply(expr: scala.quoted.Expr[Map[key, value]])(using scala.quoted.Quotes)
    :   Option[Map[key, value]] =
      expr match
        case '{Map.from($pairs: sci.List[(`key`, `value`)])} =>
          scala.quoted.FromExpr.ListFromExpr[(key, value)].unapply(pairs).map(Map.from(_))

        case _ => None

  // The primitive operations, for the typeclass instances defined in the libraries above;
  // see `List` for the rationale. Within this file the opaque alias is transparent. `read`
  // mirrors the stdlib's `Option`-returning `get`: the ergonomic `Optional` form is layered
  // above, in `vacuous`-aware libraries.
  def size[key, value](map: Map[key, value]): Int = map.size
  def nil[key, value](map: Map[key, value]): Boolean = map.isEmpty
  def defines[key, value](map: Map[key, value], index: key): Boolean = map.contains(index)
  def at[key, value](map: Map[key, value], index: key): value = map(index)
  def read[key, value](map: Map[key, value], index: key): Option[value] = map.get(index)

  // The checked read in one probe: the value for `index`, or `absent`, with no `Option` made
  // and the key hashed once, where `defines` followed by `at` hashes it twice. The by-name
  // default is a constant object, so no closure is allocated; `equals` on it is identity.
  def probe[key, value, absent](map: Map[key, value], index: key, absent: absent)
  :   value | absent =

    val found = map.getOrElse[Any](index, Missing)
    if Missing.equals(found) then absent else found.asInstanceOf[value]

  private object Missing

  def define[key, value](map: Map[key, value], index: key, value0: value): Map[key, value] =
    map.updated(index, value0)

  def omit[key, value](map: Map[key, value], index: key): Map[key, value] = map.removed(index)
  def keys[key, value](map: Map[key, value]): Set[key] = Set.of(map.keySet)
  def values[key, value](map: Map[key, value]): List[value] = List.of(map.values.toList)

  def concat[key, value](left: Map[key, value], right: Map[key, value]): Map[key, value] =
    left ++ right

  // `map` transforms the *values*, keys structural — the shape `Mappable` gives a `Map`.
  // Filled in place: the result is a new map made all at once, of the same size as its source.
  def map[key, value, value2](map: Map[key, value], lambda: value => value2): Map[key, value2] =
    val builder: Builder[key, value2]^ = new Builder[key, value2]()
    val iterator = map.iterator

    // Terminated by the source's own state.
    while iterator.hasNext do
      val pair = iterator.next()
      builder(pair._1) = lambda(pair._2)

    of(builder.result())

  def iterator[key, value](map: Map[key, value]): Iterator[(key, value)] = map.iterator

opaque type Map[key, +value] = sci.Map[key, value]
