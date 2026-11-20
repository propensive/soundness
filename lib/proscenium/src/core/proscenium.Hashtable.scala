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

// The flat hash table behind a `Map` made by `Map.build`: one open-addressing array, slot i
// holding its key at 2i and its value at 2i + 1, with the slot's hash beside it in a parallel
// array, probed linearly from a Fibonacci hash and never more than half full. A `null` key
// marks an empty slot, so a `null` key proper is stored as `Absence`.
//
// Keys are compared exactly as the standard library's maps compare them, by `==` and `##`: a
// table and the persistent tree later built from it must agree on which keys are equal, or a
// map would change its answers on its first `define`. Universal equality differs from
// `equals` and `hashCode` only for boxed numbers and characters, so every other key takes
// the direct calls, which are what make a text-keyed table fast.
private[proscenium] object Hashtable:
  object Absence

  // A build of this many entries or fewer yields the standard library's compact small maps
  // rather than a table: they are a third of the size, and as quick to read at one entry. A
  // builder keeps that many packed at the front of its slots, in the order they arrived, and
  // allocates the table only when one more comes.
  inline val Compact = 4

  // The table a fifth entry is spread into: sixteen slots, indexed by a hash's top four bits.
  inline val Capacity = 16
  inline val Shift = 28

  def marked(key: Any): Any = if key == null then Absence else key
  def unmarked(key: Any): Any = if key == Absence then null else key

  def numeric(key: Any): Boolean =
    key.isInstanceOf[java.lang.Number] || key.isInstanceOf[java.lang.Character]

  // Through `java.util.Objects`, which takes any value: the same calls as `hashCode` and
  // `equals`, with the identity test `equals` would make anyway done first.
  def hash(key: Any): Int = if numeric(key) then key.## else java.util.Objects.hashCode(key)

  def same(universal: Boolean, key: Any, found: Any): Boolean =
    if universal then key == found else java.util.Objects.equals(key, found)

  // The position of `key` among the first `count` packed entries, or -1. `key` is `marked`.
  def scan(slots: scala.Array[Any], count: Int, key: Any): Int =
    val universal = numeric(key)
    var index = 0
    var result = -1

    // Bounded by `count`, which is at most `Compact`.
    while result < 0 && index < count do
      if same(universal, key, slots(index << 1)) then result = index else index += 1

    result

  // Spreads `count` packed entries, which are distinct, into an empty table.
  def spread
    ( packed:  scala.Array[Any],
      count:   Int,
      slots2:  scala.Array[Any]^,
      hashes2: scala.Array[Int]^,
      shift:   Int )
  :   Unit =

    val mask = hashes2.length - 1
    var from = 0

    while from < count do
      val key = packed(from << 1)
      val wanted = hash(key)
      var index = (wanted*0x9E3779B9) >>> shift
      while slots2(index << 1) != null do index = (index + 1) & mask
      slots2(index << 1) = key
      slots2((index << 1) + 1) = packed((from << 1) + 1)
      hashes2(index) = wanted
      from += 1

  // The packed entries as one of the standard library's small maps, in the order they came.
  def compact[key, value](slots: scala.Array[Any], count: Int): sci.Map[key, value] =
    def key(index: Int): key = unmarked(slots(index << 1)).asInstanceOf[key]
    def value(index: Int): value = slots((index << 1) + 1).asInstanceOf[value]

    if count == 0 then sci.Map.empty[key, value]
    else if count == 1 then new sci.Map.Map1(key(0), value(0))
    else if count == 2 then new sci.Map.Map2(key(0), value(0), key(1), value(1))
    else if count == 3 then new sci.Map.Map3(key(0), value(0), key(1), value(1), key(2), value(2))
    else
      new sci.Map.Map4(key(0), value(0), key(1), value(1), key(2), value(2), key(3), value(3))

  // The slot holding `key`, or -1. `key` is already `marked`.
  def find(slots: scala.Array[Any], hashes: scala.Array[Int], shift: Int, key: Any): Int =
    val wanted = hash(key)
    val mask = hashes.length - 1
    var index = (wanted*0x9E3779B9) >>> shift
    var result = -2

    // The loop is bounded by the load: at most half the slots are occupied, so a probe
    // sequence always reaches an empty one.
    if numeric(key) then
      while result == -2 do
        val found = slots(index << 1)

        if found == null then result = -1
        else if hashes(index) == wanted && key == found then result = index
        else index = (index + 1) & mask
    else
      while result == -2 do
        val found = slots(index << 1)

        if found == null then result = -1
        else if hashes(index) == wanted && java.util.Objects.equals(key, found) then result = index
        else index = (index + 1) & mask

    result

  // Moves every entry of a full table into an empty one of twice the capacity.
  def rehash
    ( slots:   scala.Array[Any],
      hashes:  scala.Array[Int],
      slots2:  scala.Array[Any]^,
      hashes2: scala.Array[Int]^,
      shift:   Int )
  :   Unit =

    val mask = hashes2.length - 1
    var from = 0

    while from < hashes.length do
      val key = slots(from << 1)

      if key != null then
        val stored = hashes(from)
        var index = (stored*0x9E3779B9) >>> shift
        while slots2(index << 1) != null do index = (index + 1) & mask
        slots2(index << 1) = key
        slots2((index << 1) + 1) = slots((from << 1) + 1)
        hashes2(index) = stored

      from += 1

  // A table's entries as a persistent map: the tree it grows on its first update.
  def persistent[key, value]
    ( slots:   scala.Array[Any],
      builder: scala.collection.mutable.Builder[(key, value), sci.Map[key, value]] )
  :   sci.Map[key, value] =

    var from = 0

    while from < slots.length do
      val key = slots(from)

      if key != null
      then builder.addOne((unmarked(key).asInstanceOf[key], slots(from + 1).asInstanceOf[value]))

      from += 2

    builder.result()

// A table that nothing can write to any longer, as a standard-library immutable map: so it can
// stand behind the opaque `Map` beside the library's own, equal to them and hashed like them.
// Reads go to the table in one probe. The first `updated` or `removed` that changes anything
// builds the persistent tree, once, and every later one shares structure with it as usual; a
// map that is only ever read never pays for the tree at all.
private[proscenium] final class FlatMap[key, +value]
  ( slots: scala.Array[Any]^{}, hashes: scala.Array[Int]^{}, shift: Int, count: Int )
extends sci.AbstractMap[key, value]:

  // [synchronized] the tree is a cache of what the table already determines: racing threads
  // build equal trees, and `@volatile` publishes whichever is kept safely
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var tree: sci.Map[key, value] | Null = null

  private def persistent: sci.Map[key, value] =
    val cached = tree

    if cached != null then cached else
      val built = Hashtable.persistent[key, value](slots, sci.HashMap.newBuilder[key, value])
      tree = built
      built

  private def slot(key: key): Int = Hashtable.find(slots, hashes, shift, Hashtable.marked(key))

  override def size: Int = count
  override def knownSize: Int = count
  override def isEmpty: Boolean = false
  override def contains(key: key): Boolean = slot(key) >= 0

  override def apply(key: key): value =
    val index = slot(key)
    if index < 0 then default(key) else slots((index << 1) + 1).asInstanceOf[value]

  override def getOrElse[value2 >: value](key: key, default: => value2): value2 =
    val index = slot(key)
    if index < 0 then default else slots((index << 1) + 1).asInstanceOf[value]

  def get(key: key): scala.Option[value] =
    val index = slot(key)
    if index < 0 then scala.None else scala.Some(slots((index << 1) + 1).asInstanceOf[value])

  // Over the slot numbers rather than a cursor of its own: the stdlib's range iterator holds
  // the position, and the closures hold only the frozen table.
  def iterator: scala.collection.Iterator[(key, value)] =
    def occupied(index: Int): Boolean = slots(index << 1) != null

    scala.collection.Iterator.range(0, hashes.length).filter(occupied(_)).map: index =>
      ( Hashtable.unmarked(slots(index << 1)).asInstanceOf[key],
        slots((index << 1) + 1).asInstanceOf[value] )

  override def foreachEntry[result](lambda: (key, value) => result): Unit =
    var from = 0

    while from < slots.length do
      val key = slots(from)

      if key != null
      then lambda(Hashtable.unmarked(key).asInstanceOf[key], slots(from + 1).asInstanceOf[value])

      from += 2

  // Removing a key that is not there changes nothing, and so builds nothing.
  def removed(key: key): sci.Map[key, value] =
    if slot(key) < 0 then this else persistent.removed(key)

  def updated[value2 >: value](key: key, value: value2): sci.Map[key, value2] =
    persistent.updated(key, value)
