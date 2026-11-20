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

// A map whose keys were all known when it was made, read through a perfect hash of them: the
// slot a key would occupy is computed, not searched for, so a read is that computation and
// one comparison, whether or not the key is there, and the table holds no hashes and need be
// no larger than its entries. Slot i holds its key at 2i and its value at 2i + 1; an empty
// slot's key is `null`, and no key is.
//
// The hash is the one abstract member, `index`, because the class that supplies it is
// generated for one set of keys — by code staged when the keys become known (murmuration's
// `optimize`). A subclass rather than a function held in a field: a call site which only
// ever sees one such map can then have `index` inlined into its read, where a shared field's
// call could not be.
//
// Like a table from `Map.build`, it stands behind the opaque `Map` as a standard-library map,
// equal to the others and hashed like them, and its first update that changes anything builds
// the persistent tree.
abstract class PerfectMap[key, +value](slots: scala.Array[Any]^{}, count: Int)
extends sci.AbstractMap[key, value]:

  // The slot `key` occupies if it is defined: always a slot of the table, for any value of
  // the kind the keys are, and -1 for a value of any other kind.
  def index(key: Any): Int

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

  // A value of another kind than the keys can still equal one of them, as `1L` equals `1`,
  // so it is looked for the slow way. Bounded by the table.
  private def scan(key: Any): Int =
    var from = 0
    var result = -1

    while result < 0 && from < slots.length do
      val found = slots(from)
      if found != null && key == found then result = from >> 1
      from += 2

    result

  private def slot(key: key): Int =
    val at = index(key)

    if at < 0 then scan(key) else if java.util.Objects.equals(key, slots(at << 1)) then at else -1

  override def size: Int = count
  override def knownSize: Int = count
  override def isEmpty: Boolean = false
  override def contains(key: key): Boolean = slot(key) >= 0

  override def apply(key: key): value =
    val at = slot(key)
    if at < 0 then default(key) else slots((at << 1) + 1).asInstanceOf[value]

  override def getOrElse[value2 >: value](key: key, default: => value2): value2 =
    val at = slot(key)
    if at < 0 then default else slots((at << 1) + 1).asInstanceOf[value]

  def get(key: key): scala.Option[value] =
    val at = slot(key)
    if at < 0 then scala.None else scala.Some(slots((at << 1) + 1).asInstanceOf[value])

  def iterator: scala.collection.Iterator[(key, value)] =
    def occupied(at: Int): Boolean = slots(at << 1) != null

    scala.collection.Iterator.range(0, slots.length >> 1).filter(occupied(_)).map: at =>
      (slots(at << 1).asInstanceOf[key], slots((at << 1) + 1).asInstanceOf[value])

  override def foreachEntry[result](lambda: (key, value) => result): Unit =
    var from = 0

    // Bounded by the table.
    while from < slots.length do
      val key = slots(from)
      if key != null then lambda(key.asInstanceOf[key], slots(from + 1).asInstanceOf[value])
      from += 2

  def removed(key: key): sci.Map[key, value] =
    if slot(key) < 0 then this else persistent.removed(key)

  def updated[value2 >: value](key: key, value: value2): sci.Map[key, value2] =
    persistent.updated(key, value)
