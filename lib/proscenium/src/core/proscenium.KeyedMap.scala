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

// A map whose keys are kept by a policy other than the universal `==` and `##`: by an
// `Equatable` which says when two keys are the same, and a `Hashable` which agrees with it. It
// stands behind the opaque `Map` as a standard-library immutable map, as `FlatMap` does, and it
// keeps its policy through everything which gives another map of the same keys — `updated`,
// `removed`, concatenation and a mapping of its values. Each key is held in a `Slot`, whose own
// `equals` and `hashCode` are the policy's, in a persistent hash map of the standard library's.
private[proscenium] object KeyedMap:
  final class Slot[key](val key: key, hashable: key is Hashable, equatable: key is Equatable):
    override def hashCode: Int = hashable.hash(key)

    override def equals(that: Any): Boolean =
      that.isInstanceOf[Slot[?]] && equatable.equal(key, that.asInstanceOf[Slot[key]].key)

  def empty[key, value](hashable: key is Hashable, equatable: key is Equatable)
  :   KeyedMap[key, value] =

    new KeyedMap(hashable, equatable, sci.HashMap.empty[Slot[key], (key, value)])

private[proscenium] final class KeyedMap[key, +value]
  ( hashable:   key is Hashable,
    equatable:  key is Equatable,
    entries:    sci.HashMap[KeyedMap.Slot[key], (key, value)] )
extends sci.AbstractMap[key, value]:

  private def slot(key: key): KeyedMap.Slot[key] = new KeyedMap.Slot(key, hashable, equatable)

  override def size: Int = entries.size
  override def knownSize: Int = entries.size
  override def isEmpty: Boolean = entries.isEmpty
  override def contains(key: key): Boolean = entries.contains(slot(key))

  override def getOrElse[value2 >: value](key: key, default: => value2): value2 =
    val found: (key, value) | Null = entries.getOrElse[(key, value) | Null](slot(key), null)
    if found == null then default else found._2

  def get(key: key): scala.Option[value] =
    val found: (key, value) | Null = entries.getOrElse[(key, value) | Null](slot(key), null)
    if found == null then scala.None else scala.Some(found._2)

  def iterator: scala.collection.Iterator[(key, value)] = entries.valuesIterator

  def removed(key: key): KeyedMap[key, value] =
    val slot0: KeyedMap.Slot[key] = slot(key)

    if !entries.contains(slot0) then this
    else new KeyedMap(hashable, equatable, entries.removed(slot0))

  // The key already held is kept, as a standard-library map keeps it: only the value changes.
  def updated[value2 >: value](key: key, value: value2): KeyedMap[key, value2] =
    val slot0: KeyedMap.Slot[key] = slot(key)
    val held: (key, value) | Null = entries.getOrElse[(key, value) | Null](slot0, null)
    val key0: key = if held == null then key else held._1
    new KeyedMap[key, value2](hashable, equatable, entries.updated(slot0, (key0, value)))

  def joined[value2 >: value](that: sci.Map[key, value2]): KeyedMap[key, value2] =
    that.foldLeft[KeyedMap[key, value2]](this) { (map, entry) => map.updated(entry._1, entry._2) }

  def transformed[value2](lambda: value => value2): KeyedMap[key, value2] =
    new KeyedMap[key, value2]
      ( hashable, equatable, entries.map { (slot, entry) => (slot, (entry._1, lambda(entry._2))) } )
