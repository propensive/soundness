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

import anticipation.*
import denominative.*
import proscenium.*
import rudiments.*
import vacuous.*

// What `Map.build` is worth, and what a read of a `Map` costs. A map built in place is a flat
// hash table behind the same opaque type as the standard library's persistent tree, so every
// row here but the JDK's is an ordinary `Map`, differing only in how it was made and read.
object MapBench:
  // The contenders for a read. `Tree` and `Built` are `map.at(key)` on a map made by a literal
  // or `from`, and on one made by `Map.build`. The two `Once` rows are the same maps read with a
  // single `getOrElse` against a sentinel, which is what `at` would cost if it probed once
  // rather than twice. `JavaHashMap` is the JDK's table read directly: the bound.
  enum Lookup:
    case Tree, Built, TreeOnce, BuiltOnce, JavaHashMap

  // The contenders for making a map of n entries from nothing, with no size hint.
  enum Build:
    case DefineLoop, StdlibBuilder, MapBuild, JavaHashMap

  // The contenders for a persistent update. `Tree` and `Built` are one `define` on a map that
  // already exists — for a built map, one whose tree has been grown. The `Fresh` rows make the
  // map and then update it once, which is the worst a built map can be asked to do: it pays
  // for its table and then for the tree as well.
  enum Derive:
    case Tree, Built, FreshTree, FreshBuilt

  // What a miss returns: an object, so that the by-name default closes over nothing.
  object Absent

  val probeCount: Int = 1024

  // Keys of kind 0 are strings and of kind 1 boxed integers, as `AnyRef` either way so that
  // one loop serves both. The probes are equal to the keys but not the same instances, as a
  // key read from a document would be; their hashes are computed once and then cached by the
  // JDK, so what is timed is the table, not the hashing of a string.
  final class Fixture(kind: Int, size: Int):
    private def key(index: Int): AnyRef =
      if kind == 0 then new String(s"field-$index") else Integer.valueOf(index*7919 + 13).nn

    private def absent(index: Int): AnyRef =
      if kind == 0 then new String(s"absent-$index") else Integer.valueOf(-(index*7919 + 13)).nn

    val keys: scala.Array[AnyRef] = scala.Array.tabulate(size)(key(_))

    val hits: scala.Array[AnyRef] =
      if size == 0 then new scala.Array[AnyRef](0)
      else scala.Array.tabulate(probeCount) { index => key(((index*2654435761L)%size).toInt) }

    val misses: scala.Array[AnyRef] = scala.Array.tabulate(probeCount)(absent(_))

    // Made one `define` at a time, which is what makes it a persistent tree: `.to[Map]`
    // builds in place, and would give a second table.
    val tree: Map[AnyRef, AnyRef] = defineLoop(keys)
    val built: Map[AnyRef, AnyRef] = mapBuild(keys)

    val java: ju.HashMap[AnyRef, AnyRef] =
      val table = new ju.HashMap[AnyRef, AnyRef]()
      keys.foreach { key => table.put(key, key) }
      table

    val extra: AnyRef = absent(0)

    // A built map that has already grown its tree.
    val grown: Map[AnyRef, AnyRef] =
      val map = mapBuild(keys)
      map.define(extra, extra)
      map

  private val fixtures: ju.HashMap[Int, Fixture] = new ju.HashMap()

  def fixture(kind: Int, size: Int): Fixture =
    val id = size*2 + kind
    val found = fixtures.get(id)

    if found != null then found else
      val made = new Fixture(kind, size)
      fixtures.put(id, made)
      made

  private def probes(fixture: Fixture, hit: Boolean): scala.Array[AnyRef] =
    if hit then fixture.hits else fixture.misses

  // ─── reads: each loop makes `probeCount` lookups and counts those that were found ─────────

  def lookupTree(kind: Int, size: Int, hit: Boolean): Int =
    val data = fixture(kind, size)
    val map = data.tree
    val keys = probes(data, hit)
    var found = 0
    var index = 0

    while index < keys.length do
      if map.at(keys(index)).present then found += 1
      index += 1

    found

  def lookupBuilt(kind: Int, size: Int, hit: Boolean): Int =
    val data = fixture(kind, size)
    val map = data.built
    val keys = probes(data, hit)
    var found = 0
    var index = 0

    while index < keys.length do
      if map.at(keys(index)).present then found += 1
      index += 1

    found

  def lookupTreeOnce(kind: Int, size: Int, hit: Boolean): Int =
    val data = fixture(kind, size)
    val map = data.tree.stdlib
    val keys = probes(data, hit)
    var found = 0
    var index = 0

    while index < keys.length do
      if map.getOrElse(keys(index), Absent) ne Absent then found += 1
      index += 1

    found

  def lookupBuiltOnce(kind: Int, size: Int, hit: Boolean): Int =
    val data = fixture(kind, size)
    val map = data.built.stdlib
    val keys = probes(data, hit)
    var found = 0
    var index = 0

    while index < keys.length do
      if map.getOrElse(keys(index), Absent) ne Absent then found += 1
      index += 1

    found

  def lookupJavaHashMap(kind: Int, size: Int, hit: Boolean): Int =
    val data = fixture(kind, size)
    val map = data.java
    val keys = probes(data, hit)
    var found = 0
    var index = 0

    while index < keys.length do
      if map.getOrDefault(keys(index), Absent) ne Absent then found += 1
      index += 1

    found

  // ─── builds: one map of `size` entries, from nothing ──────────────────────────────────────

  def defineLoop(keys: scala.Array[AnyRef]): Map[AnyRef, AnyRef] =
    var map: Map[AnyRef, AnyRef] = Map()
    var index = 0

    while index < keys.length do
      map = map.define(keys(index), keys(index))
      index += 1

    map

  // The standard library's own builder and its own map, as the rival it is.
  def stdlibBuilder(keys: scala.Array[AnyRef]): sci.Map[AnyRef, AnyRef] =
    val builder = sci.Map.newBuilder[AnyRef, AnyRef]
    var index = 0

    while index < keys.length do
      builder += ((keys(index), keys(index)))
      index += 1

    builder.result()

  def mapBuild(keys: scala.Array[AnyRef]): Map[AnyRef, AnyRef] =
    Map.build[AnyRef, AnyRef]: map =>
      var index = 0

      while index < keys.length do
        map(keys(index)) = keys(index)
        index += 1

  def buildDefineLoop(kind: Int, size: Int): Map[AnyRef, AnyRef] =
    defineLoop(fixture(kind, size).keys)

  def buildStdlibBuilder(kind: Int, size: Int): sci.Map[AnyRef, AnyRef] =
    stdlibBuilder(fixture(kind, size).keys)

  def buildMapBuild(kind: Int, size: Int): Map[AnyRef, AnyRef] = mapBuild(fixture(kind, size).keys)

  def buildJavaHashMap(kind: Int, size: Int): ju.HashMap[AnyRef, AnyRef] =
    val keys = fixture(kind, size).keys
    val table = new ju.HashMap[AnyRef, AnyRef]()
    var index = 0

    while index < keys.length do
      table.put(keys(index), keys(index))
      index += 1

    table

  // ─── one persistent update ────────────────────────────────────────────────────────────────

  def deriveTree(kind: Int, size: Int): Map[AnyRef, AnyRef] =
    val data = fixture(kind, size)
    data.tree.define(data.extra, data.extra)

  def deriveBuilt(kind: Int, size: Int): Map[AnyRef, AnyRef] =
    val data = fixture(kind, size)
    data.grown.define(data.extra, data.extra)

  def deriveFreshTree(kind: Int, size: Int): sci.Map[AnyRef, AnyRef] =
    val data = fixture(kind, size)
    stdlibBuilder(data.keys).updated(data.extra, data.extra)

  def deriveFreshBuilt(kind: Int, size: Int): Map[AnyRef, AnyRef] =
    val data = fixture(kind, size)
    mapBuild(data.keys).define(data.extra, data.extra)

  // Whether the built map agrees with the tree in every respect the suites rely on.
  def agree(kind: Int, size: Int): Boolean =
    val data = fixture(kind, size)
    val hits = if size == 0 then 0 else probeCount

    lookupTree(kind, size, true) == hits && lookupTree(kind, size, false) == 0
    && lookupBuilt(kind, size, true) == hits && lookupBuilt(kind, size, false) == 0
    && lookupTreeOnce(kind, size, true) == hits && lookupTreeOnce(kind, size, false) == 0
    && lookupBuiltOnce(kind, size, true) == hits && lookupBuiltOnce(kind, size, false) == 0
    && lookupJavaHashMap(kind, size, true) == hits && lookupJavaHashMap(kind, size, false) == 0
    && data.built == data.tree && data.tree == data.built && data.grown == data.tree
    && data.built.hashCode == data.tree.hashCode
    && buildDefineLoop(kind, size) == data.tree
    && buildStdlibBuilder(kind, size) == data.tree.stdlib
    && buildMapBuild(kind, size) == data.tree && buildJavaHashMap(kind, size).size == size
    && deriveTree(kind, size) == deriveBuilt(kind, size)
    && deriveFreshTree(kind, size) == deriveFreshBuilt(kind, size).stdlib
