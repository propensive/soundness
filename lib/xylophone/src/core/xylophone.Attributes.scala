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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package xylophone

import java.lang as jl

import scala.collection.immutable.ListMap

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

opaque type Attributes = IArray[String]

object Attributes:
  val empty: Attributes = IArray.empty[String]

  def apply(pairs: (Text, Text)*): Attributes =
    if pairs.isEmpty then empty else
      val n = pairs.length
      val arr = new Array[String](n*2)
      var i = 0

      pairs.foreach: pair =>
        arr(i*2) = pair._1.s
        arr(i*2 + 1) = pair._2.s
        i += 1

      arr.immutable(using Unsafe)

  def from(map: Map[Text, Text]): Attributes =
    if map.isEmpty then empty else
      val n = map.size
      val arr = new Array[String](n*2)
      var i = 0
      map.foreach: (k, v) =>
        arr(i*2) = k.s
        arr(i*2 + 1) = v.s
        i += 1
      arr.immutable(using Unsafe)

  // Construct an `Attributes` directly from an interleaved `IArray`. The
  // caller guarantees the array's length is even and that every key slot
  // (even index) holds a non-null `String`. Used by the parser, which
  // assembles the interleaved array as it tokenizes attributes.
  private[xylophone] inline def fromInterleaved(array: IArray[String]): Attributes = array

  // Unwrap to the raw `Array[String]` for hot-path internal access. Safe
  // within the package: the storage is shared but never mutated outside
  // construction.
  private[xylophone] inline def storage(attrs: Attributes): Array[String] =
    attrs.asInstanceOf[Array[String]]

  extension (attrs: Attributes)
    inline def size: Int = attrs.length/2
    inline def isEmpty: Boolean = attrs.length == 0
    inline def nonEmpty: Boolean = attrs.length > 0
    inline def nil: Boolean = attrs.length == 0

    def apply(key: Text): Text =
      val a = storage(attrs)
      val keyStr: String = key.s
      val n = a.length
      var i = 0
      while i < n do
        if a(i) == keyStr then return a(i + 1).asInstanceOf[Text]
        i += 2
      throw new java.util.NoSuchElementException(s"key not found: $key")

    def at(key: Text): Optional[Text] =
      val a = storage(attrs)
      val keyStr: String = key.s
      val n = a.length
      var i = 0
      while i < n do
        if a(i) == keyStr then return a(i + 1).asInstanceOf[Text]
        i += 2
      Unset

    def contains(key: Text): Boolean =
      val a = storage(attrs)
      val keyStr: String = key.s
      val n = a.length
      var i = 0
      while i < n do
        if a(i) == keyStr then return true
        i += 2
      false

    def keys: Iterator[Text] =
      val a = storage(attrs)
      new Iterator[Text]:
        private var i: Int = 0
        def hasNext: Boolean = i < a.length

        def next(): Text =
          val k = a(i).asInstanceOf[Text]
          i += 2
          k

    def values: Iterator[Text] =
      val a = storage(attrs)
      new Iterator[Text]:
        private var i: Int = 1
        def hasNext: Boolean = i < a.length

        def next(): Text =
          val v = a(i).asInstanceOf[Text]
          i += 2
          v

    def iterator: Iterator[(Text, Text)] =
      val a = storage(attrs)
      new Iterator[(Text, Text)]:
        private var i: Int = 0
        def hasNext: Boolean = i < a.length

        def next(): (Text, Text) =
          val pair = (a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text])
          i += 2
          pair

    def toMap: Map[Text, Text] =
      val a = storage(attrs)
      if a.length == 0 then Map() else
        val b = ListMap.newBuilder[Text, Text]
        var i = 0
        while i < a.length do
          b += ((a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text]))
          i += 2
        Map.from(b.result())

    def toList: List[(Text, Text)] =
      val a = storage(attrs)
      val b = List.newBuilder[(Text, Text)]
      var i = 0
      while i < a.length do
        b += ((a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text]))
        i += 2
      b.result()

    def each(action: (Text, Text) => Unit): Unit =
      val a = storage(attrs)
      var i = 0
      while i < a.length do
        action(a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text])
        i += 2

    def foreach(action: (Text, Text) => Unit): Unit = each(action)

    def map[B](f: ((Text, Text)) => B): List[B] =
      val a = storage(attrs)
      val b = List.newBuilder[B]
      var i = 0
      while i < a.length do
        b += f((a(i).asInstanceOf[Text], a(i + 1).asInstanceOf[Text]))
        i += 2
      b.result()

    def removed(key: Text): Attributes =
      val a = storage(attrs)
      val keyStr: String = key.s
      val n = a.length
      var idx = -1
      var i = 0
      while idx < 0 && i < n do
        if a(i) == keyStr then idx = i
        i += 2
      if idx < 0 then attrs else
        val nu = new Array[String](n - 2)
        if idx > 0 then jl.System.arraycopy(a, 0, nu, 0, idx)
        if idx < n - 2 then jl.System.arraycopy(a, idx + 2, nu, idx, n - 2 - idx)
        nu.immutable(using Unsafe)

    inline def `-`(key: Text): Attributes = removed(key)

    def `--`(others: List[Text]): Attributes =
      if isEmpty then attrs else
        var result: Attributes = attrs
        others.scala.foreach { k => result = result.removed(k) }
        result

    def updated(key: Text, value: Text): Attributes =
      val a = storage(attrs)
      val keyStr: String = key.s
      val n = a.length
      var idx = -1
      var i = 0
      while idx < 0 && i < n do
        if a(i) == keyStr then idx = i
        i += 2
      if idx >= 0 then
        val nu = new Array[String](n)
        jl.System.arraycopy(a, 0, nu, 0, n)
        nu(idx + 1) = value.s
        nu.immutable(using Unsafe)
      else
        val nu = new Array[String](n + 2)
        jl.System.arraycopy(a, 0, nu, 0, n)
        nu(n) = keyStr
        nu(n + 1) = value.s
        nu.immutable(using Unsafe)

    def `++`(other: Attributes): Attributes =
      val a = storage(attrs)
      val b = storage(other)
      if b.length == 0 then attrs
      else if a.length == 0 then other
      else
        val total = a.length + b.length
        val nu = new Array[String](total)
        var written = 0
        var i = 0
        while i < a.length do
          val k = a(i)
          var bi = 0
          var found = -1
          while found < 0 && bi < b.length do
            if b(bi) == k then found = bi
            bi += 2
          nu(written) = k
          nu(written + 1) = if found >= 0 then b(found + 1) else a(i + 1)
          written += 2
          i += 2
        var j = 0
        while j < b.length do
          val k = b(j)
          var ai = 0
          var found = false
          while !found && ai < a.length do
            if a(ai) == k then found = true
            ai += 2
          if !found then
            nu(written) = k
            nu(written + 1) = b(j + 1)
            written += 2
          j += 2
        if written == total then nu.immutable(using Unsafe)
        else
          val tu = new Array[String](written)
          jl.System.arraycopy(nu, 0, tu, 0, written)
          tu.immutable(using Unsafe)

    def `++`(other: Map[Text, Text]): Attributes =
      if other.isEmpty then attrs else attrs ++ Attributes.from(other)

    // Same set of (key, value) pairs (order-insensitive). Iterates the left,
    // looks up each key in the right.
    def equalsAttributes(other: Attributes): Boolean =
      val a = storage(attrs)
      val b = storage(other)
      val n = a.length
      if n != b.length then false else
        var i = 0
        var ok = true
        while ok && i < n do
          val k = a(i)
          val va = a(i + 1)
          var j = 0
          var found = -1
          while found < 0 && j < n do
            if b(j) == k then found = j
            j += 2
          if found < 0 then ok = false
          else if va != b(found + 1) then ok = false
          i += 2
        ok

    def hashAttributes: Int =
      // Order-independent: XOR of (key.hash * 31 ^ value.hash).
      val a = storage(attrs)
      var h = 0
      var i = 0
      while i < a.length do
        h = h ^ (a(i).hashCode*31 ^ a(i + 1).hashCode)
        i += 2
      h
