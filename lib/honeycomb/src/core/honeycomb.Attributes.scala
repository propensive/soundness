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
package honeycomb

import java.lang as jl

import scala.collection.immutable.ListMap

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

// `Attributes` is an opaque view over an `IArray[String | Null]` whose entries
// alternate keys and values: at index `2k` is the (non-null) attribute label,
// at index `2k+1` is either the attribute value (a non-null `String`) or
// `null` to denote a value-less attribute (e.g. `<input disabled>`). Storing
// the pairs interleaved in a single `IArray` halves the per-`Attributes`
// allocation count compared with the previous two-`IArray` `final class`
// representation, and the opaque alias eliminates the wrapper object itself —
// so a non-empty `Attributes` costs exactly one heap allocation.
//
// Extension methods live inside the companion object so they have transparent
// access to the underlying `IArray`. Structural equality and hashing are
// exposed via dedicated `equalsAttributes` / `hashAttributes` extensions
// rather than `Object.equals`/`hashCode`, since the underlying `IArray` would
// otherwise answer with reference equality. `Element` (the only externally
// significant `Attributes` consumer that needs structural equality) calls
// these extensions explicitly.

opaque type Attributes = IArray[String | Null]

object Attributes:
  val empty: Attributes = IArray.empty[String | Null]

  def apply(pairs: (Text, Optional[Text])*): Attributes =
    if pairs.isEmpty then empty else
      val n = pairs.length
      val arr = new Array[String | Null](n*2)
      var i = 0
      pairs.foreach: pair =>
        arr(i*2) = pair._1.s
        arr(i*2 + 1) = pair._2.lay(null: String | Null)(_.s)
        i += 1
      arr.immutable(using Unsafe)

  def from(map: Map[Text, Optional[Text]]): Attributes =
    if map.isEmpty then empty else
      val n = map.size
      val arr = new Array[String | Null](n*2)
      var i = 0
      map.foreach: (k, v) =>
        arr(i*2) = k.s
        arr(i*2 + 1) = v.lay(null: String | Null)(_.s)
        i += 1
      arr.immutable(using Unsafe)

  // Construct an `Attributes` directly from an interleaved `IArray`. The
  // caller guarantees the array's length is even and that every key slot
  // (even index) holds a non-null `String`. Used by the parser, which
  // assembles the interleaved array as it tokenizes attributes.
  private[honeycomb] inline def fromInterleaved(array: IArray[String | Null]): Attributes = array

  // Unwrap to the raw `Array[String | Null]` for hot-path internal access.
  // Safe within the package: the storage is shared but never mutated outside
  // construction.
  private[honeycomb] inline def storage(attrs: Attributes): Array[String | Null] =
    attrs.asInstanceOf[Array[String | Null]]

  extension (attrs: Attributes)
    inline def size: Int = attrs.length/2
    inline def isEmpty: Boolean = attrs.length == 0
    inline def nonEmpty: Boolean = attrs.length > 0
    inline def nil: Boolean = attrs.length == 0

    def apply(key: Text): Optional[Text] =
      val a = storage(attrs)
      val keyStr: String = key.s
      val n = a.length
      var i = 0
      while i < n do
        if a(i) == keyStr then
          val value = a(i + 1)
          return if value == null then Unset else value.asInstanceOf[Text]
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

    def values: Iterator[Optional[Text]] =
      val a = storage(attrs)
      new Iterator[Optional[Text]]:
        private var i: Int = 1
        def hasNext: Boolean = i < a.length

        def next(): Optional[Text] =
          val v = a(i)
          i += 2
          if v == null then Unset else v.asInstanceOf[Text]

    def iterator: Iterator[(Text, Optional[Text])] =
      val a = storage(attrs)
      new Iterator[(Text, Optional[Text])]:
        private var i: Int = 0
        def hasNext: Boolean = i < a.length

        def next(): (Text, Optional[Text]) =
          val k = a(i).asInstanceOf[Text]
          val v = a(i + 1)
          i += 2
          (k, if v == null then Unset else v.asInstanceOf[Text])

    def toList: List[(Text, Optional[Text])] =
      val a = storage(attrs)
      val b = List.newBuilder[(Text, Optional[Text])]
      var i = 0
      while i < a.length do
        val v = a(i + 1)
        b += ((a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text]))
        i += 2
      b.result()

    def toMap: Map[Text, Optional[Text]] =
      val a = storage(attrs)
      if a.length == 0 then Map() else
        val b = ListMap.newBuilder[Text, Optional[Text]]
        var i = 0
        while i < a.length do
          val v = a(i + 1)
          b += ((a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text]))
          i += 2
        Map.from(b.result())

    def map[B](f: ((Text, Optional[Text])) => B): List[B] =
      val a = storage(attrs)
      val b = List.newBuilder[B]
      var i = 0
      while i < a.length do
        val v = a(i + 1)
        b += f((a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text]))
        i += 2
      b.result()

    def foreach[U](f: ((Text, Optional[Text])) => U): Unit =
      val a = storage(attrs)
      var i = 0
      while i < a.length do
        val v = a(i + 1)
        f((a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text]))
        i += 2

    def each(action: (Text, Optional[Text]) => Unit): Unit =
      val a = storage(attrs)
      var i = 0
      while i < a.length do
        val v = a(i + 1)
        action(a(i).asInstanceOf[Text], if v == null then Unset else v.asInstanceOf[Text])
        i += 2

    // O(N): finds and excludes the matching index, returning a new `Attributes`
    // whose backing IArray is two slots shorter. If the key is absent, returns
    // `attrs` unchanged so callers don't allocate gratuitously.
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
        val nu = new Array[String | Null](n - 2)
        if idx > 0 then jl.System.arraycopy(a, 0, nu, 0, idx)
        if idx < n - 2 then jl.System.arraycopy(a, idx + 2, nu, idx, n - 2 - idx)
        nu.immutable(using Unsafe)

    inline def `-`(key: Text): Attributes = removed(key)

    def `--`(others: List[Text]): Attributes =
      if isEmpty then attrs else
        var result: Attributes = attrs
        others.scala.foreach { k => result = result.removed(k) }
        result

    // Updates an existing key in place (preserving order) or appends a new pair
    // at the end. Always returns a new `Attributes` (the IArray is immutable).
    def updated(key: Text, value: Optional[Text]): Attributes =
      val a = storage(attrs)
      val keyStr: String = key.s
      val n = a.length
      var idx = -1
      var i = 0
      while idx < 0 && i < n do
        if a(i) == keyStr then idx = i
        i += 2
      if idx >= 0 then
        val nu = new Array[String | Null](n)
        jl.System.arraycopy(a, 0, nu, 0, n)
        nu(idx + 1) = value.lay(null: String | Null)(_.s)
        nu.immutable(using Unsafe)
      else
        val nu = new Array[String | Null](n + 2)
        jl.System.arraycopy(a, 0, nu, 0, n)
        nu(n) = keyStr
        nu(n + 1) = value.lay(null: String | Null)(_.s)
        nu.immutable(using Unsafe)

    // Combines two `Attributes`, with the right-hand side overriding duplicate
    // keys (matching `Map ++` semantics). Order: left's keys first (preserving
    // their order), then any new keys from the right.
    def `++`(other: Attributes): Attributes =
      val a = storage(attrs)
      val b = storage(other)
      if b.length == 0 then attrs
      else if a.length == 0 then other
      else
        val total = a.length + b.length
        val nu = new Array[String | Null](total)
        var written = 0
        var i = 0
        while i < a.length do
          val k = a(i).asInstanceOf[String]
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
          val k = b(j).asInstanceOf[String]
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
          val tu = new Array[String | Null](written)
          jl.System.arraycopy(nu, 0, tu, 0, written)
          tu.immutable(using Unsafe)

    def `++`(other: Map[Text, Optional[Text]]): Attributes =
      if other.isEmpty then attrs else attrs ++ Attributes.from(other)

    // Structural equality: same key/value pairs in the same order. Provided
    // explicitly because `Object.equals` on the underlying `IArray` would
    // give reference equality.
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
          val k = a(i).asInstanceOf[String]
          val va = a(i + 1)
          // Locate key in `b`.
          var j = 0
          var found = -1
          while found < 0 && j < n do
            if b(j) == k then found = j
            j += 2
          if found < 0 then ok = false
          else
            val vb = b(found + 1)
            if va != vb then ok = false
          i += 2
        ok

    def hashAttributes: Int =
      // Order-independent: XOR of (key.hash * 31 ^ value.hash) — matches the
      // previous `Attributes.hashCode` contract.
      val a = storage(attrs)
      var h = 0
      var i = 0
      while i < a.length do
        val k = a(i)
        val v = a(i + 1)
        val kh = if k == null then 0 else k.hashCode
        val vh = if v == null then Unset.hashCode else v.hashCode
        h = h ^ (kh*31 ^ vh)
        i += 2
      h

    def showAttributes: String =
      val a = storage(attrs)
      val sb = new jl.StringBuilder("Attributes(")
      var i = 0
      while i < a.length do
        if i > 0 then sb.append(", ")
        sb.append(a(i).asInstanceOf[String])
        sb.append(" -> ")
        val v = a(i + 1)
        sb.append(if v == null then Unset.toString else v.toString)
        i += 2

      sb.append(")").toString
