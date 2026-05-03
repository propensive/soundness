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

object Attributes:
  val empty: Attributes = new Attributes(IArray.empty, IArray.empty)

  def apply(pairs: (Text, Optional[Text])*): Attributes =
    if pairs.isEmpty then empty else
      val n = pairs.length
      val ks = new Array[Text](n)
      val vs = new Array[Optional[Text]](n)
      var i = 0
      pairs.each: pair =>
        ks(i) = pair._1
        vs(i) = pair._2
        i += 1
      new Attributes(ks.immutable(using Unsafe), vs.immutable(using Unsafe))

  def from(map: Map[Text, Optional[Text]]): Attributes =
    if map.isEmpty then empty else
      val n = map.size
      val ks = new Array[Text](n)
      val vs = new Array[Optional[Text]](n)
      var i = 0
      map.foreach: (k, v) =>
        ks(i) = k
        vs(i) = v
        i += 1
      new Attributes(ks.immutable(using Unsafe), vs.immutable(using Unsafe))

  // Unchecked construction from already-validated parallel arrays. Used by
  // the parser, which has already enforced uniqueness during accumulation.
  private[honeycomb] def fromArrays
                       ( ks: IArray[Text], vs: IArray[Optional[Text]] )
  :   Attributes =

    new Attributes(ks, vs)


// A pair of parallel `IArray`s — keys at `i`, values at `i`. HTML elements
// usually carry 0–5 attributes, so a linear scan is comparable to or faster
// than HashMap probing while costing roughly a third of the bytes. We
// deliberately do *not* extend `Iterable` — it inflates the vtable and
// drags in collection-machinery method dispatch on every call site, which
// measurably hurt the parse hot path on attribute-heavy fixtures. Instead
// we expose only the small set of operations actually called against
// element attributes (`apply`, `iterator`, `keys`, `each`, `map`, `to`,
// `join`, ...) directly here.
final class Attributes private[honeycomb]
                      ( private[honeycomb] val ks: IArray[Text],
                        private[honeycomb] val vs: IArray[Optional[Text]] ):

  def size: Int = ks.length
  def isEmpty: Boolean = ks.length == 0
  def nonEmpty: Boolean = ks.length > 0
  def nil: Boolean = ks.length == 0

  def apply(key: Text): Optional[Text] =
    var i = 0
    while i < ks.length do
      if ks(i) == key then return vs(i)
      i += 1
    Unset

  def contains(key: Text): Boolean =
    var i = 0
    while i < ks.length do
      if ks(i) == key then return true
      i += 1
    false

  def keys: Iterator[Text] = ks.iterator
  def values: Iterator[Optional[Text]] = vs.iterator

  def iterator: Iterator[(Text, Optional[Text])] =
    new Iterator[(Text, Optional[Text])]:
      private var i: Int = 0
      def hasNext: Boolean = i < ks.length
      def next(): (Text, Optional[Text]) =
        val r = (ks(i), vs(i))
        i += 1
        r

  def toList: List[(Text, Optional[Text])] =
    val b = List.newBuilder[(Text, Optional[Text])]
    var i = 0
    while i < ks.length do
      b += ((ks(i), vs(i)))
      i += 1
    b.result()

  def toMap: Map[Text, Optional[Text]] =
    if ks.length == 0 then ListMap.empty else
      val b = ListMap.newBuilder[Text, Optional[Text]]
      var i = 0
      while i < ks.length do
        b += ((ks(i), vs(i)))
        i += 1
      b.result()

  // Eager `map` returning an `Iterable[B]`. The renderer uses this and
  // joins the result; an Iterable lets `.join(...)` work without further
  // adaptation while keeping the call site shape unchanged.
  def map[B](f: ((Text, Optional[Text])) => B): Iterable[B] =
    val b = List.newBuilder[B]
    var i = 0
    while i < ks.length do
      b += f((ks(i), vs(i)))
      i += 1
    b.result()

  def foreach[U](f: ((Text, Optional[Text])) => U): Unit =
    var i = 0
    while i < ks.length do
      f((ks(i), vs(i)))
      i += 1

  // Two-argument form (uncurried). The renderer uses this with
  // `attributes.each: (key, value) =>`.
  def each(action: (Text, Optional[Text]) => Unit): Unit =
    var i = 0
    while i < ks.length do
      action(ks(i), vs(i))
      i += 1

  // O(N) — finds and excludes the matching index, returning a new Attributes
  // whose backing IArrays are one slot shorter. If the key is absent, returns
  // `this` unchanged so callers don't allocate gratuitously.
  def removed(key: Text): Attributes =
    val n = ks.length
    var idx = -1
    var i = 0
    while idx < 0 && i < n do
      if ks(i) == key then idx = i
      i += 1
    if idx < 0 then this else
      val nk = new Array[Text](n - 1)
      val nv = new Array[Optional[Text]](n - 1)
      if idx > 0 then
        jl.System.arraycopy(ks.asInstanceOf[Array[Text]], 0, nk, 0, idx)
        jl.System.arraycopy(vs.asInstanceOf[Array[Optional[Text]]], 0, nv, 0, idx)
      if idx < n - 1 then
        jl.System.arraycopy(ks.asInstanceOf[Array[Text]], idx + 1, nk, idx, n - 1 - idx)
        jl.System.arraycopy
         (vs.asInstanceOf[Array[Optional[Text]]], idx + 1, nv, idx, n - 1 - idx)
      new Attributes(nk.immutable(using Unsafe), nv.immutable(using Unsafe))

  inline def - (key: Text): Attributes = removed(key)

  def -- (others: Iterable[Text]): Attributes =
    if isEmpty then this else
      var result = this
      others.foreach { k => result = result.removed(k) }
      result

  // Updates an existing key in place (preserving order) or appends a new pair
  // at the end. Always returns a new Attributes (the IArrays are immutable).
  def updated(key: Text, value: Optional[Text]): Attributes =
    val n = ks.length
    var idx = -1
    var i = 0
    while idx < 0 && i < n do
      if ks(i) == key then idx = i
      i += 1
    if idx >= 0 then
      val nv = new Array[Optional[Text]](n)
      jl.System.arraycopy(vs.asInstanceOf[Array[Optional[Text]]], 0, nv, 0, n)
      nv(idx) = value
      new Attributes(ks, nv.immutable(using Unsafe))
    else
      val nk = new Array[Text](n + 1)
      val nv = new Array[Optional[Text]](n + 1)
      jl.System.arraycopy(ks.asInstanceOf[Array[Text]], 0, nk, 0, n)
      jl.System.arraycopy(vs.asInstanceOf[Array[Optional[Text]]], 0, nv, 0, n)
      nk(n) = key
      nv(n) = value
      new Attributes(nk.immutable(using Unsafe), nv.immutable(using Unsafe))

  // Combines two Attributes, with the right-hand side overriding duplicate
  // keys (matching `Map ++` semantics). Order: left's keys first (preserving
  // their order), then any new keys from the right.
  def ++ (other: Attributes): Attributes =
    if other.isEmpty then this
    else if isEmpty then other
    else
      val total = size + other.size
      val nk = new Array[Text](total)
      val nv = new Array[Optional[Text]](total)
      var written = 0
      // Carry-over from left, but defer to the right's value if duplicated.
      var i = 0
      while i < size do
        val k = ks(i)
        val v = other(k)
        nk(written) = k
        nv(written) = if other.contains(k) then v else vs(i)
        written += 1
        i += 1
      // Append right-hand keys not already seen on the left.
      var j = 0
      while j < other.size do
        val k = other.ks(j)
        if !contains(k) then
          nk(written) = k
          nv(written) = other.vs(j)
          written += 1
        j += 1
      // Trim if duplicates collapsed slots.
      if written == total then
        new Attributes(nk.immutable(using Unsafe), nv.immutable(using Unsafe))
      else
        val tk = new Array[Text](written)
        val tv = new Array[Optional[Text]](written)
        jl.System.arraycopy(nk, 0, tk, 0, written)
        jl.System.arraycopy(nv, 0, tv, 0, written)
        new Attributes(tk.immutable(using Unsafe), tv.immutable(using Unsafe))

  def ++ (other: Map[Text, Optional[Text]]): Attributes =
    if other.isEmpty then this else this ++ Attributes.from(other)

  override def equals(that: Any): Boolean = that match
    case other: Attributes =>
      val n = ks.length
      if n != other.ks.length then false else
        var i = 0
        var ok = true
        while ok && i < n do
          val k = ks(i)
          if other(k) != vs(i) then ok = false
          i += 1
        ok

    // Permit equality with a structurally-equivalent `Map` so existing tests
    // that compare against `ListMap(...)` literals still work, and so callers
    // that have already materialised the attributes as a Map can compare both.
    case other: Map[?, ?] =>
      if ks.length != other.size then false else
        var i = 0
        var ok = true
        while ok && i < ks.length do
          if other.asInstanceOf[Map[Text, Optional[Text]]].getOrElse(ks(i), Unset) != vs(i)
          then ok = false
          i += 1
        ok

    case _ => false

  override def hashCode: Int =
    // Order-independent: XOR of (key.hash * 31 ^ value.hash) — matches Map's
    // hashCode contract well enough for tests that compare equal Attributes.
    var h = 0
    var i = 0
    while i < ks.length do
      h = h ^ (ks(i).hashCode*31 ^ vs(i).hashCode)
      i += 1
    h

  override def toString: String =
    val sb = new jl.StringBuilder("Attributes(")
    var i = 0
    while i < ks.length do
      if i > 0 then sb.append(", ")
      sb.append(ks(i).s).nn.append(" -> ").nn.append(vs(i).toString)
      i += 1
    sb.append(")").toString
