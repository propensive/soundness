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
package rudiments

import scala.reflect.ClassTag

// A growable array of primitives (or anything), appended to and indexed by position: the
// unboxed replacement for `scm.ArrayBuffer[Int]`, whose backing store is an `Object[]` of
// references to boxed elements, filled by one boxed append per element and read back by one
// unboxing subscript per element. The sibling of `Scribe`, which confines its indices to a
// branded extent and is minted inside a lender; a scroll is for the descriptor builders and
// style stacks that write at computed positions and are reused across parses, so it is
// publicly constructible, `clear`able, and indexed by plain `Int`, checked against its length.
//
// As `Scribe`, the carrier holds the buffer untyped, with inline accessors that cast it to
// `scala.Array[element]`, so a scroll of a concrete element type reads and writes a primitive
// array directly: `scroll.append(1)` is an `iastore`. The element class comes from the array
// itself when it grows, not from a `ClassTag` summoned per append.
opaque type Scroll[element] = Scroll.Core

object Scroll:
  final class Core private[rudiments] (buffer0: AnyRef):
    // Untracked, as `Scribe.Core`'s: a scroll is a plain builder reached through its holder,
    // and `Stateful` would force capability typing onto every field that holds one.
    // [field-purity] growable AnyRef buffer in scroll core
    @scala.caps.unsafe.untrackedCaptures
    var buffer: AnyRef = buffer0

    // [field-purity] length cursor in scroll core
    @scala.caps.unsafe.untrackedCaptures
    var length: Int = 0

  def apply[element: ClassTag](capacity: Int = 16): Scroll[element] =
    // The cast forgets the fresh array's capture: it is reached only through this scroll.
    val buffer: scala.Array[element]^ = new scala.Array[element](capacity.max(16))
    new Core(buffer.asInstanceOf[AnyRef])

  // Doubling, as `Scribe.reserve`: the bigger array takes its element class from the buffer.
  private[rudiments] def reserve[element](core: Core, extra: Int): Unit =
    val target = core.buffer.asInstanceOf[scala.Array[element]^]
    val required = core.length + extra

    if required > target.length then
      var capacity = target.length*2
      while capacity < required do capacity *= 2

      val bigger: scala.Array[element]^ =
        java.lang.reflect.Array.newInstance(target.getClass.getComponentType.nn, capacity)
        . asInstanceOf[scala.Array[element]^]

      System.arraycopy(target, 0, bigger, 0, core.length)
      core.buffer = bigger.asInstanceOf[AnyRef]

  private def outOfBounds(index: Int, length: Int): Nothing =
    throw IndexOutOfBoundsException(s"index $index of a scroll of length $length")

  extension [element](scroll: Scroll[element])
    inline def length: Int = scroll.asInstanceOf[Core].length
    inline def isEmpty: Boolean = scroll.asInstanceOf[Core].length == 0

    inline def apply(index: Int): element =
      val core = scroll.asInstanceOf[Core]
      if index < 0 || index >= core.length then Scroll.outOfBounds(index, core.length)
      core.buffer.asInstanceOf[scala.Array[element]](index)

    inline def update(index: Int, value: element): Unit =
      val core = scroll.asInstanceOf[Core]
      if index < 0 || index >= core.length then Scroll.outOfBounds(index, core.length)
      core.buffer.asInstanceOf[scala.Array[element]^](index) = value

    inline def last: element =
      val core = scroll.asInstanceOf[Core]
      if core.length == 0 then Scroll.outOfBounds(-1, 0)
      core.buffer.asInstanceOf[scala.Array[element]](core.length - 1)

    inline def append(value: element): Unit =
      val core = scroll.asInstanceOf[Core]
      if core.length == core.buffer.asInstanceOf[scala.Array[element]].length
      then Scroll.reserve[element](core, 1)

      core.buffer.asInstanceOf[scala.Array[element]^](core.length) = value
      core.length += 1

    // The elements of another scroll, in order.
    @scala.annotation.targetName("appendScroll")
    inline def append(other: Scroll[element]): Unit =
      val source = other.asInstanceOf[Core]
      scroll.append(source.buffer.asInstanceOf[scala.Array[element]], 0, source.length)

    // A window of an array.
    inline def append(source: scala.Array[element]^{scala.caps.any.rd}, from: Int, count: Int)
    :   Unit =

      if count > 0 then
        val core = scroll.asInstanceOf[Core]
        Scroll.reserve[element](core, count)

        System.arraycopy
          ( source.asInstanceOf[scala.Array[element]],
            from,
            core.buffer.asInstanceOf[scala.Array[element]^],
            core.length,
            count )

        core.length += count

    // Forgets the last `count` elements, or all of them; the capacity is kept.
    inline def retract(count: Int): Unit =
      val core = scroll.asInstanceOf[Core]
      core.length = (core.length - count).max(0)

    inline def clear(): Unit = scroll.asInstanceOf[Core].length = 0

    // The elements so far, in an exact-size frozen array; the scroll is unchanged.
    inline def freeze: Array[element]^{} =
      val core = scroll.asInstanceOf[Core]
      val source = core.buffer.asInstanceOf[scala.Array[element]]
      val exact: scala.Array[element]^ =
        java.lang.reflect.Array.newInstance(source.getClass.getComponentType.nn, core.length)
        . asInstanceOf[scala.Array[element]^]

      System.arraycopy(source, 0, exact, 0, core.length)
      exact.asInstanceOf[Array[element]^{}]
