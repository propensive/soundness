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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package zephyrine

import language.experimental.captureChecking

import scala.collection.mutable as scm
import scala.annotation.publicInBinary
import scala.caps as sc

import anticipation.*
import denominative.*
import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

object Cursor:
  opaque type Mark = Long
  class Held() extends sc.Capability

  object Mark:
    final val Initial: Mark = -1

    def apply(block: Ordinal, position: Ordinal): Mark =
      (block.n0.toLong << 32) | (position.n0.toLong & 0xffffffffL)

    given ordered: Ordering[Mark] = Ordering.Long

  extension (mark: Mark)
    inline def block: Ordinal = (mark >> 32 & 0xffffffff).toInt.z
    inline def index: Ordinal = mark.toInt.z
    private[zephyrine] inline def increment: Mark = mark + 1
    private[zephyrine] inline def decrement: Mark = mark - 1


  inline def apply[data](iterator: Iterator[data])(using addressable0: data is Addressable)
  : Cursor[data] { val addressable: addressable0.type } =

      if iterator.hasNext then
        val initial = iterator.next()
        new Cursor[data](initial, addressable0.length(initial), iterator, addressable0)
      else new Cursor[data](addressable0.empty, 0, Iterator.empty, addressable0)

export Cursor.Mark

class Cursor[data](initial:                 data,
                   extent0:                 Int,
                   iterator:                Iterator[data],
                   val addressable: data is Addressable):
  private val buffer: scm.ArrayDeque[data] = scm.ArrayDeque()
  private var first: Ordinal = Prim
  private var current: data = initial
  private var focusBlock: Ordinal = Prim
  private var focus: Ordinal = Prim
  private var length: Int = Int.MaxValue
  // FIXME: This should be an ordinal of the first block to keep
  private var keep: Boolean = false
  private var extent: Int = extent0
  private var done: Int = 0

  protected inline def store(ordinal: Ordinal, value: data): Unit =
    val index = ordinal - first
    if buffer.length <= index then buffer.append(value) else buffer(index) = value

  protected inline def load(): data = iterator.next().tap: value =>
    extent += addressable.length(value)

  protected inline def drop(): Unit =
    val diff = focusBlock - first
    first = (first.n0 + diff).z
    buffer.drop(diff)

  protected inline def forward(): Unit =
    val block: Ordinal = focusBlock.next
    val offset: Int = block - first
    done += addressable.length(current)

    current =
      if buffer.length > offset then
        focusBlock = block
        focus = Prim
        buffer(offset)
      else if iterator.hasNext then
        var next = load()
        while addressable.length(next) == 0 do
          next = load()

        if keep then store(block, next)
        focusBlock = block
        focus = Prim
        next

      else current.also:
        focus = focus.next
        length = position.n1

  protected inline def backward(): Unit =
    val block = focusBlock.previous
    val offset = block - first
    current = buffer(offset)
    done -= addressable.length(current)
    focusBlock = block
    focus = Prim

  inline def cue(mark: Mark): Unit =
    while mark.block.n0 < focusBlock.n0 do backward()
    while mark.block.n0 > focusBlock.n0 do forward()
    focus = mark.index

  inline def next(): Boolean =
    if focus.next.n0 >= addressable.length(current) then forward()
    else focus = focus.next
    !finished

  inline def more: Boolean = !finished
  inline def mark(using held: Cursor.Held): Mark^{held} = Mark(focusBlock, focus)

  inline def datum(using Unsafe): addressable.Operand = addressable.address(current, focus)

  inline def lay[result](inline otherwise: => result)(inline lambda: addressable.Operand => result)
  : result =

      if !finished then lambda(addressable.address(current, focus)) else otherwise


  inline def process[result](inline lambda: addressable.Operand => result): Unit =
    if !finished then lambda(addressable.address(current, focus))

  inline def position: Ordinal = (done + focus.n0).z
  inline def finished: Boolean = position.n0 == length - 1
  inline def available = extent - position.n0

  inline def hold[result](inline action: Cursor.Held ?=> result): result =
    val keep0 = keep
    keep = true
    first = focusBlock
    store(focusBlock, current)
    action(using new Cursor.Held()).also { keep = keep0 }

  inline def grab(start: Mark, end: Mark): data =
    val buffer = addressable.blank(0) // FIXME: calculate length
    clone(start, end)(buffer)
    addressable.build(buffer)

  inline def clone(start: Mark, end: Mark)(target: addressable.Target): Unit =
    val last = end.block - first
    var offset = start.block - first

    if start.block == end.block
    then
      if end.index.previous.n0 > start.index.n0
      then addressable.clone(buffer(offset), start.index, end.index.previous)(target)
    else
      var focus = buffer(offset)
      addressable.clone(focus, start.index, addressable.length(focus).u)(target)

      while
        offset += 1
        offset < last
      do
        focus = buffer(offset)
        addressable.clone(focus, Prim, addressable.length(focus).u)(target)

      if end.index != Prim
      then addressable.clone (buffer(offset), Prim, end.index.previous)(target)
