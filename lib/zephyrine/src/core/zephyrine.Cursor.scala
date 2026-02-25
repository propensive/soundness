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
package zephyrine

import scala.collection.mutable as scm

import denominative.*
import prepositional.*
import rudiments.*
import vacuous.*

object Cursor:
  opaque type Mark = Long
  opaque type Offset = Long

  class Held()

  object Mark:
    final val Initial: Mark = -1

    def apply(block: Ordinal, position: Ordinal): Mark =
      (block.n0.toLong << 32) | (position.n0.toLong & 0xffffffffL)

    given ordered: Ordering[Mark] = Ordering.Long

  object Offset:
    def apply(line: Ordinal, column: Ordinal): Offset =
      (line.n0.toLong << 32) | (column.n0.toLong & 0xffffffffL)

    given ordered: Ordering[Offset] = Ordering.Long

  extension (mark: Mark)
    inline def block: Ordinal = (mark >> 32 & 0xffffffff).toInt.z
    inline def index: Ordinal = mark.toInt.z
    private[zephyrine] inline def increment: Mark = mark + 1
    private[zephyrine] inline def decrement: Mark = mark - 1

  extension (offset: Offset)
    inline def line: Ordinal = (offset >> 32 & 0xffffffff).toInt.z
    inline def column: Ordinal = offset.toInt.z


  transparent inline def apply[data](iterator: Iterator[data])
    ( using addressable0: data is Addressable,
            lineation0:   Lineation by addressable0.Operand )
  :   Cursor[data] =

    if iterator.hasNext then
      val initial = iterator.next()

      new Cursor[data]
        ( initial, addressable0.length(initial), iterator, addressable0, lineation0 )

    else
      new Cursor[data](addressable0.empty, 0, Iterator.empty, addressable0, lineation0)

class Cursor[data]
  (             initial:    data,
                extent0:    Int,
                iterator:   Iterator[data],
    tracked val addressable: data is Addressable,
    tracked val lineation:   Lineation by addressable.Operand ):

  private val buffer: scm.ArrayDeque[data] = scm.ArrayDeque()
  private val marks: scm.ArrayDeque[Mark] = scm.ArrayDeque()
  private val offsets: scm.ArrayDeque[Offset] = scm.ArrayDeque()
  private var first: Ordinal = Prim
  private var current: data = initial
  private var focusBlock: Ordinal = Prim
  private var focus: Ordinal = Prim
  private var length: Int = if extent0 == 0 then 0 else Int.MaxValue
  private var keep: Boolean = false
  private var extent: Int = extent0
  private var lineNo: Ordinal = Prim
  private var columnNo: Ordinal = Prim
  private var done: Int = 0

  protected inline def store(ordinal: Ordinal, value: data): Unit =
    val index = ordinal - first
    if buffer.length <= index then buffer.append(value)

  protected inline def load(): data = iterator.next().tap: value =>
    extent += addressable.length(value)

  protected inline def drop(): Unit =
    val diff = focusBlock - first
    first = (first.n0 + diff).z
    buffer.drop(diff)

  protected inline def forward(): Unit =
    val block: Ordinal = focusBlock.next
    val offset: Int = block - first

    if offset < buffer.length then
      focusBlock = block
      focus = Prim
      done += addressable.length(current)
      current = buffer(offset)
      if !keep then
        buffer.dropInPlace(1)
        first = first.next
    else if iterator.hasNext then
      var next = load()
      while addressable.length(next) == 0 do next = load()
      if keep then store(block, next)
      focusBlock = block
      focus = Prim
      done += addressable.length(current)
      current = next

    else
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

    inline if lineation.active then
      val offset2 = offset(mark)
      lineNo = offset2.line
      columnNo = offset2.column

  inline def consume(inline otherwise: => Unit)(inline text: String): Unit =
    ${Zephyrine.consume('this, 'text, 'otherwise)}

  inline def next(): Boolean =
    val current2 = current
    val focus2 = focus

    if focus.next.n0 >= addressable.length(current) then forward() else focus = focus.next

    if finished then false else
      inline if lineation.active then
        columnNo =
          if !lineation.track(addressable.address(current2, focus2)) then columnNo.next else
            lineNo = lineNo.next
            Prim
      true

  inline def more: Boolean = !finished
  inline def offset(mark: Mark): Offset = offsets(marks.lastIndexOf(mark))
  inline def line: Ordinal = lineNo
  inline def column: Ordinal = columnNo

  inline def seek(target: addressable.Operand): Boolean =
    var found = false
    var continue = true

    while continue do
      found = datum(using Unsafe) == target
      continue = !found && next()

    found

  inline def mark(using held: Cursor.Held): Mark = Mark(focusBlock, focus).tap: mark =>
    inline if lineation.active then
      marks.append(mark)
      offsets.append(Offset(lineNo, columnNo))

  inline def datum(using erased Unsafe): addressable.Operand = addressable.address(current, focus)

  inline def lay[result](inline otherwise: => result)(inline lambda: addressable.Operand => result)
  :   result =

    if !finished then lambda(addressable.address(current, focus)) else otherwise

  inline def let(inline lambda: addressable.Operand => Unit): Unit =
    if !finished then lambda(addressable.address(current, focus))


  inline def process[result](inline lambda: addressable.Operand => result): Unit =
    if !finished then lambda(addressable.address(current, focus))

  inline def position: Ordinal = (done + focus.n0).z
  inline def finished: Boolean = position.n0 >= length - 1
  inline def available = extent - position.n0

  inline def hold[result](inline action: Cursor.Held ?=> result): result =
    val keep0 = keep

    if !keep0 then
      keep = true
      first = focusBlock
      store(focusBlock, current)

    action(using new Cursor.Held()).also:
      keep = keep0
      if !keep then
        buffer.dropInPlace(focusBlock - first)
        first = focusBlock
        marks.clear()
        offsets.clear()

  inline def grab(start: Mark, end: Mark): data =
    // FIXME: calculate length across different blocks
    val buffer = addressable.blank(if start.block == end.block then end.index - start.index else 0)
    clone(start, end)(buffer)
    addressable.build(buffer)

  inline def take(inline otherwise: => data)(length: Int): data =
    var buffer = addressable.blank(length)
    var count = 0
    hold:
      val start = mark
      while count < length do
        next()
        count += 1

      grab(start, mark)



  inline def clone(start: Mark, end: Mark)(target: addressable.Target): Unit = if start != end then
    val last = end.block - first
    var offset = start.block - first

    if start.block == end.block then
      if end.index.previous.n0 >= start.index.n0
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
      then addressable.clone(buffer(offset), Prim, end.index.previous)(target)
