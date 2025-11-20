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

import scala.collection.mutable as scm

import anticipation.*
import denominative.*
import fulminate.*
import prepositional.*
import rudiments.*

object Cursor:
  opaque type Mark = Long
  erased trait Held

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

export Cursor.Mark

case class Cursor[data: Addressable](private val iterator: Iterator[data]):
  private val buffer: scm.ArrayDeque[data] = scm.ArrayDeque()
  private var first: Ordinal = (-1).z
  private var current: data = iterator.next()
  private var focusBlock: Ordinal = Prim
  private var focusIndex: Ordinal = (-1).z
  private var length: Int = Int.MaxValue
  // FIXME: This should be an ordinal of the first block to keep
  private var keep: Boolean = false
  private var done: Int = 0
  
  inline def finished: Boolean = position.n0 == length - 1

  protected inline def store(ordinal: Ordinal, value: data): Unit =
    val index = ordinal - first
    if buffer.length <= index then buffer.append(value) else buffer(index) = value

  protected inline def forward(): Unit =
    val block: Ordinal = focusBlock.next
    val offset: Int = block - first
    done += data.length(current)
    
    current =
      if buffer.length > offset then
        focusBlock = block
        focusIndex = Prim
        buffer(offset)
      else if iterator.hasNext then
        var next = iterator.next()
        while data.length(next) == 0 do next = iterator.next()
        if keep then store(block, next)
        focusBlock = block
        focusIndex = Prim
        next
      else current.also:
        length = position.n1
    
  protected inline def backward(): Unit =
    val block = focusBlock.previous
    val offset = block - first
    current = buffer(offset)
    done -= data.length(current)
    focusBlock = block
    focusIndex = Prim

  inline def cue(mark: Mark): Unit =
    while mark.block.n0 < focusBlock.n0 do backward()
    while mark.block.n0 > focusBlock.n0 do forward()
    focusIndex = mark.index
    
  inline def next(): Boolean =
    if focusIndex.next.n0 >= data.length(current) then forward()
    else focusIndex = focusIndex.next
    !finished
  
  inline def more: Boolean = !finished
  inline def mark(using erased Cursor.Held): Mark = Mark(focusBlock, focusIndex)
  inline def datum: data.Operand = data.address(current, focusIndex)
  inline def position: Ordinal = (done + focusIndex.n0).z
  
  inline def hold[result](inline action: (erased Cursor.Held) ?=> result): result =
    keep = true
    first = focusBlock
    store(focusBlock, current)
    action(using !![Cursor.Held]).also { keep = false }

  inline def grab(start: Mark, end: Mark)(target: data.Target): Unit =
    val last = end.block - first
    var offset = start.block - first
    
    if start.block == end.block then data.grab(buffer(offset), start.index, end.index)(target)
    else
      var focus = buffer(offset)
      data.grab(focus, start.index, data.length(focus).u)(target)
      
      while
        offset += 1
        offset < last
      do
        focus = buffer(offset)
        data.grab(focus, Prim, data.length(focus).u)(target)
      
      data.grab(buffer(offset), Prim, end.index)(target)
