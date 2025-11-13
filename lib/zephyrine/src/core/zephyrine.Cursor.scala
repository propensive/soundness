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

  object Mark:
    final val Initial: Mark = -1
    
    def apply(block: Ordinal, position: Ordinal): Mark =
      (block.n0.toLong << 32) | (position.n0.toLong & 0xffffffffL)
      
    given ordered: Ordering[Mark] = Ordering.Long
  
  extension (mark: Mark)
    inline def block: Ordinal = (mark >> 32 & 0xffffffff).toInt.z
    inline def index: Ordinal = mark.toInt.z
    inline def increment: Mark = mark + 1
    inline def decrement: Mark = mark - 1

case class Cursor[data: Addressable](private val iterator: Iterator[data]):
  private val buffer: scm.ArrayDeque[data] = scm.ArrayDeque()
  private var first: Ordinal = (-1).z
  private var current: data = iterator.next()
  private var focus: Cursor.Mark = Cursor.Mark.Initial
  private var finished0: Boolean = false
  private var keep: Boolean = false

  inline def mark: Cursor.Mark = focus
  
  inline def last: Cursor.Mark =
    if focus.index != Prim then mark.decrement
    else
      val index = focus.index.previous
      val block = get(index)
      Cursor.Mark(index, (data.length(block) - 1).z)
    
  inline def finished: Boolean = finished0

  protected inline def cue(): Unit =
    val offset: Int = focus.block - first
    if buffer.length > offset then
      current = buffer(offset)
      if !keep then
        buffer.remove(0)
        first = (first.n0 + 1).z
    else
      if iterator.hasNext then
        current = iterator.next()
        if keep then buffer.append(current)
      else finished0 = true
      current

  protected inline def get(block: Ordinal): data = buffer(block - first)

  protected inline def proceed(): Unit =
    while
      focus = Cursor.Mark(focus.block.next, Prim)
      cue()
      data.length(current) == 0
    do ()
    
  inline def next(): Boolean =
    val previous = focus
    if focus.index.next.n0 >= data.length(current) then proceed()
    else focus = if finished then previous else focus.increment
    !finished

  inline def step(inline update: Cursor.Mark => Unit): Boolean =
    val index = focus.index.next
    if index.n0 >= data.length(current) then proceed()
    else focus = focus.increment
    if finished then false else
      update(focus)
      focus
      true
    
  inline def datum: data.Operand = data.address(current, focus.index)

  inline def retain[result](inline action: => result): result =
    keep = true
    first = focus.block
    buffer.append(current)
    action.also:
      keep = false

  inline def goto(mark0: Cursor.Mark): Unit =
    focus = mark0
    cue()
  
  inline def extract(start: Cursor.Mark, end: Cursor.Mark)(action: data => Unit): Unit =
    var offset = start.block - first
    if start.block == end.block then action(data.fragment(buffer(offset), start.index, end.index))
    else
      var focus = buffer(offset)
      action(data.fragment(focus, start.index, data.length(focus).u))
      val last = end.block - first
      
      while
        offset += 1
        offset < last
      do
        focus = buffer(offset)
        action(data.fragment(focus, Prim, data.length(focus).u))
      
      action(data.fragment(buffer(offset), Prim, end.index))
