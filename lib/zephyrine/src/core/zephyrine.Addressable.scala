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

object Addressable:
  inline given Bytes is Addressable:
    type Operand = Byte

    inline def length(bytes: Bytes): Int = bytes.length
    inline def address(bytes: Bytes, index: Ordinal): Byte = bytes(index.n0)

    inline def fragment(bytes: Bytes, start: Ordinal, end: Ordinal): Bytes =
      bytes.slice(start.n0, end.n1)

  inline given Text is Addressable:
    type Operand = Char

    inline def length(text: Text): Int = text.s.length
    inline def address(text: Text, index: Ordinal): Operand = text.s.charAt(index.n0)

    inline def fragment(text: Text, start: Ordinal, end: Ordinal): Text =
      text.s.substring(start.n0, end.n1).nn.tt

trait Addressable extends Typeclass, Operable:
  inline def length(block: Self): Int
  inline def address(block: Self, index: Ordinal): Operand
  inline def fragment(block: Self, start: Ordinal, end: Ordinal): Self


object Cursor:
  opaque type Mark = Long

  object Mark:
    def apply(block: Ordinal, position: Ordinal): Mark =
      (block.n0.toLong << 32) | (position.n0.toLong & 0xffffffffL)
      
    given ordered: Ordering[Mark] = Ordering.Long
  
  extension (mark: Mark)
    def block: Ordinal = (mark >> 32 & 0xffffffff).toInt.z
    def index: Ordinal = mark.toInt.z

case class Cursor[data: Addressable](private val iterator: Iterator[data]):
  private val buffer: scm.ArrayDeque[data] = scm.ArrayDeque()
  private var first: Ordinal = (-1).z
  private var current: data = iterator.next()
  private var index: Ordinal = (-1).z
  private var done: Int = 0
  private var block: Ordinal = Prim
  private var keep: Boolean = false
  private var more: Boolean = true

  inline def position: Ordinal = (index.n0 + done).z
  inline def mark: Cursor.Mark = Cursor.Mark(block, index)

  protected inline def get(): data =
    val offset: Int = block - first
    if buffer.length > offset then buffer(offset) else
      if iterator.hasNext then
        done += data.length(current)
        current = iterator.next()
        if keep then buffer.append(current)
        current
      else
        more = false
        current

  protected inline def proceed(): Unit =
    while
      block = block.next
      index = Prim
      get()
      data.length(current) == 0
    do ()
    
  inline def next(): Boolean =
    index = index.next
    if index.n0 >= data.length(current) then proceed()
    more

  inline def datum: data.Operand = data.address(current, index)

  inline def hold[result](inline action: Cursor.Mark => result): result =
    val keep0: Boolean = keep
    if !keep then
      first = block
      buffer.append(current)
    keep = true
    action(mark).also:
      keep = keep0
      if !keep then buffer.clear()

  inline def goto(mark: Cursor.Mark): Unit =
    block = mark.block
    index = mark.index
    get()

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
