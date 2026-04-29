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

import java.util.concurrent as juc

import anticipation.*
import denominative.*
import rudiments.*
import vacuous.*

object Emittable:
  inline given text: Emittable:
    type Self = Text
    type Source = Text
    type Transport = Array[Char]

    def length(text: Text): Int = text.s.length
    def produce(block: Array[Char], size: Int): Text = new String(block, 0, size).tt
    def allocate(size: Int): Array[Char] = new Array[Char](size)


    inline def copy(source: Text, start: Ordinal, target: Array[Char], index: Ordinal, size: Int)
    :   Unit =

      source.s.getChars(start.n0, start.n0 + size, target, index.n0)

  inline given bytes: Emittable:
    type Self = Data
    type Source = Data
    type Transport = Array[Byte]

    def produce(block: Array[Byte], size: Int): Data =
      java.util.Arrays.copyOfRange(block, 0, size).nn.immutable(using Unsafe)

    def length(bytes: Data): Int = bytes.length
    def allocate(size: Int): Array[Byte] = new Array[Byte](size)


    inline def copy(source: Data, start: Ordinal, target: Array[Byte], index: Ordinal, size: Int)
    :   Unit =

      System.arraycopy(source.mutable(using Unsafe), start.n0, target, index.n0, index.n0 + size)


trait Emittable:
  type Self
  type Source
  type Transport

  def allocate(size: Int): Transport
  def length(input: Source): Int
  def produce(block: Transport, size: Int): Self
  inline def copy
    ( source: Source,
      start:  Ordinal,
      target: Transport,
      index:  Ordinal,
      size:   Int )
  :   Unit


class Emitter[data: Emittable](block: Int = 4096, window: Int = 2):
  private object Done

  private val queue: juc.ArrayBlockingQueue[data | Done.type] = juc.ArrayBlockingQueue(window)
  private val current: data.Transport = data.allocate(block)

  private var index: Ordinal = Prim

  inline def free: Int = block - index.n0

  inline def finish(): Unit =
    publish()
    queue.put(Done)

  inline def put(input: data.Source): Unit = put(input, Prim, data.length(input))

  inline def publish(): Unit =
    if index != Prim then
      queue.put(data.produce(current, index.n0))
      index = Prim

  inline def put(source: data.Source, offset: Ordinal, size: Int): Unit =
    var done = 0

    while size - done > free do
      data.copy(source, (offset.n0 + done).z, current, index, free)
      done += free
      index = (index.n0 + free).z
      publish()

    data.copy(source, (offset.n0 + done).z, current, index, size - done)
    index = (index.n0 + size - done).z

    if free == 0 then publish()

  lazy val iterator: Iterator[data] = new Iterator[data]:
    private var ready: data | Done.type = Done

    def hasNext: Boolean =
      ready = queue.take().nn
      ready != Done

    def next(): data = ready.asInstanceOf[data]
