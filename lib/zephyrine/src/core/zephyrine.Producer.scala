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

import java.io as ji
import java.lang as jl
import java.util.concurrent as juc

import anticipation.*
import denominative.*
import rudiments.*
import vacuous.*

object Producible:
  inline given text: Producible:
    type Self = Text
    type Transport = Array[Char]
    type Builder = jl.StringBuilder

    def length(text: Text): Int = text.s.length
    def produce(block: Array[Char], size: Int): Text = String(block, 0, size).tt
    def allocate(size: Int): Array[Char] = new Array[Char](size)

    def copy(source: Text, start: Ordinal, target: Array[Char], index: Ordinal, size: Int): Unit =
      source.s.getChars(start.n0, start.n0 + size, target, index.n0)

    def builder(hint: Int): jl.StringBuilder = jl.StringBuilder(hint)

    def append(builder: jl.StringBuilder, source: Text, start: Ordinal, size: Int): Unit =
      builder.append(source.s, start.n0, start.n0 + size)

    def build(builder: jl.StringBuilder): Text = builder.toString.tt

  inline given bytes: Producible:
    type Self = Data
    type Transport = Array[Byte]
    type Builder = ji.ByteArrayOutputStream

    def produce(block: Array[Byte], size: Int): Data =
      java.util.Arrays.copyOfRange(block, 0, size).nn.immutable(using Unsafe)

    def length(bytes: Data): Int = bytes.length
    def allocate(size: Int): Array[Byte] = new Array[Byte](size)

    def copy(source: Data, start: Ordinal, target: Array[Byte], index: Ordinal, size: Int): Unit =
      System.arraycopy(source.mutable(using Unsafe), start.n0, target, index.n0, size)

    def builder(hint: Int): ji.ByteArrayOutputStream = ji.ByteArrayOutputStream(hint)

    def append(builder: ji.ByteArrayOutputStream, source: Data, start: Ordinal, size: Int): Unit =
      builder.write(source.mutable(using Unsafe), start.n0, size)

    def build(builder: ji.ByteArrayOutputStream): Data =
      builder.toByteArray.nn.immutable(using Unsafe)


// A medium (text or bytes) that output can be produced into. The `allocate`/`copy`/`produce`
// trio backs the chunked streaming path; the `builder`/`append`/`build` trio backs synchronous
// collection. `Self` is both the medium written in pieces and the value finally produced.
trait Producible:
  type Self
  type Transport
  type Builder

  def allocate(size: Int): Transport
  def length(value: Self): Int
  def produce(block: Transport, size: Int): Self

  def copy
    ( source: Self,
      start:  Ordinal,
      target: Transport,
      index:  Ordinal,
      size:   Int )
  :   Unit

  def builder(hint: Int): Builder
  def append(builder: Builder, source: Self, start: Ordinal, size: Int): Unit
  def build(builder: Builder): Self


// A sink into which a value of one medium is written in pieces. The same producing code can be
// driven two ways without duplicating it: `Producer.collect` runs it synchronously and returns the
// whole result, while `Producer.apply` (streaming) writes chunks into a bounded buffer drained
// lazily through `iterator` — the producing code then runs on a separate fiber. Neither path
// allocates per `put` along a contiguous run.
object Producer:
  // Streaming: chunks are queued (with backpressure) and drained through `iterator`. The producing
  // code must run on a separate fiber, since `put` blocks once the window is full.
  def apply[medium: Producible](block: Int = 4096, window: Int = 2): Channel[medium] =
    Channel(block, window)

  // Synchronous: run `body`, accumulating directly into a builder, and return the whole value. No
  // concurrency, no chunk buffer, and none of the streaming path's single-thread deadlock risk.
  def collect[medium: Producible as medium2](hint: Int = 4096)
    ( body: Producer[medium] => Unit )
  :   medium =

    val builder = medium2.builder(hint)

    val producer = new Producer[medium]:
      def put(source: medium): Unit =
        medium2.append(builder, source, Prim, medium2.length(source))

      def put(source: medium, offset: Ordinal, size: Int): Unit =
        medium2.append(builder, source, offset, size)

    body(producer)
    medium2.build(builder)

  final class Channel[medium: Producible as medium2](block: Int, window: Int)
  extends Producer[medium]:

    private object Done

    private val queue: juc.ArrayBlockingQueue[medium | Done.type] =
      juc.ArrayBlockingQueue(window)

    private val current: medium2.Transport = medium2.allocate(block)
    private var index: Ordinal = Prim

    private inline def free: Int = block - index.n0

    private inline def publish(): Unit =
      if index != Prim then
        queue.put(medium2.produce(current, index.n0))
        index = Prim

    def put(source: medium): Unit = put(source, Prim, medium2.length(source))

    def put(source: medium, offset: Ordinal, size: Int): Unit =
      var done = 0

      while size - done > free do
        medium2.copy(source, (offset.n0 + done).z, current, index, free)
        done += free
        index = (index.n0 + free).z
        publish()

      medium2.copy(source, (offset.n0 + done).z, current, index, size - done)
      index = (index.n0 + size - done).z

      if free == 0 then publish()

    def finish(): Unit =
      publish()
      queue.put(Done)

    lazy val iterator: Iterator[medium] = new Iterator[medium]:
      private var ready: medium | Done.type = Done

      def hasNext: Boolean =
        ready = queue.take().nn
        ready != Done

      def next(): medium = ready.asInstanceOf[medium]


trait Producer[medium]:
  def put(source: medium): Unit
  def put(source: medium, offset: Ordinal, size: Int): Unit
