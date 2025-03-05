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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import scala.reflect.*

import anticipation.*
import denominative.*
import hypotenuse.*
import proscenium.*
import rudiments.*
import vacuous.*

object Conduit:
  enum State:
    case Data, Clutch, End

class Conduit(input0: Stream[Bytes]):
  private val input: Stream[Bytes] = input0.filter(_.nonEmpty)
  private var stream: Stream[Bytes] = if input.isEmpty then Stream() else input.tail
  private var current: Bytes = if input.isEmpty then Bytes() else input.head
  private var index: Ordinal = Prim
  private var done: Int = 0
  private var clutch: Boolean = false

  private var stream0: Stream[Bytes] = stream
  private var current0: Bytes = current
  private var index0: Ordinal = index
  private var done0: Int = done
  private var clutch0: Boolean = clutch

  def block: Bytes = current
  def datum: Byte = current(index.n0)

  def remainder: Stream[Bytes] = stream

  def next(): Boolean = step() match
    case Conduit.State.Clutch => cue() yet next()
    case state                => state != Conduit.State.Clutch

  def break(): Unit = if !clutch then
    val prefix = current.slice(0, index.n1)
    clutch = true

    if current.length == index.n1 then current = prefix else
      val suffix = current.drop(index.n1)
      current = prefix
      val stream0 = stream
      stream = suffix #:: stream0

  def truncate(): Unit = if !clutch then
    val prefix = current.slice(0, index.n0)
    val suffix = current.drop(index.n0)
    clutch = true
    current = prefix
    val stream0 = stream
    stream = suffix #:: stream0

  def save(): Bytes =
    val rnd = math.random()
    val length = (done + index) - (done0 + index0)
    IArray.create(length): array =>
      var sourceIndex = index0.n0
      var destinationIndex = 0
      var head = current0
      var tail = stream0

      def recur(): Unit =
        val count = (head.length - sourceIndex).min(array.length - destinationIndex)
        System.arraycopy(head.mutable(using Unsafe), sourceIndex, array, destinationIndex, count)
        destinationIndex += count
        if destinationIndex < array.length then
          head = tail.head
          tail = tail.tail
          sourceIndex = 0
          recur()

      recur()

  @tailrec
  final def seek(byte: Byte): Unit = if next() && datum != byte then seek(byte)

  @tailrec
  final def skip(count: Int): Unit = if count > 0 then next() yet skip(count - 1)

  def step(): Conduit.State =
    if clutch then
      if stream.isEmpty then Conduit.State.End else
        clutch = false
        step()
    else
      index += 1
      if index > current.ult.or(Prim - 1) then
        clutch = true
        Conduit.State.Clutch
      else Conduit.State.Data

  def cue(): Unit =
    if !stream.isEmpty then
      done += current.length
      current = stream.head
      val tail = stream.tail
      stream = tail
      index = Prim - 1
      clutch = false

  def mark(): Unit =
    current0 = current
    stream0 = stream
    index0 = index
    done0 = done
    clutch0 = clutch

  def revert(): Unit =
    current = current0
    stream = stream0
    index = index0
    done = done0
    clutch = clutch0

  def take(count: Int): Bytes =
    mark()
    skip(count)
    save()

  inline def lookahead[ResultType](inline action: => ResultType): ResultType =
    mark()
    try action finally revert()
