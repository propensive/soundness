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

import scala.reflect.*

import anticipation.*
import denominative.*
import hypotenuse.*
import proscenium.*
import rudiments.*
import symbolism.*
import vacuous.*

object Conduit:
  enum State:
    case Data, Clutch, End

  case class Snapshot
    ( stream: Stream[Data], current: Data, index: Ordinal, done: Int, clutch: Boolean )

class Conduit(input0: Stream[Data]):
  private val input: Stream[Data] = input0.filter(_.nonEmpty)

  private var stream: Stream[Data] = if input.nil then Stream() else input.tail
  private var current: Data = if input.nil then Data() else input.head
  private var index: Ordinal = Prim
  private var done: Int = 0
  private var clutch: Boolean = false
  private var stream0: Stream[Data] = stream
  private var current0: Data = current
  private var index0: Ordinal = index
  private var done0: Int = done
  private var clutch0: Boolean = clutch

  def block: Data = current
  def datum: Int = try current(index.n0) catch case _: ArrayIndexOutOfBoundsException => -1
  def ordinal: Ordinal = index
  def remainder: Stream[Data] = stream

  def next(): Boolean = step() match
    case Conduit.State.Clutch => if stream.nil then false else (cue() yet next())
    case state                => state != Conduit.State.Clutch

  def expect(chars: Char*): Boolean =
    var result = true
    chars.each: char =>
      if result && datum == char then next() else result = false
    result

  final def break(): Unit = if !clutch then
    val prefix = current.slice(0, index.n1)
    clutch = true

    if current.length == index.n1 then current = prefix else
      val suffix = current.drop(index.n1)
      current = prefix
      val stream0 = stream
      stream = suffix #:: stream0

  final def truncate(): Unit = if !clutch then
    val prefix = current.slice(0, index.n0)
    val suffix = current.drop(index.n0)
    clutch = true
    current = prefix
    val stream0 = stream
    stream = suffix #:: stream0

  final def save(): Data =
    val rnd = math.random()
    val length = (index + done) - (index0 + done0)
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
  final def seek(byte: Byte): Boolean = next() && (if datum != byte then seek(byte) else true)

  @tailrec
  final def skip(count: Int): Unit = if count > 0 then next() yet skip(count - 1)

  @tailrec
  final def step(): Conduit.State =
    if clutch then
      if stream.nil then Conduit.State.End else
        clutch = false
        step()
    else
      index += 1
      if index.n0 >= current.size then
        clutch = true
        Conduit.State.Clutch
      else Conduit.State.Data

  final def cue(): Unit = if !stream.nil then
    done += current.length
    current = stream.head
    val tail = stream.tail
    stream = tail
    index = Prim - 1
    clutch = false

  final def mark(): Unit =
    current0 = current
    stream0 = stream
    index0 = index
    done0 = done
    clutch0 = clutch

  final def revert(): Unit =
    current = current0
    stream = stream0
    index = index0
    done = done0
    clutch = clutch0

  def search(chars: Char*): Boolean =
    def recur(chars: Char*): Boolean =
      !chars.nil && seek(chars.head.toByte)
      &&
      {
        mark()
        if expect(chars*) then revert() yet true else revert() yet recur(chars*)
      }

    mark()
    if expect(chars*) then revert() yet true else recur(chars*)

  final def take(count: Int): Data =
    mark()
    skip(count)
    save()

  final inline def lookahead[result](inline action: => result): result =
    mark()
    try action finally revert()
