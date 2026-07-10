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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package turbulence

import anticipation.*
import denominative.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*
import zephyrine.*

object Aggregable:
  // Drain a pull endpoint into the medium's builder: one copy per window,
  // no intermediate chunks. The native `accept` for whole-value aggregation.
  private def gather[medium](stream: Stream[medium] over Credit): medium =
    val target = stream.addressable.blank(4096)

    def recur(): Unit = stream.refill(Credit(Long.MaxValue)) match
      case count: Int =>
        stream.addressable.cloneStorage(stream.window(using Unsafe), stream.start, count)(target)
        stream.skip(count)
        recur()

      case _ => ()

    recur()
    stream.addressable.build(target)

  given bytesData: Data is Aggregable by Data = new Aggregable:
    type Self = Data
    type Operand = Data

    override def accept(stream: Stream[Data] over Credit): Data = gather(stream)

    def aggregate(source0: LazyList[Data]): Data =
      val size = source0.foldLeft(0)(_ + _.length)

      var source = source0

      Data.build(size): array =>
        var index = Prim

        while !source.nil do
          val bytes = source.head
          array.place(bytes, index)
          index += bytes.length
          source = source.tail

  given bytesText: (decoder: CharDecoder) => ((Text is Aggregable by Data)) =
    bytesData.map(decoder.decoded)

  given textText: Text is Aggregable by Text = new Aggregable:
    type Self = Text
    type Operand = Text

    override def accept(stream: Stream[Text] over Credit): Text = gather(stream)

    def aggregate(source0: LazyList[Text]): Text =
      var source = source0

      val builder = new StringBuilder()

      while !source.nil do
        builder.append(source.head.s)
        source = source.tail

      builder.toString.tt


  given stream: [element, element2] => (aggregable: element2 is Aggregable by element)
  =>  LazyList[element2] is Aggregable by element =

    element => LazyList(aggregable.aggregate(element))

trait Aggregable extends Typeclass, Operable:
  // The capturing self type permits instances that capture (e.g. a resolution-scoped
  // tactic) while keeping the explicit self name the 3.9 SAM adaptation requires.
  aggregable: Aggregable^ =>
  def aggregate(source: LazyList[Operand]): Self

  // Consume a pull endpoint. The default materializes one chunk per refill
  // and delegates to the legacy `aggregate`; instances override it to build
  // directly from the stream's windows.
  def accept(stream: Stream[Operand] over Credit): Self =
    def recur(): LazyList[Operand] =
      stream.refill(Credit(Long.MaxValue)) match
        case count: Int =>
          val chunk =
            stream.addressable.materialize(stream.window(using Unsafe), stream.start, count)

          stream.skip(count)
          chunk #:: recur()

        case _ =>
          LazyList()

    aggregate(LazyList.defer(recur()))

  def map[self2](lambda: Self => self2): (self2 is Aggregable by Operand)^{this, lambda} = source =>
    lambda(aggregable.aggregate(source))
