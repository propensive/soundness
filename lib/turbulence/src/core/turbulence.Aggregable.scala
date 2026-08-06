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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import proscenium.compat.*
import rudiments.*
import symbolism.*
import vacuous.*
import zephyrine.*

object Aggregable:
  // Drain a pull endpoint into the medium's builder: one copy per region,
  // no intermediate chunks. The native `accept` for whole-value aggregation.
  private def gather[medium](consume stream: (Stream[medium] over Credit)^): medium =
    given stream.addressable.type = stream.addressable
    val target = stream.addressable.blank(4096)

    def recur(): Unit = stream.refill(Credit(Long.MaxValue)) match
      case count: Int =>
        stream.lend { region => range => region.cloneTo(range)(target) }
        stream.skip(count)
        recur()

      case _ => ()

    recur()
    stream.addressable.build(target)

  given bytesData: Data is Aggregable by Data = new Aggregable:
    type Self = Data
    type Operand = Data

    override def accept(stream: (Stream[Data] over Credit)^): Data =
      // Ownership moves through a neutral carrier: the trait's `accept` cannot
      // declare `consume`, so the transfer to (consuming) `gather` is by convention.
      val streamRef: AnyRef = stream.asInstanceOf[AnyRef]
      gather(streamRef.asInstanceOf[(Stream[Data] over Credit)^])

    def aggregate(source0: Chain[Data]): Data =
      val size = source0.fold(0)(_ + _.length)

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

    override def accept(stream: (Stream[Text] over Credit)^): Text =
      // Ownership moves through a neutral carrier: the trait's `accept` cannot
      // declare `consume`, so the transfer to (consuming) `gather` is by convention.
      val streamRef: AnyRef = stream.asInstanceOf[AnyRef]
      gather(streamRef.asInstanceOf[(Stream[Text] over Credit)^])

    def aggregate(source0: Chain[Text]): Text =
      var source = source0

      val builder = new StringBuilder()

      while !source.nil do
        builder.append(source.head.s)
        source = source.tail

      builder.toString.tt

  given stream: [element, element2] => (aggregable: element2 is Aggregable by element)
  =>  Chain[element2] is Aggregable by element =

    element => Chain(aggregable.aggregate(element))

trait Aggregable extends Typeclass, Operable:
  // The capturing self type permits instances that capture (e.g. a resolution-scoped
  // tactic) while keeping the explicit self name the 3.9 SAM adaptation requires.
  aggregable: Aggregable^ =>
  def aggregate(source: Chain[Operand]): Self

  // Consume a pull endpoint. The default materializes one chunk per refill
  // and delegates to the legacy `aggregate`; instances override it to build
  // directly from the stream's windows.
  def accept(stream: (Stream[Operand] over Credit)^): Self =
    given stream.addressable.type = stream.addressable

    def recur(): Chain[Operand] =
      stream.refill(Credit(Long.MaxValue)) match
        case count: Int =>
          val chunk = stream.lend { region => range => region.materialize(range) }
          stream.skip(count)
          chunk #:: recur()

        case _ =>
          Chain()

    aggregate(Chain.defer(recur()))

  def map[self2](lambda: Self => self2): (self2 is Aggregable by Operand)^{this, lambda} = source =>
    lambda(aggregable.aggregate(source))
