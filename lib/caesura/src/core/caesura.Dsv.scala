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
package caesura

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

import scala.compiletime.*

import language.dynamics

case class Dsv(data: IArray[Text], columns: Optional[Map[Text, Int]] = Unset) extends Dynamic:
  def as[cell: Decodable in Dsv]: cell = cell.decoded(this)

  def header: Optional[IArray[Text]] = columns.let: map =>
    val columns = map.map(_.swap)
    IArray.tabulate(columns.size)(columns(_))

  def selectDynamic[value: Decodable in Text](field: String)
    ( using DynamicDsvEnabler, DsvRedesignation )
  : Optional[value] =

      apply(summon[DsvRedesignation].transform(field.tt))


  def apply[value: Decodable in Text](field: Text): Optional[value] =
    columns.let(_.at(field)).let { index => data.at(index.z) }.let(value.decoded(_))

  override def hashCode: Int = data.indices.fuse(0)(state*31 + data(next).hashCode)

  override def equals(that: Any): Boolean = that.asMatchable match
    case row: Dsv =>
      data.length == row.data.length && data.indices.all: index =>
        data(index) == row.data(index)

    case _        => false

object Dsv:
  def apply(iterable: Iterable[Text]): Dsv = new Dsv(IArray.from(iterable))
  def apply(text: Text*): Dsv = new Dsv(IArray.from(text))

  given decoder: [decodable: Decodable in Text] => decodable is Decodable in Dsv =
    value => decodable.decoded(value.data.head)

  given encoder: [encodable: Encodable in Text] => encodable is Encodable in Dsv =
    value => Dsv(encodable.encode(value))

  inline given decodableDerivation: [value <: Product: ProductReflection]
               => value is Decodable in Dsv = DecodableDerivation.derived[value]

  inline given encodableDerivation: [value <: Product: ProductReflection]
               => value is Encodable in Dsv = EncodableDerivation.derived[value]

  given showable: (format: DsvFormat) => Dsv is Showable =
    _.data.map: cell =>
      if !cell.contains(format.Quote) then cell else
        Text.construct:
          append(format.quote)

          cell.s.foreach: char =>
            if char == format.quote then append(char)
            append(char)

          append(format.quote)

    . join(format.delimiter.show)

  object DecodableDerivation extends ProductDerivable[Decodable in Dsv]:
    class DsvProductDecoder[derivation](lambda: Dsv => derivation)
    extends Decodable:
      type Self = derivation
      type Form = Dsv
      def decoded(row: Dsv): derivation = lambda(row)

    inline def join[derivation <: Product: ProductReflection]: derivation is Decodable in Dsv =
      var rowNumber: Ordinal = Prim
      val spans: IArray[Int] = Spannable.derived[derivation].spans()
      var count = 0

      provide[Foci[CellRef]]:
        DsvProductDecoder[derivation]((row: Dsv) => construct:
          [field] => context =>
            val i = row.columns.let(_.at(label)).or(count)
            count += spans(index)
            val row2 = Dsv(row.data.drop(i))
            focus(CellRef(rowNumber, label)):
              typeclass.decoded(row2))

  object EncodableDerivation extends ProductDerivable[Encodable in Dsv]:
    inline def join[derivation <: Product: ProductReflection]: derivation is Encodable in Dsv =
      value =>
        val cells =
          fields(value):
            [field] => field => context.encode(field).data
          . to(List)
          . flatten

        Dsv(cells)
