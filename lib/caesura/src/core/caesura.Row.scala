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
┃    Soundness, version 0.43.0.                                                                    ┃
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

case class Row(data: IArray[Text], columns: Optional[Map[Text, Int]] = Unset) extends Dynamic:
  def as[cell: Decodable in Row]: cell = cell.decoded(this)

  def header: Optional[IArray[Text]] = columns.let: map =>
    val columns = map.map(_.swap)
    IArray.tabulate(columns.size)(columns(_))

  def selectDynamic[value: Decodable in Text](field: String)
       (using DynamicDsvEnabler, DsvRedesignation)
  : Optional[value] =

      apply(summon[DsvRedesignation].transform(field.tt))


  def apply[value: Decodable in Text](field: Text): Optional[value] =
    columns.let(_.at(field)).let { index => data.at(index.z) }.let(value.decoded(_))

  override def hashCode: Int = data.indices.fuse(0)(state*31 + data(next).hashCode)

  override def equals(that: Any): Boolean = that.asMatchable match
    case row: Row =>
      data.length == row.data.length && data.indices.all: index =>
        data(index) == row.data(index)

    case _        => false

object Row:
  def apply(iterable: Iterable[Text]): Row = new Row(IArray.from(iterable))
  def apply(text: Text*): Row = new Row(IArray.from(text))

  given decoder: [decodable: Decodable in Text] => decodable is Decodable in Row =
    value => decodable.decoded(value.data.head)

  given encoder: [encodable: Encodable in Text] => encodable is Encodable in Row =
    value => Row(encodable.encode(value))

  inline given decodableDerivation: [value <: Product: ProductReflection]
               => value is Decodable in Row = DecodableDerivation.derived[value]

  inline given encodableDerivation: [value <: Product: ProductReflection]
               => value is Encodable in Row = EncodableDerivation.derived[value]

  given showable: (format: DsvFormat) => Row is Showable =
    _.data.map: cell =>
      if !cell.contains(format.Quote) then cell else
        Text.construct:
          append(format.quote)

          cell.s.foreach: char =>
            if char == format.quote then append(char)
            append(char)

          append(format.quote)

    . join(format.delimiter.show)

  object DecodableDerivation extends ProductDerivable[Decodable in Row]:
    class DsvProductDecoder[derivation](lambda: Row => derivation)
    extends Decodable:
      type Self = derivation
      type Form = Row
      def decoded(row: Row): derivation = lambda(row)

    inline def join[derivation <: Product: ProductReflection]: derivation is Decodable in Row =
      var rowNumber: Ordinal = Prim
      val spans: IArray[Int] = Spannable.derived[derivation].spans()
      var count = 0

      provide[Foci[CellRef]]:
        DsvProductDecoder[derivation]((row: Row) => construct:
          [field] => context =>
            val i = row.columns.let(_.at(label)).or(count)
            count += spans(index)
            val row2 = Row(row.data.drop(i))
            focus(CellRef(rowNumber, label)):
              typeclass.decoded(row2))

  object EncodableDerivation extends ProductDerivable[Encodable in Row]:
    inline def join[derivation <: Product: ProductReflection]: derivation is Encodable in Row =
      value =>
        val cells =
          fields(value):
            [field] => field => context.encode(field).data
          . to(List)
          . flatten

        Row(cells)
