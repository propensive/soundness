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

import language.dynamics

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*

object Dsv:
  def apply(iterable: Iterable[Text]): Dsv = new Dsv(IArray.from(iterable))
  def apply(text: Text*): Dsv = new Dsv(IArray.from(text))

  given decoder: [decodable: Decodable in Text] => decodable is Decodable in Dsv =
    value => decodable.decoded(value.data.head)

  given encoder: [encodable: Encodable in Text] => encodable is Encodable in Dsv =
    value => Dsv(encodable.encode(value))


  inline given decodableDerivation: [value <: Product: ProductReflection]
  =>  value is Decodable in Dsv =

    DecodableDerivation.derived[value]

  // `source.read[Foo in Dsv]` shorthand for `source.read[Sheet].rows.head.as[Foo]`:
  // decodes a single DSV record (the first row) into `Foo`, mirroring the other
  // formats' `value in Format` aggregables. Multi-row sources should be read as a
  // `Sheet` instead (`Sheet.as[Foo]` yields a `Stream[Foo]`). The `Form` type-tag is
  // added by an `asInstanceOf` cast — `value in Dsv` is just `value { type Form = Dsv }`
  // so the cast is a no-op at runtime.
  given aggregableIn: [value: Decodable in Dsv] => (format: DsvFormat) => Tactic[DsvError]
  =>  (value in Dsv) is Aggregable by Text =
    text =>
      summon[Sheet is Aggregable by Text].aggregate(text).rows.head.as[value]
      . asInstanceOf[value in Dsv]


  inline given encodableDerivation: [value <: Product: ProductReflection]
  =>  value is Encodable in Dsv =

    EncodableDerivation.derived[value]


  given showable: (format: DsvFormat) => Dsv is Showable =
    _.data.map: cell =>
      val safe = !cell.contains(format.Quote) && !cell.contains(format.Delimiter) &&
        !cell.contains('\n') && !cell.contains('\r')

      if safe then cell else
        Text.build:
          append(format.quote)

          cell.chars.each: char =>
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

    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Decodable in Dsv =

      val spans: IArray[Int] = Spannable.derived[derivation].spans()

      // `count` must be local to each decode call, not captured per instance: a derived
      // decoder for a type used in more than one field (e.g. two `Foo` fields) is deduplicated
      // to a single shared instance, so per-instance mutable state would leak between
      // successive decodes.
      provide[Foci[CellRef]]:
        DsvProductDecoder[derivation]:
          (row: Dsv) =>
            var count = 0

            build[derivation]:
              [field] => context =>
                val i = row.columns.let(_.at(label)).or(count)
                count += spans(index)
                val row2 = Dsv(row.data.drop(i))

                focus(CellRef(Prim, label)):
                  contextual.decoded(row2)

  object EncodableDerivation extends ProductDerivable[Encodable in Dsv]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Encodable in Dsv =

      value =>
        val cells =
          fields(value):
            [field] => field => contextual.encode(field).data

          . to(List)
          . flatten

        Dsv(cells)

case class Dsv(data: IArray[Text], columns: Optional[Map[Text, Int]] = Unset) extends Dynamic:
  def as[cell: Decodable in Dsv]: cell = cell.decoded(this)

  def header: Optional[IArray[Text]] = columns.let: map =>
    val columns = map.map(_.swap)
    IArray.tabulate(columns.size)(columns(_))


  def selectDynamic[value: Decodable in Text](field: String)(using erased DynamicDsvEnabler)
    ( using DsvRedesignation )
  :   Optional[value] =

    apply(summon[DsvRedesignation].transform(field.tt))


  def apply[value: Decodable in Text](field: Text): Optional[value] =
    columns.let(_.at(field)).let { index => data.at(index.z) }.let(value.decoded(_))

  override def hashCode: Int = data.indices.fuse(0)(state*31 + data(next).hashCode)

  override def equals(that: Any): Boolean = that.asMatchable match
    case row: Dsv =>
      data.length == row.data.length && data.indices.all: index =>
        data(index) == row.data(index)

    case _ =>
      false
