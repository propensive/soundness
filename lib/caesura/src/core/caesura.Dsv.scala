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

trait Dsv2:
  // Generic fallback: any `Decodable in Text` (custom types, enums, `Uuid`, …)
  // decodes from the row's first cell. Held at lower priority than the primitive
  // `Decodable in Dsv` givens in `object Dsv` (which accrue errors via raise+yet)
  // so those win for Int/Long/Double/… where a `Decodable in Text` also exists —
  // mirroring how distillate keeps its generic instance in `Decodable2`.
  given decoder: [decodable: Decodable in Text] => decodable is Decodable in Dsv =
    value => decodable.decoded(value.data.head)

object Dsv extends Dsv2:
  def apply(iterable: Iterable[Text]): Dsv = new Dsv(IArray.from(iterable))
  def apply(text: Text*): Dsv = new Dsv(IArray.from(text))

  // An absent cell (a short positional row, or a header column missing from the row) decodes
  // to `Unset`; a present cell — even an empty one — decodes its inner value. The product
  // decoder hands an empty `Dsv` to a field whose cell is absent (see `conjunction`). Sits at
  // the same priority as the primitive cell decoders (above the generic `decoder` in `Dsv2`),
  // and short-circuits an absent cell to `Unset` before the inner decoder would `raise` Absent.
  given optionalDecodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( decodable: => inner is Decodable in Dsv )
  =>  value is Decodable in Dsv =
    row => if row.data.length == 0 then Unset else decodable.decoded(row)

  given encoder: [encodable: Encodable in Text] => encodable is Encodable in Dsv =
    value => Dsv(encodable.encode(value))

  // Primitive cell decoders. Unlike the generic `decoder` (which bottoms out in
  // distillate's `abort`ing text decoders), these register an error and continue
  // with a sentinel value (`raise … yet sentinel`), so that when a row is decoded
  // into a product under a `validate[CellRef]` boundary every malformed cell is
  // accrued rather than the first failure aborting the whole record. An absent
  // cell (short row / missing column) is distinguished from a present-but-
  // unparseable one. The real cell location is carried by the enclosing
  // `focus(CellRef(…))` in the product derivation, so `DsvError` itself is built
  // through the position-free companion `apply`.
  private def decodeCell[value]
    ( dsv: Dsv, expected: Text, sentinel: value )
    ( parse: Text => Optional[value] )
    ( using format: DsvFormat, tactic: Tactic[DsvError] )
  :   value =

    dsv.data.prim.lay(raise(DsvError(format, DsvError.Reason.Absent)) yet sentinel): cell =>
      parse(cell).or:
        raise(DsvError(format, DsvError.Reason.Unparseable(cell, expected))) yet sentinel

  given int: (format: DsvFormat) => Tactic[DsvError] => Int is Decodable in Dsv = dsv =>
    decodeCell(dsv, t"Int", 0): cell =>
      try Integer.parseInt(cell.s) catch case _: NumberFormatException => Unset

  given long: (format: DsvFormat) => Tactic[DsvError] => Long is Decodable in Dsv = dsv =>
    decodeCell(dsv, t"Long", 0L): cell =>
      try java.lang.Long.parseLong(cell.s) catch case _: NumberFormatException => Unset

  given double: (format: DsvFormat) => Tactic[DsvError] => Double is Decodable in Dsv = dsv =>
    decodeCell(dsv, t"Double", 0.0): cell =>
      try java.lang.Double.parseDouble(cell.s) catch case _: NumberFormatException => Unset

  given float: (format: DsvFormat) => Tactic[DsvError] => Float is Decodable in Dsv = dsv =>
    decodeCell(dsv, t"Float", 0.0f): cell =>
      try java.lang.Float.parseFloat(cell.s) catch case _: NumberFormatException => Unset

  given boolean: (format: DsvFormat) => Tactic[DsvError] => Boolean is Decodable in Dsv = dsv =>
    decodeCell(dsv, t"Boolean", false): cell =>
      cell.s match
        case "true"  => true
        case "false" => false
        case _       => Unset

  given text: (format: DsvFormat) => Tactic[DsvError] => Text is Decodable in Dsv = dsv =>
    decodeCell(dsv, t"Text", t""): cell =>
      cell

  given string: (format: DsvFormat) => Tactic[DsvError] => String is Decodable in Dsv = dsv =>
    decodeCell(dsv, t"String", ""): cell =>
      cell.s


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
    class DsvProductDecoder[derivation](lambda: Dsv -> derivation)
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
                // Positional mode (`columns` Unset): read the field's span starting at `count`.
                // Header mode: locate the field by column name; an absent column hands the field
                // an empty `Dsv` so `Optional` fields decode to `Unset` rather than misreading by
                // position.
                val row2 = row.columns.lay(Dsv(row.data.drop(count))): columns =>
                  columns.at(label).lay(Dsv(IArray[Text]())): i =>
                    Dsv(row.data.drop(i))

                count += spans(index)

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
  def as[cell: Decodable in Dsv]: cell raises DsvError tracks CellRef = cell.decoded(this)

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
