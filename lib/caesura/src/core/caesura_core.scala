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
package caesura

import scala.caps

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import panopticon.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

package dsvFormats:
  given csvFormat: Dsv.Format = Dsv.Format(false, ',', '"', '"')
  given csvWithHeaderFormat: Dsv.Format = Dsv.Format(true, ',', '"', '"')
  given tsvFormat: Dsv.Format = Dsv.Format(false, '\t', '"', '"')
  given tsvWithHeaderFormat: Dsv.Format = Dsv.Format(true, '\t', '"', '"')
  given ssvFormat: Dsv.Format = Dsv.Format(false, ' ', '"', '"')
  given ssvWithHeaderFormat: Dsv.Format = Dsv.Format(true, ' ', '"', '"')

package dsvRedesignations:
  given unchangedRedesignation: Dsv.Redesignation = identity(_)
  given lowerDottedRedesignation: Dsv.Redesignation = _.uncamel.map(_.lower).join(t" ")
  given lowerSlashedRedesignation: Dsv.Redesignation = _.uncamel.map(_.lower).join(t" ")
  given capitalizedWordsRedesignation: Dsv.Redesignation = _.uncamel.map(_.capitalize).join(t" ")
  given lowerWordsRedesignation: Dsv.Redesignation = _.uncamel.map(_.lower).join(t" ")

extension [encodable: Encodable in Dsv](value: encodable) def dsv: Dsv = encodable.encode(value)

extension [encodable: Encodable in Dsv](value: List[encodable])
  def dsv: Sheet = Sheet(value.map(encodable.encode(_)).to[Array])

extension (consume stream: (Stream[Text] over Credit)^)
  // The rows of a character stream of DSV data, as a single-consumer
  // iterator: the streaming counterpart of `read[Sheet]`, which materializes.
  // Quoted cells may span chunk (and line) boundaries; the parser carries its
  // state across refills.
  def rows(using Dsv.Format, Tactic[Dsv.Error], Buffering): Iterator[Dsv]^ =
    Sheet.parseRows(stream)

extension (consume stream: (Stream[Text] over Credit)^)
  // Each row parsed straight to `value` through its `Dsv.Parsable` — the
  // streaming direct form: no `Dsv` row value is ever built.
  def rowsOf[value](using parsable: value is Dsv.Parsable)
    ( using Dsv.Format, Tactic[Dsv.Error], Buffering )
  :   Iterator[value]^ =

    parsedIterator(Sheet.directReader(stream), parsable)

// Constructed in a helper: a local binding of the fresh reader would hide it
// from the anonymous class (the statement rule).
private def parsedIterator[value](consume reader: DsvReader^, parsable: value is Dsv.Parsable)
:   Iterator[value]^ =

  new Iterator[value]:
    @caps.unsafe.untrackedCaptures
    private var pending: Optional[value] = Unset
    @caps.unsafe.untrackedCaptures
    private var finished: Boolean = false

    def hasNext: Boolean =
      if pending.present then true
      else if finished then false
      else
        pending = if reader.nextRow() then parsable.parse(reader, 0) else Unset
        if pending.absent then finished = true
        pending.present

    def next(): value =
      if !hasNext then Iterator.empty.next()
      val row = pending.or(Iterator.empty.next())
      pending = Unset
      row

// Panopticon optics for tabular data (no nesting, so they mirror the row/cell
// structure rather than JSON's map/array). `dsvCellLens` reads/writes a cell by column
// name within a row; the `Sheet` opticals address the n-th row (`Ordinal`), every
// row (`Each`), or rows matching a predicate (`Filter`). So
// `sheet.lens(_(Sec).name = t"…")` updates the "name" column of the second row.
package optics:
  private def cell(row: Dsv, name: String): Text =
    row.columns.let(_(name.tt)).let: index => row.data.at(index.z)
    . or(t"")

  private def withCell(row: Dsv, name: String, value: Text): Dsv =
    row.columns.let(_(name.tt)).lay(row): index => row.copy(data = Array.frozen(row.data.readable.updated(index, value)))

  given dsvCellLens: [name <: Label: ValueOf] => (erased dynamicDsvEnabler: DynamicDsvEnabler)
  =>  name is Lens from Dsv onto Text =
    Lens(cell(_, valueOf[name]), withCell(_, valueOf[name], _))

  given dsvRowOptical: [element] => Ordinal is Optical from Sheet onto Dsv = ordinal =>
    Optic: (origin, lambda) =>
      origin.copy(rows = origin.rows.indexed.remap: (row, index) =>
        if index == ordinal then lambda(row) else row)

  given dsvRowEachOptical: Each.type is Optical from Sheet onto Dsv = _ =>
    Optic: (origin, lambda) => origin.copy(rows = origin.rows.remap(lambda))

  // The `predicate` laundering is for the Scala.js pipeline, which — unlike the JVM pipeline —
  // rejects the `Optic`'s capture of `filter.predicate` against the required pure `Optic` type.
  // (Compiler divergence; see #1520 and the identical laundering in `panopticon.Optical.filter`.)
  given dsvRowFilterOptical: Filter[Dsv] is Optical from Sheet onto Dsv = filter =>
    val predicate: Dsv -> Boolean = caps.unsafe.unsafeAssumePure(filter.predicate)

    Optic: (origin, lambda) =>
      origin.copy
        ( rows = origin.rows.remap { row => if predicate(row) then lambda(row) else row } )

