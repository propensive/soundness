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
package caesura

import anticipation.*
import denominative.*
import gossamer.*
import panopticon.*
import prepositional.*
import rudiments.*
import vacuous.*

package dsvFormats:
  given csvFormat: DsvFormat = DsvFormat(false, ',', '"', '"')
  given csvWithHeaderFormat: DsvFormat = DsvFormat(true, ',', '"', '"')
  given tsvFormat: DsvFormat = DsvFormat(false, '\t', '"', '"')
  given tsvWithHeaderFormat: DsvFormat = DsvFormat(true, '\t', '"', '"')
  given ssvFormat: DsvFormat = DsvFormat(false, ' ', '"', '"')
  given ssvWithHeaderFormat: DsvFormat = DsvFormat(true, ' ', '"', '"')

package dsvRedesignations:
  given unchangedRedesignation: DsvRedesignation = identity(_)
  given lowerDottedRedesignation: DsvRedesignation = _.uncamel.map(_.lower).join(t" ")
  given lowerSlashedRedesignation: DsvRedesignation = _.uncamel.map(_.lower).join(t" ")
  given capitalizedWordsRedesignation: DsvRedesignation = _.uncamel.map(_.capitalize).join(t" ")
  given lowerWordsRedesignation: DsvRedesignation = _.uncamel.map(_.lower).join(t" ")

extension [encodable: Encodable in Dsv](value: encodable) def dsv: Dsv = encodable.encode(value)

extension [encodable: Encodable in Dsv](value: Seq[encodable])
  def dsv: Sheet = Sheet(value.to(Stream).map(encodable.encode(_)))

// Panopticon optics for tabular data (no nesting, so they mirror the row/cell
// structure rather than JSON's map/array). `cellLens` reads/writes a cell by column
// name within a row; the `Sheet` opticals address the n-th row (`Ordinal`), every
// row (`Each`), or rows matching a predicate (`Filter`). So
// `sheet.lens(_(Sec).name = t"…")` updates the "name" column of the second row.
private def cell(row: Dsv, name: String): Text =
  row.columns.let(_.at(name.tt)).let: index => row.data.at(index.z)
  . or(t"")

private def withCell(row: Dsv, name: String, value: Text): Dsv =
  row.columns.let(_.at(name.tt)).lay(row): index =>
    row.copy(data = row.data.updated(index, value))

given cellLens: [name <: Label: ValueOf] => (erased DynamicDsvEnabler)
=>  name is Lens from Dsv onto Text =
  Lens(cell(_, valueOf[name]), withCell(_, valueOf[name], _))

given rowOptical: [element] => Ordinal is Optical from Sheet onto Dsv = ordinal =>
  Optic: (origin, lambda) =>
    origin.copy(rows = origin.rows.zipWithIndex.map: (row, index) =>
      if index == ordinal.n0 then lambda(row) else row)

given rowEach: Each.type is Optical from Sheet onto Dsv = _ =>
  Optic: (origin, lambda) => origin.copy(rows = origin.rows.map(lambda))

given rowFilter: Filter[Dsv] is Optical from Sheet onto Dsv = filter =>
  Optic: (origin, lambda) =>
    origin.copy
      ( rows = origin.rows.map { row => if filter.predicate(row) then lambda(row) else row } )
