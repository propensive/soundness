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
package escritoire

import gossamer.*
import vacuous.*
import anticipation.*
import beneficence.*

object Column:
  def apply[row, cell, text: Textual]
    ( title:         text,
      textAlign:     Optional[TextAlignment]     = Unset,
      verticalAlign: Optional[VerticalAlignment] = Unset,
      sizing:        Columnar                    = columnar.Paragraph,
      decorate:      row -> Optional[text -> text] = { (_: Any) => Unset } )
    ( get: row -> cell )
    ( using columnAlignment: Column.Alignment[cell] = Column.Alignment.topLeft )
    ( using text.Show[cell] )
  :   Column[row, text] =

    def contents(row: row): text = text.show(get(row))

    Column
      ( title,
        contents,
        textAlign.or(columnAlignment.text),
        verticalAlign.or(columnAlignment.vertical),
        sizing,
        decorate )

  // ColumnAlignment → Column.Alignment
  object Alignment:
    val topLeft: Column.Alignment[Any] = Column.Alignment(TextAlignment.Left, VerticalAlignment.Top)

    given byte: Column.Alignment[Byte] = Column.Alignment(TextAlignment.Right, VerticalAlignment.Top)
    given short: Column.Alignment[Short] = Column.Alignment(TextAlignment.Right, VerticalAlignment.Top)
    given int: Column.Alignment[Int] = Column.Alignment(TextAlignment.Right, VerticalAlignment.Top)
    given long: Column.Alignment[Long] = Column.Alignment(TextAlignment.Right, VerticalAlignment.Top)
    given text: Column.Alignment[Text] = Column.Alignment(TextAlignment.Left, VerticalAlignment.Top)

  case class Alignment[-column](text: TextAlignment, vertical: VerticalAlignment)
  extends Findable

case class Column[row, text: Textual]
  ( title:         text,
    get:           row -> text,
    textAlign:     TextAlignment,
    verticalAlign: VerticalAlignment,
    sizing:        Columnar,

    // A per-row DECORATION of the whole rendered cell — content plus its gutter padding —
    // applied by `Grid.render` after alignment. The renderer knows nothing of styling (the
    // `text` type stays abstract); a Teletype caller writes e.g.
    // `row => if winner(row) then { (line: Teletype) => e"${Bg(color)}($line)" } else Unset`,
    // and the e-interpolator's style combination fills the unstyled padding while any styles
    // inside the cell content win. No default here: it would clash with the factory
    // `apply`'s defaults (two overloads with default arguments); the factory supplies it.
    decorate:      row -> Optional[text -> text] ):

  def contramap[row2](lambda: row2 -> row): Column[row2, text] =
    Column[row2, text]
      ( title, row => get(lambda(row)), textAlign, verticalAlign, sizing,
        row => decorate(lambda(row)) )

  def retitle(title: text): Column[row, text] = copy(title = title)
