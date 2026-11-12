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

import scala.language.experimental.pureFunctions

import anticipation.*
import fulminate.*
import rudiments.at
import tessellate.*
import vacuous.*

object Table:
  // TableError → Table.Error
  case class Error(minimumWidth: Int, availableWidth: Int)(using Diagnostics)
  extends fulminate.Error
    ( m"""
        the table required a minimum width of $minimumWidth, but only $availableWidth was available
      """ )

  case class Cell[text]
    ( width:         Int,
      span:          Int,
      lines:         Sequence[text],
      minHeight:     Int,
      textAlign:     TextAlignment,
      verticalAlign: VerticalAlignment = VerticalAlignment.Top,
      decorate:      Optional[text -> text] = Unset ):

    // A bounds-checked positional read whose result type is total: the caller has already
    // tested `line` against `minHeight`, and there is no `text` value to fall back on.
    def apply(line: Int): text = lines.stdlib(line)

  case class Row[text](cells: Array[Cell[text]]^{}, title: Boolean, height: Int):
    def apply(column: Int): Cell[text] = cells.readUnchecked(column)

  case class Section[text](widths: Array[Int]^{}, rows: Chain[Row[text]])

  case class Style
    ( padding:    Int,
      topLine:    Optional[BoxLine],
      bottomLine: Optional[BoxLine],
      titleLine:  Optional[BoxLine],
      sideLines:  BoxLine,
      innerLines: BoxLine,
      charset:    LineCharset ):

    def columnCost: Int = padding*2 + 1
    def cost(columns: Int): Int = columns*columnCost + 1

  trait Relabelling[+target]:
    def relabelling(): Map[Text, Text]
    private lazy val labels: Map[Text, Text] = relabelling()

    def apply(label: Text): Optional[Text] =
      labels.at(label)
