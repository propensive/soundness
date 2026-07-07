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
package archimedes

import anticipation.*
import gossamer.*
import vacuous.*

// Table schemata: `<mtable>` and its rows (`<mtr>`, `<mlabeledtr>`), cells
// (`<mtd>`), and the alignment markers `<maligngroup>` and `<malignmark>`.

sealed trait Tabular extends Mathml:
  def text: Optional[Text] = Unset

object Mtable:
  def apply(rows: Mathml*): Mtable = Mtable(rows.to(List))

case class Mtable(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
  def label: Text = t"mtable"

object Mtr:
  def apply(cells: Mathml*): Mtr = Mtr(cells.to(List))

case class Mtr(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
  def label: Text = t"mtr"

object Mlabeledtr:
  def apply(cells: Mathml*): Mlabeledtr = Mlabeledtr(cells.to(List))

case class Mlabeledtr(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
  def label: Text = t"mlabeledtr"

object Mtd:
  def apply(children: Mathml*): Mtd = Mtd(children.to(List))

case class Mtd(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Tabular:
  def label: Text = t"mtd"

case class Maligngroup(attributes: List[(Text, Text)] = Nil) extends Tabular:
  def label: Text = t"maligngroup"
  def contents: List[Mathml] = Nil

case class Malignmark(attributes: List[(Text, Text)] = Nil) extends Tabular:
  def label: Text = t"malignmark"
  def contents: List[Mathml] = Nil
