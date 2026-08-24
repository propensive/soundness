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

// The frozen-Array `map` shim stays: `Reshapable` widens the (already frozen) element type
// to `Array[text]^{any.rd}`, and the nested `Array[Array[text]^{}]^{}` result rejects that.
// Pinning it with an ascription or freezing at each level does not help — the inner array is
// still `fresh.rd`. A capture-checking question, not a drain one.

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

object Scaffold:
  @targetName("make")
  def apply[row](using erased void: Void)[text: ClassTag: Textual](columns: Column[row, text]*)
  :   Scaffold[row, text] =

    new Scaffold(columns*)


case class Scaffold[row, text: {ClassTag, Textual as textual}](columns0: Column[row, text]*):
  scaffold =>

    val columns: Array[Column[row, text]]^{} = Array.from(columns0)

    // The element types are explicit: inference re-freshens the nested frozen arrays to
    // `any.rd`, which cannot flow back into the declared `^{}`.
    val titles: List[Array[Array[text]^{}]^{}] =
      List:
        Array.from[Array[text]^{}]:
          columns0.map { column => Array.from(column.title.cut(t"\n").stdlib) }

    def tabulate(data: List[row]): Tabulation[text] { type Row = row } = new Tabulation[text]:
      type Row = row

      val columns: Array[Column[Row, text]]^{} = scaffold.columns
      val titles: List[Array[Array[text]^{}]^{}] = scaffold.titles
      val dataLength: Int = data.stdlib.length

      val rows: List[Array[Array[text]^{}]^{}] =
        data.map: row =>
          columns.map[Array[text]^{}]: column =>
            Array.from(column.get(row).lines.stdlib)
