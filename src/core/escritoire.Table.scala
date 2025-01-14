/*
    Escritoire, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escritoire

import anticipation.*
import gossamer.*
import rudiments.*

import scala.collection.immutable as sci

object Table:
  @targetName("make")
  def apply[RowType](using DummyImplicit)[TextType: ClassTag: Textual]
     (columns0: Column[RowType, TextType]*)
          : Table[RowType, TextType] =

    new Table(columns0*)

case class Table[RowType, TextType: {ClassTag, Textual as textual}]
   (columns0: Column[RowType, TextType]*):
  table =>

  val columns: IArray[Column[RowType, TextType]] = IArray.from(columns0)
  val titles: Seq[IArray[IArray[TextType]]] =
    Seq(IArray.from(columns.map { column => IArray.from(column.title.cut(t"\n")) }))

  def tabulate(data: Seq[RowType]): Tabulation[TextType] { type Row = RowType } =
    new Tabulation[TextType]:
      type Row = RowType

      val columns: IArray[Column[Row, TextType]] = table.columns
      val titles: Seq[IArray[IArray[TextType]]] = table.titles
      val dataLength: Int = data.length

      val rows: Seq[IArray[IArray[TextType]]] =
        data.map { row => columns.map { column => IArray.from(column.get(row).lines) } }
