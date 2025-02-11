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

import gossamer.*
import vacuous.*

object Column:
  def apply[RowType, CellType, TextType: Textual]
     (title:      TextType,
      textAlign:    Optional[TextAlignment]     = Unset,
      verticalAlign: Optional[VerticalAlignment] = Unset,
      sizing:     Columnar                    = columnar.Prose)
     (get: RowType => CellType)
     (using columnAlignment: ColumnAlignment[CellType] = ColumnAlignment.topLeft)
     (using TextType.Show[CellType])
  :     Column[RowType, TextType] =

    def contents(row: RowType): TextType = TextType.show(get(row))

    Column
     (title,
      contents,
      textAlign.or(columnAlignment.text),
      verticalAlign.or(columnAlignment.vertical),
      sizing)

case class Column[RowType, TextType: Textual]
   (title:      TextType,
    get:        RowType => TextType,
    textAlign:    TextAlignment,
    verticalAlign: VerticalAlignment,
    sizing:     Columnar):

  def contramap[RowType2](lambda: RowType2 => RowType): Column[RowType2, TextType] =
    Column[RowType2, TextType](title, row => get(lambda(row)), textAlign, verticalAlign, sizing)

  def retitle(title: TextType): Column[RowType, TextType] = copy(title = title)
