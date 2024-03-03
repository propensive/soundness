/*
    Escritoire, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import vacuous.*

package tableStyles:
  import BoxLine.*
  given default: TableStyle = TableStyle(1, Thick, Thick, Thin, Thick, Thin, LineCharset.Default)
  given thinRounded: TableStyle = TableStyle(1, Thin, Thin, Thin, Thin, Thin, LineCharset.Rounded)
  given horizontal: TableStyle = TableStyle(1, Thin, Thin, Thin, Blank, Blank, LineCharset.Default)
  given midOnly: TableStyle = TableStyle(1, Blank, Blank, Thin, Blank, Blank, LineCharset.Default)
  given vertical: TableStyle = TableStyle(1, Blank, Blank, Blank, Thin, Thin, LineCharset.Default)
  given minimal: TableStyle = TableStyle(1, Unset, Unset, Thin, Blank, Blank, LineCharset.Default)

case class TableStyle
    (padding:    Int,
     topLine:    Optional[BoxLine],
     bottomLine: Optional[BoxLine],
     titleLine:  Optional[BoxLine],
     sideLines:  BoxLine,
     innerLines: BoxLine,
     charset:    LineCharset):

  def columnCost: Int = padding*2 + 1
  def cost(columns: Int): Int = columns*columnCost + 1