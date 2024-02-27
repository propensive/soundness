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

package tableStyles:
  given default: TableStyle =
    TableStyle(1, '│', '│', '│', '┌', '┬', '┐', '└', '┴', '┘', '├', '┼', '┤', '─', '─', '─')
 
  given horizontal: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', '─', ' ', ' ', '─', ' ', ' ', '─', ' ', '─', '─', '─')
 
  given minimalist: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '─', ' ', ' ', '─', ' ')
 
  given horizontalGaps: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '─', '─', '─')
 
  given horizontalDots: TableStyle =
    TableStyle(1, ' ', ' ', ' ', ' ', '╌', ' ', ' ', '╌', ' ', ' ', '╌', ' ', '╌', '╌', '╌')
 
  given doubled: TableStyle =
    TableStyle(1, '║', '│', '║', '╔', '╤', '╗', '╚', '╧', '╝', '╟', '┼', '╢', '═', '─', '═')
 
  given rounded: TableStyle =
    TableStyle(1, '│', '│', '│', '╭', '┬', '╮', '╰', '┴', '╯', '├', '┼', '┤', '─', '─', '─')
 
  given dotted: TableStyle =
    TableStyle(1, '┊', '┊', '┊', '┌', '┬', '┐', '└', '┴', '┘', '├', '┼', '┤', '╌', '╌', '╌')
 
  given outline: TableStyle =
    TableStyle(1, '┊', '┊', '┊', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '╌', '╌', '╌')
 
  given ascii: TableStyle =
    TableStyle(1, '|', '|', '|', '+', '+', '+', '+', '+', '+', '+', '+', '+', '-', '-', '-')
 
  given borderless: TableStyle =
    TableStyle(0, ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')

case class TableStyle
    ( pad:         Int,
      left:        Char,
      sep:         Char,
      right:       Char,
      topLeft:     Char,
      topSep:      Char,
      topRight:    Char,
      bottomLeft:  Char,
      bottomSep:   Char,
      bottomRight: Char,
      midLeft:     Char,
      midSep:      Char,
      midRight:    Char,
      topBar:      Char,
      midBar:      Char,
      bottomBar:   Char ):

  def cost(columns: Int): Int = columns*pad*2 + columns + 1