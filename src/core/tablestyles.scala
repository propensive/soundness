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

import gossamer.*
import anticipation.*

package tableStyles:
  given default: TableStyle =
    TableStyle(t" ", t"│", t"│", t"│", t"┌", t"┬", t"┐", t"└", t"┴", t"┘", t"├", t"┼", t"┤", t"─", t"─", t"─")
 
  given horizontal: TableStyle =
    TableStyle(t" ", t" ", t" ", t" ", t" ", t"─", t" ", t" ", t"─", t" ", t" ", t"─", t" ", t"─", t"─", t"─")
 
  given minimalist: TableStyle =
    TableStyle(t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t"─", t" ", t" ", t"─", t" ")
 
  given horizontalGaps: TableStyle =
    TableStyle(t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t"─", t"─", t"─")
 
  given horizontalDots: TableStyle =
    TableStyle(t" ", t" ", t" ", t" ", t" ", t"╌", t" ", t" ", t"╌", t" ", t" ", t"╌", t" ", t"╌", t"╌", t"╌")
 
  given doubled: TableStyle =
    TableStyle(t" ", t"║", t"│", t"║", t"╔", t"╤", t"╗", t"╚", t"╧", t"╝", t"╟", t"┼", t"╢", t"═", t"─", t"═")
 
  given rounded: TableStyle =
    TableStyle(t" ", t"│", t"│", t"│", t"╭", t"┬", t"╮", t"╰", t"┴", t"╯", t"├", t"┼", t"┤", t"─", t"─", t"─")
 
  given dotted: TableStyle =
    TableStyle(t" ", t"┊", t"┊", t"┊", t"┌", t"┬", t"┐", t"└", t"┴", t"┘", t"├", t"┼", t"┤", t"╌", t"╌", t"╌")
 
  given outline: TableStyle =
    TableStyle(t" ", t"┊", t"┊", t"┊", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t"╌", t"╌", t"╌")
 
  given ascii: TableStyle =
    TableStyle(t" ", t"|", t"|", t"|", t"+", t"+", t"+", t"+", t"+", t"+", t"+", t"+", t"+", t"-", t"-", t"-")
 
  given borderless: TableStyle =
    TableStyle(t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ", t" ")

case class TableStyle
    (padding:      Text,
     left:         Text,
     separator:    Text,
     right:        Text,
     topLeft:      Text,
     topSeparator: Text,
     topRight:     Text,
     bottomLeft:   Text,
     bottomSep:    Text,
     bottomRight:  Text,
     midLeft:      Text,
     midSeparator: Text,
     midRight:     Text,
     topBar:       Text,
     midBar:       Text,
     bottomBar:    Text):

  def cost(columns: Int): Int = columns*padding.length*2 + columns + 1