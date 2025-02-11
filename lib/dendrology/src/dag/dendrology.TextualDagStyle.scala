/*
    Dendrology, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package dendrology

import language.experimental.captureChecking

import anticipation.*
import gossamer.*

import DagTile.*

case class TextualDagStyle[LineType: Textual]
   (space:     Text,
    corner:    Text,
    vertical:   Text,
    firstMid:   Text,
    horizontal: Text,
    midLast:    Text,
    cross:     Text,
    overlap:    Text)
extends DagStyle[LineType]:
  def serialize(tiles: List[DagTile], node: LineType): LineType =
    LineType(tiles.map(text(_)).join)+node

  def text(tile: DagTile) = tile match
    case Space      => space
    case Corner     => corner
    case Vertical   => vertical
    case FirstMid   => firstMid
    case Horizontal => horizontal
    case MidLast    => midLast
    case Cross      => cross
    case Overlap    => overlap

  def followOnText(tile: DagTile): Text = tile match
    case Space | Horizontal | Corner | MidLast | Overlap => space
    case _                                               => vertical
