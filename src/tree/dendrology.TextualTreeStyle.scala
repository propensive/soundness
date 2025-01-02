/*
    Dendrology, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import rudiments.*

case class TextualTreeStyle[LineType: Textual](space: Text, last: Text, branch: Text, extender: Text)
extends TreeStyle[LineType]:

  def serialize(tiles: List[TreeTile], node: LineType): LineType =
    LineType(tiles.map(text(_)).join)+node

  def text(tile: TreeTile): Text = tile match
    case TreeTile.Space    => space
    case TreeTile.Last     => last
    case TreeTile.Branch   => branch
    case TreeTile.Extender => extender

  def followOnText(tile: TreeTile): Text = tile match
    case TreeTile.Space    => space
    case TreeTile.Last     => space
    case TreeTile.Branch   => extender
    case TreeTile.Extender => extender
