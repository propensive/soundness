/*
    Anticipation, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

object ColorDepth:
  def apply(colors: Int): ColorDepth = colors match
    case 8                => ColorDepth.Indexed8
    case 15 | 16          => ColorDepth.Indexed16
    case 52 | 64 | 88     => ColorDepth.Cube4
    case 256              => ColorDepth.Cube6
    case 65536 | 16777216 => ColorDepth.TrueColor
    case _                => ColorDepth.NoColor

enum ColorDepth:
  case NoColor, Indexed8, Indexed16, Cube4, Cube6, TrueColor
