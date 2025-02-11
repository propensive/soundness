/*
    Profanity, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package profanity

import language.experimental.captureChecking

import proscenium.*

trait Keyboard:
  type Keypress
  def process(stream: Stream[Char]): Stream[Keypress]

object Keyboard:
  import Keypress.*

  def modified(code: Char, keypress: EditKey | FunctionKey)
  :     EditKey | FunctionKey | Shift | Alt | Ctrl | Meta =
    val n = code - '1'
    val shift: EditKey | FunctionKey | Shift = if (n&1) == 1 then Shift(keypress) else keypress
    val alt: EditKey | FunctionKey | Shift | Alt = if (n&2) == 2 then Alt(shift) else shift
    val ctrl: EditKey | FunctionKey | Shift | Alt | Ctrl = if (n&4) == 4 then Ctrl(alt) else alt

    if (n&8) == 8 then Meta(ctrl) else ctrl

  def navigation(code: Char): Keypress.EditKey = code match
    case 'A' => Keypress.Up
    case 'B' => Keypress.Down
    case 'C' => Keypress.Right
    case 'D' => Keypress.Left
    case 'F' => Keypress.End
    case 'H' => Keypress.Home
    case _   => Keypress.Escape

  def vt(code: Char): Keypress.EditKey = code match
    case '1' | '7' => Keypress.Home
    case '2'       => Keypress.Insert
    case '3'       => Keypress.Delete
    case '4' | '8' => Keypress.End
    case '5'       => Keypress.PageUp
    case '6'       => Keypress.PageDown
    case _         => Keypress.Escape
