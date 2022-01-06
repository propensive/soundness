/*
    Escapade, version 0.1.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escapade

import rudiments.*

type Escape = Ansi.Input.Esc

object Escape:
  def apply(code: String, reset: Maybe[String] = Unset): Escape =
    Ansi.Input.Esc(code, reset.otherwise(""))

object escapes:
  object foreground:
    val Black = Escape("[30m", "[39m")
    val Red = Escape("[31m", "[39m")
    val Green = Escape("[32m", "[39m")
    val Yellow = Escape("[33m", "[39m")
    val Blue = Escape("[34m", "[39m")
    val Magenta = Escape("[35m", "[39m")
    val Cyan = Escape("[36m", "[39m")
    val White = Escape("[37m", "[39m")

    val BrightBlack = Escape("[90m", "[39m")
    val BrightRed = Escape("[91m", "[39m")
    val BrightGreen = Escape("[92m", "[39m")
    val BrightYellow = Escape("[93m", "[39m")
    val BrightBlue = Escape("[94m", "[39m")
    val BrightMagenta = Escape("[95m", "[39m")
    val BrightCyan = Escape("[96m", "[39m")
    val BrightWhite = Escape("[97m", "[49m")

  object background:
    val Black = Escape("[40m", "[49m")
    val Red = Escape("[41m", "[49m")
    val Green = Escape("[42m", "[49m")
    val Yellow = Escape("[43m", "[49m")
    val Blue = Escape("[44m", "[49m")
    val Magenta = Escape("[45m", "[49m")
    val Cyan = Escape("[46m", "[49m")
    val White = Escape("[47m", "[49m")
    
    val BrightBlack = Escape("[100m", "[49m")
    val BrightRed = Escape("[101m", "[49m")
    val BrightGreen = Escape("[102m", "[49m")
    val BrightYellow = Escape("[103m", "[49m")
    val BrightBlue = Escape("[104m", "[49m")
    val BrightMagenta = Escape("[105m", "[49m")
    val BrightCyan = Escape("[106m", "[49m")
    val BrightWhite = Escape("[107m", "[49m")

  object styles:
    val Bold: Escape = Escape("[1m", "[22m")
    val Light: Escape = Escape("[2m", "[22m")
    val Italic: Escape = Escape("[3m", "[23m")
    val Underline: Escape = Escape("[4m", "[24m")
    val SlowBlink: Escape = Escape("[5m", "[25m")
    val FastBlink: Escape = Escape("[6m", "[25m")
    val Reverse: Escape = Escape("[7m", "[27m")
    val Conceal: Escape = Escape("[8m", "[28m")
    val Strike: Escape = Escape("[9m", "[29m")

  val Reset: Escape = Escape("[0m", "[0m")

  def title(name: String) = Escape(s"]0;$name${27.toChar}\\")