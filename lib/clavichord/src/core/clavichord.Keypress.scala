                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package clavichord

import anticipation.*
import gossamer.*
import spectacular.*

// A key, as pressed: an ordinary character, a named editing or function key, or one of those
// wrapped in the modifiers held down with it. Terminals and browsers agree on what a keypress
// *is* and disagree only on how it is encoded, so the model lives here, apart from either —
// profanity decodes terminal escape sequences into it, and tarantula renders it as the WebDriver
// actions the protocol defines.
object Keypress:
  type EditKey = Tab.type | Home.type | End.type | PageUp.type | PageDown.type | Insert.type |
    Delete.type | Enter.type | Backspace.type | Escape.type | Left.type | Right.type | Up.type |
    Down.type

  // Renders a keypress with a Unicode symbol in square brackets for each special
  // key (and modifier), joining a modifier to the key it modifies with `+`; an
  // ordinary character is shown as itself. E.g. `[⇧]+[↵]`, `[⌃]+C`, `[⌥]+[→]`.
  private def render(keypress: Keypress): Text =
    def key(symbol: Text): Text = t"[$symbol]"

    keypress match
      // Typed patterns with field access rather than extractors: under capture checking, an
      // enum-case unapply of a union-typed field fails to unify with the synthesized
      // capture-variable-decorated scrutinee.
      case shift: Shift => t"${key(t"⇧")}+${render(shift.keypress)}"
      case alt: Alt     => t"${key(t"⌥")}+${render(alt.keypress)}"
      case meta: Meta   => t"${key(t"⌘")}+${render(meta.keypress)}"

      case ctrl: Ctrl =>
        // Widened to a clean binary union first: a type test against the raw field type's
        // GADT-narrowed intersections fails to unify with its capture-variable-decorated
        // form under capture checking.
        val inner: Keypress | Char = ctrl.keypress

        inner match
          case char: Char      => t"${key(t"⌃")}+${key(char.show)}"
          case other: Keypress => t"${key(t"⌃")}+${render(other)}"

      case CharKey(' ')      => key(t"␣")
      case CharKey(char)     => key(char.show)
      case FunctionKey(n)    => key(t"F${n.show}")
      case EscapeSeq(id, _*) => key(t"⎋${id.show}")

      case Tab       => key(t"⇥")
      case Enter     => key(t"↵")
      case Backspace => key(t"⌫")
      case Delete    => key(t"⌦")
      case Escape    => key(t"⎋")
      case Up        => key(t"↑")
      case Down      => key(t"↓")
      case Left      => key(t"←")
      case Right     => key(t"→")
      case Home      => key(t"↖")
      case End       => key(t"↘")
      case PageUp    => key(t"⇞")
      case PageDown  => key(t"⇟")
      case Insert    => key(t"⎀")

  given showable: Keypress is Showable = render(_)

enum Keypress:
  case Tab, Home, End, PageUp, PageDown, Insert, Delete, Enter, Backspace, Escape, Left, Right, Up,
    Down

  case CharKey(char: Char)
  case FunctionKey(number: Int)
  case EscapeSeq(id: Char, content: Char*)
  case Shift(keypress: Keypress.EditKey | FunctionKey)
  case Alt(keypress: Shift | Keypress.EditKey | FunctionKey)

  case
    Ctrl
      ( keypress: Alt | Shift | Keypress.EditKey | FunctionKey | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' |
        'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' |
        'V' | 'W' | 'X' | 'Y' | 'Z' | '[' | '\\' | ']' | '^' | '_' | '@' )

  case Meta(keypress: Ctrl | Alt | Shift | Keypress.EditKey | FunctionKey)
