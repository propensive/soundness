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

import soundness.*

// `soundness.Keypress` is an export forwarder, and reaching a case through it widens the case's
// singleton type to `Keypress`. The `Shift`, `Alt`, `Ctrl` and `Meta` fields are unions of
// singletons, so nothing built that way would typecheck; this by-name import shadows the
// forwarder with the enum itself, which keeps the cases precise.
import clavichord.Keypress

object Tests extends Suite(m"Clavichord tests"):
  def run(): Unit =
    def rendered(keypress: Keypress): Text = keypress.show

    suite(m"Edit keys"):
      test(m"Tab")       (rendered(Keypress.Tab))       .assert(_ == t"[⇥]")
      test(m"Enter")     (rendered(Keypress.Enter))     .assert(_ == t"[↵]")
      test(m"Backspace") (rendered(Keypress.Backspace)) .assert(_ == t"[⌫]")
      test(m"Delete")    (rendered(Keypress.Delete))    .assert(_ == t"[⌦]")
      test(m"Escape")    (rendered(Keypress.Escape))    .assert(_ == t"[⎋]")
      test(m"Up")        (rendered(Keypress.Up))        .assert(_ == t"[↑]")
      test(m"Down")      (rendered(Keypress.Down))      .assert(_ == t"[↓]")
      test(m"Left")      (rendered(Keypress.Left))      .assert(_ == t"[←]")
      test(m"Right")     (rendered(Keypress.Right))     .assert(_ == t"[→]")
      test(m"Home")      (rendered(Keypress.Home))      .assert(_ == t"[↖]")
      test(m"End")       (rendered(Keypress.End))       .assert(_ == t"[↘]")
      test(m"PageUp")    (rendered(Keypress.PageUp))    .assert(_ == t"[⇞]")
      test(m"PageDown")  (rendered(Keypress.PageDown))  .assert(_ == t"[⇟]")
      test(m"Insert")    (rendered(Keypress.Insert))    .assert(_ == t"[⎀]")

      test(m"every edit key renders distinctly"):
        val keys = List(Keypress.Tab, Keypress.Home, Keypress.End, Keypress.PageUp,
            Keypress.PageDown, Keypress.Insert, Keypress.Delete, Keypress.Enter,
            Keypress.Backspace, Keypress.Escape, Keypress.Left, Keypress.Right, Keypress.Up,
            Keypress.Down)

        keys.map(rendered(_)).to(Set).size
      . assert(_ == 14)

    suite(m"Character and function keys"):
      test(m"an ordinary character is bracketed"):
        rendered(Keypress.CharKey('a'))
      . assert(_ == t"[a]")

      test(m"a space is shown as its own symbol, not as a space"):
        rendered(Keypress.CharKey(' '))
      . assert(_ == t"[␣]")

      test(m"a digit is bracketed like any other character"):
        rendered(Keypress.CharKey('7'))
      . assert(_ == t"[7]")

      test(m"a function key is numbered"):
        rendered(Keypress.FunctionKey(5))
      . assert(_ == t"[F5]")

      test(m"a two-digit function key keeps both digits"):
        rendered(Keypress.FunctionKey(12))
      . assert(_ == t"[F12]")

      test(m"an escape sequence shows its identifier"):
        rendered(Keypress.EscapeSeq('R'))
      . assert(_ == t"[⎋R]")

      test(m"an escape sequence's content does not appear"):
        rendered(Keypress.EscapeSeq('R', '1', '2'))
      . assert(_ == t"[⎋R]")

    suite(m"Modifiers"):
      test(m"shift joins to the key it modifies"):
        rendered(Keypress.Shift(Keypress.Enter))
      . assert(_ == t"[⇧]+[↵]")

      test(m"alt joins to the key it modifies"):
        rendered(Keypress.Alt(Keypress.Right))
      . assert(_ == t"[⌥]+[→]")

      test(m"meta joins to the key it modifies"):
        rendered(Keypress.Meta(Keypress.Shift(Keypress.Tab)))
      . assert(_ == t"[⌘]+[⇧]+[⇥]")

      test(m"shift applies to a function key"):
        rendered(Keypress.Shift(Keypress.FunctionKey(3)))
      . assert(_ == t"[⇧]+[F3]")

      // `Ctrl`'s field is a union of `Keypress` and a set of `Char` literals, and `render`
      // widens it before testing, so the two arms are reached by different code. Both are
      // covered here.
      test(m"control of a character brackets the character"):
        rendered(Keypress.Ctrl('C'))
      . assert(_ == t"[⌃]+[C]")

      test(m"control of a punctuation character brackets it too"):
        rendered(Keypress.Ctrl('['))
      . assert(_ == t"[⌃]+[[]")

      test(m"control of a keypress renders that keypress"):
        rendered(Keypress.Ctrl(Keypress.Left))
      . assert(_ == t"[⌃]+[←]")

      test(m"nested modifiers are joined with plus"):
        rendered(Keypress.Ctrl(Keypress.Shift(Keypress.Enter)))
      . assert(_ == t"[⌃]+[⇧]+[↵]")

      test(m"the full modifier stack nests outermost-first"):
        rendered(Keypress.Meta(Keypress.Ctrl(Keypress.Alt(Keypress.Shift(Keypress.Home)))))
      . assert(_ == t"[⌘]+[⌃]+[⌥]+[⇧]+[↖]")
