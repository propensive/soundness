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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package profanity

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

object Interaction:
  given selectMenu: [item: Showable] => (terminal: Terminal) => Interaction[item, SelectMenu[item]]:
    given Stdio = terminal.stdio
    override def before(): Unit = Out.print(t"\e[?25l")
    override def after(): Unit = Out.print(t"\e[J\e[?25h")

    def render(old: Optional[SelectMenu[item]], menu: SelectMenu[item]) =
      val cols = terminal.knownColumns.max(1)

      Out.print:
        Text.build:
          append(t"\e[J")
          var totalRows = 0

          menu.options.each: option =>
            val full = (if option == menu.current then t" > $option" else t"   $option")
            append(full)
            append(t"\e[E")
            totalRows += (full.length - 1)/cols + 1

          if totalRows > 0 then append(t"\e[${totalRows}F")

    def result(state: SelectMenu[item]): item = state.current

  given lineEditor: (terminal: Terminal) => Interaction[Text, LineEditor]:
    given Stdio = terminal.stdio
    override def after(): Unit = Out.println()

    def render(old: Optional[LineEditor], editor: LineEditor): Unit =
      val cols = terminal.knownColumns.max(1)
      val len = editor.value.length
      val curRow = editor.position/cols
      val curCol = editor.position%cols

      Out.print:
        Text.build:
          old.let: o =>
            val oldRow = o.position/cols
            if oldRow > 0 then append(t"\e[${oldRow}F") else append(t"\r")

          append(t"\e[J")
          append(editor.value)

          if len > 0 then
            val printedRows = (len - 1)/cols
            if printedRows > 0 then append(t"\e[${printedRows}F") else append(t"\r")

          if curRow > 0 then append(t"\e[${curRow}B")
          if curCol > 0 then append(t"\e[${curCol + 1}G")

    def result(editor: LineEditor): Text = editor.value


trait Interaction[result, question]:
  def before(): Unit = ()
  def render(state: Optional[question], menu: question): Unit
  def after(): Unit = ()
  def result(state: question): result

  // Which event submits the answer (default: Enter). A multi-line editor overrides
  // this — e.g. to submit on Shift+Enter and let plain Enter insert a newline.
  def submits(event: TerminalEvent): Boolean = event match
    case Keypress.Enter => true
    case _              => false


  @tailrec
  final def recur
    ( events: Iterator[TerminalEvent], state: question, oldState: Optional[question] )
    ( key: (question, TerminalEvent) => question )
  :   Optional[result] =

    render(oldState, state)

    if !events.hasNext then Unset
    else events.next() match
      case Keypress.Ctrl('C' | 'D') => Unset
      case Keypress.Escape          => Unset
      case event if submits(event)  => result(state)
      case other                    => recur(events, key(state, other), state)(key)


  def apply(events: Iterator[TerminalEvent], state: question)
    ( key: (question, TerminalEvent) => question )
  :   Optional[result] =

    before()
    recur(events, state, Unset)(key).also(after())
