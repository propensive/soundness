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
┃    Soundness, version 0.36.0.                                                                    ┃
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
import gossamer.{where as _, *}
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

trait Interaction[result, question]:
  def before(): Unit = ()
  def render(state: Optional[question], menu: question): Unit
  def after(): Unit = ()
  def result(state: question): result


  @tailrec
  final def recur
             (stream: Stream[TerminalEvent], state: question, oldState: Optional[question])
             (key: (question, TerminalEvent) => question)
  : Optional[(result, Stream[TerminalEvent])] =

      render(oldState, state)

      stream match
        case Keypress.Enter #:: tail           => (result(state), tail)
        case Keypress.Ctrl('C' | 'D') #:: tail => Unset
        case Keypress.Escape #:: tail          => Unset
        case other #:: tail                    => recur(tail, key(state, other), state)(key)
        case _                                 => Unset


  def apply(stream: Stream[TerminalEvent], state: question)
       (key: (question, TerminalEvent) => question)
  : Optional[(result, Stream[TerminalEvent])] =

      before()
      recur(stream, state, Unset)(key).also(after())


object Interaction:
  given selectMenu: [item: Showable] => Stdio => Interaction[item, SelectMenu[item]]:
    override def before(): Unit = Out.print(t"\e[?25l")
    override def after(): Unit = Out.print(t"\e[J\e[?25h")

    def render(old: Optional[SelectMenu[item]], menu: SelectMenu[item]) =
      menu.options.each: option =>
        Out.print((if option == menu.current then t" > $option" else t"   $option")+t"\e[K\n")
      Out.print(t"\e[${menu.options.length}A")

    def result(state: SelectMenu[item]): item = state.current

  given lineEditor: Stdio => Interaction[Text, LineEditor]:
    override def after(): Unit = Out.println()

    def render(editor: Optional[LineEditor], editor2: LineEditor): Unit = Out.print:
      Text.construct:
        editor.let { editor => if editor.position > 0 then append(t"\e[${editor.position}D") }
        append(t"\e[K")

        val line =
          t"${editor2.value}${t" "*(editor.or(editor2).value.length - editor2.value.length)}"

        append(line)
        if line.length > 0 then append(t"\e[${line.length}D")
        if editor2.position > 0 then append(t"\e[${editor2.position}C")

    def result(editor: LineEditor): Text = editor.value
