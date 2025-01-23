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

import anticipation.*
import gossamer.{where as _, *}
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

trait Interaction[ResultType, QuestionType]:
  def before(): Unit = ()
  def render(state: Optional[QuestionType], menu: QuestionType): Unit
  def after(): Unit = ()
  def result(state: QuestionType): ResultType

  @tailrec
  final def recur(stream: LazyList[TerminalEvent], state: QuestionType, oldState: Optional[QuestionType])
     (key: (QuestionType, TerminalEvent) => QuestionType)
  :     Optional[(ResultType, LazyList[TerminalEvent])] =

    render(oldState, state)

    stream match
      case Keypress.Enter #:: tail           => (result(state), tail)
      case Keypress.Ctrl('C' | 'D') #:: tail => Unset
      case Keypress.Escape #:: tail          => Unset
      case other #:: tail                    => recur(tail, key(state, other), state)(key)
      case _                                 => Unset

  def apply(stream: LazyList[TerminalEvent], state: QuestionType)
     (key: (QuestionType, TerminalEvent) => QuestionType)
  :     Optional[(ResultType, LazyList[TerminalEvent])] =

    before()
    recur(stream, state, Unset)(key).also(after())

object Interaction:
  given selectMenu[ItemType: Showable](using Stdio): Interaction[ItemType, SelectMenu[ItemType]] with
    override def before(): Unit = Out.print(t"\e[?25l")
    override def after(): Unit = Out.print(t"\e[J\e[?25h")

    def render(old: Optional[SelectMenu[ItemType]], menu: SelectMenu[ItemType]) =
      menu.options.each: option =>
        Out.print((if option == menu.current then t" > $option" else t"   $option")+t"\e[K\n")
      Out.print(t"\e[${menu.options.length}A")

    def result(state: SelectMenu[ItemType]): ItemType = state.current

  given lineEditor(using Stdio): Interaction[Text, LineEditor] with
    override def after(): Unit = Out.println()

    def render(editor: Optional[LineEditor], editor2: LineEditor): Unit = Out.print:
      Text.construct:
        editor.let { editor => if editor.position > 0 then append(t"\e[${editor.position}D") }
        append(t"\e[K")
        val line = t"${editor2.value}${t" "*(editor.or(editor2).value.length - editor2.value.length)}"
        append(line)
        if line.length > 0 then append(t"\e[${line.length}D")
        if editor2.position > 0 then append(t"\e[${editor2.position}C")

    def result(editor: LineEditor): Text = editor.value
