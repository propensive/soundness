/*
    Profanity, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import vacuous.*
import gossamer.{where as _, *}
import spectacular.*
import turbulence.*
import contingency.*
import anticipation.*
import fulminate.*

//import language.experimental.captureChecking

case class DismissError() extends Error(m"the user dismissed an interaction")

trait Question[AnswerType]:
  def apply(keypress: TerminalEvent): Question[AnswerType]

case class LineEditor(value: Text = t"", position0: Optional[Int] = Unset) extends Question[Text]:
  val position = position0.or(value.length)
  import Keypress.*

  def apply(keypress: TerminalEvent): LineEditor = try keypress match
    case CharKey(ch)      => copy(t"${value.take(position)}$ch${value.drop(position)}", position + 1)
    case Ctrl('U')        => copy(value.drop(position), 0)

    case Ctrl('W')        => val prefix = value.take(0 max (position - 1)).reverse.dropWhile(_ != ' ').reverse
                             copy(t"$prefix${value.drop(position)}", prefix.length)

    case Delete      => copy(t"${value.take(position)}${value.drop(position + 1)}")
    case Backspace   => copy(t"${value.take(position - 1)}${value.drop(position)}", (position - 1) max 0)
    case Home        => copy(position0 = 0)
    case End         => copy(position0 = value.length)
    case Left        => copy(position0 = (position - 1) `max` 0)
    case Ctrl(Left)  => copy(position0 = ((position - 2 `max` 0) to 0 by -1).where(value.at(_) == ' ').lay(0)(_ + 1))

    case Ctrl(Right) => val range = ((position + 1) `min` (value.length - 1)) to (value.length - 1)
                        val position2 = range.where(value.at(_) == ' ').lay(value.length)(_ + 1)
                        copy(position0 = position2 `min` value.length)
    case Right       => copy(position0 = (position + 1) `min` value.length)
    case _           => this

  catch case e: RangeError => this

  def ask(using interactivity: Interactivity[TerminalEvent], interaction: Interaction[Text, LineEditor])
      [ResultType]
      (lambda: Interactivity[TerminalEvent] ?=> Text => ResultType)
          : ResultType raises DismissError =

    interaction(interactivity.eventStream(), this)(_(_)).lay(abort(DismissError())): (result, stream) =>
      lambda(using Interactivity(stream))(result)

trait Interaction[ResultType, QuestionType]:
  def before(): Unit = ()
  def render(state: Optional[QuestionType], menu: QuestionType): Unit
  def after(): Unit = ()
  def result(state: QuestionType): ResultType

  @tailrec
  final def recur(stream: LazyList[TerminalEvent], state: QuestionType, oldState: Optional[QuestionType])
      (key: (QuestionType, TerminalEvent) => QuestionType)
          : Optional[(ResultType, LazyList[TerminalEvent])] =

    render(oldState, state)

    stream match
      case Keypress.Enter #:: tail           => (result(state), tail)
      case Keypress.Ctrl('C' | 'D') #:: tail => Unset
      case Keypress.Escape #:: tail          => Unset
      case other #:: tail                    => recur(tail, key(state, other), state)(key)
      case _                                 => Unset

  def apply(stream: LazyList[TerminalEvent], state: QuestionType)
      (key: (QuestionType, TerminalEvent) => QuestionType)
          : Optional[(ResultType, LazyList[TerminalEvent])] =

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

case class SelectMenu[ItemType](options: List[ItemType], current: ItemType) extends Question[ItemType]:
  import Keypress.*

  def apply(keypress: TerminalEvent): SelectMenu[ItemType] = try keypress match
    case Up   => copy(current = options(0 max options.indexOf(current) - 1))
    case Down => copy(current = options(options.size - 1 min options.indexOf(current) + 1))
    case Home => copy(current = options.head)
    case End  => copy(current = options.last)
    case _    => this

  catch case e: RangeError => this

  def ask
      (using interactivity: Interactivity[TerminalEvent],
             interaction: Interaction[ItemType, SelectMenu[ItemType]])
      [ResultType]
      (lambda: Interactivity[TerminalEvent] ?=> ItemType => ResultType)
          : ResultType raises DismissError =

    interaction(interactivity.eventStream(), this)(_(_)).lay(abort(DismissError())): (result, stream) =>
      lambda(using Interactivity(stream))(result)

given realm: Realm = realm"profanity"
