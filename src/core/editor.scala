/*
    Profanity, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import spectacular.*
import turbulence.*
import anticipation.*
import eucalyptus.*

//import language.experimental.captureChecking

object LineEditor:
  def concealed(str: Text): Text = str.mapChars { _ => '*' }

  def ask(initial: Text = t"", render: Text => Text = identity(_))(using Terminal): Text =
    Out.print(render(initial))

    def finished(key: TerminalEvent) =
      key == Keypress.Enter || key == Keypress.Control('D') || key == Keypress.Control('C')

    val buffer: StringBuilder = StringBuilder()

    summon[Terminal].events.takeWhile(!finished(_)).foldLeft(LineEditor(initial, initial.length)):
      case (editor, next) =>
        if editor.position > 0 then buffer.append(t"\e[${editor.position}D")
        val editor2 = editor(next)
        val line = t"${editor2.value}${t" "*(editor.value.length - editor2.value.length)}"
        buffer.append(t"\e[0K")
        buffer.append(render(line))
        val left = line.length - editor2.position
        if left > 0 then buffer.append(t"\e[${left}D")
        Out.print(buffer.text)
        buffer.clear()
        editor2
    .value

case class LineEditor(value: Text = t"", position: Int = 0):
  import Keypress.*

  def apply(keypress: TerminalEvent): LineEditor = try keypress match
    case CharKey(ch)      => copy(t"${value.take(position)}$ch${value.drop(position)}", position + 1)
    case Control('U')     => copy(value.drop(position), 0)

    case Control('W')     => val prefix = value.take(0 max (position - 1)).reverse.dropWhile(_ != ' ').reverse
                             copy(t"$prefix${value.drop(position)}", prefix.length)

    case Delete      => copy(t"${value.take(position)}${value.drop(position + 1)}")
    case Backspace   => copy(t"${value.take(position - 1)}${value.drop(position)}", (position - 1) max 0)
    case Home        => copy(position = 0)
    case End         => copy(position = value.length)
    case Left        => copy(position = (position - 1) max 0)
    case Ctrl(Left)  => copy(position = (position - 2 max 0 to 0 by -1).find(value(_) == ' ').fold(0)(_ + 1))

    case Ctrl(Right) => val range = ((position + 1) min (value.length - 1)) to (value.length - 1)
                        val position2 = range.find(value(_) == ' ').fold(value.length)(_ + 1)
                        copy(position = position2 min value.length)
    case Right       => copy(position = (position + 1) min value.length)
    case _           => this

  catch case e: OutOfRangeError => this

  def unapply
      (stream: LazyList[TerminalEvent])(using interaction: Interaction[LineEditor, Text])
      (using Terminal)
      : Option[(Text, LazyList[TerminalEvent])] =
    interaction(summon[Terminal].events, this)(_(_))

trait Interaction[StateType, ResultType]:
  def before(): Unit = ()
  def render(state: Maybe[StateType], menu: StateType): Unit
  def after(): Unit = ()
  def result(state: StateType): ResultType

  @tailrec
  final def recur
      (stream: LazyList[TerminalEvent], state: StateType, oldState: Maybe[StateType])
      (key: (StateType, TerminalEvent) => StateType)
      : Option[(ResultType, LazyList[TerminalEvent])] =
    
    render(oldState, state)

    stream match
      case Keypress.Enter #:: tail              => Some((result(state), tail))
      case Keypress.Control('C' | 'D') #:: tail => None
      case other #:: tail                       => recur(tail, key(state, other), state)(key)
      case _                                    => None

  def apply
      (stream: LazyList[TerminalEvent], state: StateType)
      (key: (StateType, TerminalEvent) => StateType)
      : Option[(ResultType, LazyList[TerminalEvent])] =
    
    before()
    recur(stream, state, Unset)(key).tap(after().waive)

object Interaction:
  given [ItemType: Show](using Terminal): Interaction[SelectMenu[ItemType], ItemType] with
    override def before(): Unit = Out.print(t"\e[?25l")
    override def after(): Unit = Out.print(t"\e[J\e[?25h")

    def render(old: Maybe[SelectMenu[ItemType]], menu: SelectMenu[ItemType]) =
      menu.options.foreach: opt =>
        Out.print((if opt == menu.current then t" > $opt" else t"   $opt")+t"\e[K\n")
      Out.print(t"\e[${menu.options.length}A")

    def result(state: SelectMenu[ItemType]): ItemType = state.current

  given (using Terminal): Interaction[LineEditor, Text] with
    override def before(): Unit = Out.print(t"\e[?25l")
    override def after(): Unit = Out.print(t"\e[?25h")
    def render(editor: Maybe[LineEditor], editor2: LineEditor): Unit =
      val prior = editor.or(editor2)
      Out.print(t"\e[?25l")
      if prior.position > 0 then Out.print(t"\e[${prior.position}D")
      val line = t"${editor2.value}${t" "*(prior.value.length - editor2.value.length)}"
      Out.print(t"\e[K")
      Out.print(line)
      if line.length > 0 then Out.print(t"\e[${line.length}D")
      if editor2.position > 0 then Out.print(t"\e[${editor2.position}C")
      Out.print(t"\e[?25h")

    def result(editor: LineEditor): Text = editor.value

case class SelectMenu[ItemType](options: List[ItemType], current: ItemType):
  import Keypress.*
  def apply(keypress: TerminalEvent): SelectMenu[ItemType] = try keypress match
    case Up   => copy(current = options(0 max options.indexOf(current) - 1))
    case Down => copy(current = options(options.size - 1 min options.indexOf(current) + 1))
    case _    => this
  catch case e: OutOfRangeError => this

  def unapply
      (stream: LazyList[TerminalEvent])
      (using terminal: Terminal, interaction: Interaction[SelectMenu[ItemType], ItemType])
      : Option[(ItemType, LazyList[TerminalEvent])] =

    interaction(summon[Terminal].events, this)(_(_))

given realm: Realm = realm"profanity"
