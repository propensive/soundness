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
import parasite.*
import turbulence.*
import anticipation.*
import eucalyptus.*

object LineEditor:
  def concealed(str: Text): Text = str.mapChars { _ => '*' }

  def ask(initial: Text = t"", render: Text => Text = identity(_))(using Terminal, Monitor): Text =
    Out.print(render(initial))

    def finished(key: TerminalEvent) =
      key == Keypress.Enter || key == Keypress.Control('D') || key == Keypress.Control('C')

    summon[Terminal].events.takeWhile(!finished(_)).foldLeft(LineEditor(initial, initial.length)):
      case (editor, next) =>
        if editor.pos > 0 then Out.print(t"\e${editor.pos}D")
        val editor2 = editor(next)
        val line = t"${editor2.content}${t" "*(editor.content.length - editor2.content.length)}"
        Out.print(t"\e0K")
        Out.print(render(line))
        if line.length > 0 then Out.print(t"\e${line.length}D")
        if editor2.pos > 0 then Out.print(t"\e${editor2.pos}C")
        editor2
    .content

case class LineEditor(content: Text = t"", pos: Int = 0):
  import Keypress.*

  def apply(keypress: TerminalEvent): LineEditor = try keypress match
    case CharKey(ch)      => copy(t"${content.take(pos)}$ch${content.drop(pos)}", pos + 1)
    case Control('U')     => copy(content.drop(pos), 0)

    case Control('W')     => val prefix = content.take(0 max (pos - 1)).reverse.dropWhile(_ != ' ').reverse
                             copy(t"$prefix${content.drop(pos)}", prefix.length)

    case Delete           => copy(t"${content.take(pos)}${content.drop(pos + 1)}")
    case Backspace        => copy(t"${content.take(pos - 1)}${content.drop(pos)}", (pos - 1) max 0)
    case Home             => copy(pos = 0)
    case End              => copy(pos = content.length)
    case LeftArrow        => copy(pos = (pos - 1) max 0)
    case Ctrl(LeftArrow)  => copy(pos = (pos - 2 max 0 to 0 by -1).find(content(_) == ' ').fold(0)(_ + 1))

    case Ctrl(RightArrow) => val range = ((pos + 1) min (content.length - 1)) to (content.length - 1)
                             val newPos = range.find(content(_) == ' ').fold(content.length)(_ + 1)
                             copy(pos = newPos min content.length)
    case RightArrow       => copy(pos = (pos + 1) min content.length)
    case _                => this
  catch case e: OutOfRangeError => this

  def unapply(stream: LazyList[TerminalEvent])(using interaction: Interaction[LineEditor, Text])
             (using Monitor, Terminal)
             : Option[(Text, LazyList[TerminalEvent])] =
    interaction(summon[Terminal].events, this)(_(_))

trait Interaction[StateType, ResultType]:
  def before(): Unit = ()
  def render(state: Maybe[StateType], menu: StateType): Unit
  def after(): Unit = ()
  def result(state: StateType): ResultType

  @tailrec
  final def recur(stream: LazyList[TerminalEvent], state: StateType, oldState: Maybe[StateType])
                 (key: (StateType, TerminalEvent) => StateType)
                 : Option[(ResultType, LazyList[TerminalEvent])] =
    render(oldState, state)

    stream match
      case Keypress.Enter #:: tail              => Some((result(state), tail))
      case Keypress.Control('C' | 'D') #:: tail => None
      case other #:: tail                       => recur(tail, key(state, other), state)(key)
      case _                                    => None

  def apply(stream: LazyList[TerminalEvent], state: StateType)(key: (StateType, TerminalEvent) => StateType)
           : Option[(ResultType, LazyList[TerminalEvent])] =
    before()
    recur(stream, state, Unset)(key).tap(after().waive)

object Interaction:
  given [ItemType: Show](using Terminal): Interaction[SelectMenu[ItemType], ItemType] with
    override def before(): Unit = Out.print(t"\e?25l")
    override def after(): Unit = Out.print(t"\eJ\e?25h")

    def render(old: Maybe[SelectMenu[ItemType]], menu: SelectMenu[ItemType]) =
      menu.options.foreach: opt =>
        Out.print((if opt == menu.current then t" > $opt" else t"   $opt")+t"\eK\n")
      Out.print(t"\e${menu.options.length}A")

    def result(state: SelectMenu[ItemType]): ItemType = state.current

  given (using Terminal): Interaction[LineEditor, Text] with
    def render(editor: Maybe[LineEditor], editor2: LineEditor): Unit =
      val prior = editor.or(editor2)
      if prior.pos > 0 then Out.print(t"\e${prior.pos}D")
      val line = t"${editor2.content}${t" "*(prior.content.length - editor2.content.length)}"
      Out.print(t"\e0K")
      Out.print(line)
      if line.length > 0 then Out.print(t"\e${line.length}D")
      if editor2.pos > 0 then Out.print(t"\e${editor2.pos}C")

    def result(editor: LineEditor): Text = editor.content

case class SelectMenu[ItemType](options: List[ItemType], current: ItemType):
  import Keypress.*
  def apply(keypress: TerminalEvent): SelectMenu[ItemType] = try keypress match
    case UpArrow   => copy(current = options(0 max options.indexOf(current) - 1))
    case DownArrow => copy(current = options(options.size - 1 min options.indexOf(current) + 1))
    case _         => this
  catch case e: OutOfRangeError => this

  def unapply
      (stream: LazyList[TerminalEvent])
      (using terminal: Terminal, interaction: Interaction[SelectMenu[ItemType], ItemType])
      (using Monitor)
      : Option[(ItemType, LazyList[TerminalEvent])] =

    interaction(summon[Terminal].events, this)(_(_))

given realm: Realm = realm"profanity"
