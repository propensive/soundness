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
package profanity

import anticipation.*
import clavichord.Keypress
import denominative.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

object Interaction:
  // Both widgets render through a `Board`: positioning is `move`/`put`, width
  // comes from `surface.width`, and the surface decides whether those map to
  // relative motion at the prompt (a standalone `InlineBoard`) or absolute
  // placement within a panel (an `Extent`). The surface is summoned from scope,
  // so a panel's `Extent` is used automatically when a widget runs inside one.
  // `new`-instance form (not a colon-body given): the synthesized given class does not admit
  // the `^{surface}` result annotation the capturing surface requires.
  given selectMenu: [item: Showable] => (surface: Board^)
  =>  (Interaction[item, SelectMenu[item]]^{surface}) = new Interaction[item, SelectMenu[item]]:
    override def before(): Unit = surface.cursor(false)

    override def after(): Unit =
      surface.move(Prim, Prim)
      surface.clear()
      surface.cursor(true)
      surface.flush()

    def render(old: Optional[SelectMenu[item]], menu: SelectMenu[item]) =
      val cols = surface.width.max(1)
      surface.move(Prim, Prim)
      surface.clear()
      var row = 0

      menu.options.each: option =>
        surface.move(Prim, row.z)
        val full = if option == menu.current then t" > $option" else t"   $option"
        surface.put(full)
        row += (full.length - 1)/cols + 1

      surface.flush()

    def result(state: SelectMenu[item]): item = state.current

  given lineEditor: (surface: Board^)
  =>  (Interaction[Text, LineEditor]^{surface}) = new Interaction[Text, LineEditor]:
    // The last row the editor's content reached, so `after` can drop the cursor
    // onto a fresh line below it.
    @scala.caps.unsafe.untrackedCaptures
    private var endRow: Int = 0

    override def after(): Unit =
      surface.move(Prim, endRow.z)
      surface.put(t"\n")
      surface.flush()

    override def submits(event: Terminal.Event, editor: LineEditor): Boolean =
      editor.submitsOn(event)

    // Redraws the whole editor from its top-left each frame: `cursorPosition`
    // counts embedded newlines and wrapping (the surface draws each `\n` as a
    // newline within its own coordinate space), then the caret is placed.
    def render(old: Optional[LineEditor], editor: LineEditor): Unit =
      val cols             = surface.width.max(1)
      val (curRow, curCol) = LineEditor.cursorPosition(editor.value, editor.position, cols)
      endRow = LineEditor.cursorPosition(editor.value, editor.value.length, cols)._1

      surface.move(Prim, Prim)
      surface.clear()
      surface.put(editor.value)
      // Place the caret after flushing: in a panel, `flush` blits the grid through
      // the parent's cursor, so the caret must be positioned last to survive it.
      surface.flush()
      surface.showCaret(curCol.z, curRow.z)

    def result(editor: LineEditor): Text = editor.value

trait Interaction[result, question]:
  def before(): Unit = ()
  def render(state: Optional[question], menu: question): Unit
  def after(): Unit = ()
  def result(state: question): result

  // Which event submits the answer, given the current state (default: Enter). A
  // multi-line editor overrides this to consult the editor's mode — e.g. submitting
  // on Shift+Enter, or on Enter only when the content is "complete".
  def submits(event: Terminal.Event, state: question): Boolean = event match
    case Keypress.Enter => true
    case _              => false

  // Reacts to an editing event (one that neither submits nor dismisses), returning a
  // possibly-updated state and performing any side effects — e.g. requesting
  // completions on Tab and inserting a unique one. Returns the state unchanged by
  // default.
  def react(state: question, event: Terminal.Event): question = state


  @tailrec
  final def recur
    ( events: Iterator[Terminal.Event], state: question, oldState: Optional[question] )
    ( key: (question, Terminal.Event) => question )
  :   Optional[result] =

    render(oldState, state)

    if !events.hasNext then Unset
    else events.next() match
      case Keypress.Ctrl('C' | 'D')       => Unset
      case Keypress.Escape                => Unset
      case event if submits(event, state) => result(state)

      case other =>
        // `react` may transform the state (e.g. inserting a completion); `oldState`
        // stays the just-rendered `state`, so the next render's cursor maths match
        // where the cursor actually is.
        val reacted = react(state, other)
        recur(events, key(reacted, other), state)(key)


  def apply(events: Iterator[Terminal.Event], state: question)
    ( key: (question, Terminal.Event) => question )
  :   Optional[result] =

    before()
    recur(events, state, Unset)(key).also(after())
