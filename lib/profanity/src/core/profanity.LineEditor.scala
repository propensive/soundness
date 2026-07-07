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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import hieroglyph.*, textMetrics.wideCharacterWidthMetric
import rudiments.*
import spectacular.*
import vacuous.*

object LineEditor:
  // Whether the editor is a single line (Enter submits) or accepts multiple lines.
  // In `Multiline` mode the arrow keys move the cursor between lines, and `submit`
  // decides — from the current content — whether Enter submits or inserts a newline
  // (Shift+Enter always submits).
  enum Mode:
    case SingleLine
    case Multiline(submit: Text => Boolean)

  // (row, column) of character offset `position` in `text` laid out `cols` wide,
  // exactly as a cell grid does: it walks grapheme clusters (so a wide CJK glyph or an
  // emoji is one cell of display-width 2, a combining mark 0), advancing by display
  // width and wrapping a grapheme that would exceed `cols`, with an embedded newline
  // moving to the start of the next row. Reduces to `position/cols`, `position%cols`
  // for plain single-width text with no newlines.
  def cursorPosition(text: Text, position: Int, cols: Int): (Int, Int) =
    val metric:     Grapheme is Measurable = summon[Grapheme is Measurable]
    val string:     String                 = text.s
    val limit:      Int                    = position.min(string.length)
    val boundaries: IArray[Int]            = Writing(text).boundaries
    var row:        Int                    = 0
    var column:     Int                    = 0
    var index:      Int                    = 0

    while index < boundaries.length - 1 && boundaries(index) < limit do
      val start = boundaries(index)
      val end   = boundaries(index + 1)

      if end - start == 1 && string.charAt(start) == '\n' then
        row += 1
        column = 0
      else
        val cellWidth = metric.width(Grapheme(string.substring(start, end).nn))

        if cols > 0 && column + cellWidth > cols then
          row += 1
          column = 0

        column += cellWidth

      index += 1

    // An exact-fill row leaves the caret at column `cols`; wrap it to the next row's
    // start, matching the grid (where the next cell would wrap).
    if cols > 0 && column >= cols then
      row += column/cols
      column = column%cols

    (row, column)

case class LineEditor
  ( value:     Text            = t"",
    position0: Optional[Int]   = Unset,
    mode:      LineEditor.Mode = LineEditor.Mode.SingleLine )
extends Question[Text]:

  val position = position0.or(value.length)

  import Keypress.*

  // Whether (per `mode`) the given event submits the answer rather than editing it.
  def submitsOn(event: TerminalEvent): Boolean = mode match
    case LineEditor.Mode.SingleLine =>
      event match
        case Enter => true
        case _     => false

    case LineEditor.Mode.Multiline(submit) =>
      event match
        case Enter        => submit(value)
        case Shift(Enter) => true
        case _            => false

  private def multiline: Boolean = mode match
    case LineEditor.Mode.Multiline(_) => true
    case _                            => false

  // The logical lines, their start offsets, and the index of the cursor's line.
  private def layout: (List[Text], List[Int], Int) =
    val lines:  List[Text] = value.cut(t"\n").to(List)
    val starts: List[Int]  = lines.scanLeft(0)(_ + _.length + 1).init
    (lines, starts, starts.lastIndexWhere(_ <= position).max(0))

  private def moveVertically(rows: Int): LineEditor =
    val (lines, starts, current) = layout
    val column = position - starts(current)
    val target = (current + rows).max(0).min(lines.length - 1)
    copy(position0 = starts(target) + column.min(lines(target).length))

  def apply(keypress: TerminalEvent): LineEditor =
    try
      keypress match
        case CharKey(ch) => copy(t"${value.keep(position)}$ch${value.skip(position)}", position + 1)
        case Enter       => copy(t"${value.keep(position)}\n${value.skip(position)}", position + 1)
        case Ctrl('U')   => copy(value.skip(position), 0)
        case Delete      => copy(t"${value.keep(position)}${value.skip(position + 1)}")

        case Up if multiline   => moveVertically(-1)
        case Down if multiline => moveVertically(1)

        case Home if multiline =>
          val (_, starts, current) = layout
          copy(position0 = starts(current))

        case End if multiline =>
          val (lines, starts, current) = layout
          copy(position0 = starts(current) + lines(current).length)

        case Home  => copy(position0 = 0)
        case End   => copy(position0 = value.length)
        case Left  => copy(position0 = (position - 1) `max` 0)
        case Right => copy(position0 = (position + 1) `min` value.length)

        case Ctrl('W') =>
          val prefix = value.keep(0 max (position - 1)).skip(_ != ' ', Rtl)
          copy(t"$prefix${value.skip(position)}", prefix.length)

        case Backspace =>
          copy(t"${value.keep(position - 1)}${value.skip(position)}", (position - 1) max 0)

        case Ctrl(Left) =>
          val position2 =
            ((position - 2 `max` 0) to 0 by -1).seek: index =>
              value.at(index.z) == ' '

          copy(position0 = position2.lay(0)(_ + 1))

        case Ctrl(Right) =>
          val range = ((position + 1) `min` (value.length - 1)) to (value.length - 1)
          val position2 = range.seek { index => value.at(index.z) == ' ' }.lay(value.length)(_ + 1)
          copy(position0 = position2 `min` value.length)

        case _ =>
          this

    catch case e: RangeError => this


  def ask
    ( using interactivity: Interactivity[TerminalEvent],
            interaction:   Interaction[Text, LineEditor] )
    [ result ]
    ( lambda: Interactivity[TerminalEvent] ?=> Text => result )
  :   result raises DismissError =

    val events = interactivity.eventIterator()

    interaction(events, this)(_(_)).lay(abort(DismissError())):
      result => lambda(using Interactivity(events))(result)
