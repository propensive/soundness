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
package ultimatum

import anticipation.*
import escapade.*
import gossamer.*
import symbolism.*
import vacuous.*

object Standing:
  // The six glyphs one design uses for the six standings, and the engine that draws them. A
  // standing is carried by its glyph first and its colour second, so that the distinction survives
  // a monochrome terminal and a redirected stream.
  case class Marks
    ( succeeded:  Text,
      failed:     Text,
      warned:     Text,
      skipped:    Text,
      running:    Text,
      pending:    Text,
      columns:    Int              = 1,
      repertoire: Gaugeable.Glyphs = Gaugeable.Glyphs.Unicode,
      narrower:   Optional[Marks]  = Unset ):

    def apply(standing: Standing): Text = standing match
      case Standing.Succeeded => succeeded
      case Standing.Failed    => failed
      case Standing.Warned    => warned
      case Standing.Skipped   => skipped
      case Standing.Running   => running
      case Standing.Pending   => pending

    // The first design in the fallback chain that fits and is permitted.
    def fit(width: Int, gauging: Gauging): Optional[Marks] =
      if columns <= width && gauging.permits(repertoire) then this
      else narrower.lay(Unset: Optional[Marks])(_.fit(width, gauging))

    def leastColumns: Int = narrower.lay(columns)(_.leastColumns.min(columns))

    // Draw one standing, padded to `width`.
    def draw(standing: Standing, width: Int, gauging: Gauging): Teletype =
      fit(width, gauging).lay(Teletype(t" "*width.max(0))): marks =>
        val glyph = gauging.tint(gauging.palette.colorOf(standing))(Teletype(marks(standing)))
        val padding = width - marks.columns
        if padding > 0 then e"$glyph${t" "*padding}" else glyph

    def gaugeable(using gauging: Gauging): Standing is Gaugeable = new Gaugeable:
      type Self = Standing
      override def elastic: Boolean = false
      override def minWidth(status: Standing): Int = leastColumns
      override def columns(status: Standing): Int = leastColumns

      def rows(status: Standing, tick: Tick, width: Int): List[Teletype] =
        List(draw(status, width, gauging))

  // The default marks, used when nothing is imported: the near-universal tick and cross.
  given gaugeable: Gauging => Standing is Gaugeable = standings.tickStanding

// How one unit of work has turned out — or has not yet. Kept separate from the glyphs that show
// it, so that the same vocabulary serves a one-cell status marker, a checklist row and a colour
// lookup in a palette.
enum Standing:
  case Pending, Running, Succeeded, Failed, Warned, Skipped
