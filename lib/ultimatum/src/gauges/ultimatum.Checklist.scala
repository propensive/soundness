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
import hieroglyph.*
import symbolism.*
import tessellate.*
import vacuous.*

object Checklist:
  // The frames the running step's marker cycles through, so that a checklist shows which step is
  // working rather than merely which is next.
  private val working: Text = t"⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
  private val asciiWorking: Text = t"-\\|/"

// How a run of steps is drawn. These differ in *height* — `Rows` is one row per step, the rest are
// a single row — which is why `Procession` has no default: the choice changes the layout around it,
// so it is the caller's.
enum Checklist:
  case Rows, Breadcrumb, Beads, Numbered, Ribbon

  def gaugeable(using gauging: Gauging): Sequence[Step] is Gaugeable = new Gaugeable:
    type Self = Sequence[Step]

    // Only a design that shows a working step needs the clock.
    override def period: Optional[Int] = Checklist.this match
      case Rows | Beads => 80
      case _            => Unset

    override def minWidth(status: Sequence[Step]): Int = 3

    override def height(status: Sequence[Step], width: Int): Int = Checklist.this match
      case Rows => status.stdlib.length.max(1)
      case _    => 1

    def rows(status: Sequence[Step], tick: Tick, width: Int): List[Teletype] =
      Checklist.this.draw(status, tick, width, gauging)

  def draw(steps: Sequence[Step], tick: Tick, width: Int, gauging: Gauging): List[Teletype] =
    // Derived here rather than carried by a wrapper type: a run of steps is just a `Sequence`.
    val current: Optional[Step] = steps.stdlib.find(_.standing == Standing.Running).optional
    val started = steps.stdlib.count(_.standing != Standing.Pending)
    val count = steps.stdlib.length

    val palette = gauging.palette
    val plain = !gauging.permits(Gaugeable.Glyphs.Unicode)

    // The running step's marker, animated; every other standing keeps its static glyph.
    def marker(standing: Standing): Text =
      val frames = if plain then Checklist.asciiWorking else Checklist.working
      val count = frames.length

      standing match
        case Standing.Running   => frames.s.charAt(tick.index.abs%count).toString.tt
        case Standing.Succeeded => if plain then t"+" else t"✓"
        case Standing.Failed    => if plain then t"x" else t"✗"
        case Standing.Warned    => t"!"
        case Standing.Skipped   => if plain then t"-" else t"‑"
        case Standing.Pending   => if plain then t"." else t"·"

    def pad(content: Teletype): Teletype =
      given Text is Measurable = gauging.metric
      Alignment.Left.pad(content, width)

    def clip(text: Text, cells: Int): Text =
      given Text is Measurable = gauging.metric
      Flow.shorten(text, cells)

    this match
      case Rows =>
        // One row per step: the canonical multi-row widget, and the one that makes the layout grow
        // as steps are appended.
        val lines = steps.stdlib.map: step =>
          val glyph = gauging.tint(palette.colorOf(step.standing))(Teletype(marker(step.standing)))
          val faded = step.standing == Standing.Succeeded || step.standing == Standing.Skipped
          val color = if faded then palette.muted else palette.caption
          val name = gauging.tint(color)(Teletype(clip(step.name, (width - 2).max(1))))

          pad(e"$glyph $name")

        if lines.isEmpty then List(pad(e"")) else List.of(lines.toList)

      case Numbered =>
        val name = current.lay(t"")(_.name)
        val position = t"[$started/$count]"
        val label = clip(name, (width - position.length - 1).max(0))

        val marker = gauging.tint(palette.muted)(Teletype(position))
        val text = gauging.tint(palette.caption)(Teletype(label))

        List(pad(e"$marker $text"))

      case Beads =>
        // A compact chain: one bead per step, joined by a rule. Fixed at `2n - 1` cells.
        val link = if plain then t"-" else t"━"

        val beads = steps.stdlib.zipWithIndex.map: (step, index) =>
          val glyph = step.standing match
            case Standing.Pending => if plain then t"o" else t"○"
            case Standing.Running => if plain then t"*" else t"◐"
            case _                => if plain then t"@" else t"●"

          val bead = gauging.tint(palette.colorOf(step.standing))(Teletype(glyph))
          if index == 0 then bead else e"${gauging.tint(palette.track)(Teletype(link))}$bead"

        if beads.isEmpty then List(pad(e"")) else
          List(pad(beads.reduceLeft { (left, right) => e"$left$right" }))

      case Breadcrumb =>
        val separator = if plain then t">" else t"›"

        val crumbs = steps.stdlib.map: step =>
          val faded = step.standing == Standing.Pending
          val color = if faded then palette.track else palette.colorOf(step.standing)
          gauging.tint(color)(Teletype(step.name))

        if crumbs.isEmpty then List(pad(e"")) else
          val joined = crumbs.reduceLeft: (left, right) =>
            e"$left ${gauging.tint(palette.muted)(Teletype(separator))} $right"

          List(pad(joined))

      case Ribbon =>
        // Powerline: `escapade.Ribbon` already draws the separators and picks a legible foreground
        // per segment, so this is only a matter of choosing the backgrounds.
        val names = steps.stdlib.map: step => Teletype(step.name)

        if names.isEmpty then List(pad(e"")) else
          // Qualified: this enum's own `Ribbon` case shadows the one from escapade.
          val colors = palette.steps(names.length).stdlib.map(Bg(_))
          List(pad(escapade.Ribbon(colors*).fill(names*)))
