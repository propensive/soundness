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
import denominative.*
import escapade.*
import gossamer.*
import hieroglyph.*
import rudiments.*
import spectacular.*
import symbolism.*
import tessellate.*
import vacuous.*
import denominative.dysasymptotics.linearSize

object Dial:
  private val levels: Text = t"▁▂▃▄▅▆▇█"

// How a bounded reading is drawn. A meter is not progress — it can fall as well as rise — so these
// designs mark the scale, and colour the reading by where it sits on it rather than by how far it
// has come.
// They differ in *height* as well as width (a thermometer is a column), which is why `Meter` has no
// default design: the choice changes the layout, so it belongs to the caller.
enum Dial:
  case Battery, Thermometer, Needle, Bullet, Column, Ascii

  def columnCount: Int = this match
    case Battery     => 8
    case Thermometer => 1
    case Needle      => 12
    case Bullet      => 20
    case Column      => 1
    case Ascii       => 10

  def rowCount: Int = this match
    case Thermometer => 5
    case _           => 1

  def gaugeable(using gauging: Gauging): Meter is Gaugeable = new Gaugeable:
    type Self = Meter
    override def elastic: Boolean = Dial.this != Column && Dial.this != Thermometer
    override def minWidth(status: Meter): Int = if rowCount > 1 then 1 else 3
    override def columns(status: Meter): Int = columnCount
    override def height(status: Meter, width: Int): Int = rowCount

    def rows(status: Meter, tick: Tick, width: Int): List[Teletype] =
      Dial.this.draw(status, width, gauging)

  def draw(meter: Meter, width: Int, gauging: Gauging): List[Teletype] =
    val palette = gauging.palette
    val fraction = meter.fraction
    val plain = !gauging.permits(Gaugeable.Glyphs.Unicode)

    def level(fraction: Fraction): Text =
      val index = (fraction.value*Dial.levels.length).toInt.min(Dial.levels.length - 1).max(0)
      Dial.levels.at(index.z).let(_.show).or(t" ")

    def pad(content: Teletype): Teletype =
      given Text is Measurable = gauging.metric
      Alignment.Left.pad(content, width)

    this match
      case Column =>
        // One cell: the reading as a height, coloured by severity.
        List(gauging.tint(palette.severity(fraction.value))(Teletype(level(fraction))))

      case Thermometer =>
        // A column read from the bottom up, with a bulb beneath it. The top filled cell is a
        // partial, so the reading moves smoothly rather than a fifth at a time.
        val stem = rowCount - 1
        val total = fraction.value*stem
        val color = palette.severity(fraction.value)

        val cells = (0 until stem).map: row =>
          val fromTop = stem - 1 - row
          val filled = (total - fromTop).max(0.0).min(1.0)

          if filled <= 0 then gauging.tint(palette.track)(Teletype(t"░"))
          else gauging.tint(color)(Teletype(level(Fraction(filled))))

        List.of(cells.toList) :+ gauging.tint(color)(Teletype(if plain then t"o" else t"◍"))

      case Battery =>
        // Caps, cells and a terminal nub. A battery reddens as it *empties*, so the severity ramp
        // is read backwards.
        val inner = (width - 3).max(1)
        val lit = (fraction.value*inner).toInt.min(inner)
        val color = palette.severity(1 - fraction.value)
        val body = gauging.tint(color)(Teletype((if plain then t"#" else t"█")*lit))
        val empty = if plain then t"-" else t"░"
        val rest = gauging.tint(palette.track)(Teletype(empty*(inner - lit)))

        if plain then List(pad(e"[$body$rest]"))
        else
          val nub = gauging.tint(color)(Teletype(t"╸"))
          val left = gauging.tint(palette.track)(Teletype(t"▐"))
          val right = gauging.tint(palette.track)(Teletype(t"▌"))

          List(pad(e"$left$body$rest$right$nub"))

      case Needle =>
        // A tick travelling along a scale, with the ends marked: a position on a range, with no
        // suggestion that the space behind it has been filled.
        val span = (width - 2).max(1)
        val at = (fraction.value*(span - 1)).toInt.min(span - 1).max(0)
        val rail = if plain then t"-" else t"─"
        val head = if plain then t"|" else t"┃"
        val cap = if plain then t"+" else t"╷"
        val before = gauging.tint(palette.track)(Teletype(rail*at))
        val after = gauging.tint(palette.track)(Teletype(rail*(span - at - 1)))
        val marker = gauging.tint(palette.severity(fraction.value))(Teletype(head))
        val ends = gauging.tint(palette.muted)(Teletype(cap))

        List(pad(e"$ends$before$marker$after$ends"))

      case Bullet =>
        // A measure drawn over a qualitative track: the bands say what counts as low, fair and
        // high, so the reading is interpretable without a legend.
        val lit = (fraction.value*width).toInt.min(width)

        val cells = (0 until width).map: index =>
          val band = index.toDouble/width.max(1)

          if index < lit then gauging.tint(palette.severity(fraction.value))(Teletype(t"█"))
          else if plain then gauging.tint(palette.track)(Teletype(t"-"))
          else
            val shade = if band < 0.5 then t"░" else if band < 0.8 then t"▒" else t"▓"
            gauging.tint(palette.track)(Teletype(shade))

        List(pad(cells.reduceLeft { (l, r) => e"$l$r" }))

      case Ascii =>
        val inner = (width - 2).max(1)
        val lit = (fraction.value*inner).toInt.min(inner)
        val body = gauging.tint(palette.severity(fraction.value))(Teletype(t"#"*lit))
        val rest = gauging.tint(palette.track)(Teletype(t"-"*(inner - lit)))

        List(pad(e"[$body$rest]"))
