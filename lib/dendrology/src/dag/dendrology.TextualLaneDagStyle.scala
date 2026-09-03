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
package dendrology

// Deliberate stdlib opt-out, as in the diagram implementations.
import scala.collection.immutable.{Map, Set}

import anticipation.*
import gossamer.*
import gossamer.Textual.concatenable
import hieroglyph.*
import rudiments.{map, zip, indexed, fold}
import symbolism.*
import vacuous.*

import DagTile.*

case class TextualLaneDagStyle[line: Textual]
  ( space:      Text,
    vertical:   Text,
    horizontal: Text,
    cornerNe:   Text,
    cornerNw:   Text,
    cornerSe:   Text,
    cornerSw:   Text,
    teeN:       Text,
    teeS:       Text,
    teeE:       Text,
    teeW:       Text,
    junction:   Text,
    crossing:   Text,
    node:       Text )
  ( using metric: Text is Measurable )
extends LaneDagStyle[line]:
  // Display cells, not code units, so wide characters and combining marks lay out correctly.
  def width(glyph: line): Int = metric.width(summon[line is Textual].text(glyph))

  def serialize
    ( tiles:  List[DagTile],
      glyphs: Map[Int, line],
      widths: List[Int],
      label:  Optional[line] )
  :   line =

    val parts = tiles.zip(widths).indexed.map:
      case ((Node, w), i) =>
        val g = glyphs.getOrElse(i.n0, line(node))
        val gw = width(g)
        if gw >= w then g else g+line(Text(" ".repeat(w - gw).nn))

      case ((t, w), _) =>
        val base = text(t)

        val cell =
          if base.s.length >= 1 then base.s.charAt(0).toString else " "

        val filler =
          if base.s.length >= 2 then base.s.charAt(1).toString else " "

        val padding = if w > 1 then filler.repeat(w - 1).nn else ""
        line(Text(cell + padding))

    parts.fold(line(t""))(_+_)+label.or(line(t""))

  def text(tile: DagTile): Text = tile match
    case Space      => space
    case Vertical   => vertical
    case Horizontal => horizontal
    case CornerNe   => cornerNe
    case CornerNw   => cornerNw
    case CornerSe   => cornerSe
    case CornerSw   => cornerSw
    case TeeN       => teeN
    case TeeS       => teeS
    case TeeE       => teeE
    case TeeW       => teeW
    case Junction   => junction
    case Crossing   => crossing
    case Node       => this.node
