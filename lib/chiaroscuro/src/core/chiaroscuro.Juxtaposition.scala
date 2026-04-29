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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package chiaroscuro

import anticipation.*
import dendrology.*
import denominative.*
import escapade.*
import escritoire.*, columnAttenuation.ignore
import gossamer.*
import hieroglyph.*
import iridescence.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Juxtaposition:

  type JuxtapositionPalette = Palette:
    type Form = Srgb
    def unaccented: Color in Srgb
    def informative: Color in Srgb
    def subdued: Color in Srgb
    def positive: Color in Srgb
    def negative: Color in Srgb


  given (measurable: Text is Measurable) => (palette: JuxtapositionPalette)
  =>  Juxtaposition is Teletypeable =

    value =>
      val subdued = Fg(palette.unaccented)
      val fg = Fg(palette.foreground)
      val positive = Fg(palette.positive)
      val negative = Fg(palette.negative)
      val informative = Fg(palette.informative)
      val dim = Bg(palette.subdued)
      val positiveBg = Bg(palette.positive)
      val negativeBg = Bg(palette.negative.chroma)

      value match
        case Juxtaposition.Collation(name, comparison, _, _) =>
          import tableStyles.default
          val columns = 110
          val length = comparison.length
          val topRule = e"\n$subdued(────┬${(t"─"*(length.min(columns)))}┬────)\n"
          val midRule = e"$subdued(────┼${(t"─"*(length.min(columns)))}┼────)\n"
          val bottomRule = e"$subdued(────┴${(t"─"*(length%columns))}┴────)\n"

          val penultimateRule = if length%columns == 0 then midRule else
            val count = columns - length%columns - 1
            e"$subdued(────┼${(t"─"*(length%columns))}┬${(t"─"*count)}┴────)\n"

          if comparison.all(_(1).singleChar) then
            var topSum = 0
            var bottomSum = 0
            def pad(value: Text): Char = value.at(Prim).let(Unicode.visible).or(' ')

            comparison.grouped(columns).zipWithIndex.map: (comparison2, index) =>
              val first = index == 0
              val last = index == comparison.length/columns

              val observed = comparison2.map:
                case (_, Same(char))            => e"$dim($informative(${pad(char)}))"
                case (_, Different(char, _, _)) => e"$positiveBg(${pad(char)})"
                case _                          => e""

              . join

              val expected = comparison2.map:
                case (_, Same(char))            => e"$dim($informative(${pad(char)}))"
                case (_, Different(_, char, _)) => e"$negativeBg(${pad(char)})"
                case _                          => e""

              . join

              val margin1 = topSum.show.superscripts.pad(4, Rtl, ' ')
              val margin2 = bottomSum.show.subscripts.pad(4, Rtl, ' ')
              topSum += comparison2.sumBy(_(1).leftWidth)
              bottomSum += comparison2.sumBy(_(1).rightWidth)
              val margin3 = topSum.show.superscripts.pad(4, Ltr, ' ')
              val margin4 = bottomSum.show.subscripts.pad(4, Ltr, ' ')

              val leftEdge1 = if first then e"$fg($margin1)│" else e"$margin1 "
              val leftEdge2 = if first then e"$fg($margin2)│" else e"$margin2 "
              val rightEdge3 = if last then e"│$fg($margin3)" else e" $margin3"
              val rightEdge4 = if last then e"│$fg($margin4)" else e" $margin4"

              val line1 = e"$subdued($leftEdge1)$observed$subdued($rightEdge3)"
              val line2 = e"$subdued($leftEdge2)$expected$subdued($rightEdge4)"

              e"$line1\n$line2\n"

            . to(Iterable)
            . join(topRule, midRule, penultimateRule, bottomRule)

          else
            def children(comp: (Text, Juxtaposition)): List[(Text, Juxtaposition)] = comp(1) match
              case Same(value)                           => Nil
              case Different(left, right, difference)    => Nil

              case Collation(_, comparison, left, right) =>
                if comparison.all(_(1).singleChar) then Nil else comparison.to(List)

            case class Row(treeLine: Text, left: Teletype, right: Teletype, memo: Teletype)

            given treeStyle: (Text is Textual) => TreeStyle[Row] = (tiles, row) =>
              row.copy(treeLine = tiles.map(treeStyles.default.text(_)).join+row.treeLine)

            def line(data: (Text, Juxtaposition)): Row =
              def line(bullet: Text): Text = t"$bullet ${data(0)}"

              data(1) match
                case Same(v) =>
                  Row
                    ( line(t"▪"),
                      e"${Fg(palette.informative)}($v)",
                      e"${Fg(palette.informative)}($v)",
                      e"" )

                case Different(left, right, difference) =>
                  Row
                    ( line(t"▪"),
                      e"${Fg(palette.positive)}($left)",
                      e"${Fg(palette.negative)}($right)",
                      difference.let(_.teletype).or(e"") )

                case Collation(_, comparison, left, right) =>
                  if comparison.all(_(1).singleChar)
                  then
                    import proximities.levenshteinDistance
                    import caseSensitivity.sensitive
                    val distance = left.proximity(right).toInt

                    Row
                      ( line(t"▪"),
                        e"${Fg(palette.positive)}($left)",
                        e"${Fg(palette.negative)}($right)",
                        e"lev = $distance" )

                  else Row
                    ( line(t"■"),
                      e"${Fg(palette.informative)}($left)",
                      e"${Fg(palette.informative)}($right)",
                      e"" )

            val table = Scaffold[Row]
              ( Column(e"$name")(_.treeLine),
                Column(e"Expected", textAlign = TextAlignment.Left)(_.left),
                Column(e"Observed")(_.right),
                Column(e"Details")(_.memo.teletype) )

            table
            . tabulate(TreeDiagram.by(children(_))(comparison*).render(line))
            . grid(200)
            . render
            . join(e"\n")

        case Different(left, right, difference) =>
          val ws = if right.contains(Lf) then e"\n" else e" "
          val ws2 = if left.contains(Lf) then e"\n" else e" "

          e"The result$ws$negative($right)${ws}did not equal$ws2$positive($left)"

        case Same(value) =>
          e"The value $subdued($value) was expected"

enum Juxtaposition:
  case Same(value: Text)
  case Different(left: Text, right: Text, difference: Optional[Text] = Unset)
  case Collation(typeName: Text, comparison: List[(Text, Juxtaposition)], left: Text, right: Text)

  def singleChar: Boolean = this match
    case Same(value)                        => value.length == 1
    case Different(left, right, difference) => left.length <= 1 && right.length <= 1
    case Collation(_, _, _, _)              => false

  def leftWidth: Int = this match
    case Same(value)                    => value.length
    case Different(left, _, _)          => left.length
    case Collation(_, comparison, _, _) => comparison.sumBy(_(1).leftWidth)

  def rightWidth: Int = this match
    case Same(value)                    => value.length
    case Different(_, right, _)         => right.length
    case Collation(_, comparison, _, _) => comparison.sumBy(_(1).rightWidth)
