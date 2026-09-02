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
package chiaroscuro

import anticipation.*
import dendrology.*
import denominative.*
import escapade.*
import escritoire.*, columnAttenuation.ignoreAttenuation
import gossamer.*
import hieroglyph.*
import iridescence.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

// The comparison list is rendered whole (and batched into rows), so counting its entries
// is a linear walk of a list that is about to be walked anyway.
import denominative.dysasymptotics.linearSize

// These were members of `Juxtaposition`'s companion object, where the enum's cases were in
// scope unqualified; out here they must be imported.
import Juxtaposition.*


// A real trait, not a structural refinement of `Palette`: structural member selection goes
// through `iridescence.Palette.selectDynamic` — runtime reflection, which Scala Native does
// not support — whereas these are ordinary virtual calls. (probably's `TestPalette` extends
// this, so the test reporter's palette satisfies the `Teletypeable` given below.)
trait JuxtapositionPalette extends Palette:
  type Form = Srgb
  def unaccented: Color in Srgb
  def informative: Color in Srgb
  def subdued: Color in Srgb
  def positive: Color in Srgb
  def negative: Color in Srgb

package teletypeables:
  given juxtapositionTeletype: (measurable: Text is Measurable) => (palette: JuxtapositionPalette)
  =>  Juxtaposition is Teletypeable =

    value =>
      val subdued = Fg(palette.unaccented)
      val fg = Fg(palette.foreground)
      val positive = Fg(palette.positive)
      val negative = Fg(palette.negative)
      val informative = Fg(palette.informative)
      val positiveBg = Bg(palette.subdue(palette.positive, 0.5))
      val negativeBg = Bg(palette.subdue(palette.negative, 0.5))

      value match
        case Juxtaposition.Collation(name, comparison, _, _) =>
          import tableStyles.defaultTableStyle
          val columns = 110
          val length = comparison.size
          val topRule = e"\n$subdued(────┬${(t"─"*(length.min(columns)))}┬────)\n"
          val midRule = e"$subdued(────┼${(t"─"*(length.min(columns)))}┼────)\n"
          val bottomRule = e"$subdued(────┴${(t"─"*(length%columns))}┴────)\n"

          val penultimateRule = if length%columns == 0 then midRule else
            val count = columns - length%columns - 1
            e"$subdued(────┼${(t"─"*(length%columns))}┬${(t"─"*count)}┴────)\n"

          if comparison.all(_(1).singleChar) then
            var topSum = 0
            var bottomSum = 0
            def pad(value: Text): Char = value(Prim).let(Unicode.visible).or(' ')

            def leftTotal(row: List[(Text, Juxtaposition)]): Int =
              val widths: List[Int] = row.map(_(1).leftWidth)
              widths.total

            def rightTotal(row: List[(Text, Juxtaposition)]): Int =
              val widths: List[Int] = row.map(_(1).rightWidth)
              widths.total

            val rows: List[List[(Text, Juxtaposition)]] = comparison.batched(columns)
            val lastRow: Int = length/columns
            val indexedRows: List[(List[(Text, Juxtaposition)], Ordinal)] = rows.indexed

            indexedRows.map: (comparison2, ordinal) =>
              val first = ordinal.n0 == 0
              val last = ordinal.n0 == lastRow

              val observed = comparison2.map:
                case (_, Same(char))            => e"$informative(${pad(char)})"
                case (_, Different(char, _, _)) => e"$positiveBg(${pad(char)})"
                case _                          => e""

              . join

              val expected = comparison2.map:
                case (_, Same(char))            => e"$informative(${pad(char)})"
                case (_, Different(_, char, _)) => e"$negativeBg(${pad(char)})"
                case _                          => e""

              . join

              val margin1 = topSum.show.superscripts.pad(4, Rtl, ' ')
              val margin2 = bottomSum.show.subscripts.pad(4, Rtl, ' ')
              topSum += leftTotal(comparison2)
              bottomSum += rightTotal(comparison2)
              val margin3 = topSum.show.superscripts.pad(4, Ltr, ' ')
              val margin4 = bottomSum.show.subscripts.pad(4, Ltr, ' ')

              val leftEdge1 = if first then e"$fg($margin1)│" else e"$margin1 "
              val leftEdge2 = if first then e"$fg($margin2)│" else e"$margin2 "
              val rightEdge3 = if last then e"│$fg($margin3)" else e" $margin3"
              val rightEdge4 = if last then e"│$fg($margin4)" else e" $margin4"

              val line1 = e"$subdued($leftEdge1)$observed$subdued($rightEdge3)"
              val line2 = e"$subdued($leftEdge2)$expected$subdued($rightEdge4)"

              e"$line1\n$line2\n"

            // The mapped lines only read the two summary buffers; laundered pure.
            . asInstanceOf[List[Teletype]]
            . join(topRule, midRule, penultimateRule, bottomRule)

          else
            def children(comp: (Text, Juxtaposition)): List[(Text, Juxtaposition)] = comp(1) match
              case Same(value)                           => Nil
              case Different(left, right, difference)    => Nil

              case Juxtaposition.Collation(_, comparison, left, right) =>
                if comparison.all(_(1).singleChar) then Nil else comparison

            case class Row(treeLine: Text, left: Teletype, right: Teletype, memo: Teletype)

            given treeStyle: (Text is Textual) => TreeStyle[Row] = (tiles, row) =>
              row.copy(treeLine = tiles.map(treeStyles.defaultTreeStyle.text(_)).join+row.treeLine)

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

                case Juxtaposition.Collation(_, comparison, left, right) =>
                  if comparison.all(_(1).singleChar)
                  then
                    import proximities.levenshteinProximity
                    import caseSensitivity.caseSensitive
                    val distance = left.proximity(right).toInt

                    Row
                      ( line(t"▪"),
                        e"${Fg(palette.positive)}($left)",
                        e"${Fg(palette.negative)}($right)",
                        e"lev = $distance" )

                  else
                    Row
                      ( line(t"■"),
                        e"${Fg(palette.informative)}($left)",
                        e"${Fg(palette.informative)}($right)",
                        e"" )

            val table =
              Scaffold[Row]
                ( Column(e"$name")(_.treeLine),
                  Column(e"Expected", textAlign = TextAlignment.Left)(_.left),
                  Column(e"Observed")(_.right),
                  Column(e"Details")(_.memo.teletype) )

            table
            . tabulate(TreeDiagram.by(children(_))(comparison*).render(line).to[List])
            . grid(200)
            . render
            . join(e"\n")

        case Different(left, right, difference) =>
          val ws = if right.contains(Lf) then e"\n" else e" "
          val ws2 = if left.contains(Lf) then e"\n" else e" "

          e"The result$ws$negative($right)${ws}did not equal$ws2$positive($left)"

        case Same(value) =>
          e"The value $subdued($value) was expected"

