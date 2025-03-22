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
┃    Soundness, version 0.27.0.                                                                    ┃
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
import rudiments.*
import spectacular.*
import vacuous.*

enum Juxtaposition:
  case Same(value: Text)
  case Different(left: Text, right: Text, difference: Optional[Text] = Unset)
  case Collation(comparison: IArray[(Text, Juxtaposition)], left: Text, right: Text)

  def singleChar: Boolean = this match
    case Same(value)                        => value.length == 1
    case Different(left, right, difference) => left.length <= 1 && right.length <= 1
    case Collation(_, _, _)                 => false

  def leftWidth: Int = this match
    case Same(value)                 => value.length
    case Different(left, _, _)       => left.length
    case Collation(comparison, _, _) => comparison.sumBy(_(1).leftWidth)

  def rightWidth: Int = this match
    case Same(value)                 => value.length
    case Different(_, right, _)      => right.length
    case Collation(comparison, _, _) => comparison.sumBy(_(1).rightWidth)

object Juxtaposition:
  given (measurable: Text is Measurable) => Juxtaposition is Teletypeable =
    case Juxtaposition.Collation(comparison, _, _) =>
      import tableStyles.default
      import webColors.{Black, Gray}

      val columns = 110
      val length = comparison.length
      val topRule = e"\n$Gray(    ┬${(t"─"*(length.min(columns)))}┬)\n"
      val midRule = e"$Gray(    ┼${(t"─"*(length.min(columns)))}┼)\n"
      val bottomRule = e"$Gray(    ┴${(t"─"*(length%columns))}┴)\n"

      val penultimateRule = if length%columns == 0 then midRule else
        e"$Gray(    ┴${(t"─"*(length%columns))}┬${(t"─"*(columns - length%columns))}┴)\n"

      if comparison.all(_(1).singleChar) then
        var topSum = 0
        var bottomSum = 0
        def pad(value: Text): Char = Unicode.visible(value.at(Prim).or(' '))

        comparison.grouped(columns).map: comparison =>
          val observed = comparison.map:
            case (_, Same(char))            => e"${Bg(rgb"#003399")}(${pad(char)})"
            case (_, Different(char, _, _)) => e"${Bg(rgb"#003300")}(${pad(char)})"
            case _                          => e""
          . join

          val expected = comparison.map:
            case (_, Same(char))            => e"${Bg(rgb"#003399")}(${pad(char)})"
            case (_, Different(_, char, _)) => e"${Bg(rgb"#660000")}(${pad(char)})"
            case _                          => e""
          . join

          val margin1 = topSum.show.superscripts.pad(4, Rtl, ' ')
          val margin2 = bottomSum.show.subscripts.pad(4, Rtl, ' ')
          topSum += comparison.sumBy(_(1).leftWidth)
          bottomSum += comparison.sumBy(_(1).rightWidth)
          val margin3 = topSum.show.superscripts.pad(4, Ltr, ' ')
          val margin4 = bottomSum.show.subscripts.pad(4, Ltr, ' ')

          e"$Gray($margin1│)$observed$Gray(│$margin3)\n$Gray($margin2│)$expected$Gray(│$margin4)\n"

        . to(Iterable)
        . join(topRule, midRule, penultimateRule, bottomRule)

      else
        def children(comp: (Text, Juxtaposition)): List[(Text, Juxtaposition)] = comp(1) match
          case Same(value)                        => Nil
          case Different(left, right, difference) => Nil
          case Collation(comparison, left, right) => comparison.to(List)

        case class Row(treeLine: Text, left: Teletype, right: Teletype)

        given (Text is Textual) => TreeStyle[Row] = (tiles, row) =>
          row.copy(treeLine = tiles.map(treeStyles.default.text(_)).join+row.treeLine)

        def line(data: (Text, Juxtaposition)): Row =
          def line(bullet: Text): Text = t"$bullet ${data(0)}"

          data(1) match
            case Same(v) =>
              Row(line(t"▪"), e"${rgb"#667799"}($v)", e"${rgb"#667799"}($v)")

            case Different(left, right, difference) =>
              Row(line(t"▪"), e"${rgb"#bb0000"}($left)", e"${rgb"#00aa00"}($right)")

            case Collation(comparison, left, right) =>
              Row(line(t"■"), e"$left", e"$right")

        val table = Table[Row]
                     (Column(e"")(_.treeLine),
                      Column(e"Expected", textAlign = TextAlignment.Left)(_.left),
                      Column(e"Observed")(_.right))

        table
        . tabulate(TreeDiagram.by(children(_))(comparison*).render(line))
        . grid(200)
        . render
        . join(e"\n")

    case Different(left, right, difference) =>
      val ws = if right.contains('\n') then e"\n" else e" "
      val ws2 = if left.contains('\n') then e"\n" else e" "

      import webColors.*

      e"The result$ws$Crimson($right)${ws}did not equal$ws2$YellowGreen($left)"

    case Same(value) =>
      e"The value ${webColors.Gray}($value) was expected"
