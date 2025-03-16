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
import escapade.*
import escritoire.*, columnAttenuation.ignore
import gossamer.*
import hieroglyph.*
import iridescence.*
import spectacular.*
import vacuous.*

enum Juxtaposition:
  case Same(value: Text)
  case Different(left: Text, right: Text, difference: Optional[Text] = Unset)
  case Collation(comparison: IArray[(Text, Juxtaposition)], left: Text, right: Text)

object Juxtaposition:
  given (measurable: Text is Measurable) => Juxtaposition is Teletypeable =
    case Juxtaposition.Collation(cmp, l, r) =>
      import tableStyles.default

      def children(comp: (Text, Juxtaposition)): List[(Text, Juxtaposition)] = comp(1) match
        case Same(value)                        => Nil
        case Different(left, right, difference) => Nil
        case Collation(comparison, left, right) => comparison.to(List)

      case class Row(treeLine: Text, left: Teletype, right: Teletype, difference: Teletype)

      given (Text is Textual) => TreeStyle[Row] = (tiles, row) =>
        row.copy(treeLine = tiles.map(treeStyles.default.text(_)).join+row.treeLine)

      def mkLine(data: (Text, Juxtaposition)) =
        def line(bullet: Text): Text = t"$bullet ${data(0)}"

        data(1) match
          case Same(v) =>
            Row(line(t"▪"), e"${rgb"#667799"}($v)", e"${rgb"#667799"}($v)", e"")

          case Different(left, right, difference) =>
            Row(line(t"▪"), e"${webColors.YellowGreen}($left)", e"${webColors.Crimson}($right)",
                e"${rgb"#40bbcb"}(${difference.or(t"")})")

          case Collation(cmp, left, right) =>
            Row(line(t"■"), e"$left", e"$right", e"")

      val table = Table[Row](
        Column(e"")(_.treeLine),
        Column(e"Expected", textAlign = TextAlignment.Left)(_.left),
        Column(e"Found")(_.right),
        Column(e"Difference")(_.difference)
      )

      table.tabulate(TreeDiagram.by(children(_))(cmp*).render(mkLine)).grid(200).render.join(e"\n")

    case Different(left, right, difference) =>
      val ws = if right.contains('\n') then e"\n" else e" "
      val ws2 = if left.contains('\n') then e"\n" else e" "

      import webColors.*

      e"The result$ws$Crimson($right)${ws}did not equal$ws2$YellowGreen($left)"

    case Same(value) =>
      e"The value ${webColors.Gray}($value) was expected"
