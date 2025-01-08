/*
    Chiaroscuro, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package chiaroscuro

import anticipation.*
import dendrology.*
import escapade.*
import escritoire.*, columnAttenuation.ignore
import gossamer.*
import hieroglyph.*
import iridescence.*
import rudiments.*
import spectacular.*
import vacuous.*

enum Semblance:
  case Identical(value: Text)
  case Different(left: Text, right: Text, difference: Optional[Text] = Unset)
  case Breakdown(comparison: IArray[(Text, Semblance)], left: Text, right: Text)

object Semblance:
  given (using calc: TextMetrics) => Semblance is Teletypeable =
    case Semblance.Breakdown(cmp, l, r) =>
      import tableStyles.default

      def children(comp: (Text, Semblance)): List[(Text, Semblance)] = comp(1) match
        case Identical(value)                   => Nil
        case Different(left, right, difference) => Nil
        case Breakdown(comparison, left, right) => comparison.to(List)

      case class Row(treeLine: Text, left: Teletype, right: Teletype, difference: Teletype)

      given (using Text is Textual) => TreeStyle[Row] = (tiles, row) =>
        row.copy(treeLine = tiles.map(treeStyles.default.text(_)).join+row.treeLine)

      def mkLine(data: (Text, Semblance)) =
        def line(bullet: Text): Text = t"$bullet ${data(0)}"

        data(1) match
          case Identical(v) =>
            Row(line(t"▪"), e"${rgb"#667799"}($v)", e"${rgb"#667799"}($v)", e"")

          case Different(left, right, difference) =>
            Row(line(t"▪"), e"${webColors.YellowGreen}($left)", e"${webColors.Crimson}($right)",
                e"${rgb"#40bbcb"}(${difference.or(t"")})")

          case Breakdown(cmp, left, right) =>
            Row(line(t"■"), e"$left", e"$right", e"")

      val table = Table[Row](
        Column(e"")(_.treeLine),
        Column(e"Expected", textAlign = TextAlignment.Right)(_.left),
        Column(e"Found")(_.right),
        Column(e"Difference")(_.difference.or(e""))
      )

      table.tabulate(TreeDiagram.by(children(_))(cmp*).render(mkLine)).grid(200).render.join(e"\n")

    case Different(left, right, difference) =>
      val whitespace = if right.contains('\n') then e"\n" else e" "
      val whitespace2 = if left.contains('\n') then e"\n" else e" "
      e"The result$whitespace${webColors.Crimson}($right)${whitespace}did not equal$whitespace2${webColors.YellowGreen}($left)"

    case Identical(value) =>
      e"The value ${webColors.Gray}($value) was expected"
