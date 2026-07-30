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
package decorum

import scala.collection.mutable

object BalanceRules:
  // R30 (811): a formal multi-line block pads its interior — a space is
  // required directly inside both the opener and the closer — and its
  // closing bracket shares a line with the last parameter rather than
  // standing alone. A formal pair that opens and closes on one line
  // checks both interior edges on the spot; one that spans lines checks
  // the opener's inside edge on the opening line and the closer's
  // leading edge on the closing line. The pair inventory and formality
  // classification come from `ctx.brackets` (see `decorum.Brackets`).
  object FormalBlockSpacing extends Rule:
    def id: String = "811"
    def principle: Principle = Principle.Balance

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()

      ctx.brackets.pairs.foreach: pair =>
        if pair.formal then
          if pair.singleLine then checkInline(ctx, pair, out)
          else
            checkOpener(ctx, pair, out)
            if pair.matched then checkCloser(ctx, pair, out)

      out.toList

    private def checkInline
      ( ctx: Context, pair: Brackets.Pair, out: mutable.ListBuffer[Violation] )
    :   Unit =

      val line = ctx.lines(pair.openLine - 1)
      val arr  = line.arr
      val cols = line.cols

      if pair.closeIdx > pair.openIdx + 1 then
        if arr(pair.openIdx + 1).kind != Sort.Space then
          out +=
            Violation
              ( ctx.file, pair.openLine, cols(pair.openIdx + 1), "811",
                s"a space is required after `${arr(pair.openIdx).text}` in a multi-line block" )

        if arr(pair.closeIdx - 1).kind != Sort.Space then
          out +=
            Violation
              ( ctx.file, pair.openLine, cols(pair.closeIdx), "811",
                s"a space is required before `${arr(pair.closeIdx).text}` in a " +
                  "multi-line block" )

    private def checkOpener
      ( ctx: Context, pair: Brackets.Pair, out: mutable.ListBuffer[Violation] )
    :   Unit =

      val line = ctx.lines(pair.openLine - 1)
      val arr  = line.arr
      val cols = line.cols

      if pair.openIdx + 1 >= arr.length || arr(pair.openIdx + 1).kind != Sort.Space then
        out +=
          Violation
            ( ctx.file, pair.openLine, cols(pair.openIdx + 1), "811",
              s"a space is required after `${arr(pair.openIdx).text}` in a multi-line block" )

    private def checkCloser
      ( ctx: Context, pair: Brackets.Pair, out: mutable.ListBuffer[Violation] )
    :   Unit =

      val line = ctx.lines(pair.closeLine - 1)
      val arr  = line.arr
      val cols = line.cols
      val text = arr(pair.closeIdx).text

      val firstSemantic = arr.indexWhere: t => t.kind != Sort.Space && t.kind != Sort.Comment

      if pair.closeIdx == firstSemantic then
        out +=
          Violation
            ( ctx.file, pair.closeLine, cols(pair.closeIdx), "811",
              s"`$text` cannot appear alone on its line; the closing bracket of a " +
                "multi-line block goes on the same line as the last parameter" )
      else if pair.closeIdx > 0 && arr(pair.closeIdx - 1).kind != Sort.Space then
        out +=
          Violation
            ( ctx.file, pair.closeLine, cols(pair.closeIdx), "811",
              s"a space is required before `$text` in a multi-line block" )

  // R12 (402): a compact single-line bracket pair — one that is not a
  // formal block — must not pad its interior with spaces. Tuples on a
  // fresh line after `=>` (lambda and match-case body tuples) are
  // exempt: the author may use either tight or formal-style spacing.
  object CompactBracketSpacing extends Rule:
    def id: String = "402"
    def principle: Principle = Principle.Balance

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()

      ctx.brackets.pairs.foreach: pair =>
        if pair.singleLine && !pair.formal && !pair.lambdaBodyTuple &&
          pair.closeIdx > pair.openIdx + 1
        then
          val line = ctx.lines(pair.openLine - 1)
          val arr  = line.arr
          val cols = line.cols

          val firstInside = arr(pair.openIdx + 1)
          val lastInside  = arr(pair.closeIdx - 1)

          if firstInside.kind == Sort.Space && firstInside.text.length > 0 then
            out +=
              Violation
                ( ctx.file, pair.openLine, cols(pair.openIdx + 1), "402",
                  s"no space is permitted directly after `${arr(pair.openIdx).text}`" )

          if lastInside.kind == Sort.Space && lastInside.text.length > 0 &&
            (pair.closeIdx - 1) != (pair.openIdx + 1)
          then
            out +=
              Violation
                ( ctx.file, pair.openLine, cols(pair.closeIdx - 1), "402",
                  s"no space is permitted directly before `${arr(pair.closeIdx).text}`" )

      out.toList
