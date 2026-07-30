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

object ContinuationRules:
  // R-616: a symbolic infix operator whose operands wrap onto separate source
  // lines must terminate the first line (616.1), and the continuation must be
  // indented exactly two columns beyond the first line of the operator chain
  // (616.2). The bad shapes are the operator beginning the continuation line
  // (`left\n  ++ right`), the operator alone on its own line, and any
  // continuation indent other than +2.
  object OperatorContinuation extends Rule:
    def id: String = "616"
    def principle: Principle = Principle.ContinuationMarking

    def check(ctx: Context): List[Violation] =
      ctx.operatorSites.flatMap: op =>
        if op.multiline then
          if op.opLine != op.leftEndLine then
            List
              ( Violation
                  ( ctx.file, op.opLine, op.opCol, "616.1",
                    "a multi-line infix operator must terminate the first line, not begin "
                      +"the continuation line" ) )
          else if op.rightCol - 1 != op.anchorIndent + 2 then
            List
              ( Violation
                  ( ctx.file, op.rightLine, op.rightCol, "616.2",
                    s"a multi-line infix continuation must be indented ${op.anchorIndent + 2} "
                      +s"columns (found ${op.rightCol - 1})" ) )
          else
            Nil
        else
          Nil

  // R-444: continuation markers are followed by a fixed "hard space". A line
  // whose first token is the `=>` continuation marker must separate it from
  // what follows by exactly two spaces; a heavy-signature return-type line —
  // one that begins with `:` and ends with the body-introducing `=` — must
  // follow the `:` with exactly three spaces.
  object HardSpace extends Rule:
    def id: String = "444"
    def principle: Principle = Principle.ContinuationMarking

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      var idx = 0

      while idx < ctx.lines.length do
        val line        = ctx.lines(idx)
        val lineNum     = idx + 1
        val rest        = line.rest
        val leadingCols = line.leadingCols

        rest.headOption match
          case Some(tok) if tok.text == "=>" =>
            if rest.length >= 2 then
              val next = rest(1)

              if next.kind != Sort.Space || next.text != "  " then
                out +=
                  Violation
                    ( ctx.file, lineNum, leadingCols + 3, "444",
                      "`=>` continuation line must be followed by exactly two spaces" )

          case Some(tok) if tok.text == ":" && lineEndsWithEqualsToken(rest) =>
            if rest.length >= 2 then
              val next = rest(1)

              if next.kind != Sort.Space || next.text != "   " then
                out +=
                  Violation
                    ( ctx.file, lineNum, leadingCols + 2, "444",
                      "heavy-signature return type `:` must be followed by exactly three spaces" )

          case _ => ()

        idx += 1

      out.toList

    private def lineEndsWithEqualsToken(rest: IndexedSeq[Lexeme]): Boolean =
      val nonWs = rest.filter: t => t.kind != Sort.Space && t.kind != Sort.Comment
      nonWs.lastOption.exists(_.text == "=")

  // R-163: a `. method` chain continuation line relates to the previous code
  // line through blank separation. When the previous code line is *more*
  // indented, the continuation resumes an outer chain, so a blank line must
  // separate them (163.1); when the previous code line is at the *same*
  // indent, the chain is unbroken, so no blank line is permitted (163.2).
  // Comment-only, annotation-only and triple-quoted-string continuation
  // lines belong to what follows them and don't count as the previous code
  // line.
  object ChainContinuation extends Rule:
    def id: String = "163"
    def principle: Principle = Principle.ContinuationMarking

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      var prevCodeLineIndent = -1
      var prevLineWasBlank   = false
      var idx = 0

      while idx < ctx.lines.length do
        val line    = ctx.lines(idx)
        val lineNum = idx + 1

        if !line.isBlank then
          line.firstReal.foreach: tok =>
            if tok.text == "." && prevCodeLineIndent >= 0 then
              if prevCodeLineIndent > line.leadingCols && !prevLineWasBlank then
                out +=
                  Violation
                    ( ctx.file, lineNum, line.leadingCols + 1, "163.1",
                      "blank line is required before `. method` continuation following a " +
                        "more-indented line" )
              else if prevCodeLineIndent == line.leadingCols && prevLineWasBlank then
                out +=
                  Violation
                    ( ctx.file, lineNum, line.leadingCols + 1, "163.2",
                      "no blank line is permitted before `. method` continuation at the " +
                        "same indent" )

          val isCommentOnly    = line.firstReal.exists(_.kind == Sort.Comment)
          val isAnnotationOnly = line.firstReal.exists(_.text.startsWith("@"))

          val isStringContinuation =
            line.firstReal.exists(_.kind == Sort.Strs) && line.leadingWs.isEmpty

          if !isCommentOnly && !isAnnotationOnly && !isStringContinuation then
            prevCodeLineIndent = line.leadingCols

        prevLineWasBlank = line.isBlank
        idx += 1

      out.toList
