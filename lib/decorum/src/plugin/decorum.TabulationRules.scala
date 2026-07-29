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

object TabulationRules:
  // R19/R20: walk each `Match`/`Try` case group from the parsed tree.
  // Within a group, runs of *single-line* cases at the same indent with no
  // intervening blank lines must have their `=>` columns aligned (R19);
  // any *multi-line* case other than the first in its group must be
  // preceded by a blank line (R20). A separate sub-check requires exactly
  // one space before `=>` in a multi-line case.
  object CaseAlignment extends Rule:
    def id: String = "326"
    def principle: Principle = Principle.Tabulation

    def check(ctx: Context): List[Violation] =
      val file  = ctx.file
      val lines = ctx.tokenRows
      val out   = mutable.ListBuffer[Violation]()

      ctx.caseGroups.foreach: group =>
        // R20 (blank line before multi-line case) is subsumed by R28's
        // generic chunk-separation rule (`ChunkSeparation`). Here we
        // retain only the sub-check requiring exactly one space before
        // `=>` in a multi-line case.
        group.foreach: c =>
          // Multi-line pattern: the heavy-pattern shape
          //   case Foo
          //     ( a, b ) =>
          //     rhs
          // requires `=>` to trail the last pattern token on the same line.
          // It must not stand alone on its own line, and the body must
          // begin on a fresh line after the `=>`.
          //
          // Cases whose *guard* spans multiple lines are exempt: the Scala
          // parser needs the `=>` dedented below the indented guard to end
          // the guard expression, so a trailing `=>` isn't an option there.
          if c.patternMultiLine && !c.guardMultiLine then
            if c.arrowAloneOnLine then
              out +=
                Violation
                  ( file, c.arrowLine, c.arrowCol, "R33-multiline-pattern-arrow-position",
                    "`=>` of a multi-line case pattern must trail the last pattern "
                      +"token on the same line, not stand alone on its own line" )
            else if !c.bodyStartsAfterArrow then
              out +=
                Violation
                  ( file, c.arrowLine, c.arrowCol, "R33-multiline-pattern-body-newline",
                    "body of a multi-line case pattern must start on the line "
                      +"after `=>`" )
          // Single-line pattern, multi-line body: keep the original sub-rule
          // (exactly one space before `=>` on the case-keyword line). Only
          // meaningful when `=>` trails content on a line; if the case has a
          // multi-line guard that forces `=>` onto its own line, the leading
          // whitespace isn't a "space before `=>`" in the rule's sense.
          else if !c.isSingleLine && !c.arrowAloneOnLine && c.spacesBeforeArrow != 1 then
            out +=
              Violation
                ( file, c.arrowLine, c.arrowCol, "R33-multiline-case-arrow-space",
                  "exactly one space is required before `=>` in a multi-line case" )

        // R19: split into runs of consecutive single-line cases at the same
        // case-keyword column with no blank line between them; align `=>` in
        // any run of size ≥ 2.
        val runs = mutable.ListBuffer[mutable.ListBuffer[CaseInfo]]()
        var current: Option[mutable.ListBuffer[CaseInfo]] = None
        var prev:    Option[CaseInfo]                     = None
        group.foreach: c =>
          val canExtend = c.isSingleLine && current.exists: run =>
            val head = run.head
            head.caseCol == c.caseCol && !blankBetween(prev.get.endLine, c.caseLine, lines)
          if canExtend then current.get += c
          else
            current =
              if c.isSingleLine then
                val r = mutable.ListBuffer[CaseInfo](c)
                runs += r
                Some(r)
              else
                None
          prev = Some(c)

        runs.foreach: run =>
          if run.size >= 2 then
            val expected = run.iterator.map(_.arrowCol).max
            run.foreach: c =>
              if c.arrowCol != expected then
                out +=
                  Violation
                    ( file, c.arrowLine, c.arrowCol, "326",
                      s"`=>` should be at column $expected to align with the case run" )

      out.toList

    private def blankBetween
      ( prevEnd:  Int,
        curStart: Int,
        lines:    IndexedSeq[IndexedSeq[Lexeme]] )
    :   Boolean =

      var l = prevEnd + 1
      while l < curStart do
        val idx = l - 1
        if idx >= 0 && idx < lines.length then
          val toks = lines(idx)
          if toks.isEmpty || toks.forall(_.kind == Sort.Space) then return true
        l += 1
      false

  // R34: alignment within each for-comprehension's enumerator block. The
  // first non-filter generator defines the **anchor** — its LHS column
  // and `<-`/`=` column. Other generators must match both; bindings
  // (`y = e`) match the same way; filter lines (`if expr`) must instead
  // sit at the anchor's operator column. Single-line comprehensions
  // collapse to one enumerator per group and skip the check.
  object ForComprehensionAlignment extends Rule:
    def id: String = "924"
    def principle: Principle = Principle.Tabulation

    def check(ctx: Context): List[Violation] =
      val file = ctx.file
      val out  = mutable.ListBuffer[Violation]()

      ctx.forGroups.foreach: gens =>
        if gens.length >= 2 then
          val anchorGen    = gens.find(!_.isFilter).getOrElse(gens.head)
          val anchorLhsCol = anchorGen.startCol
          val anchorOpCol  = anchorGen.opCol

          var i = 0
          while i < gens.length do
            val gl = gens(i)
            if !(gl.line == anchorGen.line && gl.startCol == anchorGen.startCol) then
              if gl.isFilter then
                // A filter that shares a line with another enumerator
                // (typically the generator above, e.g. `x <- xs if cond`) is
                // accepted as-is: the user is laying the filter inline rather
                // than as a separate enumerator row, and the column-alignment
                // rule does not apply.
                val sameLineAsOther =
                  gens.exists(o => (o ne gl) && o.line == gl.line)
                if !sameLineAsOther then
                  // A separate-line filter must not be separated from the
                  // preceding enumerator by a blank line.
                  if i > 0 then
                    val prev = gens(i - 1)
                    if prev.line < gl.line - 1
                    && Checker.hasBlankLineBetween(prev.line, gl.line, ctx.text, ctx.source)
                    then
                      out +=
                        Violation
                          ( file, gl.line, gl.startCol, "924.4",
                            "`if` filter must not be separated from the preceding "
                              +"enumerator by a blank line" )
                  if gl.startCol != anchorOpCol then
                    out +=
                      Violation
                        ( file, gl.line, gl.startCol, "924.3",
                          s"`if` filter should align with `<-`/`=` at column $anchorOpCol "
                            +s"(found ${gl.startCol})" )
              else
                if gl.startCol != anchorLhsCol then
                  out +=
                    Violation
                      ( file, gl.line, gl.startCol, "924.2",
                        s"generator should align with the first generator's LHS at column "
                          +s"$anchorLhsCol (found ${gl.startCol})" )
                if gl.opCol != anchorOpCol then
                  out +=
                    Violation
                      ( file, gl.line, gl.opCol, "924.1",
                        s"`<-`/`=` should be vertically aligned at column $anchorOpCol "
                          +s"(found ${gl.opCol})" )
            i += 1

      out.toList
