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

import dotty.tools.dotc.util.SourceFile

object ProximityRules:
  // R28: a **chunk** — any statement (or expression at statement position)
  // that spans two or more source lines — must be separated from its
  // siblings by a blank line. A chunk that's the first member of its
  // enclosing scope is exempt from "preceded-by-blank" by construction
  // (no preceding sibling to check). Single-line siblings don't trigger
  // the rule.
  //
  // This rule generalises and supersedes the prior 982 ("blank before
  // multi-line `case`") and the multi-line side of 315 ("sibling-
  // declaration padding"): both are instances of the same principle.
  // Single-line declarations can still sit adjacent with zero blanks
  // between them — only chunks compel a separator.
  object ChunkSeparation extends Rule:
    def id: String = "315"
    def principle: Principle = Principle.Proximity

    def check(ctx: Context): List[Violation] =
      val file    = ctx.file
      val content = ctx.text
      val source  = ctx.source
      val out     = mutable.ListBuffer[Violation]()

      ctx.stmtGroups.foreach: group =>
        val stmts = group.stmts
        var i = 1
        while i < stmts.length do
          val prev = stmts(i - 1)
          val cur  = stmts(i)
          if (prev.isMultiLine || cur.isMultiLine)
          && cur.startLine > prev.endLine
          && !Checker.hasBlankLineBetween(prev.endLine, cur.startLine, content, source)
          && !startsWithContinuationOperator(content, source, cur.startLine)
          && !endsWithContinuationOperator(content, source, prev.endLine, cur.startLine)
          then
            // Emit at `prev.endLine + 1` — the line immediately AFTER the
            // previous statement's last line. Inserting a blank line there
            // separates `prev` from `cur` regardless of where dotty's tree
            // places `cur.startLine` (which can be on a nested-body line
            // for control-flow constructs like `if cond then\n  body`).
            out +=
              Violation
                ( file, prev.endLine + 1, 1, "315",
                  "a multi-line statement must be separated from its siblings "
                    +"by a blank line" )
          i += 1

      out.toList

    // Lines whose first non-whitespace token is an infix continuation
    // operator (`||`, `&&`, `+`, `==`, `::`, `.`, `?`, etc.) extend the
    // previous expression, even though dotty may parse the boundary as
    // two separate `Block` stats. Inserting a blank line between them
    // would actually break the operator continuation, so R28 must not
    // fire on these.
    private def startsWithContinuationOperator
      ( content: String, source: SourceFile, line: Int )
    :   Boolean =
      // Defensively check the file lines either side of the reported
      // `line` (dotty's `offsetToLine` / `lineToOffset` indexing in 3.8.3
      // can drift by ±1 across versions, and the parser may split an
      // operator-continuation expression so the operator token sits on a
      // different line from what `Statements.collect` reports). If any of
      // the three lines starts with an infix operator, the boundary is
      // inside an operator chain and the rule must skip — inserting a
      // blank line there would break parsing.
      (line - 1 to line + 1).exists: l =>
        lineStartsWithOperator(content, source, l)

    private def lineStartsWithOperator
      ( content: String, source: SourceFile, line: Int )
    :   Boolean =
      if line < 1 then false
      else
        val start =
          try source.lineToOffset(line - 1)
          catch case _: Throwable => -1
        if start < 0 || start >= content.length then false
        else
          var i = start
          while i < content.length
                && content.charAt(i) != '\n'
                && (content.charAt(i) == ' ' || content.charAt(i) == '\t')
          do i += 1
          if i >= content.length then false
          else
            val c = content.charAt(i)
            "|&+-*/<>=!~^%?.:".indexOf(c) >= 0

    // Symmetric to `startsWithContinuationOperator`: with R616 the operator of a
    // wrapped expression sits at the END of the first line, so a parser-split
    // boundary lies inside an operator chain when the previous statement's last
    // line — or the current statement's first line (a multi-line operand
    // expression) — ends with a symbolic infix operator. Inserting a blank line
    // there would break the continuation, so R28 (315) must skip it.
    private def endsWithContinuationOperator
      ( content: String, source: SourceFile, prevEndLine: Int, curStartLine: Int )
    :   Boolean =
      lineEndsWithContinuationOperator(content, source, prevEndLine)
        || lineEndsWithContinuationOperator(content, source, curStartLine)

    // Trailing tokens that end a line but are NOT R616 infix continuations: a
    // definition/assignment `=`, the case/lambda `=>`/`?=>`, a colon-block `:`,
    // a generator `<-`, and type bounds `<:`/`>:`. A line ending in one of these
    // opens a body or clause, not a wrapped operand.
    private val NonContinuationTrailers: Set[String] =
      Set("=", "=>", "?=>", ":", "<-", "<:", ">:")

    private def lineEndsWithContinuationOperator
      ( content: String, source: SourceFile, line: Int )
    :   Boolean =
      if line < 1 then false
      else
        val start =
          try source.lineToOffset(line - 1)
          catch case _: Throwable => -1
        if start < 0 || start >= content.length then false
        else
          var i    = start
          var last = -1
          while i < content.length && content.charAt(i) != '\n' do
            val c = content.charAt(i)
            if c != ' ' && c != '\t' then last = i
            i += 1
          val opChars = "+-*/%&|^~<>=!?:#@"
          if last < 0 || opChars.indexOf(content.charAt(last)) < 0 then false
          else
            var j = last
            while j > start && opChars.indexOf(content.charAt(j - 1)) >= 0 do j -= 1
            // A binary operator is whitespace-separated from its left operand;
            // requiring that excludes suffixes like `name_=` and `xs*`/`import x.*`.
            val precededBySpace =
              j == start || content.charAt(j - 1) == ' ' || content.charAt(j - 1) == '\t'
            precededBySpace && !NonContinuationTrailers.contains(content.substring(j, last + 1).nn)
