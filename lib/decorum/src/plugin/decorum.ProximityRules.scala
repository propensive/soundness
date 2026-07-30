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
          && !Scans.hasBlankLineBetween(prev.endLine, cur.startLine, content, source)
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

  // R-677: the return-type line of a heavy signature — one that begins with
  // `:` followed by exactly three spaces and ends with the body-introducing
  // `=` — must be separated from the body that follows it by a blank line.
  // The signature and the body are distinct in kind, and the blank line
  // keeps that distinction visible.
  object ReturnTypeBlank extends Rule:
    def id: String = "677"
    def principle: Principle = Principle.Proximity

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      var prevWasReturnType = false
      var idx = 0

      while idx < ctx.lines.length do
        val line    = ctx.lines(idx)
        val lineNum = idx + 1

        if prevWasReturnType then
          if !line.isBlank then
            out +=
              Violation
                ( ctx.file, lineNum, 1, "677",
                  "a blank line is required between a heavy-signature return type and the body" )

          prevWasReturnType = false

        if !line.isBlank && isReturnTypeLine(line.rest) then prevWasReturnType = true
        idx += 1

      out.toList

    private def isReturnTypeLine(rest: IndexedSeq[Lexeme]): Boolean =
      rest.length >= 2 && rest(0).text == ":" && rest(1).kind == Sort.Space &&
        rest(1).text == "   " && rest.lastOption.exists(_.text == "=")

  // R6 (783): at most two consecutive blank lines. The third and every
  // subsequent blank line in a run each fire once, exactly as the old
  // walk's `consecutiveBlanks` counter did.
  object BlankLineRun extends Rule:
    def id: String = "783"
    def principle: Principle = Principle.Proximity

    def check(ctx: Context): List[Violation] =
      val out    = mutable.ListBuffer[Violation]()
      var blanks = 0
      var idx    = 0

      while idx < ctx.lines.length do
        if ctx.lines(idx).isBlank then
          blanks += 1

          if blanks > 2 then
            out += Violation(ctx.file, idx + 1, 1, "783", "more than two consecutive blank lines")
        else
          blanks = 0

        idx += 1

      out.toList

  // R-551: an annotation must sit directly above the declaration it
  // annotates. A blank line between them fires 551.2 (at the blank line,
  // which also clears the pending annotation, so a run of blanks fires
  // only once); an annotation that is never followed by another line at
  // all — the end-of-file flush — fires 551.1 at the annotation's own
  // line. `ctx.annotationEndLines` supplies the final line of every
  // annotation from the tree, so multi-line annotations arm the check on
  // their last line only.
  object AnnotationAdjacency extends Rule:
    def id: String = "551"
    def principle: Principle = Principle.Proximity

    def check(ctx: Context): List[Violation] =
      val out                = mutable.ListBuffer[Violation]()
      val annotationEndLines = ctx.annotationEndLines
      var prevWasAnnotation  = false
      var prevLineNum        = 0
      var idx                = 0

      while idx < ctx.lines.length do
        val lineNum = idx + 1

        if ctx.lines(idx).isBlank then
          if prevWasAnnotation then
            out +=
              Violation
                ( ctx.file, lineNum, 1, "551.2",
                  "blank line is not permitted between an annotation and the declaration " +
                    "it annotates" )

            prevWasAnnotation = false
        else
          prevWasAnnotation = annotationEndLines.contains(lineNum)
          prevLineNum = lineNum

        idx += 1

      if prevWasAnnotation then
        out +=
          Violation
            ( ctx.file, prevLineNum, 1, "551.1", "annotation is not followed by a declaration" )

      out.toList

  // R11/R37 (529): comma spacing. No space is permitted before a comma
  // (529.1) and exactly one space is required after it (529.2) — except
  // when the extra spaces are part of a multi-row alignment column. A
  // line's multi-space comma sites are deferred rather than fired when
  // the alignment question is still open: the line "continues" an
  // alignment iff one of its extra-space comma columns matches a column
  // the previous line also had, and a deferral is likewise cancelled when
  // the *next* line shares one of its columns. Deferred violations that
  // are never cancelled — including any still pending at end of file —
  // are flushed.
  object CommaSpacing extends Rule:
    def id: String = "529"
    def principle: Principle = Principle.Proximity

    def check(ctx: Context): List[Violation] =
      val out               = mutable.ListBuffer[Violation]()
      var prevAlignmentCols = Set[Int]()
      var pending           = List[Violation]()
      var idx               = 0

      while idx < ctx.lines.length do
        val lineNum = idx + 1
        val line    = ctx.lines(idx)
        val arr     = line.arr
        val cols    = line.cols

        val deferred      = mutable.ListBuffer[Violation]()
        val alignmentCols = mutable.Set[Int]()

        var i = 0

        while i < arr.length do
          if arr(i).text == "," && arr(i).kind == Sort.Code then
            if i > 0 && arr(i - 1).kind == Sort.Space && arr(i - 1).text.length > 0 then
              out +=
                Violation
                  ( ctx.file, lineNum, cols(i), "529.1", "no space is permitted before a comma" )

            if i + 1 < arr.length then
              val next = arr(i + 1)

              if next.kind != Sort.Space then
                out +=
                  Violation
                    ( ctx.file, lineNum, cols(i + 1), "529.2",
                      "exactly one space is required after a comma" )
              else if next.text != " " then
                // Extra spaces after comma — possibly part of a multi-row
                // alignment column. Record the column where the *next* token
                // begins (i.e. the would-be-aligned column) and defer until
                // we can confirm the previous line shared that column.
                val nextTokCol = cols(i + 1) + next.text.length
                alignmentCols += nextTokCol

                if !next.text.startsWith("\n") then
                  deferred +=
                    Violation
                      ( ctx.file, lineNum, cols(i + 1), "529.2",
                        "exactly one space is required after a comma" )

          i += 1

        // The current line "continues" an alignment iff one of its
        // multi-space comma sites lands at a column the previous line also
        // had. Otherwise the extra spaces are unjustified — fire.
        val continuesAlignment =
          alignmentCols.nonEmpty && alignmentCols.exists(prevAlignmentCols.contains)

        if continuesAlignment then pending = Nil
        else
          pending.foreach(out += _)
          pending = Nil

        if continuesAlignment then
          // Both directions confirmed — drop current deferred too.
          ()
        else if alignmentCols.nonEmpty then
          // Current line has extra-space commas but doesn't continue a known
          // alignment run; defer in case the *next* line shares a column.
          pending = deferred.toList
        else
          deferred.foreach(out += _)

        if !line.isBlank then prevAlignmentCols = alignmentCols.toSet
        else prevAlignmentCols = Set.empty

        idx += 1

      pending.foreach(out += _)
      out.toList
