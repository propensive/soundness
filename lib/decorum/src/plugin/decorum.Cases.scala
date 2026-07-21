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

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile

case class CaseInfo
  ( caseLine:             Int,
    caseCol:              Int,
    arrowLine:            Int,
    arrowCol:             Int,
    spacesBeforeArrow:    Int,
    bodyLine:             Int,
    endLine:              Int,
    isSingleLine:         Boolean,
    patternMultiLine:     Boolean,
    guardMultiLine:       Boolean,
    arrowAloneOnLine:     Boolean,
    bodyStartsAfterArrow: Boolean )

object Cases:
  // Group every `case` arm by its enclosing `Match` or `Try.cases`. R19 and
  // R20 both reason within one such group: alignment of `=>` columns within
  // a run of single-line cases, and the blank-line-before requirement for a
  // multi-line case that follows an earlier case at the same indent.
  def extract(tree: untpd.Tree, source: SourceFile): List[List[CaseInfo]] =
    val out = mutable.ListBuffer[List[CaseInfo]]()

    def visit(t: untpd.Tree): Unit =
      t match
        case m: untpd.Match =>
          val infos = m.cases.flatMap(infoFor(_, source))
          if infos.nonEmpty then out += infos

        case tr: untpd.Try =>
          val infos = tr.cases.flatMap(infoFor(_, source))
          if infos.nonEmpty then out += infos

        case _ =>
          ()
      t.productIterator.foreach(descend(_, visit))

    visit(tree)
    out.toList

  private def descend(x: Any, visit: untpd.Tree => Unit): Unit = x match
    case sub: untpd.Tree  => visit(sub)
    case it:  Iterable[?] => it.foreach(descend(_, visit))
    case _                => ()

  // Build a `CaseInfo` for one `CaseDef`. The `=>` column is recovered by
  // searching the source between the pattern (or guard) end and the body
  // start; this is more reliable than re-tokenising the line because the
  // pattern itself can contain `=>` (e.g. function-type ascription) and the
  // body can also (e.g. a lambda inside the body).
  private def infoFor(c: untpd.CaseDef, source: SourceFile): Option[CaseInfo] =
    val csp = c.span
    val psp = c.pat.span
    val gsp = c.guard.span
    val bsp = c.body.span
    if !csp.exists || !psp.exists || !bsp.exists then None
    else
      val content      = String(source.content)
      val patternEnd   = if gsp.exists then gsp.end else psp.end
      val bodyStart    = bsp.start
      val arrowOffset  = content.indexOf("=>", patternEnd)
      // The body's span often begins at the `=>` token itself (when the
      // body is a synthetic `Block`), so we don't enforce arrow < bodyStart;
      // we only require the arrow to be inside the case clause's span.
      if arrowOffset < 0 || arrowOffset >= csp.end then None
      else
        val caseLine     = source.offsetToLine(csp.start) + 1
        val caseCol      = source.column(csp.start) + 1
        val arrowLine    = source.offsetToLine(arrowOffset) + 1
        val arrowCol     = source.column(arrowOffset) + 1
        val bodyLine     = source.offsetToLine(bodyStart) + 1
        val endLine      = source.offsetToLine((bsp.end - 1).max(bsp.start)) + 1
        // The body's `Block` span often opens at the `=>` token rather than
        // the body content, so `bodyLine == caseLine` mis-classifies
        // `case X =>\n  body` as single-line. Compare the body span's END
        // line instead.
        val isSingleLine = endLine == caseLine
        // Count contiguous spaces immediately before `=>` (only relevant
        // for multi-line cases — R33 sub-check requires exactly one).
        var k = arrowOffset - 1
        while k >= 0 && content.charAt(k) == ' ' do k -= 1
        val spacesBeforeArrow = arrowOffset - 1 - k
        // The pattern (and optional guard) spans multiple source lines if
        // its end-line is greater than the case-keyword's line. This is
        // the heavy-pattern shape: `case Foo\n  ( a, b )\n=> rhs`.
        val patternEndLine = source.offsetToLine((patternEnd - 1).max(psp.start)) + 1
        val patternMultiLine = patternEndLine > caseLine
        // A guard that spans multiple source lines (typically an indented
        // `if\n  cond1\n  && cond2`) forces the `=>` to live on its own
        // line dedented from the guard — the Scala parser needs that
        // dedent to terminate the indented guard expression. R33's
        // "trail the last pattern token" rule therefore does not apply
        // here.
        val guardMultiLine =
          gsp.exists
            && source.offsetToLine((gsp.end - 1).max(gsp.start)) > source.offsetToLine(gsp.start)
        // True when the `=>` is the first non-whitespace token on its line.
        val arrowLineStart = source.startOfLine(arrowOffset)
        var aw = arrowLineStart
        while aw < arrowOffset
              && (content.charAt(aw) == ' ' || content.charAt(aw) == '\t')
        do aw += 1
        val arrowAloneOnLine = aw == arrowOffset
        // True if the body begins on a fresh line after `=>` (dotty's body
        // span often opens at the `=>` token itself, so we scan the source
        // for the first non-whitespace character after `=>` rather than
        // trusting `bsp.start`).
        var bw  = arrowOffset + 2
        var nl  = false
        val sn  = content.length
        while bw < sn
              && (content.charAt(bw) == ' ' || content.charAt(bw) == '\t'
                  || content.charAt(bw) == '\n' || content.charAt(bw) == '\r')
        do
          if content.charAt(bw) == '\n' then nl = true
          bw += 1
        val bodyStartsAfterArrow = nl
        Some(CaseInfo
              ( caseLine          = caseLine,
                caseCol           = caseCol,
                arrowLine         = arrowLine,
                arrowCol          = arrowCol,
                spacesBeforeArrow = spacesBeforeArrow,
                bodyLine          = bodyLine,
                endLine           = endLine,
                isSingleLine         = isSingleLine,
                patternMultiLine     = patternMultiLine,
                guardMultiLine       = guardMultiLine,
                arrowAloneOnLine     = arrowAloneOnLine,
                bodyStartsAfterArrow = bodyStartsAfterArrow ))
