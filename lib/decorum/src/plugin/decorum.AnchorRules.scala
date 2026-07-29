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

object AnchorRules:
  // -----------------------------------------------------------------------
  // R33: keyword-sequence layout (`if`/`then`/`else`, `for`/`yield`,
  // `for`/`do`, `while`/`do`, `try`/`catch`/`finally`).
  //
  // The chain elements (K₁, K₂, …) and their positions are extracted from
  // the untyped tree by `Sequences.extract`; this checker only applies the
  // rules. See `decorum.Sequences` for how the anchor and bridge concepts
  // map onto `untpd.If` / `WhileDo` / `ForYield` / `ForDo` / `Try`.
  //
  // A sequence K_1 B_1 K_2 B_2 … K_n B_n has an **anchor point** at the
  // (line L, column C) of K_1 (or the leftmost modifier prefix such as
  // `inline` / `transparent inline`, when present).
  //
  //   * Keyword placement (833.1): every subsequent keyword K_i (i ≥ 2) is
  //     either **inline** (starts on line L) or **broken** (starts a new
  //     line in column C). Once any K_i is broken, every later K_j must
  //     also be broken — i.e. the chain has a single break point,
  //     keywords before it on line L, keywords from it on their own lines
  //     in column C.
  //   * Body cascade (833.2): once any B_i (i ≥ 2) is indented onto its
  //     own line(s), every later body must be indented too. B_1 (the
  //     condition of `if`/`while`, the generators of `for`, the body of
  //     `try`) is exempt.
  //
  //   `else if` bridge: when an `else` is immediately followed by an `if`
  //   on the same line (possibly with modifiers between them), the trio
  //   `else if … then` is folded into a single chain element. Its
  //   placement column is the `else`'s, and the body whose layout is
  //   checked is the body after the inner `then`. This means
  //
  //       if cond1 then
  //         body1
  //       else if cond2 then
  //         body2
  //       else
  //         body3
  //
  //   is accepted: the inline `if cond2 then` does not register as an
  //   un-indented body for the outer `else`. The reference column for
  //   every broken keyword in the chain — every `then`, every `else`,
  //   every `else if` — remains the column of the first `if`.
  //
  //   If `else` is on its own line and the next `if` is on a later,
  //   more-indented line, the inner `if` is not a bridge: it starts a
  //   fresh chain with its own anchor, and the outer chain ends at
  //   `else`.
  //
  // The cascades fire forward only — a break in K_i does not retroactively
  // require K_1..K_{i-1} to break.
  // -----------------------------------------------------------------------
  object SequenceLayout extends Rule:
    def id: String = "833"
    def principle: Principle = Principle.Anchoring

    def check(ctx: Context): List[Violation] =
      val out = mutable.ListBuffer[Violation]()
      ctx.sequences.foreach(apply(ctx.file, _, out))
      out.toList

    // Apply the placement and body-cascade rules to one keyword sequence.
    // `seq.elements.head` is K₁; its (line, col) is the anchor point.
    private def apply(file: String, seq: Sequence, out: mutable.ListBuffer[Violation]): Unit =
      if seq.elements.length < 2 then return
      val anchor     = seq.anchor
      val anchorLine = anchor.line
      val anchorCol  = anchor.col

      var indentedMode = false
      var i            = 1
      while i < seq.elements.length do
        val elem         = seq.elements(i)
        val onAnchorLine = elem.line == anchorLine
        val brokenAtCol  = elem.startsLine && elem.col == anchorCol
        if !(onAnchorLine || brokenAtCol) then
          out +=
            Violation
              ( file, elem.line, elem.col, "833.1",
                s"keyword `${elem.label}` must start on the same line as "
                  +s"`${anchor.label}` (line $anchorLine) or on a new line in "
                  +s"column $anchorCol (found line ${elem.line}, column ${elem.col})" )
        // Body cascade applies to bodies after K₂ onward. K₁'s body (the
        // condition/generators/try-body) doesn't trigger the cascade.
        if indentedMode && !elem.bodyIndented then
          out +=
            Violation
              ( file, elem.line, elem.bodyCol, "833.2",
                s"body after `${elem.label}` must be indented onto a new line because "
                  +s"an earlier body in this sequence is" )
        if elem.bodyIndented then indentedMode = true
        i += 1

  // R33.3: a definition's type-annotation `:` must sit either on the same
  // line as the definition's anchor or in the anchor's column.
  //
  // Definition anchors are extracted from the untyped tree (see
  // `decorum.Definitions`), so the rule does not depend on the token
  // stream's bracket-nesting heuristics. The parser pairs each `:`
  // with its containing `ValOrDefDef` for us.
  object DefinitionAnchors extends Rule:
    def id: String = "833.3"
    def principle: Principle = Principle.Anchoring

    def check(ctx: Context): List[Violation] =
      ctx.definitions.flatMap: a =>
        if a.colonLine != a.anchorLine && a.colonCol != a.anchorCol then
          List
            ( Violation
                ( ctx.file, a.colonLine, a.colonCol, "833.3",
                  s"type-annotation `:` should align with the definition's anchor at "
                    +s"column ${a.anchorCol} (found ${a.colonCol})" ) )
        else
          Nil

  // R560: a multi-line `m`/`j`/`x`/`y`/`tel` triple-quoted string is laid out as
  // a block — the opening quotes end their line, the content is indented two
  // columns beyond the prefix, no content line is indented less than the first,
  // and the closing `"""` is alone on its line aligned with the prefix. A
  // leading `( ` before the opener and a trailing `,`/`)` after the closer are
  // permitted (the surrounding application syntax), so "alone" means the string
  // content does not share the opener/closer line.
  object InterpolationLayout extends Rule:
    def id: String = "560"
    def principle: Principle = Principle.Anchoring

    // Interpolators whose leading/trailing whitespace is insignificant, so a
    // multi-line `"""…"""` may be laid out as a block (R560). Other interpolators
    // (`t`, `s`, `sh`, …) and raw `"""` strings carry significant whitespace and
    // are exempt — see the R2/R4 relaxation in `checkLine`.
    private val LayoutInterpolators: Set[String] = Set("m", "j", "x", "y", "tel")

    def check(ctx: Context): List[Violation] =
      val file = ctx.file
      val out = mutable.ListBuffer[Violation]()
      val lines = ctx.text.split("\n", -1).nn
      def lineText(n: Int): String = if n >= 1 && n <= lines.length then lines(n - 1).nn else ""
      // 1-based column of the first non-space character, or 0 if the line is blank.
      def firstCol(text: String): Int =
        var i = 0
        while i < text.length && (text.charAt(i) == ' ' || text.charAt(i) == '\t') do i += 1
        if i >= text.length then 0 else i + 1

      ctx.interpolations.foreach: info =>
        if LayoutInterpolators.contains(info.prefix) && info.triple
          && info.closeLine > info.openLine
        then
          val col      = info.openCol         // column of the prefix character
          val expected = col + 2              // required column of the content
          val q        = "\"\"\""
          val openText = lineText(info.openLine)
          val afterOpen = (info.openCol - 1) + info.prefix.length + 3

          val openerEndsLine =
            afterOpen >= openText.length || openText.substring(afterOpen).nn.trim.nn.isEmpty

          if !openerEndsLine then
            out +=
              Violation
                ( file, info.openLine, afterOpen + 1, "560.1",
                  s"the content of a multi-line `${info.prefix}$q` string must begin on the "
                    +"line after the opening quotes" )
          else
            var first = -1
            var ln    = info.openLine + 1
            while ln < info.closeLine do
              val fc = firstCol(lineText(ln))
              if fc != 0 then
                if first < 0 then first = fc
                if fc < expected then
                  out +=
                    Violation
                      ( file, ln, fc, "560.3",
                        s"a line of a multi-line `${info.prefix}$q` string must not be indented "
                          +s"less than the first content line (column $expected)" )
              ln += 1
            if first >= 0 && first != expected then
              out +=
                Violation
                  ( file, info.openLine + 1, first, "560.2",
                    s"the content of a multi-line `${info.prefix}$q` string must be indented to "
                      +s"column $expected" )

          val closeText  = lineText(info.closeLine)
          val closeFirst = firstCol(closeText)
          val afterClose = (info.closeCol - 1) + 3
          val trailing   =
            if afterClose <= closeText.length then closeText.substring(afterClose).nn else ""
          val trailingOk = trailing.forall(c => c == ' ' || c == '\t' || c == ',' || c == ')')
          if closeFirst != info.closeCol || info.closeCol != col || !trailingOk then
            out +=
              Violation
                ( file, info.closeLine, (if closeFirst == 0 then 1 else closeFirst), "560.4",
                  s"the closing `$q` of a multi-line `${info.prefix}$q` string must be alone on "
                    +s"its line, aligned with the opening quotes (column $col)" )

      out.toList
