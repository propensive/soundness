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

// Per-file extraction of quote/splice layout sites for the frozen rule
// family 473.2–473.7 (see §5 "Macro quotes and splices" in
// doc/standards/syntax.md). A *multi-line* quote/splice is a `' {` or
// `$ {` opener whose `{` is the last semantic token of its line, so the
// matching `}` lives on a later line; an *inline* quote/splice opens and
// closes on the same line.
//
// The extraction is deliberately lexical — the same whole-file brace scan
// the original checker used — rather than tree-derived. Rule 473.5's
// "innermost open brace" semantics depend on every `{`, not only quote
// braces, and the standalone parser mangles capture-checking syntax in a
// few files, so `untpd.Quote`/`untpd.Splice` spans cannot reproduce the
// frozen behaviour exactly. The scan is confined to this extractor;
// `QuoteRules.QuoteSpliceLayout` applies the rules declaratively to the
// extracted model.
object QuoteSites:
  // One layout-relevant site, in source order (line order, then token
  // order within the line, with a line's multi-line opener/closer sites
  // preceding its inline sites). The rule preserves this order when
  // emitting, so positional collisions between sub-rules keep resolving
  // exactly as the original per-line walk did.
  sealed trait Site:
    def line: Int

  // A multi-line quote/splice opener (`' {` / `$ {` ending its line).
  // `braceCol` is the 1-based column of the `{`; `firstSemanticCol` is the
  // column of the line's first semantic token; `singleSpace` records
  // whether exactly one space separates the prefix from the `{`; and
  // `badBefore` records whether anything other than enclosing `(` / `{`
  // brackets precedes the prefix on the line.
  case class Opener
    ( line:             Int,
      prefix:           String,
      braceCol:         Int,
      firstSemanticCol: Int,
      singleSpace:      Boolean,
      badBefore:        Boolean )
  extends Site

  // The `}` closing a multi-line quote/splice. `openBraceCol` is the
  // column of the matching opener's `{`; `semanticBefore` records whether
  // any semantic token precedes the `}` on its line; and `badAfter`
  // records whether anything other than enclosing `)` / `}` closers, an
  // arrow, or a line-final symbolic infix operator (an R616 operator
  // continuation) follows it.
  case class Closer
    ( line:           Int,
      col:            Int,
      openBraceCol:   Int,
      semanticBefore: Boolean,
      badAfter:       Boolean )
  extends Site

  // An inline quote/splice (`'{…}`, `'[…]`, `${…}`) padded by a space just
  // inside its opener and/or closer. `spaceAfterCol`/`spaceBeforeCol` are
  // the columns of the offending space runs, or -1 where the site is not
  // padded on that side. Unpadded inline sites carry no layout facts, so
  // they are not recorded.
  case class Inline
    ( line:           Int,
      prefix:         String,
      openText:       String,
      spaceAfterCol:  Int,
      spaceBeforeCol: Int )
  extends Site

  // The innermost unclosed brace carried *into* a line, when it is a
  // multi-line quote/splice brace: the column of its `{` and the line the
  // opener appeared on. Rule 473.5 checks such lines' indentation.
  case class BodyTop(braceCol: Int, openerLine: Int)

  case class Model(sites: List[Site], bodyTops: Map[Int, BodyTop])

  // One open `{` on the brace stack. `braceCol >= 0` means the opener is
  // a multi-line quote/splice and carries the `{` column; `braceCol ==
  // -1` is the sentinel for any other `{`.
  private case class Entry(braceCol: Int, openerLine: Int)

  def extract(lines: IndexedSeq[Line]): Model =
    val sites    = mutable.ListBuffer[Site]()
    val bodyTops = Map.newBuilder[Int, BodyTop]
    val stack    = mutable.Stack[Entry]()
    var idx      = 0

    while idx < lines.length do
      val line    = lines(idx)
      val lineNum = idx + 1

      if stack.nonEmpty && stack.top.braceCol >= 0 then
        bodyTops += lineNum -> BodyTop(stack.top.braceCol, stack.top.openerLine)

      scanBraces(line, lineNum, stack, sites)
      scanInline(line, lineNum, sites)
      idx += 1

    Model(sites.toList, bodyTops.result())

  // Maintain the whole-file brace stack across `line`'s tokens, recording
  // an `Opener` site for each multi-line quote/splice opener and a
  // `Closer` site for each `}` that closes one. Braces inside strings and
  // comments are not `Sort.Code` tokens, so they never affect the stack.
  private def scanBraces
    ( line:    Line,
      lineNum: Int,
      stack:   mutable.Stack[Entry],
      sites:   mutable.ListBuffer[Site] )
  :   Unit =

    val arr  = line.arr
    val cols = line.cols

    val firstSemantic = arr.indexWhere: t => t.kind != Sort.Space && t.kind != Sort.Comment
    val lastSemantic  = arr.lastIndexWhere: t => t.kind != Sort.Space && t.kind != Sort.Comment

    var i = 0

    while i < arr.length do
      if arr(i).kind == Sort.Code then
        val text = arr(i).text

        if text == "{" then
          var j = i - 1
          while j >= 0 && (arr(j).kind == Sort.Space || arr(j).kind == Sort.Comment) do j -= 1

          val precededByQuoteSplice =
            j >= 0 && arr(j).kind == Sort.Code && (arr(j).text == "'" || arr(j).text == "$")

          if i == lastSemantic && precededByQuoteSplice then
            val singleSpace =
              i - 1 >= 0 && arr(i - 1).kind == Sort.Space && arr(i - 1).text == " " && (i - 2) == j

            val badBefore =
              (0 until j).exists: k =>
                val t = arr(k)
                t.kind != Sort.Space && t.kind != Sort.Comment && t.text != "(" && t.text != "{"

            sites +=
              Opener
                ( lineNum, arr(j).text, cols(i), cols(firstSemantic), singleSpace, badBefore )

            stack.push(Entry(braceCol = cols(i), openerLine = lineNum))
          else
            stack.push(Entry(braceCol = -1, openerLine = lineNum))
        else if text == "}" then
          if stack.nonEmpty then
            val opener = stack.pop()

            if opener.braceCol >= 0 then
              val semanticBefore = firstSemantic >= 0 && firstSemantic < i

              val badAfter =
                (i + 1 until arr.length).exists: k =>
                  val t = arr(k)
                  t.kind != Sort.Space && t.kind != Sort.Comment &&
                    t.text != ")" && t.text != "}" && t.text != "=>" &&
                    !(k == lastSemantic && Scans.isSymbolicOperator(t.text))

              sites += Closer(lineNum, cols(i), opener.braceCol, semanticBefore, badAfter)

      i += 1

  // Record an `Inline` site for each same-line quote/splice group on
  // `line` that pads its contents with a space just inside the opener or
  // closer. Every `{`/`[` with a `'`/`$` prefix is considered
  // independently, so nested inline groups are each examined.
  private def scanInline(line: Line, lineNum: Int, sites: mutable.ListBuffer[Site]): Unit =
    val arr  = line.arr
    val cols = line.cols
    var i    = 0

    while i < arr.length do
      if arr(i).kind == Sort.Code then
        val text = arr(i).text

        if text == "{" || text == "[" then
          var j = i - 1
          while j >= 0 && arr(j).kind == Sort.Space do j -= 1
          val prefix = if j >= 0 && arr(j).kind == Sort.Code then arr(j).text else ""

          val isQuoteOpener =
            (text == "{" && (prefix == "'" || prefix == "$")) || (text == "[" && prefix == "'")

          if isQuoteOpener then
            val closeText = if text == "{" then "}" else "]"
            var depth = 1
            var k = i + 1

            while k < arr.length && depth > 0 do
              if arr(k).kind == Sort.Code then
                val t = arr(k).text
                if t == text then depth += 1 else if t == closeText then depth -= 1

              if depth > 0 then k += 1

            if depth == 0 && k < arr.length then
              val afterOpen   = i + 1
              val beforeClose = k - 1

              val spaceAfterCol =
                if afterOpen < k && arr(afterOpen).kind == Sort.Space &&
                  arr(afterOpen).text.nonEmpty
                then cols(afterOpen)
                else -1

              val spaceBeforeCol =
                if beforeClose > i && beforeClose != afterOpen &&
                  arr(beforeClose).kind == Sort.Space &&
                  arr(beforeClose).text.nonEmpty
                then cols(beforeClose)
                else -1

              if spaceAfterCol >= 0 || spaceBeforeCol >= 0 then
                sites += Inline(lineNum, prefix, text, spaceAfterCol, spaceBeforeCol)

      i += 1
