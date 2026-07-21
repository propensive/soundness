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

// One statement (in the broad sense — any tree at a "sibling position":
// a member of a class/trait/object body, a top-level stat of a package,
// a stat of a block, a case clause of a `match` or `try`). `startLine`
// is the line of the first source character of the statement; `endLine`
// is the line of the last. `isMultiLine` is true when those differ.
case class StmtInfo
  ( startLine:   Int,
    endLine:     Int,
    isMultiLine: Boolean )

// A list of sibling statements drawn from one scope. Each `StmtGroup`
// comes from a single `Template.body`, `PackageDef.stats`, `Block`,
// `Match.cases`, or `Try.cases`.
case class StmtGroup(stmts: List[StmtInfo])

object Statements:
  // Walk the untyped tree and produce one `StmtGroup` per scope of
  // sibling statements. A group with fewer than two members is dropped
  // (no adjacent pair to check).
  def extract(tree: untpd.Tree, source: SourceFile): List[StmtGroup] =
    val out = mutable.ListBuffer[StmtGroup]()

    def visit(t: untpd.Tree): Unit =
      t match
        case pkg: untpd.PackageDef =>
          collect(pkg.stats, source).foreach(out += _)

        case tmpl: untpd.Template =>
          // The parser populates `preBody` eagerly as a plain `List`; we
          // avoid `Template.body` which would require an implicit `Context`.
          tmpl.unforcedBody match
            case list: List[untpd.Tree @unchecked] =>
              collect(list, source).foreach(out += _)

            case _ => ()

        case m: untpd.Match =>
          collect(m.cases, source).foreach(out += _)

        case tr: untpd.Try =>
          collect(tr.cases, source).foreach(out += _)

        case b: untpd.Block =>
          // Statements inside a function body, lambda, or control-flow body.
          // We include the trailing expression so the last → second-to-last
          // adjacency is checked too. Single-line stats with no multi-line
          // neighbour are collected but produce no violations downstream;
          // R28 (315) only fires when at least one of an adjacent pair is
          // multi-line.
          collect(b.stats :+ b.expr, source).foreach(out += _)

        case _ => ()
      t.productIterator.foreach(descend(_, visit))

    visit(tree)
    out.toList

  private def descend(x: Any, visit: untpd.Tree => Unit): Unit = x match
    case sub: untpd.Tree  => visit(sub)
    case it:  Iterable[?] => it.foreach(descend(_, visit))
    case _                => ()

  private def collect
    ( stmts: List[untpd.Tree], source: SourceFile )
  :   Option[StmtGroup] =

    val content = String(source.content)
    val infos   = mutable.ListBuffer[StmtInfo]()
    stmts.foreach: s =>
      val sp = s.span
      if sp.exists then
        val startLine   = source.offsetToLine(sp.start) + 1
        val endLine     = trueEndLine(sp.start, sp.end, content, source)
        val isMultiLine = startLine != endLine
        infos += StmtInfo(startLine, endLine, isMultiLine)
    if infos.length < 2 then None else Some(StmtGroup(infos.toList))

  // Dotty's parser sometimes extends a statement's span through trailing
  // whitespace to the start of the next token. We need the *visual* end
  // line, so walk back from `spanEnd` past whitespace to find the last
  // line containing real content.
  private def trueEndLine
    ( spanStart: Int, spanEnd: Int, content: String, source: SourceFile ): Int =
    var i = spanEnd - 1
    val lo = spanStart
    while i > lo && i < content.length && isWhitespace(content.charAt(i)) do i -= 1
    if i < 0 then source.offsetToLine(spanStart) + 1
    else source.offsetToLine(i) + 1

  private def isWhitespace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\n' || c == '\r'
