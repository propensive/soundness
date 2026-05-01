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
┃    Soundness, version 0.54.0.                                                                    ┃
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

case class GenLine
   ( line:     Int,
     startCol: Int,
     opCol:    Int,
     isFilter: Boolean )

object Comprehensions:
  // Walk the tree for every `for ... yield`/`for ... do` and return each
  // comprehension's enumerators (`x <- xs`, `y = expr`, `if filter`) as a
  // `List[GenLine]`. R34 then checks LHS / `<-`-or-`=` / filter alignment
  // within each list. Comprehensions whose enumerators all sit on a single
  // line don't need alignment — those lists collapse to one entry and the
  // rule check no-ops.
  def extract(tree: untpd.Tree, source: SourceFile): List[List[GenLine]] =
    val out = mutable.ListBuffer[List[GenLine]]()

    def visit(t: untpd.Tree): Unit =
      t match
        case fy: untpd.ForYield =>
          val gens = fy.enums.flatMap(genLine(_, source))
          if gens.nonEmpty then out += gens
        case fd: untpd.ForDo =>
          val gens = fd.enums.flatMap(genLine(_, source))
          if gens.nonEmpty then out += gens
        case _ =>
          ()
      t.productIterator.foreach(descend(_, visit))

    visit(tree)
    out.toList

  private def descend(x: Any, visit: untpd.Tree => Unit): Unit = x match
    case sub: untpd.Tree => visit(sub)
    case it:  Iterable[?] => it.foreach(descend(_, visit))
    case _                => ()

  // Build a `GenLine` for one enumerator. `GenFrom` is `x <- xs`; `GenAlias`
  // is `x = e`. Anything else in `enums` is a filter expression whose span
  // begins at the `if` keyword (the parser uses `atSpan(in.skipToken())`).
  private def genLine(t: untpd.Tree, source: SourceFile): Option[GenLine] =
    t match
      case g: untpd.GenFrom  => binaryEnum(t, g.pat, g.expr, "<-", source)
      case g: untpd.GenAlias => binaryEnum(t, g.pat, g.expr, "=",  source)
      case other =>
        val sp = other.span
        if !sp.exists then None
        else
          // The filter's tree span starts at the expression itself (`x > 0`),
          // not at the `if` keyword the user actually typed. Walk back across
          // whitespace to recover the `if` position so R34.3 compares against
          // the column the user sees.
          val content   = String(source.content)
          val ifOffset  = findIfBefore(content, sp.start)
          val keyOffset = if ifOffset >= 0 then ifOffset else sp.start
          val line      = source.offsetToLine(keyOffset) + 1
          val col       = source.column(keyOffset) + 1
          Some(GenLine(line, col, col, isFilter = true))

  // Locate the `if` keyword that immediately precedes the given offset,
  // skipping any whitespace between them. Returns -1 if no preceding `if`
  // is found (e.g. the filter is the first token on its line — only the
  // generator-expression survives parsing in some recovery paths).
  private def findIfBefore(content: String, start: Int): Int =
    var i = start - 1
    while i >= 0 && content.charAt(i).isWhitespace do i -= 1
    if i >= 1 && content.charAt(i) == 'f' && content.charAt(i - 1) == 'i' then
      val before = i - 2
      if before < 0 || !isWordChar(content.charAt(before)) then i - 1 else -1
    else -1

  private def isWordChar(c: Char): Boolean = c.isLetterOrDigit || c == '_'

  // For a generator/binding, the LHS column is the pattern's start column,
  // and the operator column is found by scanning the source between the
  // pattern's end and the right-hand-side expression's start for the
  // literal `<-` or `=`.
  private def binaryEnum
    ( t:      untpd.Tree,
      pat:    untpd.Tree,
      expr:   untpd.Tree,
      opText: String,
      source: SourceFile )
  :   Option[GenLine] =
    val patSp  = pat.span
    val exprSp = expr.span
    val sp     = t.span
    if !patSp.exists || !sp.exists then None
    else
      val line     = source.offsetToLine(sp.start) + 1
      val startCol = source.column(patSp.start) + 1
      val from     = patSp.end
      val to       = if exprSp.exists then exprSp.start else sp.end
      val content  = String(source.content)
      val opOffset = content.indexOf(opText, from)
      val opCol =
        if opOffset < 0 || opOffset >= to then -1
        else source.column(opOffset) + 1
      Some(GenLine(line, startCol, opCol, isFilter = false))
