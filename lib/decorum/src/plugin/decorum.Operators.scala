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

// One symbolic-infix-operator application whose two operands may wrap onto
// separate source lines. `opLine`/`opCol` locate the operator token itself;
// `leftEndLine` is the last line occupied by the left operand; `leftIndent`
// is the indentation of the line the left operand *starts* on (the anchor for
// the required continuation indent); `rightLine`/`rightCol` locate the start
// of the right operand. `multiline` is true when the operands do not share a
// single source line — the only case the continuation rule applies to.
case class OpInfo
  ( opLine:      Int,
    opCol:       Int,
    leftEndLine: Int,
    leftIndent:  Int,
    rightLine:   Int,
    rightCol:    Int,
    multiline:   Boolean )

object Operators:
  // Walk the untyped tree and emit one `OpInfo` per *symbolic* infix-operator
  // node (`untpd.InfixOp`). This covers both value operators (`a ++ b`,
  // `x && y`) and symbolic infix *type* operators (`A | B`, `A & B`,
  // `A *: B`) — both follow the same continuation rule. Pattern alternatives
  // (`case A | B`) are `untpd.Alternative` nodes and never match here; `.`
  // method selection is a `Select` node (and `.` is not a symbolic operator),
  // so chain continuation stays the province of rules 163.1/163.2.
  def extract(tree: untpd.Tree, source: SourceFile): List[OpInfo] =
    val content = String(source.content)
    val out     = mutable.ListBuffer[OpInfo]()

    def visit(t: untpd.Tree): Unit =
      t match
        case op: untpd.InfixOp => infoFor(op, source, content).foreach(out += _)
        case _                 => ()
      t.productIterator.foreach(descend(_, visit))

    visit(tree)
    out.toList

  private def descend(x: Any, visit: untpd.Tree => Unit): Unit = x match
    case sub: untpd.Tree  => visit(sub)
    case it:  Iterable[?] => it.foreach(descend(_, visit))
    case _                => ()

  // Build an `OpInfo` for one `InfixOp`. Word-named infix operators (`is`,
  // `raises`, `max`, …) are exempt, so a node is only reported when its
  // operator name is wholly symbolic. Synthetic or position-less nodes (no
  // span on the node, the operator, or either operand) are skipped.
  private def infoFor
    ( node: untpd.InfixOp, source: SourceFile, content: String )
  :   Option[OpInfo] =

    val nsp = node.span
    val osp = node.op.span
    val lsp = node.left.span
    val rsp = node.right.span
    if !nsp.exists || !osp.exists || !lsp.exists || !rsp.exists then None
    else if !isSymbolicOperator(node.op.name.toString) then None
    else
      val opOffset    = osp.start
      val leftStart   = lsp.start
      val rightStart  = rsp.start
      val opLine      = source.offsetToLine(opOffset) + 1
      val opCol       = source.column(opOffset) + 1
      val leftEndLine = source.offsetToLine((lsp.end - 1).max(leftStart)) + 1
      val rightLine   = source.offsetToLine(rightStart) + 1
      val rightCol    = source.column(rightStart) + 1
      // The indent of the line the left operand starts on: count leading
      // whitespace from that line's start, stopping at the first non-space
      // (which may be code that precedes the operand, e.g. `val x = a ++ b`).
      val lineStart   = source.startOfLine(leftStart)
      var i           = lineStart
      while i < leftStart && (content.charAt(i) == ' ' || content.charAt(i) == '\t') do
        i += 1
      val leftIndent  = i - lineStart
      val multiline   = leftEndLine != rightLine
      Some(OpInfo(opLine, opCol, leftEndLine, leftIndent, rightLine, rightCol, multiline))

  // True iff every character of `text` is a symbolic-operator character.
  // Mirrors `Checker.isSymbolicOperator`; replicated here so each extractor
  // stays self-contained.
  private def isSymbolicOperator(text: String): Boolean =
    text.nonEmpty && text.forall: c =>
      c match
        case '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' => true
        case '<' | '>' | '=' | '!' | '?' | ':' | '@' | '#'       => true
        case _                                                   => false
