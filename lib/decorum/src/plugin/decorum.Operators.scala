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

// One symbolic-infix-operator application whose two operands may wrap onto
// separate source lines. `opLine`/`opCol` locate the operator token itself;
// `leftEndLine` is the last line occupied by the left operand; `anchorIndent`
// is the indentation of the first line of the whole operator chain this node
// belongs to (the anchor for the required continuation indent — see
// `Operators`); `rightLine`/`rightCol` locate the start of the right operand.
// `multiline` is true when the operands do not share a single source line —
// the only case the continuation rule applies to.
case class OpInfo
  ( opLine:       Int,
    opCol:        Int,
    leftEndLine:  Int,
    anchorIndent: Int,
    rightLine:    Int,
    rightCol:     Int,
    multiline:    Boolean )

object Operators:
  // Walk the untyped tree and emit one `OpInfo` per *symbolic* infix-operator
  // node (`untpd.InfixOp`). This covers both value operators (`a ++ b`,
  // `x && y`) and symbolic infix *type* operators (`A | B`, `A & B`,
  // `A *: B`) — both follow the same continuation rule. Pattern alternatives
  // (`case A | B`) are `untpd.Alternative` nodes and never match here; `.`
  // method selection is a `Select` node (and `.` is not a symbolic operator),
  // so chain continuation stays the province of rules 163.1/163.2.
  //
  // The required continuation indent is anchored on the first line of the
  // whole *chain*, not on each operator's immediate left operand: a chain of
  // operators wraps to one flat indent (first-line + 2) regardless of
  // associativity. The parser nests left-associative chains (`a ++ b ++ c`)
  // to the left and right-associative ones (`a #:: b #:: c`) to the right, so
  // a per-operator left-operand anchor would (wrongly) demand a staircase for
  // the right-associative case. We therefore compute the anchor once at the
  // top of each chain and pass it down to every operator reached through a
  // direct `InfixOp` child; crossing any non-`InfixOp` boundary (parentheses,
  // an application, a block) starts a fresh chain with its own anchor.
  def extract(tree: untpd.Tree, source: SourceFile): List[OpInfo] =
    val content = String(source.content)
    val out     = mutable.ListBuffer[OpInfo]()

    // Leading-whitespace count of the line containing `offset`.
    def lineIndent(offset: Int): Int =
      val lineStart = source.startOfLine(offset)
      var i         = lineStart
      while i < content.length && (content.charAt(i) == ' ' || content.charAt(i) == '\t') do
        i += 1
      i - lineStart

    // `anchor` >= 0 means an enclosing chain has already fixed the indent;
    // -1 means this node (if an `InfixOp`) starts a fresh chain.
    def visit(t: untpd.Tree, anchor: Int): Unit = t match
      case op: untpd.InfixOp if isReportable(op) =>
        val a = if anchor >= 0 then anchor else lineIndent(op.left.span.start)
        infoFor(op, a, source).foreach(out += _)
        descendChild(op.left, a)
        descendChild(op.right, a)

      case _ =>
        t.productIterator.foreach(descendFresh)

    // A direct operand of a reportable `InfixOp`: if it is itself an `InfixOp`
    // it continues the same chain (inherit the anchor); otherwise its subtree
    // begins fresh chains.
    def descendChild(t: untpd.Tree, anchor: Int): Unit = t match
      case op: untpd.InfixOp if isReportable(op) => visit(op, anchor)
      case _                                     => descendFresh(t)

    def descendFresh(x: Any): Unit = x match
      case sub: untpd.Tree  => visit(sub, -1)
      case it:  Iterable[?] => it.foreach(descendFresh)
      case _                => ()

    visit(tree, -1)
    out.toList

  // A node is reportable when it has full position information and its
  // operator name is wholly symbolic. Word-named infix operators (`is`,
  // `raises`, `max`, …) are exempt; synthetic or position-less nodes are
  // skipped.
  private def isReportable(node: untpd.InfixOp): Boolean =
    node.span.exists && node.op.span.exists && node.left.span.exists &&
      node.right.span.exists && isSymbolicOperator(node.op.name.toString)

  // Build an `OpInfo` for one reportable `InfixOp`, given the indent anchor of
  // the chain it belongs to.
  private def infoFor
    ( node: untpd.InfixOp, anchorIndent: Int, source: SourceFile )
  :   Option[OpInfo] =

    val osp         = node.op.span
    val lsp         = node.left.span
    val rsp         = node.right.span
    val leftStart   = lsp.start
    val rightStart  = rsp.start
    val opLine      = source.offsetToLine(osp.start) + 1
    val opCol       = source.column(osp.start) + 1
    val leftEndLine = source.offsetToLine((lsp.end - 1).max(leftStart)) + 1
    val rightLine   = source.offsetToLine(rightStart) + 1
    val rightCol    = source.column(rightStart) + 1
    val multiline   = leftEndLine != rightLine
    Some(OpInfo(opLine, opCol, leftEndLine, anchorIndent, rightLine, rightCol, multiline))

  // True iff every character of `text` is a symbolic-operator character.
  // Mirrors `Checker.isSymbolicOperator`; replicated here so each extractor
  // stays self-contained.
  private def isSymbolicOperator(text: String): Boolean =
    text.nonEmpty && text.forall: c =>
      c match
        case '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' => true
        case '<' | '>' | '=' | '!' | '?' | ':' | '@' | '#'       => true
        case _                                                   => false
