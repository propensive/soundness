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

// R312 — lambda layout. Each `param => body` syntactic lambda (`untpd.Function`)
// must use one of three forms depending on its layout and on whether the
// parameter list is named or all-placeholder (`_`-only):
//
//   • Named single-line: `f { x => expr }`, or `f: x => expr` when the
//     lambda is the last thing on its source line.
//   • Named multi-line:  `f: x =>\n  body` (no braces).
//   • Anonymous (every param a synthesised placeholder name like `x$1` /
//     `_$1`): `f(_.bar)` — parens always.
//
// The extractor returns a `LambdaSite` per `Function` node along with the
// detected syntactic form, classification, and a "last-on-line" flag. The
// checker translates these into 312.1–312.4 violations.
object Lambdas:
  enum Opener:
    case Paren, Brace, Colon, Unknown

  case class LambdaSite
   ( opener:       Opener,
     line:         Int,           // line of `=>`
     col:          Int,           // 1-based column of `=>`
     openerLine:   Int,
     openerCol:    Int,
     isAnonymous:  Boolean,
     isMultiLine: Boolean,
     lastOnLine:   Boolean )

  def extract(tree: untpd.Tree, source: SourceFile): List[LambdaSite] =
    val out     = mutable.ListBuffer[LambdaSite]()
    val content = String(source.content)

    def visit(t: untpd.Tree, parentApply: Option[untpd.Apply]): Unit =
      t match
        case f: untpd.Function => parentApply.foreach: parent =>
          siteFor(f, parent, content, source).foreach(out += _)

        case _ => ()

      val nextParent: Option[untpd.Apply] = t match
        case a: untpd.Apply => Some(a)
        case _              => parentApply

      t.productIterator.foreach(descend(_, nextParent, visit))

    visit(tree, None)
    out.toList

  private def descend
     ( x:           Any,
       parent:      Option[untpd.Apply],
       visit:       (untpd.Tree, Option[untpd.Apply]) => Unit )
  :   Unit =

    x match
      case sub: untpd.Tree  => visit(sub, parent)
      case it:  Iterable[?] => it.foreach(descend(_, parent, visit))
      case _                => ()


  private def siteFor
     ( f:      untpd.Function,
       parent: untpd.Apply,
       content: String,
       source: SourceFile )
  :   Option[LambdaSite] =

    val funSp  = parent.fun.span
    val lamSp  = f.span
    if !funSp.exists || !lamSp.exists then None
    else
      val (op, openerOffset) = detectOpener(content, funSp.end)
      if op == Opener.Unknown then None
      else
        val arrowOffset = findArrow(content, lamSp.start, lamSp.end)
        if arrowOffset < 0 then None
        else
          val arrowLine = source.offsetToLine(arrowOffset) + 1
          val arrowCol  = arrowOffset - source.startOfLine(arrowOffset) + 1
          val openerLn  = source.offsetToLine(openerOffset) + 1
          val openerCl  = openerOffset - source.startOfLine(openerOffset) + 1
          val startLine = source.offsetToLine(lamSp.start) + 1
          val endLine   = source.offsetToLine(lamSp.end - 1) + 1
          val anon      = isAnonymous(f)
          val multi     = endLine > startLine
          val last      = isLastOnLine(content, lamSp.end)
          Some
           ( LambdaSite
              ( opener      = op,
                line        = arrowLine,
                col         = arrowCol,
                openerLine  = openerLn,
                openerCol   = openerCl,
                isAnonymous = anon,
                isMultiLine = multi,
                lastOnLine  = last ) )


  // Scan forward from `from` (typically the end of the call receiver's span)
  // through whitespace until we land on `(`, `{`, or `:` — those are the
  // three argument-list openers we care about. Returns the opener kind and
  // its source offset. Anything else (e.g. `.` for a chain continuation,
  // `;`, EOF) yields `Unknown` and the call site is skipped.
  private def detectOpener(content: String, from: Int): (Opener, Int) =
    var i = from
    val n = content.length
    while i < n && isSpaceOrNewline(content.charAt(i)) do i += 1
    if i >= n then (Opener.Unknown, -1)
    else content.charAt(i) match
      case '(' => (Opener.Paren, i)
      case '{' => (Opener.Brace, i)
      case ':' => (Opener.Colon, i)
      case _   => (Opener.Unknown, -1)

  private def isSpaceOrNewline(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\n' || c == '\r'

  // Locate the `=>` token within the function's span. Naive scan, treating
  // strings (`"…"`) as opaque so an arrow-looking sequence inside a string
  // literal doesn't fool the search.
  private def findArrow(content: String, start: Int, end: Int): Int =
    var i = start
    val n = end min content.length
    while i < n - 1 do
      val c = content.charAt(i)
      if c == '"' then
        i += 1
        while i < n && content.charAt(i) != '"' do
          if content.charAt(i) == '\\' && i + 1 < n then i += 1
          i += 1
        i += 1
      else if c == '=' && content.charAt(i + 1) == '>' then return i
      else i += 1
    -1

  // A lambda is "anonymous" in the placeholder sense if every parameter has
  // a synthesised name (e.g. `_$1`, `x$1`) — these are the names dotty
  // assigns when lifting a `_` placeholder into a `Function`. Explicit
  // `x => …` lambdas have user-given names.
  private def isAnonymous(f: untpd.Function): Boolean =
    f.args.nonEmpty && f.args.forall:
      case v: untpd.ValDef =>
        val name = v.name.toString
        // Dotty uses `_$N` for `_`-derived placeholders; some Scala
        // versions use `x$N` for synthetics. Either matches.
        (name.startsWith("_$") || name.startsWith("x$")) && name.tail.tail.forall(_.isDigit)

      case _ => false

  // True if nothing but whitespace, closing brackets, commas, or semicolons
  // follows the lambda's last token until the next newline. The colon-arg
  // form `f: x => …` is only legal when the lambda is the trailing item on
  // its source line — this is what permits / forbids that form.
  private def isLastOnLine(content: String, endOffset: Int): Boolean =
    var i = endOffset
    val n = content.length
    while i < n do
      val c = content.charAt(i)
      if c == '\n' || c == '\r' then return true
      if c != ' ' && c != '\t' && c != ')' && c != '}' && c != ',' && c != ';' then return false
      i += 1
    true
