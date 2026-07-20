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

    // The rule only applies to lambdas that are the *sole direct argument*
    // to a call. Restricting to single-argument applies means the
    // suggested transformation `f(λ) → f { λ } → f: λ` is always safe —
    // multi-arg calls like `f(λ, other)` can't be rewritten in the brace
    // or colon form without losing the other argument(s). Lambdas
    // nested inside tuples, conditionals, or non-Apply parents are
    // skipped for the same reason.
    def walk(t: untpd.Tree): Unit =
      t match
        case a: untpd.Apply if a.args.length == 1 =>
          directLambda(a.args.head).foreach: f =>
            siteFor(f, a, content, source).foreach(out += _)

        case _ => ()
      t.productIterator.foreach(descend(_, walk))

    walk(tree)
    out.toList

  private def directLambda(t: untpd.Tree): Option[untpd.Function] = t match
    case f: untpd.Function => Some(f)

    case b: untpd.Block if b.stats.isEmpty && b.expr.isInstanceOf[untpd.Function] =>
      Some(b.expr.asInstanceOf[untpd.Function])

    case _ => None

  private def descend(x: Any, visit: untpd.Tree => Unit): Unit = x match
    case sub: untpd.Tree  => visit(sub)
    case it:  Iterable[?] => it.foreach(descend(_, visit))
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
      // The opener relevant to R312 is the *immediate* wrapper of the
      // lambda — so for `f({ x => … })` we want `{`, not the outer `(`.
      // Scan backward from the function's start past whitespace; the
      // first non-whitespace character is the wrapper.
      val (op, openerOffset) = detectOpener(content, lamSp.start)
      if op == Opener.Unknown then None
      else
        val arrowOffset = findArrow(content, lamSp.start, lamSp.end)
        val isPlaceholder = arrowOffset < 0
        // An explicit `_ => …` (or `(_, _) => …`) is exempt from R312: the
        // author chose `_` as the parameter name to signal that the value
        // is unused, and that signal is independent of `(…)` / `{…}` /
        // `: …` choice. Detect by: the function HAS an explicit `=>` in
        // source, and every parameter is a synthetic `_$N` (the name
        // dotty assigns to a user-written `_` parameter).
        val unusedExplicit = !isPlaceholder && allParamsUnderscore(f)
        if unusedExplicit then None
        else
          val pivotOffset = if isPlaceholder then lamSp.start else arrowOffset
          val pivotLine   = source.offsetToLine(pivotOffset) + 1
          val pivotCol    = pivotOffset - source.startOfLine(pivotOffset) + 1
          val openerLn    = source.offsetToLine(openerOffset) + 1
          val openerCl    = openerOffset - source.startOfLine(openerOffset) + 1
          val startLine   = source.offsetToLine(lamSp.start) + 1
          val endLine     = source.offsetToLine(lamSp.end - 1) + 1
          val multi       = endLine > startLine
          // "Last on line" governs whether `f: x => …` is a syntactically
          // valid alternative: it is only if the *whole call* extends to
          // end-of-line. Anchor the check after the enclosing Apply's
          // span, not after the lambda — so a lambda nested inside an
          // outer call like `recur(0, xs.map { … })` correctly reports
          // not-last (`)` of `recur` trails the call).
          val last        = isLastOnLine(content, parent.span.end)
          // "Anonymous" here means *placeholder syntax* — `f(_.bar)`,
          // `f(_ + 1)` — distinguished from `f(_ => body)` by the absence
          // of `=>` in the function's source span.
          Some
            ( LambdaSite
              ( opener      = op,
                line        = pivotLine,
                col         = pivotCol,
                openerLine  = openerLn,
                openerCol   = openerCl,
                isAnonymous = isPlaceholder,
                isMultiLine = multi,
                lastOnLine  = last ) )

  // True if every parameter of the function is a synthesised `_$N` name —
  // dotty's renaming for both placeholder-lifting and an explicit `_`
  // user-written parameter name.
  private def allParamsUnderscore(f: untpd.Function): Boolean =
    f.args.nonEmpty && f.args.forall:
      case v: untpd.ValDef =>
        val name = v.name.toString
        name.startsWith("_$") && name.drop(2).forall(_.isDigit)

      case _ => false


  // Scan *backward* from `from` (the lambda's source start) past whitespace
  // and look at the first non-whitespace character. That character is the
  // immediate wrapper of the lambda — `{`, `(`, or `:` for an arg-list
  // colon. Any other character (e.g. `;` for the tail of a multi-statement
  // block, `,` for a sibling argument) yields `Unknown` and the call site
  // is skipped. The "look at the immediate wrapper" approach correctly
  // classifies `f({ x => … })` as brace-wrapped even though the outer call
  // uses parens, because the `{` sits closer to the lambda.
  private def detectOpener(content: String, from: Int): (Opener, Int) =
    var i = from - 1
    while i >= 0 && isSpaceOrNewline(content.charAt(i)) do i -= 1
    if i < 0 then (Opener.Unknown, -1)
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

  // True if only whitespace and end-of-line comments follow `endOffset`
  // until the next newline. The colon-arg form `f: x => …` is only legal
  // when the *whole call* extends to the end of the source line —
  // any subsequent `)`, `}`, `,`, etc. would have to come after the
  // colon-arg block, which the syntax does not allow.
  private def isLastOnLine(content: String, endOffset: Int): Boolean =
    var i = endOffset
    val n = content.length
    while i < n do
      val c = content.charAt(i)
      if c == '\n' || c == '\r' then return true
      if c != ' ' && c != '\t' then return false
      i += 1
    true
