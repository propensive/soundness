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

// One string-interpolation site. `prefix` is the interpolator name (e.g. "m",
// "tel", "t"); `openLine`/`openCol` locate the prefix character (the `m` of
// `m"""`); `closeLine`/`closeCol` locate the first quote of the closing `"""`.
// All positions are 1-based. `triple` is true for a `"""…"""` string (the only
// kind that can span lines).
case class InterpolationInfo
  ( prefix:    String,
    openLine:  Int,
    openCol:   Int,
    closeLine: Int,
    closeCol:  Int,
    triple:    Boolean )

object Interpolations:
  // Walk the untyped tree and emit one `InterpolationInfo` per
  // `untpd.InterpolatedString` node. These survive in the pre-desugar
  // `untpdTree` that Decorum reads (desugaring to `StringContext` happens
  // later, during typing), just like `untpd.InfixOp`.
  def extract(tree: untpd.Tree, source: SourceFile): List[InterpolationInfo] =
    val content = String(source.content)
    val out     = mutable.ListBuffer[InterpolationInfo]()

    def visit(t: untpd.Tree): Unit =
      t match
        case interp: untpd.InterpolatedString => infoFor(interp, source, content).foreach(out += _)
        case _                                => ()
      t.productIterator.foreach(descend(_, visit))

    visit(tree)
    out.toList

  private def descend(x: Any, visit: untpd.Tree => Unit): Unit = x match
    case sub: untpd.Tree  => visit(sub)
    case it:  Iterable[?] => it.foreach(descend(_, visit))
    case _                => ()

  private def infoFor
    ( node: untpd.InterpolatedString, source: SourceFile, content: String )
  :   Option[InterpolationInfo] =

    val sp = node.span
    if !sp.exists then None
    else
      val prefix     = node.id.toString
      val openOffset = sp.start
      // The node span starts at the prefix character. The opening quote(s)
      // begin just after the prefix; a triple-quoted string opens with `"""`.
      val quoteStart = openOffset + prefix.length
      val triple =
        quoteStart + 2 < content.length && content.charAt(quoteStart) == '"'
          && content.charAt(quoteStart + 1) == '"' && content.charAt(quoteStart + 2) == '"'
      // The closing quote run ends the span. For a triple-quoted string the
      // closing `"""` occupies the three code units before `span.end`.
      val closeQuoteLen = if triple then 3 else 1
      val closeStart    = (sp.end - closeQuoteLen).max(openOffset)
      val openLine      = source.offsetToLine(openOffset) + 1
      val openCol       = source.column(openOffset) + 1
      val closeLine     = source.offsetToLine(closeStart) + 1
      val closeCol      = source.column(closeStart) + 1
      Some(InterpolationInfo(prefix, openLine, openCol, closeLine, closeCol, triple))
