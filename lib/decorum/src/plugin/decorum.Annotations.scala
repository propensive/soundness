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

object Annotations:
  // Walk the tree collecting the END-lines of every annotation on a
  // `MemberDef`'s modifier list. R15.2 forbids a blank line between such
  // an annotation and the declaration it annotates; the checker uses
  // this set to flag any blank line whose predecessor closes an
  // annotation. Multi-line annotations like `@Foo(\n  arg\n)` correctly
  // record their *closing* line — the line a trailing blank would
  // appear on — rather than the opening `@` line.
  def collectEndLines(tree: untpd.Tree, source: SourceFile): Set[Int] =
    val out = mutable.Set[Int]()
    walk(tree): t =>
      t match
        case d: untpd.MemberDef if d.span.exists =>
          // A MemberDef's `span.point` is set to the opening keyword (e.g.
          // `class`, `def`, `val`) — or, for unkeyworded definitions like
          // parameter `ValDef`s, to the defined identifier. Annotations and
          // modifiers extend `span.start` *before* the point. We use the
          // point's line to mean "where the declaration itself begins":
          // any annotation ending on an earlier line is a candidate for
          // 551.2, while annotations sharing a line with the declaration
          // (e.g. `@ident email: Text` in a parameter list) cannot have a
          // blank line between themselves and what they annotate.
          val declPoint = d.span.point.max(0).min(source.content.length - 1)
          val declLine  = source.offsetToLine(declPoint) + 1
          d.mods.annotations.foreach: ann =>
            val sp = ann.span
            if sp.exists then
              val endOffset = (sp.end - 1).max(sp.start).max(0).min(source.content.length - 1)
              if endOffset >= 0 then
                val annLine = source.offsetToLine(endOffset) + 1
                if annLine < declLine then out += annLine

        case _ => ()
    out.toSet

  // Generic untyped-tree pre-order traversal driven off `productIterator`,
  // so we don't need to enumerate every tree shape. We descend into any
  // field that is itself a `Tree`, and into any `Iterable` recursively
  // (covering `List[Tree]` and `List[List[Tree]]` for parameter clauses).
  private def walk(t: untpd.Tree)(visit: untpd.Tree => Unit): Unit =
    visit(t)
    t.productIterator.foreach(descend(_, visit))

  private def descend(x: Any, visit: untpd.Tree => Unit): Unit = x match
    case sub: untpd.Tree  => walk(sub)(visit)
    case it:  scala.collection.Iterable[?] => it.foreach(descend(_, visit))
    case _                => ()
