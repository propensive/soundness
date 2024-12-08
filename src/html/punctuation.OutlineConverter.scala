/*
    Punctuation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package punctuation

import honeycomb.*
import fulminate.*
import rudiments.*
import vacuous.*
import anticipation.*
import gossamer.*

object OutlineConverter extends HtmlConverter():
  case class Entry(label: Text, children: List[Entry])

  override def convert(nodes: Seq[Markdown.Ast.Node]): Seq[Html[Flow]] =
    def recur(entries: List[Entry]): List[Html[Ul.Content]] = entries.map: entry =>
      val link = A(href = t"#${slug(entry.label)}")(entry.label)
      if entry.children.isEmpty then Li(link) else Li(link, Ul(recur(entry.children)))

    List(Ul(recur(structure(Unset, nodes.to(List), Nil))*))

  @tailrec
  def structure
     (minimum: Optional[Int], nodes: List[Markdown.Ast.Node], stack: List[List[Entry]])
          : List[Entry] =
    nodes match
      case Nil => stack match
        case Nil         => Nil
        case last :: Nil => last.reverse

        case head :: (Entry(label, something) :: tail) :: more =>
          structure(minimum, Nil, (Entry(label, head.reverse) :: tail) :: more)

        case head :: Nil :: tail =>
          structure(minimum, Nil, List(Entry(t"", head.reverse)) :: tail)

      case Markdown.Ast.Block.Heading(level, children*) :: more if minimum.lay(true)(level >= _) =>
        val minimum2 = minimum.or(level)
        val depth = stack.length + minimum2 - 1

        if level > depth then structure(minimum2, nodes, Nil :: stack) else stack match
          case Nil =>
            panic(m"Stack should always be non-empty")

          case head :: next :: stack2 =>
            if level < depth then next match
              case Entry(label, Nil) :: tail =>
                structure(minimum2, nodes, (Entry(label, head.reverse) :: tail) :: stack2)

              case _ =>
                structure(minimum2, nodes, (Entry(t"", head.reverse) :: Nil) :: stack2)

            else
              structure(minimum2, more, (Entry(text(children), Nil) :: head) :: stack.tail)

          case other :: Nil =>
            structure(minimum2, more, (Entry(text(children), Nil) :: other) :: Nil)

      case _ :: more =>
        structure(minimum, more, stack)
