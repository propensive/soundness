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
package frontier

import scala.collection.mutable

// Parses the tab-separated tree the macro pickles into `Sentinel.missing`'s
// second argument and renders the structured result as plain text. The
// macro encodes its `seek` result (Missing/Candidate/Available/Found) so
// the plugin can show alternative candidates at the deepest level without
// having to re-run implicit search itself.
object Diagnose:
  sealed trait Node:
    def name: String
    def children: List[Node]

  case class Missing(name: String, children: List[Node])   extends Node
  case class Candidate(name: String, children: List[Node]) extends Node
  case class Available(name: String, children: List[Node]) extends Node
  case class Found(name: String, children: List[Node])     extends Node

  // Parse one line of the macro's serialised tree:
  //     <depth>\t<kind>\t<name>
  // Returns `None` for malformed or blank lines (defensive).
  private case class Line(depth: Int, kind: Char, name: String)

  private def parseLine(line: String): Option[Line] =
    val parts = line.split('\t')
    if parts.length < 3 then None
    else
      try Some(Line(parts(0).toInt, parts(1).charAt(0), parts.drop(2).mkString("\t")))
      catch case _: NumberFormatException => None

  // Convert a single line to an empty Node of the right kind. Children are
  // filled in as the parser walks deeper into the line stream.
  private def emptyNode(line: Line): Option[Node] = line.kind match
    case 'M' => Some(Missing(line.name, Nil))
    case 'C' => Some(Candidate(line.name, Nil))
    case 'A' => Some(Available(line.name, Nil))
    case 'F' => Some(Found(line.name, Nil))
    case _   => None

  private def withChildren(node: Node, children: List[Node]): Node = node match
    case _: Missing   => Missing(node.name, children)
    case _: Candidate => Candidate(node.name, children)
    case _: Available => Available(node.name, children)
    case _: Found     => Found(node.name, children)

  // Parse the full serialised tree. Builds nodes top-down using a stack
  // keyed by depth: each new line either deepens (push child onto top
  // frame), stays at the same depth (sibling), or unwinds to a shallower
  // frame (collapse and reattach).
  def parse(text: String): Option[Node] =
    val lines = text.linesIterator.toList.flatMap(parseLine)
    if lines.isEmpty then None
    else
      // Stack of (current node, accumulated children, depth)
      val frames = mutable.ArrayBuffer.empty[(Node, mutable.ArrayBuffer[Node], Int)]
      lines.foreach: line =>
        emptyNode(line).foreach: node =>
          while frames.nonEmpty && frames.last._3 >= line.depth do
            val (parentNode, parentChildren, _) = frames.remove(frames.size - 1)
            val completed = withChildren(parentNode, parentChildren.toList)
            if frames.nonEmpty then frames.last._2 += completed
            else frames += ((completed, mutable.ArrayBuffer(), -1))
          frames += ((node, mutable.ArrayBuffer.empty, line.depth))
      while frames.size > 1 do
        val (parentNode, parentChildren, _) = frames.remove(frames.size - 1)
        frames.last._2 += withChildren(parentNode, parentChildren.toList)
      val (top, topChildren, _) = frames.head
      Some(withChildren(top, topChildren.toList))

  // Render a parsed Node at the requested indent. Format mirrors the
  // macro's own tree style, just with plain characters instead of ANSI.
  def render(node: Node, indent: String = ""): String =
    val sb = new StringBuilder
    renderInto(sb, node, indent, isRoot = true)
    sb.toString

  private def renderInto(sb: StringBuilder, node: Node, indent: String, isRoot: Boolean): Unit =
    val marker = node match
      case _: Missing   => "■ resolving"
      case _: Candidate => "▪ candidate"
      case _: Available => "▸ propose"
      case _: Found     => "✓ found"
    sb.append(indent).append(' ').append(marker).append(' ').append(node.name).append('\n')
    node.children.foreach(renderInto(sb, _, indent + "  ", isRoot = false))
