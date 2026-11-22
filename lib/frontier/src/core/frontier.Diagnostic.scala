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
package frontier

import anticipation.*
import delicious.*
import dendrology.*
import denominative.*
import denominative.dysasymptotics.linearSize
import escapade.*
import gossamer.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

// A format-neutral, pre-rendered diagnostic tree describing a failed implicit
// search. Names are already rendered to `Text` (by whichever macro builds the
// tree), so the renderer carries no compiler dependency and the same model can
// be produced by Frontier's catch-all *and* by other macros — e.g. Wisteria's
// generic derivation, which can attach a `label` (a field or variant name) to
// each node.
// Not exported: clashes with harlequin.Diagnostic in the umbrella; reach it via frontier.Diagnostic
@unexported
object Diagnostic:
  given treeStyle: [text: Textual] => TreeDiagram.TextualStyle[text] =
    TreeDiagram.TextualStyle(t"   ", t" └─", t" ├─", t" │ ")

  given expandable: Diagnostic is Expandable = _.children

  // Render the diagnostic as ANSI text. The root is expected to be a
  // `Resolving` node, which supplies the "■ resolving …" headline; its
  // children form the tree below. The default headline matches the legacy
  // Frontier wording; callers (e.g. Wisteria) may override it.
  //
  // At most `limit` lines of the tree are shown, followed by a count of those
  // omitted, and a subtree identical to one already shown is written as its
  // first line alone, marked `as above`: an explanation in a large scope can
  // otherwise run to many thousands of lines, which nobody can read (#2318).
  def render
    ( diagnostic: Diagnostic,
      headline:   Text = t"contextual value not found",
      limit:      Int  = 100 )
  :   Text =

    diagnostic match
      case Diagnostic.Resolving(name, label, children) =>
        val shown = Shown(children)
        val omitted = shown.total - limit

        val ending =
          if omitted <= 0 then e"\n" else e"\n \e[38;5;243m… $omitted more\e[0m\n"

        TreeDiagram[Diagnostic](shown.nodes*).render(line).keep(limit)
        . join
          ( e"$headline\n\n \e[38;5;88m$Bold(■)\e[0m resolving " +
            e"\e[38;5;208m$Italic($name)\e[0m${labelled(label)}\n",
            e"\n",
            ending )

        . render(termcapDefinitions.xterm256Termcap)

      case other =>
        render(Diagnostic.Resolving(other.name, Unset, List(other)), headline, limit)

  // Render the diagnostic as semantic markup (`delicious.Markup`): the text `render` writes,
  // without its colours, with a `tree` marker around the tree and a `node` marker around each
  // node's line and the lines of its children. Stripped of the markers it is that text
  // exactly, so a consumer which wants only text loses nothing; one which parses the markers
  // has the tree itself, and need not read it back out of the box-drawing.
  def markup
    ( diagnostic: Diagnostic,
      headline:   Text = t"contextual value not found",
      limit:      Int  = 100 )
  :   Text =

    diagnostic match
      case root@Diagnostic.Resolving(_, _, children) =>
        val shown = Shown(children)
        val omitted = shown.total - limit
        val style = treeStyle[Text]
        type Line = (List[TreeDiagram.Tile], Diagnostic)

        def marked(node: Diagnostic, inner: Text): Text =
          val note: Optional[Text] = shown.repeats.label(node)

          val attributes = List
            ( t"role"     -> node.role.word,
              t"name"     -> node.name,
              t"note"     -> note.or(t""),
              t"repeated" -> (if shown.repeats.contains(node) then t"true" else t"") )

          Markup.wrap(t"node", attributes.filter(_(1) != t""), t"${unstyled(line(node))}$inner")

        // Takes from `lines`, which are in depth-first order, the nodes at `depth` and
        // everything beneath them, each drawn on a line of its parent's text.
        def nest(lines: List[Line], depth: Int): (Text, List[Line]) = lines match
          case (tiles, node) :: rest if tiles.size == depth =>
            val (inner, after) = nest(rest, depth + 1)
            val (siblings, remainder) = nest(after, depth)
            (t"\n${style.serialize(tiles, t"")}${marked(node, inner)}$siblings", remainder)

          case _ =>
            (t"", lines)

        val lines = TreeDiagram[Diagnostic](shown.nodes*).lines.keep(limit).to[List]
        val body = marked(root, nest(lines, 1)(0))
        val gap = if lines.nil then t"\n" else t""

        if omitted <= 0 then t"$headline\n\n${Markup.wrap(t"tree", Nil, t"$body$gap")}\n" else
          val more = t"$body\n … $omitted more"
          t"$headline\n\n${Markup.wrap(t"tree", List(t"omitted" -> omitted.show), more)}\n"

      case other =>
        markup(Diagnostic.Resolving(other.name, Unset, List(other)), headline, limit)

  // The nodes to show beneath the root: a subtree identical to one already shown is cut down
  // to its first line. `repeats` has each node so cut down, with the label it had before
  // `as above` was added to it, and `total` counts the lines there would be without a limit.
  private class Shown(children: List[Diagnostic]):
    private val seen = scala.collection.mutable.HashSet[Diagnostic]()
    private val cut = new java.util.IdentityHashMap[Diagnostic, Diagnostic]()

    private def collapse(node: Diagnostic): Diagnostic =
      if node.children.nil then node
      else if !seen.add(node) then
        val repeat = node.rebuild(node.label.lay(t"as above") { label => t"$label, as above" }, Nil)
        cut.put(repeat, node)
        repeat
      else node.rebuild(node.label, node.children.map(collapse))

    val nodes: List[Diagnostic] = children.map(collapse)

    // Every node becomes one line, and one which is cut down has no children left to count.
    private def size(node: Diagnostic): Int = node.children.fuse(1)(state + size(next))
    val total: Int = nodes.fuse(0)(state + size(next))

    object repeats:
      def contains(node: Diagnostic): Boolean = cut.containsKey(node)
      def label(node: Diagnostic): Optional[Text] =
        if contains(node) then cut.get(node).nn.label else node.label

  // A line's text alone: its colours are written into it as escapes, which are not text.
  private def unstyled(line: Teletype): Text =
    line.plain.s.replaceAll("\u001B\\[[;\\d]*m", "").nn.tt

  private def labelled(label: Optional[Text]): Teletype =
    label.lay(e""): text => e" \e[38;5;243m($Italic($text))\e[0m"

  private def line(node: Diagnostic): Teletype = node match
    case Diagnostic.Found(name, label, _) =>
      e" \e[38;5;34m$Bold(✓)\e[0m found \e[38;5;119m$Italic($name)\e[0m${labelled(label)}"

    case Diagnostic.Requires(name, label, _) =>
      e" \e[38;5;88m$Bold(✗)\e[0m requires \e[38;5;114m$Italic($name)\e[0m${labelled(label)}"

    case Diagnostic.Candidate(name, label, _) =>
      e" \e[38;5;208m$Bold(▪)\e[0m candidate \e[38;5;227m$Italic($name)\e[0m${labelled(label)}"

    case Diagnostic.Propose(name, label, _) =>
      e" \e[38;5;75m$Bold(▸)\e[0m propose \e[38;5;117m$Italic($name)\e[0m${labelled(label)}"

    case Diagnostic.Prohibits(name, label, _) =>
      e" \e[38;5;208m$Bold(⊘)\e[0m prohibits \e[38;5;114m$Italic($name)\e[0m${labelled(label)}"

    case Diagnostic.Resolving(name, label, _) =>
      e" \e[38;5;88m$Bold(■)\e[0m resolving \e[38;5;208m$Italic($name)\e[0m${labelled(label)}"

enum Diagnostic(val name: Text, val label: Optional[Text], val children: List[Diagnostic]):
  // The same kind of node, with the same name, but a new label and children.
  def role: Markup.Role = this match
    case Resolving(_, _, _) => Markup.Role.Resolving
    case Candidate(_, _, _) => Markup.Role.Candidate
    case Requires(_, _, _)  => Markup.Role.Requires
    case Propose(_, _, _)   => Markup.Role.Propose
    case Prohibits(_, _, _) => Markup.Role.Prohibits
    case Found(_, _, _)     => Markup.Role.Found

  def rebuild(label: Optional[Text], children: List[Diagnostic]): Diagnostic = this match
    case Resolving(name, _, _) => Resolving(name, label, children)
    case Candidate(name, _, _) => Candidate(name, label, children)
    case Requires(name, _, _)  => Requires(name, label, children)
    case Propose(name, _, _)   => Propose(name, label, children)
    case Prohibits(name, _, _) => Prohibits(name, label, children)
    case Found(name, _, _)     => Found(name, label, children)

  case Resolving(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)

  case Candidate(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)

  case Requires(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)

  case Propose(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)

  // A `NotGiven[X]` requirement, unwrapped to `X`: its child is the `Found`
  // that violates it, or it has none and the label `absent` when it holds.
  case Prohibits(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)

  case Found(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)
