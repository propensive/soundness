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
import dendrology.*
import denominative.*
import escapade.*
import gossamer.*
import proscenium.*
import rudiments.*
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
  given treeStyle: [text: Textual] => TextualTreeStyle[text] =
    TextualTreeStyle(t"   ", t" └─", t" ├─", t" │ ")

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
        val seen = scala.collection.mutable.HashSet[Diagnostic]()
        var total = 0

        def collapse(node: Diagnostic): Diagnostic =
          total += 1

          if node.children.nil then node
          else if !seen.add(node)
          then node.rebuild(node.label.lay(t"as above") { label => t"$label, as above" }, Nil)
          else node.rebuild(node.label, node.children.map(collapse))

        val collapsed = children.map(collapse)
        val omitted = total - limit

        val ending =
          if omitted <= 0 then e"\n" else e"\n \e[38;5;243m… $omitted more\e[0m\n"

        TreeDiagram[Diagnostic](collapsed*).render(line).keep(limit)
        . join
          ( e"$headline\n\n \e[38;5;88m$Bold(■)\e[0m resolving " +
            e"\e[38;5;208m$Italic($name)\e[0m${labelled(label)}\n",
            e"\n",
            ending )

        . render(termcapDefinitions.xterm256Termcap)

      case other =>
        render(Diagnostic.Resolving(other.name, Unset, List(other)), headline, limit)

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
