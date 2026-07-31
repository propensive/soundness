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
import escapade.*
import gossamer.*
import proscenium.*
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
  def render(diagnostic: Diagnostic, headline: Text = t"contextual value not found"): Text =
    diagnostic match
      case Diagnostic.Resolving(name, label, children) =>
        TreeDiagram[Diagnostic](children*).render(line)
        . join
          ( e"$headline\n\n \e[38;5;88m$Bold(■)\e[0m resolving " +
            e"\e[38;5;208m$Italic($name)\e[0m${labelled(label)}\n",
            e"\n",
            e"\n" )

        . render(termcapDefinitions.xterm256Termcap)

      case other =>
        render(Diagnostic.Resolving(other.name, Unset, List(other)), headline)

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

    case Diagnostic.Resolving(name, label, _) =>
      e" \e[38;5;88m$Bold(■)\e[0m resolving \e[38;5;208m$Italic($name)\e[0m${labelled(label)}"

enum Diagnostic(val name: Text, val label: Optional[Text], val children: List[Diagnostic]):
  case Resolving(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)

  case Candidate(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)

  case Requires(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)

  case Propose(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)

  case Found(text: Text, annotation: Optional[Text], nodes: List[Diagnostic])
  extends Diagnostic(text, annotation, nodes)
