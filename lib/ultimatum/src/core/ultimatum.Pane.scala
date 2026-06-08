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
package ultimatum

// The content-bearing layout tree built by the `file`/`rank`/`panel` DSL. A
// `Leaf` pairs a `Sizing` with deferred content (run later, once its rectangle
// is known, with its `Extent` in context); a `Branch` splits its space among
// children along an `Axis`. A pane projects to a pure `Frame` for solving (via
// `frame`) and yields its leaves in order (via `leaves`), so a solved
// `Placement`'s cells line up one-to-one with the leaves.
enum Pane:
  def sizing: Sizing

  case Leaf(sizing: Sizing, content: Extent => Unit)
  case Widget(sizing: Sizing, focus: Focus)
  case Branch(sizing: Sizing, axis: Axis, panes: Panes)

  // The pure layout structure, with content discarded, for the solver. A widget
  // contributes only its declared `Sizing` here; the interactive driver injects
  // each widget's live intrinsic size (measured at its actual width) when it
  // re-solves, so growing content can push the rest of the layout around. A
  // branch reads its container's current contents, so a mutated layout re-solves.
  def frame: Frame = this match
    case Leaf(sizing, _)   => Frame.Cell(sizing)
    case Widget(sizing, _) => Frame.Cell(sizing)

    case Branch(sizing, axis, panes) =>
      Frame.Split(sizing, axis, panes.contents.map(_.frame).to(List))

  // The leaves in frame order (left to right for files, top to bottom for ranks).
  def leaves: List[Pane] = this match
    case Branch(_, _, panes) => panes.contents.to(List).flatMap(_.leaves)
    case leaf                => List(leaf)

  // A copy of this pane re-weighted for use as a child of a split.
  def weight(fraction: Double): Pane = this match
    case Leaf(sizing, content)       => Leaf(sizing.copy(fraction = fraction), content)
    case Widget(sizing, focus)       => Widget(sizing.copy(fraction = fraction), focus)
    case Branch(sizing, axis, panes) => Branch(sizing.copy(fraction = fraction), axis, panes)
