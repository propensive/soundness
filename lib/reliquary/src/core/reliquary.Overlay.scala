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
package reliquary

import contingency.*
import rudiments.*
import vacuous.*

import Lira.Error.Reason

// Overlay semantics (§9.3): a non-root section's materialized form is
//
//   materialize(overlay) = (root − overlay.delete) ⊕ overlay.tree
//
// An overlay carries only content absent from, or differing from, the root; `diff` constructs
// exactly that minimal overlay, and `materialize` refuses non-minimal input (L107), keeping
// platform divergence visible in the manifest rather than buried in the payload.
object Overlay:

  def materialize(root: Lira.Tree, delete: List[TreePath], overlay: Lira.Tree)
  :   Lira.Tree raises Lira.Error =

    delete.each: path =>
      if root.get(path).absent then abort(Lira.Error(Reason.OverlayNotMinimal(path.text)))

      // A deleted-and-re-added path is a replacement spelled redundantly; overlays are minimal
      // by construction, so the redundant spelling is invalid.
      if overlay.get(path).present then abort(Lira.Error(Reason.OverlayNotMinimal(path.text)))

    overlay.entries.each: entry =>
      root.get(entry.path).let: existing =>
        if Blob.compare(existing.blob, entry.blob) == 0
        then abort(Lira.Error(Reason.OverlayNotMinimal(entry.path.text)))

    val deleted = delete.map(_.text).stdlib.toSet

    val kept = root.entries.filter: entry =>
      !deleted.contains(entry.path.text) && overlay.get(entry.path).absent

    Lira.Tree.of(List.from(kept.stdlib ++ overlay.entries.stdlib))

  // The producer inverse: the minimal `(tree, delete)` pair such that
  // `materialize(root, delete, tree) == target`.
  def diff(root: Lira.Tree, target: Lira.Tree): (Lira.Tree, List[TreePath]) raises Lira.Error =
    val delete = root.entries.filter { entry => target.get(entry.path).absent }.map(_.path)

    val changed = target.entries.filter: entry =>
      root.get(entry.path) match
        case existing: TreeEntry => Blob.compare(existing.blob, entry.blob) != 0
        case _                   => true

    (Lira.Tree.of(changed), delete)
