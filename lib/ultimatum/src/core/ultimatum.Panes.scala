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
package ultimatum

import scala.collection.immutable.Vector

import scala.caps

// A mutable, ordered container of child panes, backed by a `Sequence` for random
// access. Holding a reference to it lets the layout change while a `form` is
// running: appending a pane, or inserting one before or after an existing pane,
// re-tiles the running form. Mutations are picked up the next time the form
// re-derives its tree; when the container is bound into a running form, a
// mutation also wakes the event loop so the change is shown immediately (even
// from a background task).
// Panes themselves are pure (a pane tree captures nothing), so `Panes` need not be a capability. Its
// one effectful field is the installed repaint callback.
class Panes(initial: Pane*):
  // Internally a raw `Vector`: this is imperative container state, and the mutation operations
  // (`patch`, `indexWhere`, `:+`) belong to the stdlib surface. The public API exposes `Sequence`.
  @scala.caps.unsafe.untrackedCaptures
  private var vector: Vector[Pane] = initial.to(Vector)

  // Installed by the running form so a mutation requests a repaint; a no-op until the container is
  // bound. Typed as a *pure* function so a pane tree (and `Panes`) captures nothing and can be freely
  // collected and traversed; the installed callback genuinely captures the running form's event loop,
  // reconciled in `bindWake`.
  @scala.caps.unsafe.untrackedCaptures
  private var onChange: () -> Unit = () => ()

  // Install the running form's repaint trigger. The callback captures the form's event loop, which
  // outlives this assignment, so it escapes into the long-lived container — a growing capture set
  // that capture checking cannot yet track. The escape is sound by construction: `onChange` is
  // re-bound on every `run` (so it never references a finished form) and is only ever called from
  // within a mutation while that form is live. Hence the single, localised `unsafeAssumePure`.
  private[ultimatum] def bindWake(wake: () => Unit): Unit =
    onChange = caps.unsafe.unsafeAssumePure(wake)

  def contents: Sequence[Pane] = Sequence.from(vector)
  def size: Int = vector.length
  def apply(index: Int): Pane = vector(index)

  private def revise(updated: Vector[Pane]): Unit =
    vector = updated
    onChange()

  def append(pane: Pane): Unit = revise(vector :+ pane)
  def prepend(pane: Pane): Unit = revise(pane +: vector)

  // Insert at a position, clamped to the container's bounds.
  def insert(index: Int, pane: Pane): Unit =
    revise(vector.patch(index.min(vector.length).max(0), Vector(pane), 0))

  // Insert immediately before `reference` (by identity); appends if it is absent.
  def insertBefore(reference: Pane, pane: Pane): Unit =
    val index = vector.indexWhere(_ eq reference)
    if index < 0 then append(pane) else insert(index, pane)

  // Insert immediately after `reference` (by identity); appends if it is absent.
  def insertAfter(reference: Pane, pane: Pane): Unit =
    val index = vector.indexWhere(_ eq reference)
    if index < 0 then append(pane) else insert(index + 1, pane)

  // Remove `reference` (by identity), if present.
  def remove(reference: Pane): Unit = revise(vector.filter(_ ne reference))
