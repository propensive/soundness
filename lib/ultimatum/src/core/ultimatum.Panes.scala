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

// A mutable, ordered container of child panes, backed by a `Series` for random
// access. Holding a reference to it lets the layout change while a `form` is
// running: appending a pane, or inserting one before or after an existing pane,
// re-tiles the running form. Mutations are picked up the next time the form
// re-derives its tree; when the container is bound into a running form, a
// mutation also wakes the event loop so the change is shown immediately (even
// from a background task).
// Panes themselves are pure (a pane tree captures nothing), so `Panes` need not be a capability. Its
// one effectful field is the installed repaint callback.
class Panes(initial: Pane*):
  private var series: Series[Pane] = initial.to(Series)

  // Installed by the running form so a mutation requests a repaint; a no-op until the container is
  // bound. Typed as a *pure* function so a pane tree (and `Panes`) captures nothing and can be freely
  // collected and traversed; the installed callback genuinely captures the running form's event loop,
  // reconciled in `bindWake`.
  private var onChange: () -> Unit = () => ()

  // Install the running form's repaint trigger. The callback captures the form's event loop, which
  // outlives this assignment, so it escapes into the long-lived container — a growing capture set
  // that capture checking cannot yet track. The escape is sound by construction: `onChange` is
  // re-bound on every `run` (so it never references a finished form) and is only ever called from
  // within a mutation while that form is live. Hence the single, localised `unsafeAssumePure`.
  private[ultimatum] def bindWake(wake: () => Unit): Unit =
    onChange = caps.unsafe.unsafeAssumePure(wake)

  def contents: Series[Pane] = series
  def size: Int = series.length
  def apply(index: Int): Pane = series(index)

  private def revise(updated: Series[Pane]): Unit =
    series = updated
    onChange()

  def append(pane: Pane): Unit = revise(series :+ pane)
  def prepend(pane: Pane): Unit = revise(pane +: series)

  // Insert at a position, clamped to the container's bounds.
  def insert(index: Int, pane: Pane): Unit =
    revise(series.patch(index.min(series.length).max(0), Series(pane), 0))

  // Insert immediately before `reference` (by identity); appends if it is absent.
  def insertBefore(reference: Pane, pane: Pane): Unit =
    val index = series.indexWhere(_ eq reference)
    if index < 0 then append(pane) else insert(index, pane)

  // Insert immediately after `reference` (by identity); appends if it is absent.
  def insertAfter(reference: Pane, pane: Pane): Unit =
    val index = series.indexWhere(_ eq reference)
    if index < 0 then append(pane) else insert(index + 1, pane)

  // Remove `reference` (by identity), if present.
  def remove(reference: Pane): Unit = revise(series.filter(_ ne reference))
