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

import scala.caps

import rudiments.Atomic

// A mutable cell holding a gauge's current status. Assigning to it publishes the new value and
// wakes the running form, so a gauge updated from a background task repaints at once — the same
// contract a `Panes` mutation has.
// The status itself is plain data (a pane tree stays pure); the one effectful field is the
// installed repaint callback.
class Reading[status](initial: status):
  // Written from background tasks and read on the form's thread, and `amend` must not lose an
  // update to a concurrent one, so the status lives in an atomic cell. `Atomic.Ref[status]`, not
  // `Atomic[status]`: the match type cannot reduce for an abstract type parameter.
  private val current: Atomic.Ref[status] = Atomic.Ref(initial)

  // A no-op until the cell is bound into a running form. Atomic too, so that a task updating the
  // cell sees the callback the form bound, rather than a stale no-op that never wakes the form.
  private val onChange: Atomic.Ref[() -> Unit] = Atomic.Ref(() => ())

  // Install the running form's repaint trigger. As in `Panes.bindWake`, the callback genuinely
  // captures the form's event loop and escapes into this longer-lived cell — a growing capture set
  // that capture checking cannot yet express. It is sound by construction for the same reasons: it
  // is re-bound on every `run` (so it never references a finished form) and is only ever called
  // from a mutation while that form is live. Hence the single, localised `unsafeAssumePure`.
  private[ultimatum] def bindWake(wake: () => Unit): Unit =
    // [field-purity] form wake callback escapes into Reading field
    onChange() = caps.unsafe.unsafeAssumePure(wake)

  def apply(): status = current()

  // Paired with `apply`, this gives assignment syntax: `reading() = Fraction(0.42)`.
  def update(status: status): Unit =
    current() = status
    onChange()()

  // A compare-and-set transition, so two tasks amending at once both land. `lambda` may be re-run
  // under contention, so it must be pure — the contract `Accrual`'s `combine` has.
  def amend(lambda: status => status): Unit =
    current.revise(lambda)
    onChange()()
