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
package contingency

import scala.language.experimental.pureFunctions

import java.util.concurrent.atomic as juca

import fulminate.*

// The tactic interposed by `venture(…)` between its block and the ambient tactic. `record`
// forwards to the ambient tactic — errors accrue exactly as they would outside the venture, and
// under a fail-fast ambient tactic the forwarded `record` escapes immediately, preserving
// fail-fast semantics verbatim. `abort`, however, is DELIMITED: it records the error to the
// ambient accrual but abandons only the venture, yielding `Venture.Failed` — so a leaf may abort
// honestly without destroying the enclosing aggregation scope. The tactic is also the venture's
// own `Guard`: forcing a failed venture within the block escapes here, failing this venture
// without recording anything further.
class VentureTactic[error <: Hazard, value]
  ( outer: Tactic[error]^, label: boundary.Label[Venture[value]] )
  ( using diagnostics0: Diagnostics )
extends Tactic[error], Guard:
  private given boundary.Label[Venture[value]] = label

  // An atomic box rather than a `var`, like `AccrueTactic`'s accumulator: a `var` would classify
  // the tactic as `Stateful`, imposing update-method discipline on the whole `Tactic` interface.
  private val count: juca.AtomicInteger = juca.AtomicInteger(0)

  def diagnostics: Diagnostics = diagnostics0

  // Whether errors were recorded during THIS venture's evaluation — the condition under which the
  // completed block's value is discarded as `Failed`. Distinct from `tainted`, which also reflects
  // the ambient scope: an earlier venture's failure must not fail this one.
  def failed: Boolean = count.get() > 0

  override def tainted: Boolean = failed || outer.tainted

  def record(error: Diagnostics ?=> error): Unit =
    count.incrementAndGet()
    outer.record(error)

  def abort(error: Diagnostics ?=> error): Nothing =
    count.incrementAndGet()
    outer.record(error)
    boundary.break(Venture.failed)

  def certify(): Unit = if failed then boundary.break(Venture.failed)

  def escape(): Nothing = boundary.break(Venture.failed)
