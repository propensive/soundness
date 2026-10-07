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

import fulminate.*
import vacuous.*

// A `Tactic` is an `Emit` that can additionally `abort`: a value-replacing abnormal exit. `raises
// error` (`Tactic[error] ?=>`) is therefore the stronger obligation than `emits error`; code with a
// `Tactic` can satisfy either, but a fire-and-forget context offering only an `Emit` cannot supply
// the `abort` half. See [[Emit]].
trait Tactic[-error <: Hazard] extends Emit[error]:
  private inline def tactic: this.type = this
  def abort(error: Diagnostics ?=> error): Nothing

  // The flush point of an aggregation scope: if `tainted`, `certify` does not return — it
  // surrenders the scope to its accumulated errors; otherwise it is a no-op. On a fail-fast
  // tactic it is always a no-op, since a recorded error would already have escaped.
  def certify(): Unit

  // Whether any error has been recorded in this tactic's aggregation scope. Truthfully `false`
  // on every fail-fast tactic (`record` never returns there), and forwarded through `contramap`
  // so that taint is visible across `mitigate`d library boundaries.
  def tainted: Boolean = false

  // Runs `block` and discards whatever it raises: its result, or `Unset` once an error has been
  // recorded or the block aborts. The lever for a codec that tolerates a fault in an optional
  // slot. The default catches the escape every fail-fast tactic makes (a thrown error or a
  // boundary break); an accruing tactic overrides it to roll its accrual back as well.
  def tolerate[result](block: => result): Optional[result] =
    try block catch case _: Exception => Unset

  override def contramap[error2 <: Hazard](lambda: error2 -> error)
  :   Tactic[error2]^{this} =

    new Tactic[error2]:
      def diagnostics: Diagnostics = tactic.diagnostics
      def record(error: Diagnostics ?=> error2): Unit = tactic.record(lambda(error))
      def abort(error: Diagnostics ?=> error2): Nothing = tactic.abort(lambda(error))
      def certify(): Unit = tactic.certify()
      override def tainted: Boolean = tactic.tainted
      override def tolerate[result](block: => result): Optional[result] = tactic.tolerate(block)
