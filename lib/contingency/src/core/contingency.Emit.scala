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

import scala.caps
import scala.language.experimental.pureFunctions

import beneficence.*
import fulminate.*

object Emit:
  // Builds an `Emit` whose `record` simply runs `handler` as a side-effect at the emit point — the
  // basis of `handle`, where each covered error type gets an `Emit` backed by its case body.
  // `handler` may capture control capabilities — an outer emitter the case body raises into — but
  // nothing else, since the emitter it becomes is itself a `caps.Control` capability, and a
  // classified capability may retain only capabilities of its own classifier.
  def apply[error <: Hazard]
    ( consume handler: error ->{caps.any.only[caps.Control]} Unit )
    ( using diagnostics0: Diagnostics )
  :   Emit[error]^ =

    new Emit[error]:
      def diagnostics: Diagnostics = diagnostics0
      def record(error: Diagnostics ?=> error): Unit = handler(error(using diagnostics0))

// The capability to *emit* an error of type `error` as a side-effect: `record` reports it but does
// not, of itself, abort — control may continue (whether it actually does is the implementation's
// choice). `Tactic` adds the value-replacing `abort`. `raise` needs only an `Emit`; `abort` needs a
// full `Tactic`. So `emits error` (`Emit[error] ?=>`) is the weaker obligation than `raises error`
// (`Tactic[error] ?=>`), and a `Tactic` in scope discharges either.
// An `Emit` is a *capability*: raising an error is an effect, so an emitter must be
// capture-tracked wherever it is retained. A `caps.Control` capability, like a `boundary.Label` or
// a `CanThrow`: it captures a return point on the stack of the code that installed it, so a body
// that runs on another thread (`->{caps.any.only[anticipation.Durable]}`, as a task's does) cannot
// retain one — a boundary tactic broken from the wrong thread would unwind a stack that is not
// its own, and any tactic used after its scope closed would report into a handler that is gone.
// `Control` is shared, not Exclusive, which is also what an emitter needs: one emitter is
// legitimately aliased by everything that reports into its scope — a codec summoned for `as`
// captures the same tactic that `as` takes as evidence — and raising is not a consuming use, so
// aliases of one emitter must not read as a separation overlap (rep/sepcheck-probes/p15; the
// Exclusive classification forced a separation seal at every such call). What Shared
// gives up is the separation checker's eye on an emitter's own state: a tactic that accrues must
// be internally sequential, the position `Monitor` already takes. The boundary-based tactics
// additionally close over a stack `boundary.Label` (`Emit[error]^{label}`). Combinators that
// retain the receiver and a user lambda — `contramap`, `Emit.apply` — annotate their result with
// that capture set, exactly as `LzyList.map` returns `^{xs, f}`.
trait Emit[-error <: Hazard] extends Findable, caps.Control:
  private inline def emitter: this.type = this
  def diagnostics: Diagnostics
  def record(error: Diagnostics ?=> error): Unit

  // `lambda` is a pure arrow: a shared capability may retain only shared capabilities and pure
  // values, and an error transformer has no business capturing anything else.
  def contramap[error2 <: Hazard](lambda: error2 -> error)
  :   Emit[error2]^{this} =

    new Emit[error2]:
      def diagnostics: Diagnostics = emitter.diagnostics
      def record(error: Diagnostics ?=> error2): Unit = emitter.record(lambda(error))
