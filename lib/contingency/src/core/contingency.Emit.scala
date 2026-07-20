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

import language.experimental.pureFunctions

import beneficence.*
import fulminate.*

object Emit:
  // Builds an `Emit` whose `record` simply runs `handler` as a side-effect at the emit point — the
  // basis of `handle`, where each covered error type gets an `Emit` backed by its case body.
  def apply[error <: Hazard](handler: error => Unit)(using diagnostics0: Diagnostics)
  :   Emit[error]^ =

    new Emit[error]:
      def diagnostics: Diagnostics = diagnostics0
      def record(error: Diagnostics ?=> error): Unit = handler(error(using diagnostics0))

// The capability to *emit* an error of type `error` as a side-effect: `record` reports it but does
// not, of itself, abort — control may continue (whether it actually does is the implementation's
// choice). `Tactic` adds the value-replacing `abort`. `raise` needs only an `Emit`; `abort` needs a
// full `Tactic`. So `emits error` (`Emit[error] ?=>`) is the weaker obligation than `raises error`
// (`Tactic[error] ?=>`), and a `Tactic` in scope discharges either.
// An `Emit` is a *capability* (`caps.ExclusiveCapability`): raising an error is an effect, so an
// emitter must be capture-tracked wherever it is retained. Exclusive (rather than Shared) is the
// conservative classification — an emitter belongs to one handler scope and is not shared across
// separation boundaries. The boundary-based tactics additionally close over a stack
// `boundary.Label` (`Emit[error]^{label}`); the ambient strategies are `caps.Unscoped` so they can
// flow into any scope. Combinators that retain the receiver and a user lambda — `contramap`,
// `Emit.apply` — annotate their result with that capture set, exactly as `LzyList.map` returns
// `^{xs, f}`.
trait Emit[-error <: Hazard] extends Findable, caps.ExclusiveCapability:
  private inline def emitter: this.type = this
  def diagnostics: Diagnostics
  def record(error: Diagnostics ?=> error): Unit

  def contramap[error2 <: Hazard](lambda: error2 => error)
  :   Emit[error2]^ =

    new Emit[error2]:
      def diagnostics: Diagnostics = emitter.diagnostics
      def record(error: Diagnostics ?=> error2): Unit = emitter.record(lambda(error))
