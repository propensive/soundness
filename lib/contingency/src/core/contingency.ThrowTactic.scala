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

import fulminate.*

object ThrowTactic:
  // The ambient throwing tactic behind `strategies.throwUnsafely`: a pure value, as upstream's
  // `unsafeExceptions.canThrowAny` is (`caps.unsafe.unsafeErasedValue`, sealed by design). An
  // unscoped throw retains no return point, so nothing is lost by not tracking it, and tracking
  // it would be worse than useless: a fresh tactic per summon cannot satisfy a `raises` resolved
  // inside a derivation's synthesised by-name thunk, and a static tracked one would be captured
  // by every lambda that raises through it. The scoped tactics stay tracked `caps.Control`
  // capabilities, which is what keeps them out of a body that runs on another thread.
  // [ambient-strategy]
  val ambient: ThrowTactic[Hazard]^{} = caps.unsafe.unsafeAssumePure(ThrowTactic())

// A throwing tactic captures nothing (`CanThrow` is erased), so it is the natural ambient
// strategy; the scoped forms (`unsafely`, `throwErrors`) mint their own. Contravariant in `error`
// so that the ambient `ThrowTactic[Hazard]` satisfies every `Tactic[error]`.
// The `CanThrow` evidence lives on the `strategies` givens that provide throwing tactics, not on
// the class: even an erased constructor parameter is a retained capture under capture checking.
class ThrowTactic[-error <: Hazard]() extends Tactic[error]:
  def diagnostics: Diagnostics = Diagnostics.capture

  // `canThrowAny` is imported method-locally rather than retained from the constructor: the
  // caller's `CanThrow` evidence is erased type-level permission, and holding it as a field
  // would make the tactic capture a scoped control capability.
  def record(error: Diagnostics ?=> error): Unit =
    import unsafeExceptions.canThrowAny
    throw error(using diagnostics)

  def abort(error: Diagnostics ?=> error): Nothing =
    import unsafeExceptions.canThrowAny
    throw error(using diagnostics)

  def certify(): Unit = ()
