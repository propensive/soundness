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
import rudiments.*

// Captures an `accrue` handler together with its initial accrual and combining function. Unlike
// `recover`/`mitigate`, the block may raise *several* covered errors; each is folded into the
// accrual via `combine`, and if anything accrued, `.protect` aborts the enclosing `Tactic[accrual]`
// with the accumulated error. `accrue(initial)(combine) { case … => … }.protect { … }`.
object Accrual:
  // The `Tactic` injected into an `accrue` block: each covered error is folded into the accrual
  // rather than escaping; `accumulated`/`changed` report the result to `.protect`.
  class AccrueTactic[error <: Hazard, accrual <: Hazard]
    ( initial: accrual, combine: (accrual, Exception) => accrual )
    ( using val diagnostics: Diagnostics )
  extends Tactic[error]:

    // `Atomic.Ref[accrual]`, not `Atomic[accrual]`: the match type cannot reduce for an
    // abstract type parameter, so the concrete cell is named here.
    private val accrued: Atomic.Ref[accrual] = Atomic.Ref(initial)

    // The error is forced ONCE, outside the retry. `revise` may re-run its transition under
    // contention, and `error` is a by-name which constructs an `Exception` — capturing
    // diagnostics and, under some strategies, a stack trace. Only `combine`, which `Accrual`'s
    // contract already requires to be pure, is re-run. `revise` rather than `since` because
    // `combine` is a constructor parameter: a function value, whose shape cannot be read.
    def record(error: Diagnostics ?=> error): Unit =
      val raised: Exception = error(using diagnostics)
      accrued.revise(combine(_, raised))

    def abort(error: Diagnostics ?=> error): Nothing =
      import scala.unsafeExceptions.canThrowAny
      record(error)
      throw accumulated

    // If anything has accrued, surrender the block to its accumulated error (the throw unwinds
    // to `accrueBody`'s `catch`, which reports the accrual to the outer tactic). Formerly a
    // no-op, which broke the uniform `certify` contract for accruing scopes.
    def certify(): Unit =
      if changed then
        import scala.unsafeExceptions.canThrowAny
        throw accumulated

    override def tainted: Boolean = changed

    def accumulated: accrual = accrued()
    def changed: Boolean = accumulated != initial

  extension [accrual <: Hazard, lambda[_]](inline accrual: Accrual[accrual, lambda])
    inline def protect[result](inline body: lambda[result])
      ( using outer: Tactic[accrual]^, diagnostics: Diagnostics )
    :   result =

      $ {
          contingency.internal.accrueBody[accrual, lambda, result]
            ( 'accrual,
              '{accrual.initial},
              '{accrual.combine},
              'body,
              'outer,
              'diagnostics )
        }

class Accrual[accrual <: Hazard, lambda[_]]
  ( val handler: PartialFunction[Exception, Any],
    val initial: accrual,
    val combine: (accrual, Exception) => accrual )
