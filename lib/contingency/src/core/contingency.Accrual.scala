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
package contingency

import language.experimental.pureFunctions

import java.util.concurrent.atomic as juca

import scala.language.unsafeNulls

import fulminate.*

// Captures an `accrue` handler together with its initial accrual and combining function. Unlike
// `recover`/`mitigate`, the block may raise *several* covered errors; each is folded into the
// accrual via `combine`, and if anything accrued, `.protect` aborts the enclosing `Tactic[accrual]`
// with the accumulated error. `accrue(initial)(combine) { case … => … }.protect { … }`.
object Accrual:
  // The `Tactic` injected into an `accrue` block: each covered error is folded into the accrual
  // rather than escaping; `accumulated`/`changed` report the result to `.protect`.
  class AccrueTactic[error <: Exception, accrual <: Exception]
    ( initial: accrual, combine: (accrual, Exception) => accrual )
    ( using val diagnostics: Diagnostics )
  extends Tactic[error]:

    private val ref: juca.AtomicReference[accrual] = juca.AtomicReference(initial)

    def record(error: Diagnostics ?=> error): Unit =
      ref.updateAndGet: curr => combine(curr.nn, error(using diagnostics))

    def abort(error: Diagnostics ?=> error): Nothing =
      import scala.unsafeExceptions.canThrowAny
      record(error)
      throw accumulated

    def certify(): Unit = ()

    def accumulated: accrual = ref.get().nn
    def changed: Boolean = ref.get().nn != initial

  extension [accrual <: Exception, lambda[_]](inline accrual: Accrual[accrual, lambda])
    inline def protect[result](inline body: lambda[result])
      ( using outer: Tactic[accrual], diagnostics: Diagnostics )
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

class Accrual[accrual <: Exception, lambda[_]]
  ( val handler: PartialFunction[Exception, Any],
    val initial: accrual,
    val combine: (accrual, Exception) => accrual )
