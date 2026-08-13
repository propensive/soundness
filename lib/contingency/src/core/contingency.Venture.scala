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

// The outcome of a `venture(…)`: either the computed value itself, or the `Failed` sentinel
// marking that one or more errors were recorded while it was computed. A sentinel union (like
// vacuous's `Optional`) rather than an enum, so a successful venture allocates nothing — but
// OPAQUE, unlike `Optional`: a transparent union would let the `apply()` extension unify with any
// type at all, polluting extension resolution repo-wide. The errors themselves are NOT held
// here — they were already recorded into the ambient tactic's accrual at the venture's declaration
// site; `Failed` is only the "this value is unusable" marker, which is why forcing a failed
// venture escapes without reporting anything further.
object Venture:
  case object Failed

  opaque type Type[+value] = Failed.type | value

  // Not `inline`: an inline body is re-elaborated at each expansion site, where the opaque
  // `Type`'s RHS is invisible, so `Failed.type <: Type[value]` no longer holds (the
  // branded-opaque inline-widening trap). Both are trivial and JIT-inlined anyway.
  def apply[value](value: value): Type[value] = value
  def failed[value]: Type[value] = Failed

  extension [value](venture: Type[value])
    inline def ready: Boolean = venture.asInstanceOf[AnyRef] ne Failed

    // Forcing requires a `Guard`: the witness of an enclosing skip-scope (a `venture` or `guard`
    // block) to which a failed venture can escape. With no skip-scope in context, forcing does
    // not compile — there would be nowhere well-defined to skip to.
    inline def apply()(using guard: Guard^): value =
      if venture.ready then venture.asInstanceOf[value] else guard.escape()

    // Escape-free forcing for a venture the caller has already established is ready — for code
    // (e.g. a decoder's construction pass) that checks a whole batch of ventures before using
    // any. Panics on a failed venture: reaching one here is a logic error, not an error state.
    inline def vouch: value =
      if venture.ready then venture.asInstanceOf[value]
      else panic(m"a failed venture was vouched for")
