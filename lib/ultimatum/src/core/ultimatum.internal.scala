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

import aviation.*
import quantitative.*
import symbolism.*
import vacuous.*

// The opaque status types, and their companions.
// They are declared *inside* an object rather than at the top level of the package, which is the
// convention throughout Soundness (`anticipation.internal.Text`, `denominative.internal.Ordinal`,
// …). A top-level opaque type is lifted into a synthesized `Xyz$package` wrapper — the compiler
// warns that this "can lead to undefined behaviour" — and in practice its abstraction leaks: a type
// variable inferred from it widens to the underlying representation, so `Reading(Fraction(0.5))`
// would infer `Reading[Double]` and no design could be found for it.
// The `caps.Pure` bound is the other half: a status is data, and saying so here is what lets it be
// assigned into a `Reading` without the capture checker demanding a purity the type never claimed.
object internal:
  opaque type Fraction <: Matchable & caps.Pure = Double & caps.Pure
  // No `Matchable` bound: `Duration` is itself opaque, and the intersection conflicts.
  opaque type Countdown <: caps.Pure = Duration & caps.Pure

  // `caps.Pure` is an erased marker, so each type still erases to its representation; the casts
  // are runtime no-ops that let the capture checker treat the status as pure.
  object Fraction:
    private inline def make(value: Double): Fraction = value.asInstanceOf[Fraction]

    def apply(value: Double): Fraction =
      make(if value.isNaN then 0.0 else value.max(0.0).min(1.0))

    def of(done: Long, total: Long): Fraction =
      if total <= 0 then Fraction(0.0) else Fraction(done.toDouble/total)

    // Work in flight whose total is not known. Progress is `Optional[Fraction]` rather than a
    // separate "busy" type, so a job that starts out unmeasurable and later learns its total does
    // not change type half way through — and every design decides both renderings.
    val indeterminate: Optional[Fraction] = Unset

    // The default design: the smooth eighth-block bar, which is what a Soundness progress bar has
    // always looked like. Every candidate bar is one row and says the same thing, so a default is
    // safe here in a way it is not for a meter or a procession.
    // `Optional[Fraction]` needs no separate default: `Gaugeable.optional` lifts this one, and a
    // bar's `absent` rendering sweeps.
    given gaugeable: Gauging => Fraction is Gaugeable = bars.smoothBar

    extension (fraction: Fraction)
      def value: Double = fraction.asInstanceOf[Double]
      def percentage: Int = (fraction.value*100).toInt
      def complete: Boolean = fraction.value >= 1.0

  object Countdown:
    private inline def make(duration: Duration): Countdown = duration.asInstanceOf[Countdown]

    // Clamped at zero, so a deadline that has passed reads as `0s` rather than as a negative
    // interval.
    def apply(duration: Duration): Countdown =
      make(if duration.value < 0 then 0.0*Second else duration)

    given gaugeable: Gauging => Countdown is Gaugeable = timers.compactCountdown

    extension (countdown: Countdown) def duration: Duration = countdown.asInstanceOf[Duration]
