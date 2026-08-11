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

import anticipation.*
import escapade.*
import gossamer.*
import spectacular.*
import vacuous.*

// Figures rather than pictures: counts for a `Reckoning`, and a composed report for a `Transfer`.
// Import one by name; `Reckoning` has a default, `Transfer` deliberately does not.
package counters:
  given plainCounter: Gauging => Reckoning is Gaugeable = Reckoning.Counter.Plain.gaugeable
  given paddedCounter: Gauging => Reckoning is Gaugeable = Reckoning.Counter.Padded.gaugeable
  given wordCounter: Gauging => Reckoning is Gaugeable = Reckoning.Counter.Words.gaugeable
  given scaledCounter: Gauging => Reckoning is Gaugeable = Reckoning.Counter.Scaled.gaugeable

  given percentageCounter: Gauging => Reckoning is Gaugeable =
    Reckoning.Counter.Percentage.gaugeable

  // The moved/total figures, the rate and the estimate, in descending order of what a reader can
  // do without: at a narrow width the estimate goes first, then the rate, leaving the figures.
  // `Transfer` has no default design, so choosing one of these is also choosing whether to show a
  // rate at all — which is the editorial decision the absent default was reserving for the caller.
  private def report(showRate: Boolean, showEstimate: Boolean, binary: Boolean)
    ( using gauging: Gauging )
  :   Transfer is Gaugeable =

    new Gaugeable:
      type Self = Transfer
      override def minWidth(status: Transfer): Int = 3

      def rows(status: Transfer, tick: Tick, width: Int): List[Teletype] =
        val palette = gauging.palette

        def caption(text: Text): Teletype = gauging.tint(palette.caption)(Teletype(text))
        def muted(text: Text): Teletype = gauging.tint(palette.muted)(Teletype(text))

        val moved = Magnitude.bytes(status.moved.value, binary)

        val figures = status.total.lay(moved): total =>
          t"$moved/${Magnitude.bytes(total.value, binary)}"

        val parts = scala.collection.mutable.ListBuffer[Facet]()
        parts += Facet.fixed(0, caption(figures))

        if showRate then parts += Facet.fixed(2, muted(Magnitude.rate(status.rate.value, binary)))

        if showEstimate then status.estimate.let: remaining =>
          parts += Facet.fixed(3, muted(t"${Magnitude.interval(remaining.value)} left"))

        List(Facet.solve(List.of(parts.toList), width))

  // Binary prefixes (`4.01 MiB`), the convention for anything counted in blocks on a disk or a
  // wire.
  given transferCounter: Gauging => Transfer is Gaugeable = report(true, true, true)
  given rateTransferCounter: Gauging => Transfer is Gaugeable = report(true, false, true)
  given terseTransferCounter: Gauging => Transfer is Gaugeable = report(false, false, true)

  // Decimal prefixes (`4.20 MB`), the convention a drive manufacturer and a network operator use.
  given decimalTransferCounter: Gauging => Transfer is Gaugeable = report(true, true, false)
