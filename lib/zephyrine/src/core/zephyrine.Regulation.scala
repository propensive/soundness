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
package zephyrine

import prepositional.*

// Operations over a backpressure-demand type, the "reactive message" of a
// streaming pipeline. Demand values do not travel as messages at runtime: a
// stage computes its current demand on request, by converting its downstream's
// demand (see `Duct.translate`), bottoming out in shared state at an
// asynchronous boundary. `encode`/`decode` require every demand type to pack
// into a `Long`, so that shared state is a single atomic value, whatever the
// demand type.
object Regulation:
  given credit: Credit is Regulation:
    def initial: Credit = Credit(Long.MaxValue)
    def grant(demand: Credit): Int = demand.count.max(0L).min(Int.MaxValue).toInt
    def spend(demand: Credit, count: Int): Credit = Credit(demand.count - count)
    def measured(demand: Credit): Boolean = false
    def encode(demand: Credit): Long = demand.count
    def decode(bits: Long): Credit = Credit(bits)

  given pace: Pace is Regulation:
    def initial: Pace = Pace.Free

    def grant(demand: Pace): Int = demand match
      case Pace.Halted => 0
      case _           => Int.MaxValue

    def spend(demand: Pace, count: Int): Pace = demand
    def measured(demand: Pace): Boolean = demand == Pace.Measured
    def encode(demand: Pace): Long = demand.ordinal
    def decode(bits: Long): Pace = Pace.fromOrdinal(bits.toInt)

trait Regulation extends Typeclass.Pure:
  def initial: Self
  def grant(demand: Self): Int
  def spend(demand: Self, count: Int): Self

  // A `Measured` demand grants freely but asks the pump to pace itself (e.g.
  // `relent()` or sleep between blocks); pacing is the pump's concern, not a
  // grant-arithmetic one.
  def measured(demand: Self): Boolean

  def encode(demand: Self): Long
  def decode(bits: Long): Self
