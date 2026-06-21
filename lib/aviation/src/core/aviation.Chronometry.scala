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
package aviation

import prepositional.*

// A `Chronometry` interprets the underlying `Long` of an `Instant over X`: which timeline `X`
// counts on, expressed as a conversion to and from TAI (International Atomic Time) — the one
// strictly-linear scale, counting every SI second with no discontinuities. Conversions between any
// two chronometries compose through TAI. Each chronometry is a distinct marker type (`Tai`,
// `Posix`, …); the phantom `X` in `Instant over X` tags an instant with its chronometry at the type
// level, so instants on different timelines never mix without an explicit `.over[…]` conversion.
//
// (v1 works in milliseconds; sub-millisecond timelines such as `Monotonic` or nanosecond clocks are
// a later extension, where the conversion's working unit becomes part of the chronometry.)
object Chronometry:
  // The default chronometry for instants that don't state one — selected by importing one of the
  // `chronometries.*` givens (e.g. `import chronometries.posix`). It names the marker type so a
  // constructed `Instant` takes that chronometry at the type level.
  trait Ambient:
    type Transport
    def chronometry: Transport is Chronometry

  // TAI itself: the identity interpretation.
  given tai: Tai is Chronometry:
    def toTai(value: Long): Long = value
    def fromTai(tai: Long): Long = tai

  // Unix/POSIX time: leap seconds aren't counted, so the offset from TAI grows by one second at
  // each leap. `fromTai` is the (table-driven) inverse; a TAI value inside an inserted leap second
  // has no Unix preimage and maps to the following second by convention.
  given posix: Posix is Chronometry:
    def toTai(value: Long): Long = LeapSeconds.tai(value)
    def fromTai(tai: Long): Long = LeapSeconds.untai(tai)

trait Chronometry extends Typeclass:
  def toTai(value: Long): Long
  def fromTai(tai: Long): Long
