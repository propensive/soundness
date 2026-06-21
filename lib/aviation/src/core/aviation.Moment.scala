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

import java.time as jt

import anticipation.*
import prepositional.*

import abstractables.instantAbstractable

object Moment:
  given generic: RomanCalendar => Moment is Abstractable across Instants to Long =
    _.instant.generic

case class Moment
  ( date:       Date,
    time:       Clockface,
    timezone:   Timezone,
    occurrence: Occurrence = Occurrence.First,
    leap:       Leap       = Leap.None ):

  def instant(using calendar: RomanCalendar, gap: GapPolicy): Instant =
    val ldt =
      jt.LocalDateTime.of
        ( date.year(),
          date.month.numerical,
          date.day(),
          time.hour,
          time.minute,
          time.second,
          time.nanos ).nn

    val rules = jt.ZoneId.of(timezone.name.s).nn.getRules.nn

    def at(offset: jt.ZoneOffset): Instant = Instant(ldt.toInstant(offset).nn.toEpochMilli)

    val base =
      rules.getTransition(ldt) match
        case null       => at(rules.getOffset(ldt).nn)

        case transition =>
          val before = transition.getOffsetBefore.nn
          val after = transition.getOffsetAfter.nn

          // A gap (the wall-clock time was skipped) is resolved by the contextual policy; an
          // overlap (the wall-clock time occurs twice) is resolved by the stored `occurrence`.
          if transition.isGap then gap.resolve(at(before), at(after))
          else occurrence match
            case Occurrence.First  => at(before)
            case Occurrence.Second => at(after)

    // An inserted leap second is stored as `:59`; on the (leap-free) Unix line it shares the next
    // second's instant, so grounding it advances by one second.
    leap match
      case Leap.None     => base
      case Leap.Inserted => Instant(base.long + 1000L)

  // The absolute SI instant, counting leap seconds per the contextual strategy. For an inserted
  // leap second the TAI value is exactly one SI second before the following second's.
  def tai(using RomanCalendar, GapPolicy, LeapSeconds.Strategy): TaiInstant = leap match
    case Leap.None     => instant.tai
    case Leap.Inserted => TaiInstant(instant.tai.long - 1000L)

  def timestamp: Timestamp = Timestamp(date, time)
