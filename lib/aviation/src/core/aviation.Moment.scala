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
import gossamer.*
import hieroglyph.*, textMetrics.uniformMetric
import hypotenuse.*
import prepositional.*
import spectacular.*

import abstractables.instantAbstractable

object Moment:
  given generic: RomanCalendar => Moment is Abstractable across Instants to Long =
    _.instant.generic

  // Moments order by their grounded instant (so a `Second`-occurrence overlap sorts after the
  // `First`, and zones are compared on absolute time), hence the `RomanCalendar`/`GapPolicy` need.
  given orderable: (RomanCalendar, GapPolicy) => Moment is Orderable =
    summon[(Instant over Posix) is Orderable].contramap(_.instant)

  given ordering: (RomanCalendar, GapPolicy) => Ordering[Moment] = Ordering.by(_.instant.long)

  // An ISO-8601 zoned date-time, e.g. `2016-12-31T23:59:60+00:00`. An inserted leap second renders
  // as the 61st second (`:60`); the offset is the zone's offset at this moment's instant.
  given showable: (RomanCalendar, GapPolicy) => Moment is Showable = moment =>
    def pad(value: Int, width: Int): Text = value.show.pad(width, Bidi.Rtl, '0')

    val second = moment.leap match
      case Leap.None     => moment.time.second: Int
      case Leap.Inserted => 60

    val rules = jt.ZoneId.of(moment.timezone.name.s).nn.getRules.nn
    val offset = rules.getOffset(jt.Instant.ofEpochMilli(moment.instant.long)).nn.getId.nn

    val year = pad(moment.date.year(), 4)
    val month = pad(moment.date.month.numerical, 2)
    val day = pad(moment.date.day(), 2)
    val hour = pad(moment.time.hour, 2)
    val minute = pad(moment.time.minute, 2)

    t"$year-$month-${day}T$hour:$minute:${pad(second, 2)}${offset.tt}"

case class Moment
  ( date:       Date,
    time:       Clockface,
    timezone:   Timezone,
    occurrence: Occurrence = Occurrence.First,
    leap:       Leap       = Leap.None ):

  // A `Moment` grounds to the Unix/POSIX timeline (`java.time` works in epoch milliseconds).
  def instant(using calendar: RomanCalendar, gap: GapPolicy): Instant over Posix =
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

    def at(offset: jt.ZoneOffset): Instant over Posix =
      Instant.of[Posix](ldt.toInstant(offset).nn.toEpochMilli)

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
      case Leap.Inserted => Instant.of[Posix](base.long + 1000L)

  // The absolute SI instant on the atomic (TAI) timeline. For an inserted leap second the TAI value
  // is exactly one SI second before the following second's.
  def tai(using RomanCalendar, GapPolicy): Instant over Tai = leap match
    case Leap.None     => instant.over[Tai]
    case Leap.Inserted => Instant.of[Tai](instant.over[Tai].long - 1000L)

  def timestamp: Timestamp = Timestamp(date, time)
