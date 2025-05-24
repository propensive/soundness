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
┃    Soundness, version 0.31.0 for Scala 3.7.                                                      ┃
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
package soundness

export aviation
. { Base24, Base60, Calendar, Clock, Clockface, DateError, Horology, Period, LeapSeconds, Moment,
    Month, RomanCalendar, StandardTime, Timespan, Timestamp, Chronology, Timezone, TimezoneError,
    Tzdb, TzdbError, Weekday, Monthstamp, now, today, TimeEvent, am, pm, year, month, week, day,
    hour, minute, second, years, months, weeks, days, hours, minutes, seconds, tz, TimestampError,
    Instant, Duration, Date, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec,
    DateNumerics, DateSeparation, Endianness, YearFormat, Meridiem, TimeFormat, TimeSeparation,
    TimeNumerics, TimeSpecificity, Year }

package calendars:
  export aviation.calendars.{gregorian, julian}

package dateFormats:
  export aviation.dateFormats.{european, american, unitedKingdom, southEastAsia, iso8601}

  package endianness:
    export aviation.dateFormats.endianness.{littleEndian, middleEndian, bigEndian}

  package numerics:
    export aviation.dateFormats.numerics.{fixedWidth, variableWidth}

  package separation:
    export aviation.dateFormats.separation.{dot, slash, hyphen, space}

  package yearFormats:
    export aviation.dateFormats.yearFormats.{full, twoDigits}

package timeFormats:
  export aviation.timeFormats
  . { iso8601, military, civilian, associatedPress, french, railway, ledger }

  package hourCount:
    export aviation.timeFormats.hourCount.{twentyFourHour, twelveHour}

  package meridiems:
    export aviation.timeFormats.meridiems.{upper, lower, lowerPunctuated, upperPunctuated}

  package numerics:
    export aviation.timeFormats.numerics.{fixedWidth, variableWidth}

  package separation:
    export aviation.timeFormats.separation.{dot, colon, french, none}
