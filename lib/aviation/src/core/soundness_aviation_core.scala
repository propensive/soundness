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
package soundness

export
  aviation
  . { am, Anniversary, Apr, Aug, Base24, Base60, Calendar, Chronology, Clock, Clockface, Date,
      DateNumerics, DateSeparation, Day, days, Dec, Duration, Endianness, Feb, Fri, Hebdomad,
      Holiday, Holidays, Horology, hours, Instant, Jan, Jul, Jun, LeapSeconds, Mar, May, Meridiem,
      minutes, Moment, Mon, Month, Months, months, Monthstamp, Nov, now, Oct, Period, pm,
      RomanCalendar, Sat, seconds, Sep, StandardTime, Sun, Thu, TimeError, TimeEvent, TimeFormat,
      TimeNumerics, TimeSeparation, Timespan, TimeSpecificity, Timestamp, TimestampError, Timezone,
      TimezoneError, today, Tue, tz, Tzdb, TzdbError, Wed, Weekday, Weekdays, weeks, WorkingDays,
      Year, Years, years }

package calendars:
  export aviation.calendars.{gregorian, julian}

  package nonexistentLeapDays:
    export aviation.calendars.nonexistentLeapDays.{raiseErrors, roundDown, roundUp}

package dateFormats:
  export aviation.dateFormats.{american, european, iso8601, southEastAsia, unitedKingdom}

  package endianness:
    export aviation.dateFormats.endianness.{bigEndian, littleEndian, middleEndian}

  package numerics:
    export aviation.dateFormats.numerics.{fixedWidth, variableWidth}

  package separators:
    export aviation.dateFormats.separators.{dot, hyphen, slash, space}

  package years:
    export aviation.dateFormats.years.{full, twoDigits}

  package weekdays:
    export
      aviation.dateFormats.weekdays
      . { english, englishShort, oneLetterAmbiguous, shortestUnambiguous, twoLetter }

  package months:
    export
      aviation.dateFormats.months
      . { english, englishShort, numeric, oneLetterAmbiguous, twoDigit }

package timeFormats:
  export
    aviation.timeFormats
    . { associatedPress, civilian, french, iso8601, ledger, military, railway }

  package hours:
    export aviation.timeFormats.hours.{twelveHour, twentyFourHour}

  package meridiems:
    export aviation.timeFormats.meridiems.{lower, lowerPunctuated, upper, upperPunctuated}

  package numerics:
    export aviation.timeFormats.numerics.{fixedWidth, variableWidth}

  package separation:
    export aviation.timeFormats.separators.{colon, dot, french, none}

package hebdomads:
  export aviation.hebdomads.{european, jewish, northAmerican}

package instantDecodables:
  export aviation.instantDecodables.{iso8601, rfc1123}
