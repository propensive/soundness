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
┃    Soundness, version 0.40.0.                                                                    ┃
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
. { Base24, Base60, Calendar, Clock, Clockface, TimeError, Horology, Period, LeapSeconds, Moment,
    Month, RomanCalendar, StandardTime, Timespan, Timestamp, Chronology, Timezone, TimezoneError,
    Tzdb, TzdbError, Weekday, Monthstamp, now, today, TimeEvent, am, pm, Anniversary,
    years, months, weeks, days, hours, minutes, seconds, tz, TimestampError,
    Instant, Duration, Date, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, Day,
    DateNumerics, DateSeparation, Endianness, Years, Meridiem, TimeFormat, TimeSeparation,
    TimeNumerics, TimeSpecificity, Year, Months, Weekdays, Hebdomad, Mon, Tue, Wed, Thu, Fri, Sat,
    Sun, Holiday, Holidays, WorkingDays }

package calendars:
  export aviation.calendars.{gregorian, julian}

  package nonexistentLeapDays:
    export aviation.calendars.nonexistentLeapDays.{roundUp, roundDown, raiseErrors}

package dateFormats:
  export aviation.dateFormats.{european, american, unitedKingdom, southEastAsia, iso8601}

  package endianness:
    export aviation.dateFormats.endianness.{littleEndian, middleEndian, bigEndian}

  package numerics:
    export aviation.dateFormats.numerics.{fixedWidth, variableWidth}

  package separators:
    export aviation.dateFormats.separators.{dot, slash, hyphen, space}

  package years:
    export aviation.dateFormats.years.{full, twoDigits}

  package weekdays:
    export aviation.dateFormats.weekdays
    . { english, englishShort, oneLetterAmbiguous, shortestUnambiguous, twoLetter }

  package months:
    export aviation.dateFormats.months
    . { english, englishShort, numeric, twoDigit, oneLetterAmbiguous }

package timeFormats:
  export aviation.timeFormats
  . { iso8601, military, civilian, associatedPress, french, railway, ledger }

  package hours:
    export aviation.timeFormats.hours.{twentyFourHour, twelveHour}

  package meridiems:
    export aviation.timeFormats.meridiems.{upper, lower, lowerPunctuated, upperPunctuated}

  package numerics:
    export aviation.timeFormats.numerics.{fixedWidth, variableWidth}

  package separation:
    export aviation.timeFormats.separators.{dot, colon, french, none}

package hebdomads:
  export aviation.hebdomads.{european, northAmerican, jewish}
