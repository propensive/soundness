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
  . { am, AlexandrianCalendar, Anniversary, Apr, Aug, Base24, base24Extractable, Base60,
      base60Extractable, Calendar, Chronometry, Clock, Clockface, CopticCalendar, CopticMonth, Date,
      DateNumerics, DateSeparation, Day, Dec, dur,
      Disambiguation, Duration, Endianness, EthiopianCalendar, EthiopianMonth, Feb,
      FrenchRepublicanCalendar, FrenchRepublicanMonth, Frequency, Fri, Hebdomad, Holiday, Holidays,
      Horology,
      HebrewCalendar, HebrewMonth, Hour, IndianCalendar, IndianMonth, Instant, IslamicCalendar,
      GapPolicy, IslamicMonth, Iso8601, Jan, Jul, Jun, Leap, LeapMode, LeapSeconds, Mar, May,
      Meridiem, Minute, Moment, Mon, monotonic, Monotonic, Month, Months, Monthstamp, Nov, now,
      Occurrence, Oct, OffsetCalendar, OrdinalCalendar, Period, PersianCalendar, PersianMonth, pm,
      following, occurrences, Posix, rec, Recurrence, RecurrenceError, recInterpolator,
      RecurrenceLiteral, RecurrenceSet, Recurrent, Regime, Resolution, Rfc1123, RomanCalendar,
      Rrule, RruleError, Tai, until, WeekdayOrdinal, within,
      Sat, Sep, Sun, Thu, TimeError, TimeEvent, TimeFormat, TimeNumerics,
      TimeSeparation, TimeSpecificity, Timespan, Timestamp, TimestampError, Timezone, TimezoneError,
      today, ts, tsInterpolator, Tue, tz, Tzdb, TzdbError, Wed, Week, WeekDate, Weekday, Weekdays,
      WorkingDays, Year, Years }

package calendars:
  export aviation.calendars.{gregorianCalendar, julianCalendar, copticCalendar, ethiopianCalendar,
      islamicCalendar, persianCalendar, indianCalendar, hebrewCalendar, frenchRepublicanCalendar,
      buddhistCalendar, minguoCalendar, ordinalCalendar, papalCutover, britishCutover}

package nonexistentLeapDays:
  export aviation.calendars.nonexistentLeapDays.{raiseErrorsLeapDay, roundDownLeapDay,
      roundUpLeapDay}

package monthEnds:
  export aviation.monthEnds.{clampMonthEnd, overflowMonthEnd, raiseMonthEnd}

package gapPolicies:
  export aviation.gapPolicies.{pushBackward, rejectGap}

package leapModes:
  export aviation.leapModes.exact

package chronometries:
  export aviation.chronometries.{posix, atomic}

package dateFormats:
  export aviation.dateFormats.{americanDateFormat, europeanDateFormat, iso8601DateFormat,
      southEastAsiaDateFormat, unitedKingdomDateFormat}

package endianness:
  export aviation.dateFormats.endianness.{bigEndian, littleEndian, middleEndian}

package dateNumerics:
  export aviation.dateFormats.numerics.{fixedWidthDateNumerics, variableWidthDateNumerics}

package dateSeparators:
  export aviation.dateFormats.separators.{dotDateSeparator, hyphenDateSeparator, slashDateSeparator,
      spaceDateSeparator}

package yearFormats:
  export aviation.dateFormats.years.{fullYears, twoDigitsYears}

package weekdays:
  export
    aviation.dateFormats.weekdays
    . { englishWeekdays, englishShortWeekdays, oneLetterAmbiguousWeekdays,
        shortestUnambiguousWeekdays, twoLetterWeekdays, frenchWeekdays, germanWeekdays,
        spanishWeekdays }

package monthFormats:
  export
    aviation.dateFormats.months
    . { englishMonths, englishShortMonths, numericMonths, oneLetterAmbiguousMonths,
        twoDigitMonths, frenchMonths, germanMonths, spanishMonths }

package timeFormats:
  export
    aviation.timeFormats
    . { associatedPressTimeFormat, civilianTimeFormat, frenchTimeFormat, iso8601TimeFormat,
        ledgerTimeFormat, militaryTimeFormat, railwayTimeFormat }

package timespanFormats:
  export aviation.timespanFormats.{englishRelative, frenchRelative, germanRelative, spanishRelative}

package hourFormats:
  export aviation.timeFormats.hours.{twelveHourClock, twentyFourHourClock}

package meridiems:
  export aviation.timeFormats.meridiems.{lowerMeridiem, lowerPunctuatedMeridiem, upperMeridiem,
      upperPunctuatedMeridiem}

package timeNumerics:
  export aviation.timeFormats.numerics.{fixedWidthTimeNumerics, variableWidthTimeNumerics}

package timeSeparators:
  export aviation.timeFormats.separators.{colonTimeSeparator, dotTimeSeparator, frenchTimeSeparator,
      noneTimeSeparator}

package hebdomads:
  export aviation.hebdomads.{europeanHebdomad, jewishHebdomad, northAmericanHebdomad}

package instantDecodables:
  export aviation.instantDecodables.{iso8601InstantDecodable, rfc1123InstantDecodable}
