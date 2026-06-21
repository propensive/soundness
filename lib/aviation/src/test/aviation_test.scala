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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import abstractables.instantAbstractable

object Tests extends Suite(m"Aviation Tests"):
  def run(): Unit =
    suite(m"Parsing tests"):
      test(m"Parse a canonical date"):
        t"2011-12-13".decode[Date]
      .check(_ == 2011-Dec-13)

      test(m"Parse a date with a single-digit month"):
        t"2011-09-13".decode[Date]
      . assert(_ == 2011-Sep-13)

      test(m"Parse a date in the distant past"):
        t"59-09-13".decode[Date]
      . assert(_ == 59-Sep-13)

      test(m"Month cannot be higher than 12"):
        capture(t"59-13-13".decode[Date])
      . assert(_ == TimeError(_.Unknown(t"13", t"month")))

      test(m"Month cannot be less than 1"):
        capture(t"59-0-13".decode[Date])
      . assert(_ == TimeError(_.Unknown(t"0", t"month")))

      test(m"Day cannot be over 31"):
        capture(t"59-11-32".decode[Date])
      . assert(_ == TimeError(_.Invalid(59, 11, 32, calendars.gregorianCalendar)))

      test(m"Day must exist in month"):
        capture(t"59-11-31".decode[Date])
      . assert(_ == TimeError(_.Invalid(59, 11, 31, calendars.gregorianCalendar)))


    suite(m"Leap Seconds tests"):
      test(m"There are 10 leap seconds for any year before 1972"):
        LeapSeconds.during(Year(1900), false)
      . assert(_ == 10)

      test(m"There are 10 leap seconds in the first half of 1972"):
        LeapSeconds.during(Year(1972), false)
      . assert(_ == 10)

      test(m"There are 11 leap seconds in the second half of 1972"):
        LeapSeconds.during(Year(1972), true)
      . assert(_ == 11)

      test(m"There are 12 leap seconds in the first half of 1973"):
        LeapSeconds.during(Year(1973), false)
      . assert(_ == 12)

      test(m"There are 12 leap seconds in the second half of 1973"):
        LeapSeconds.during(Year(1973), true)
      . assert(_ == 12)

      test(m"There are 13 leap seconds in the first half of 1974"):
        LeapSeconds.during(Year(1974), false)
      . assert(_ == 13)

      test(m"There are 13 leap seconds in the second half of 1974"):
        LeapSeconds.during(Year(1974), true)
      . assert(_ == 13)

      test(m"There are 19 leap seconds in the first half of 1980"):
        LeapSeconds.during(Year(1980), false)
      . assert(_ == 19)

      test(m"There are 19 leap seconds in the first half of 1981"):
        LeapSeconds.during(Year(1981), false)
      . assert(_ == 19)

      test(m"There are 20 leap seconds in the second half of 1981"):
        LeapSeconds.during(Year(1981), true)
      . assert(_ == 20)

      test(m"There are 20 leap seconds in the first half of 1982"):
        LeapSeconds.during(Year(1982), false)
      . assert(_ == 20)

      test(m"There are 21 leap seconds in the second half of 1982"):
        LeapSeconds.during(Year(1982), true)
      . assert(_ == 21)

      test(m"There are 24 leap seconds in the second half of 1988"):
        LeapSeconds.during(Year(1988), true)
      . assert(_ == 24)

      test(m"There are 32 leap seconds in the first half of 2000"):
        LeapSeconds.during(Year(2000), false)
      . assert(_ == 32)

      test(m"There are 37 leap seconds in the first half of 2017"):
        LeapSeconds.during(Year(2017), false)
      . assert(_ == 37)

      test(m"There are 37 leap seconds in the first half of 2100"):
        LeapSeconds.during(Year(2100), false)
      . assert(_ == 37)

    suite(m"Gregorian Calendar Tests"):
      test(m"2000 is a leap year"):
        calendars.gregorianCalendar.leapYear(Year(2000))
      . assert(_ == true)

      test(m"1800, 1900, 2100, 2200 are not leap years"):
        List(Year(1800), Year(1900), Year(2100), Year(2200)).map(calendars.gregorianCalendar.leapYear)
      . assert(_.all(!_))

      test(m"Years not divisble by 4 are never leap years"):
        List(Year(1985), Year(200), Year(202), Year(1843)).map(calendars.gregorianCalendar.leapYear)
      . assert(_.all(!_))

      test(m"A valid literal date compiles"):
        demilitarize((2012-Mar-8).jdn)
      . assert(_.isEmpty)

      test(m"An impossible day-of-month literal fails to compile"):
        demilitarize((2012-Feb-30).jdn)
      . assert(_.nonEmpty)

      test(m"A 31st of a thirty-day-month literal fails to compile"):
        demilitarize((2012-Apr-31).jdn)
      . assert(_.nonEmpty)

      test(m"A non-leap-year 29 February literal fails to compile"):
        demilitarize((2011-Feb-29).jdn)
      . assert(_.nonEmpty)

      test(m"A leap-year 29 February literal compiles"):
        demilitarize((2012-Feb-29).jdn)
      . assert(_.isEmpty)

      test(m"A zero day-of-month literal fails to compile"):
        demilitarize((2012-Mar-0).jdn)
      . assert(_.nonEmpty)

      test(m"A literal date equals its `ts` string counterpart"):
        (2012-Mar-8)
      . assert(_ == ts"2012-03-08")

      test(m"1900-Feb-29 fails to compile under the default Gregorian calendar"):
        demilitarize((1900-Feb-29).jdn)
      . assert(_.nonEmpty)

      test(m"1900-Feb-29 compiles under a contextual Julian calendar"):
        demilitarize:
          import calendars.julianCalendar
          (1900-Feb-29).jdn
      . assert(_.isEmpty)

      test(m"A contextual Julian calendar yields the Julian Julian-day for the literal"):
        given julian: RomanCalendar = calendars.julianCalendar
        (1900-Feb-29).jdn
      . assert(_ == unsafely(calendars.julianCalendar.jdn(Year(1900), Feb, Day(29))).jdn)

      test(m"Check recent Julian Day"):
        (2022-Dec-16).jdn
      . assert(_ == 2459930)

      test(m"Check Julian Day in 1950"):
        (1950-Mar-10).jdn
      . assert(_ == 2433351)

      test(m"Check Julian Day in 1650"):
        (1650-Mar-10).jdn
      . assert(_ == 2323779)

      test(m"Check Julian Day in Year 1582"):
        (1582-Oct-15).jdn
      . assert(_ == 2299161)

      test(m"Check Julian Day in Year 1600"):
        (1600-Jan-1).jdn
      . assert(_ == 2305448)

      test(m"Check Julian Day in Year 1"):
        (1-Jan-1).jdn
      . assert(_ == 1721426)

      test(m"Get zeroth day of year"):
        (2010-Jan-1).jdn -> (calendars.gregorianCalendar.zerothDayOfYear(Year(2010)).jdn + 1)
      . assert(_ == _)

      test(m"Get days in non-leap-year"):
        calendars.gregorianCalendar.daysInYear(Year(1995))
      . assert(_ == 365)

      test(m"Get days in leap-year"):
        calendars.gregorianCalendar.daysInYear(Year(1996))
      . assert(_ == 366)

      test(m"Get Year from Date"):
        given calendar: Calendar = calendars.gregorianCalendar
        val date = 2016-Jul-11
        calendar.annual(date)()
      . assert(_ == 2016)

      test(m"Check Gregorian date"):
        given calendar: Calendar = calendars.gregorianCalendar
        2016-Apr-11
      . assert(_ == 2016-Apr-11)

      test(m"Subtract dates"):
        2018-Nov-19 - (2017-Sep-1)
      . assert(_ == (Quanta(444): Quanta[Days[1]]))

      test(m"Subtract days from a date"):
        2018-Nov-19 - (Quanta(2): Quanta[Days[1]])
      . assert(_ == 2018-Nov-17)

      test(m"Add days to a date"):
        2018-Nov-19 + (Quanta(2): Quanta[Days[1]])
      . assert(_ == 2018-Nov-21)

      test(m"Add days to a date, order reversed"):
        (Quanta(2): Quanta[Days[1]]) + (2018-Nov-19)
      . assert(_ == 2018-Nov-21)

      test(m"Get Gregorian date"):
        given calendar: Calendar = calendars.gregorianCalendar
        val date = 2016-Jul-11
        (date.year, date.month, date.day)
      . assert(_ == (2016, Jul, 11))

      test(m"Specify times"):
        2.01.am
      . assert(_ == Clockface(2, 1, 0))

      test(m"Specify times 2"):
        2.59.am
      . assert(_ == Clockface(2, 59, 0))

      test(m"Specify times 3"):
        11.40.am
      . assert(_ == Clockface(11, 40, 0))

      test(m"Specify times 4"):
        7.25.pm
      . assert(_ == Clockface(19, 25, 0))

      test(m"Time with too many minutes is invalid"):
        demilitarize(7.88.am)
      . assert(_.nonEmpty)

      test(m"Time with too few minute digits is invalid"):
        demilitarize(7.8.am)
      . assert(_.nonEmpty)

      test(m"Time with too many minute digits is invalid"):
        demilitarize(7.585.am)
      . assert(_.nonEmpty)

      test(m"Specify afternoon time with hour `12`"):
        12.25.pm
      . assert(_ == Clockface(12, 25, 0))

      test(m"Specify morning time with hour `12`"):
        12.25.am
      . assert(_ == Clockface(0, 25, 0))

      import calendars.gregorianCalendar

      test(m"Specify datetime"):
        5.25.pm on 2018-Aug-11
      . assert(_ == Timestamp(Date(Year(2018), Aug, Day(11)), Clockface(17, 25, 0)))

      // test(m"Read TZDB file"):
      //   Tzdb.parseFile(t"europe")
      // .assert(_ == List())

    suite(m"Regime / calendar cutover tests"):
      test(m"Papal cutover accepts the last Julian date"):
        given papal: Regime = calendars.papalCutover
        (1582-Oct-4).jdn
      . assert(_ == 2299161)

      test(m"Papal cutover accepts the first Gregorian date"):
        given papal: Regime = calendars.papalCutover
        (1582-Oct-15).jdn
      . assert(_ == 2299161)

      test(m"The last Julian day and first Gregorian day are the same day"):
        given papal: Regime = calendars.papalCutover
        (1582-Oct-15).jdn == (1582-Oct-4).jdn
      . assert(_ == true)

      test(m"Papal cutover rejects a gap date at compile time"):
        demilitarize:
          import calendars.papalCutover
          (1582-Oct-10).jdn
      . assert(_.nonEmpty)

      test(m"Papal cutover rejects a gap date at runtime"):
        import calendars.papalCutover
        val day = 10
        capture((1582-Oct-day).jdn)
      . assert(_ == TimeError(_.Invalid(1582, 10, 10, calendars.papalCutover)))

      test(m"A pre-cutover date uses the Julian calendar under the Papal regime"):
        given papal: Regime = calendars.papalCutover
        (1500-Mar-10).jdn
      . assert(_ == unsafely(calendars.julianCalendar.jdn(Year(1500), Mar, Day(10))).jdn)

      test(m"A post-cutover date uses the Gregorian calendar under the Papal regime"):
        given papal: Regime = calendars.papalCutover
        (1700-Mar-10).jdn
      . assert(_ == unsafely(calendars.gregorianCalendar.jdn(Year(1700), Mar, Day(10))).jdn)

      test(m"British cutover rejects a date in its gap at compile time"):
        demilitarize:
          import calendars.britishCutover
          (1752-Sep-5).jdn
      . assert(_.nonEmpty)

      test(m"British cutover accepts the day after its gap"):
        given british: Regime = calendars.britishCutover
        (1752-Sep-14).jdn
      . assert(_ == 2361222)

    suite(m"Decoding instants"):
      suite(m"ISO 8601"):
        import instantDecodables.iso8601InstantDecodable
        test(m"with Z suffix (UTC)"):
          t"1994-11-06T08:49:37Z".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"with positive timezone offset"):
          t"1994-11-06T09:49:37+01:00".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"with negative timezone offset"):
          t"1994-11-06T03:49:37-05:00".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"with fractional seconds (.123)"):
          t"2020-02-29T12:34:56.123Z".decode[Instant]
        . assert(_ == Instant(1582979696123L))

        test(m"with nanosecond precision (.123456789)"):
          t"2020-02-29T12:34:56.123456789Z".decode[Instant]
        . assert(_ == Instant(1582979696123L))

        test(m"date-only format (midnight UTC)"):
          t"2020-12-31".decode[Instant]
        . assert(_ == Instant(1609372800000L))

        test(m"ISO 8601 leap second accepted as next second"):
          t"2016-12-31T23:59:60Z".decode[Instant]
        . assert(_ == Instant(1483228800000L))

        test(m"Month-only format"):
          t"2012-11".decode[Instant]
        . assert(_ == Instant(1351728000000L))

        test(m"ISO 8601 with timezone offset and fractional seconds"):
          t"2023-03-25T10:15:30.456+02:00".decode[Instant]
        . assert(_ == Instant(1679732130456L))

        test(m"Calendar date, full time, Z"):
          t"2023-05-28T14:30:59Z".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Calendar date, basic format, full time, Z"):
          t"20230528T143059Z".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Calendar date, full time, offset +00:00"):
          t"2023-05-28T14:30:59+00:00".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Calendar date, full time, offset +02:00"):
          t"2023-05-28T14:30:59+02:00".decode[Instant]
        . assert(_ == Instant(1685277059000L))

        test(m"Calendar date, full time, offset -05:00"):
          t"2023-05-28T14:30:59-05:00".decode[Instant]
        . assert(_ == Instant(1685302259000L))

        test(m"Calendar date, fractional seconds, Z"):
          t"2023-05-28T14:30:59.123Z".decode[Instant]
        . assert(_ == Instant(1685284259123L))

        test(m"Calendar date, comma as decimal separator, Z"):
          t"2023-05-28T14:30:59,123Z".decode[Instant]
        . assert(_ == Instant(1685284259123L))

        test(m"Calendar date, hours and minutes, Z"):
          t"2023-05-28T14:30Z".decode[Instant]
        . assert(_ == Instant(1685284200000L))

        test(m"Calendar date, hour only, Z"):
          t"2023-05-28T14Z".decode[Instant]
        . assert(_ == Instant(1685282400000L))

        test(m"Week date, Sunday of week 21, full time, Z"):
          t"2023-W21-7T14:30:59Z".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Week date, Monday of week 21, full time, Z"):
          t"2023-W21-1T14:30:59Z".decode[Instant]
        . assert(_ == Instant(1684765859000L))

        test(m"Week date, basic format, Sunday of week 21, full time, Z"):
          t"2023W217T143059Z".decode[Instant]
        . assert(_ == Instant(1685284259000L))

        test(m"Start of UNIX epoch"):
          t"1970-01-01T00:00:00Z".decode[Instant]
        . assert(_ == Instant(0L))

        test(m"Leap day 2020"):
          t"2020-02-29T12:00:00Z".decode[Instant]
        . assert(_ == Instant(1582977600000L))

        test(m"End of 1999"):
          t"1999-12-31T23:59:59Z".decode[Instant]
        . assert(_ == Instant(946684799000L))

        test(m"Start of 2000"):
          t"2000-01-01T00:00:00Z".decode[Instant]
        . assert(_ == Instant(946684800000L))

        test(m"Far future 2199"):
          t"2199-12-31T23:59:59Z".decode[Instant]
        . assert(_ == Instant(7258118399000L))

        test(m"Far past 1800"):
          t"1800-01-01T00:00:00Z".decode[Instant]
        . assert(_ == Instant(-5364662400000L))

        test(m"Middle of the day"):
          t"2023-06-15T12:00:00Z".decode[Instant]
        . assert(_ == Instant(1686830400000L))

        test(m"New Year 2023"):
          t"2023-01-01T00:00:00Z".decode[Instant]
        . assert(_ == Instant(1672531200000L))

        test(m"DST change irrelevant (UTC)"):
          t"2021-03-28T01:30:00Z".decode[Instant]
        . assert(_ == Instant(1616895000000L))

        test(m"Millisecond rounding test"):
          t"2022-11-15T23:59:59Z".decode[Instant]
        . assert(_ == Instant(1668556799000L))

      suite(m"RFC 1123"):
        import instantDecodables.rfc1123InstantDecodable
        test(m"basic with GMT timezone"):
          t"Sun, 06 Nov 1994 08:49:37 GMT".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"with unusual day of week (consistency check)"):
          t"Tue, 01 Jan 2019 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1546300800000L))

        test(m"Standard RFC1123 date with full weekday and month names"):
          t"Sun, 06 Nov 1994 08:49:37 GMT".decode[Instant]
        . assert(_ == Instant(784111777000L))

        test(m"Leap day in a leap year"):
          t"Mon, 29 Feb 2016 12:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1456747200000L))

        test(m"Epoch start date"):
          t"Thu, 01 Jan 1970 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(0L))

        test(m"Last second before Y2K"):
          t"Fri, 31 Dec 1999 23:59:59 GMT".decode[Instant]
        . assert(_ == Instant(946684799000L))

        test(m"First second of Y2K"):
          t"Sat, 01 Jan 2000 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(946684800000L))

        test(m"Date with single-digit day"):
          capture(t"Wed, 3 Jul 2002 17:45:00 GMT".decode[Instant])
        . assert(_ == TimeError(_.Format(t"Wed, 3 Jul 2002 17:45:00 GMT", Rfc1123, Prim + 6)(Rfc1123.Issue.Digit)))

        test(m"Date with single-digit hour, minute, and second"):
          t"Tue, 02 Mar 2021 07:08:09 GMT".decode[Instant]
        . assert(_ == Instant(1614668889000L))

        test(m"Date with correct day of week (Monday)"):
          t"Mon, 15 Aug 2022 18:30:00 GMT".decode[Instant]
        . assert(_ == Instant(1660588200000L))

        test(m"Incorrect weekday (should be Sunday, not Monday)"):
          t"Mon, 31 Dec 2006 23:59:59 GMT".decode[Instant]
        . assert(_ == Instant(1167609599000L))

        test(m"Date around DST end (should be in UTC, so no DST effect)"):
          t"Sun, 01 Nov 2020 01:30:00 GMT".decode[Instant]
        . assert(_ == Instant(1604194200000L))

        test(m"Far future date (2100)"):
          t"Fri, 01 Jan 2100 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(4102444800000L))

        test(m"Far past date (1900)"):
          t"Mon, 01 Jan 1900 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(-2208988800000L))

        test(m"Time with 2-digit hour 23"):
          t"Wed, 08 Sep 2021 23:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1631142000000L))

        test(m"Midday exactly (12:00:00)"):
          t"Sat, 25 Dec 2021 12:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1640433600000L))

        test(m"New Year's Eve (2022)"):
          t"Sat, 31 Dec 2022 23:59:59 GMT".decode[Instant]
        . assert(_ == Instant(1672531199000L))

        test(m"Short month name test (Jan)"):
          t"Sun, 01 Jan 2023 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1672531200000L))

        test(m"Minimum valid time (00:00:00)"):
          t"Sun, 10 Oct 2021 00:00:00 GMT".decode[Instant]
        . assert(_ == Instant(1633824000000L))

      suite(m"Working days tests"):
        suite(m"Counting days")
          test(m"Count the Saturdays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Sat)
          . assert(_ == 5)

          test(m"Count the Sundays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Sun)
          . assert(_ == 5)

          test(m"Count the Mondays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Mon)
          . assert(_ == 5)

          test(m"Count the Tuesdays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Tue)
          . assert(_ == 4)

          test(m"Count the Wednesdays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Wed)
          . assert(_ == 4)

          test(m"Count the Thursdays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Thu)
          . assert(_ == 4)

          test(m"Count the Fridays in March"):
            Weekday.count(2025-Mar-1, 2025-Apr-1, Fri)
          . assert(_ == 4)

          test(m"Count the Saturdays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Sat)
          . assert(_ == 4)

          test(m"Count the Sundays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Sun)
          . assert(_ == 4 )

          test(m"Count the Mondays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Mon)
          . assert(_ == 4)

          test(m"Count the Tuesdays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Tue)
          . assert(_ == 5)

          test(m"Count the Wednesdays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Wed)
          . assert(_ == 5)

          test(m"Count the Thursdays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Thu)
          . assert(_ == 4)

          test(m"Count the Fridays in April"):
            Weekday.count(2025-Apr-1, 2025-May-1, Fri)
          . assert(_ == 4)

          test(m"Count the Saturdays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Sat)
          . assert(_ == 5)

          test(m"Count the Sundays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Sun)
          . assert(_ == 4)

          test(m"Count the Mondays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Mon)
          . assert(_ == 4)

          test(m"Count the Tuesdays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Tue)
          . assert(_ == 4)

          test(m"Count the Wednesdays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Wed)
          . assert(_ == 4)

          test(m"Count the Thursdays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Thu)
          . assert(_ == 5)

          test(m"Count the Fridays in May"):
            Weekday.count(2025-May-1, 2025-Jun-1, Fri)
          . assert(_ == 5)

        suite(m"UK holidays example"):
          given Holidays = Holidays(List
            ( Holiday(2025-Jan-1, t"New Year's Day"),
              Holiday(2025-Apr-21, t"Good Friday"),
              Holiday(2025-May-5, t"Early May Bank Holiday"),
              Holiday(2025-May-26, t"Spring Bank Holiday"),
              Holiday(2025-Aug-25, t"Summer Bank Holiday"),
              Holiday(2025-Dec-25, t"Christmas Day"),
              Holiday(2025-Dec-26, t"Boxing Day")) )

          test(m"Check the next working day after Monday is Tuesday"):
            import hebdomads.europeanHebdomad
            2025-Aug-18 + WorkingDays(1)
          . assert(_ == 2025-Aug-19)

          test(m"Check two working days after a Monday is Wednesday"):
            import hebdomads.europeanHebdomad
            2025-Aug-18 + WorkingDays(2)
          . assert(_ == 2025-Aug-20)

          test(m"Check four working days after a Monday is Friday"):
            import hebdomads.europeanHebdomad
            2025-Aug-18 + WorkingDays(4)
          . assert(_ == 2025-Aug-22)

          test(m"Check five working days after a Monday is Monday"):
            import hebdomads.europeanHebdomad
            2025-Aug-11 + WorkingDays(5)
          . assert(_ == 2025-Aug-18)

          test(m"Check one working days after a Friday is Monday"):
            import hebdomads.europeanHebdomad
            2025-Apr-11 + WorkingDays(1)
          . assert(_ == 2025-Apr-14)

          test(m"Check the working day after Christmas Eve"):
            import hebdomads.europeanHebdomad
            2025-Dec-24 + WorkingDays(1)
          . assert(_ == 2025-Dec-29)

          test(m"Check the working day after Maundy Thursday is Easter Monday"):
            import hebdomads.europeanHebdomad
            2025-Apr-18 + WorkingDays(1)
          . assert(_ == 2025-Apr-22)

          test(m"Working day after Good Friday is Easter Tuesday"):
            import hebdomads.europeanHebdomad
            2025-Apr-19 + WorkingDays(1)
          . assert(_ == 2025-Apr-23)

          test(m"Check there are 253 working days in a year"):
            import hebdomads.europeanHebdomad
            2025-Jan-3 + WorkingDays(253)
          . assert(_ == 2026-Jan-1)

    suite(m"Hebdomad implementations"):
      suite(m"European hebdomad"):
        import hebdomads.europeanHebdomad

        test(m"starts on Monday"):
          summon[Hebdomad].start
        . assert(_ == Mon)

        test(m"Saturday is a weekend day"):
          Sat.weekend
        . assert(_ == true)

        test(m"Sunday is a weekend day"):
          Sun.weekend
        . assert(_ == true)

        test(m"Friday is not a weekend day"):
          Fri.weekend
        . assert(_ == false)

        test(m"Monday is the first weekday (ordinal 0)"):
          Mon.number.n0
        . assert(_ == 0)

        test(m"Sunday is the seventh weekday (ordinal 6)"):
          Sun.number.n0
        . assert(_ == 6)

      suite(m"North American hebdomad"):
        import hebdomads.northAmericanHebdomad

        test(m"starts on Sunday"):
          summon[Hebdomad].start
        . assert(_ == Sun)

        test(m"Saturday is a weekend day"):
          Sat.weekend
        . assert(_ == true)

        test(m"Sunday is the first weekday (ordinal 0)"):
          Sun.number.n0
        . assert(_ == 0)

        test(m"Monday is the second weekday (ordinal 1)"):
          Mon.number.n0
        . assert(_ == 1)

      suite(m"Jewish hebdomad"):
        import hebdomads.jewishHebdomad

        test(m"starts on Sunday"):
          summon[Hebdomad].start
        . assert(_ == Sun)

        test(m"Friday is a weekend day"):
          Fri.weekend
        . assert(_ == true)

        test(m"Saturday is a weekend day"):
          Sat.weekend
        . assert(_ == true)

        test(m"Sunday is not a weekend day"):
          Sun.weekend
        . assert(_ == false)

    suite(m"Year operations"):
      test(m"Year addition"):
        Year(2024) + 5
      . assert(_ == Year(2029))

      test(m"Year subtraction"):
        Year(2024) - 100
      . assert(_ == Year(1924))

      test(m"Year ordering: less than"):
        Year(1999) < Year(2000)
      . assert(_ == true)

      test(m"Year ordering: greater than"):
        Year(2024) > Year(2023)
      . assert(_ == true)

      test(m"Year ordering: equal"):
        Year(2024) == Year(2024)
      . assert(_ == true)

      test(m"Year decode from text"):
        t"2024".decode[Year]
      . assert(_ == Year(2024))

    suite(m"Month operations"):
      test(m"Month from text: Jan"):
        Month(t"Jan")
      . assert(_ == Jan)

      test(m"Month from text: Dec"):
        Month(t"Dec")
      . assert(_ == Dec)

      test(m"Month from invalid text"):
        capture(Month(t"foo"))
      . assert(_ == TimeError(_.Unknown(t"foo", t"month")))

      test(m"Month from invalid number 0"):
        capture(Month(0))
      . assert(_ == TimeError(_.Unknown(t"0", t"month")))

      test(m"Month from invalid number 13"):
        capture(Month(13))
      . assert(_ == TimeError(_.Unknown(t"13", t"month")))

      test(m"Month.unapply on text matches"):
        t"jan" match
          case Month(m) => m
          case _        => Jan
      . assert(_ == Jan)

      test(m"Month.unapply on int matches"):
        7 match
          case Month(m) => m
          case _        => Jan
      . assert(_ == Jul)

      test(m"Month.unapply rejects invalid int"):
        13 match
          case Month(m) => true
          case _        => false
      . assert(_ == false)

      test(m"Jan.numerical"):
        Jan.numerical
      . assert(_ == 1)

      test(m"Dec.numerical"):
        Dec.numerical
      . assert(_ == 12)

      test(m"Mar.offset(false) == 59"):
        Mar.offset(false)
      . assert(_ == 59)

      test(m"Mar.offset(true) == 60"):
        Mar.offset(true)
      . assert(_ == 60)

      test(m"Jan.offset(true) == 0"):
        Jan.offset(true)
      . assert(_ == 0)

      test(m"Feb.offset(true) == 31"):
        Feb.offset(true)
      . assert(_ == 31)

      test(m"Month.all has 12 entries"):
        Month.all.length
      . assert(_ == 12)

    suite(m"AM/PM literal corners"):
      test(m"12.00.am is midnight"):
        12.00.am
      . assert(_ == Clockface(0, 0, 0))

      test(m"12.00.pm is noon"):
        12.00.pm
      . assert(_ == Clockface(12, 0, 0))

      test(m"0.00.am is midnight"):
        0.00.am
      . assert(_ == Clockface(0, 0, 0))

      test(m"13.00.am is a compile error"):
        demilitarize(13.00.am)
      . assert(_.nonEmpty)

      test(m"hour above 12 is a compile error"):
        demilitarize(15.30.pm)
      . assert(_.nonEmpty)

    suite(m"Anniversary"):
      import calendars.gregorianCalendar

      test(m"Mar - 14 constructs an Anniversary"):
        val ann: Anniversary = Mar - 14
        (ann.month, ann.day())
      . assert(_ == (Mar, 14))

      test(m"Anniversary applied to a year produces a Date"):
        given Anniversary.NonexistentLeapDay = nonexistentLeapDays.roundDownLeapDay
        (Mar - 14)(Year(2024))
      . assert(_ == 2024-Mar-14)

      test(m"Feb 29 in non-leap year roundDown gives Feb 28"):
        given Anniversary.NonexistentLeapDay = nonexistentLeapDays.roundDownLeapDay
        (Feb - 29)(Year(2023))
      . assert(_ == 2023-Feb-28)

      test(m"Feb 29 in non-leap year roundUp gives Mar 1"):
        given Anniversary.NonexistentLeapDay = nonexistentLeapDays.roundUpLeapDay
        (Feb - 29)(Year(2023))
      . assert(_ == 2023-Mar-1)

      test(m"Feb 29 in non-leap year raiseErrors raises TimeError"):
        given Anniversary.NonexistentLeapDay = nonexistentLeapDays.raiseErrorsLeapDay
        capture((Feb - 29)(Year(2023)))
      . matches:
          case TimeError(_) =>

      test(m"Feb 29 in a leap year is unaffected by rounding strategy"):
        given Anniversary.NonexistentLeapDay = nonexistentLeapDays.roundDownLeapDay
        (Feb - 29)(Year(2024))
      . assert(_ == 2024-Feb-29)

      test(m"date.anniversary extracts month and day"):
        val ann = (2025-Mar-14).anniversary
        (ann.month, ann.day())
      . assert(_ == (Mar, 14))

      test(m"Anniversary Showable, big-endian dot separator"):
        import endianness.bigEndian, dateSeparators.dotDateSeparator
        import monthFormats.englishShortMonths
        (Mar - 14).show
      . assert(_ == t"Mar.14")

      test(m"Anniversary Showable, little-endian dot separator"):
        import endianness.littleEndian, dateSeparators.dotDateSeparator
        import monthFormats.englishShortMonths
        (Mar - 14).show
      . assert(_ == t"14.Mar")

    suite(m"Monthstamp"):
      import calendars.gregorianCalendar

      test(m"2024 - Jan constructs a Monthstamp"):
        val ms: Monthstamp = 2024 - Jan
        (ms.year(), ms.month)
      . assert(_ == (2024, Jan))

      test(m"Monthstamp - day produces a Date"):
        (2024 - Jan) - 15
      . assert(_ == 2024-Jan-15)

      test(m"Monthstamp - day for end of month"):
        (2024 - Feb) - 29
      . assert(_ == 2024-Feb-29)

      test(m"date.monthstamp extracts year and month"):
        val ms = (2025-Mar-14).monthstamp
        (ms.year(), ms.month)
      . assert(_ == (2025, Mar))

      test(m"Monthstamp Showable, big-endian dot separator"):
        import endianness.bigEndian, dateSeparators.dotDateSeparator, yearFormats.fullYears
        import monthFormats.englishShortMonths
        (2024 - Mar).show
      . assert(_ == t"Mar.2024")

      test(m"Monthstamp Showable, little-endian"):
        import endianness.littleEndian, dateSeparators.dotDateSeparator, yearFormats.fullYears
        import monthFormats.englishShortMonths
        (2024 - Mar).show
      . assert(_ == t"2024.Mar")

    suite(m"Holidays methods"):
      val holidays = Holidays(List
        ( Holiday(2025-Jan-1, t"New Year's Day"),
          Holiday(2025-Apr-21, t"Good Friday"),
          Holiday(2025-Dec-25, t"Christmas Day"),
          Holiday(2025-Dec-26, t"Boxing Day") ))

      test(m"holiday returns the matching Holiday"):
        holidays.holiday(2025-Dec-25).let(_.name).or(t"")
      . assert(_ == t"Christmas Day")

      test(m"holiday on a non-holiday returns Unset"):
        holidays.holiday(2025-Mar-15).absent
      . assert(_ == true)

      test(m"workDay is false on a holiday"):
        holidays.workDay(2025-Jan-1)
      . assert(_ == false)

      test(m"workDay is true on a normal day"):
        holidays.workDay(2025-Mar-15)
      . assert(_ == true)

      test(m"between returns holidays in [start, end)"):
        holidays.between(2025-Apr-1, 2025-Dec-25).map(_.date)
      . assert(_ == List(2025-Apr-21))

      test(m"between excludes the end date"):
        holidays.between(2025-Dec-25, 2025-Dec-26).map(_.date)
      . assert(_ == List(2025-Dec-25))

      test(m"between with empty range"):
        holidays.between(2025-Mar-1, 2025-Apr-1)
      . assert(_ == List())

      test(m"between returns sorted results"):
        holidays.between(2025-Jan-1, 2025-Dec-31).map(_.date)
      . assert(_ == List(2025-Jan-1, 2025-Apr-21, 2025-Dec-25, 2025-Dec-26))

    suite(m"Date Showable formats"):
      val date = 2025-Apr-7

      test(m"european format"):
        import dateFormats.europeanDateFormat
        date.show
      . assert(_ == t"07.04.2025")

      test(m"american format"):
        import dateFormats.americanDateFormat
        date.show
      . assert(_ == t"04/07/2025")

      test(m"unitedKingdom format"):
        import dateFormats.unitedKingdomDateFormat
        date.show
      . assert(_ == t"07/04/2025")

      test(m"southEastAsia format"):
        import dateFormats.southEastAsiaDateFormat
        date.show
      . assert(_ == t"07-04-2025")

      test(m"iso8601 format"):
        import dateFormats.iso8601DateFormat
        date.show
      . assert(_ == t"2025-04-07")

      test(m"two-digit year variant"):
        import endianness.bigEndian
        import dateNumerics.fixedWidthDateNumerics
        import dateSeparators.hyphenDateSeparator
        import yearFormats.twoDigitsYears
        date.show
      . assert(_ == t"25-04-07")

      test(m"variable width single-digit month"):
        import endianness.bigEndian
        import dateNumerics.variableWidthDateNumerics
        import dateSeparators.hyphenDateSeparator
        import yearFormats.fullYears
        date.show
      . assert(_ == t"2025-4-7")

    suite(m"Clockface Showable formats"):
      val time = Clockface(14, 30, 59)
      val noon = Clockface(12, 0, 0)
      val midnight = Clockface(0, 0, 0)

      test(m"military format"):
        import timeFormats.militaryTimeFormat
        time.show
      . assert(_ == t"1430")

      test(m"civilian format at 14:30"):
        import timeFormats.civilianTimeFormat
        time.show
      . assert(_ == t"02:30 PM")

      test(m"civilian format at noon"):
        import timeFormats.civilianTimeFormat
        noon.show
      . assert(_ == t"12:00 PM")

      test(m"civilian format at midnight"):
        import timeFormats.civilianTimeFormat
        midnight.show
      . assert(_ == t"12:00 AM")

      test(m"associatedPress format"):
        import timeFormats.associatedPressTimeFormat
        time.show
      . assert(_ == t"2:30 p.m.")

      test(m"french format"):
        import timeFormats.frenchTimeFormat
        time.show
      . assert(_ == t"14h30")

      test(m"iso8601 time format"):
        import timeFormats.iso8601TimeFormat
        time.show
      . assert(_ == t"14:30:59")

      test(m"ledger format"):
        import timeFormats.ledgerTimeFormat
        time.show
      . assert(_ == t"14.30")

      test(m"railway format"):
        import timeFormats.railwayTimeFormat
        time.show
      . assert(_ == t"14:30")

    suite(m"Weekday name formatters"):

      test(m"english full names: Mon"):
        weekdays.englishWeekdays.name(Mon)
      . assert(_ == t"Monday")

      test(m"english full names: Sun"):
        weekdays.englishWeekdays.name(Sun)
      . assert(_ == t"Sunday")

      test(m"englishShort: Mon"):
        weekdays.englishShortWeekdays.name(Mon)
      . assert(_ == t"Mon")

      test(m"englishShort: Sun"):
        weekdays.englishShortWeekdays.name(Sun)
      . assert(_ == t"Sun")

      test(m"oneLetterAmbiguous: Mon"):
        weekdays.oneLetterAmbiguousWeekdays.name(Mon)
      . assert(_ == t"M")

      test(m"oneLetterAmbiguous: Tue"):
        weekdays.oneLetterAmbiguousWeekdays.name(Tue)
      . assert(_ == t"T")

      test(m"shortestUnambiguous: Tue"):
        weekdays.shortestUnambiguousWeekdays.name(Tue)
      . assert(_ == t"Tu")

      test(m"shortestUnambiguous: Thu"):
        weekdays.shortestUnambiguousWeekdays.name(Thu)
      . assert(_ == t"Th")

      test(m"twoLetter: Mon"):
        weekdays.twoLetterWeekdays.name(Mon)
      . assert(_ == t"Mo")

      test(m"twoLetter: Sat"):
        weekdays.twoLetterWeekdays.name(Sat)
      . assert(_ == t"Sa")

    suite(m"Month name formatters"):

      test(m"english full names: Jan"):
        monthFormats.englishMonths.name(Jan)
      . assert(_ == t"January")

      test(m"english full names: Sep"):
        monthFormats.englishMonths.name(Sep)
      . assert(_ == t"September")

      test(m"englishShort: Jan"):
        monthFormats.englishShortMonths.name(Jan)
      . assert(_ == t"Jan")

      test(m"englishShort: Sep"):
        monthFormats.englishShortMonths.name(Sep)
      . assert(_ == t"Sep")

      test(m"oneLetterAmbiguous: Jan"):
        monthFormats.oneLetterAmbiguousMonths.name(Jan)
      . assert(_ == t"J")

      test(m"oneLetterAmbiguous: Mar"):
        monthFormats.oneLetterAmbiguousMonths.name(Mar)
      . assert(_ == t"M")

      test(m"numeric: Jan"):
        monthFormats.numericMonths.name(Jan)
      . assert(_ == t"1")

      test(m"numeric: Dec"):
        monthFormats.numericMonths.name(Dec)
      . assert(_ == t"12")

      test(m"twoDigit: Jan"):
        monthFormats.twoDigitMonths.name(Jan)
      . assert(_ == t"01")

      test(m"twoDigit: Dec"):
        monthFormats.twoDigitMonths.name(Dec)
      . assert(_ == t"12")

    suite(m"Meridiem formatters"):

      test(m"upper Am"):
        meridiems.upperMeridiem.text(Meridiem.Am)
      . assert(_ == t"AM")

      test(m"upper Pm"):
        meridiems.upperMeridiem.text(Meridiem.Pm)
      . assert(_ == t"PM")

      test(m"lower Am"):
        meridiems.lowerMeridiem.text(Meridiem.Am)
      . assert(_ == t"am")

      test(m"lower Pm"):
        meridiems.lowerMeridiem.text(Meridiem.Pm)
      . assert(_ == t"pm")

      test(m"upperPunctuated Am"):
        meridiems.upperPunctuatedMeridiem.text(Meridiem.Am)
      . assert(_ == t"A.M.")

      test(m"lowerPunctuated Pm"):
        meridiems.lowerPunctuatedMeridiem.text(Meridiem.Pm)
      . assert(_ == t"p.m.")

    suite(m"Date arithmetic edge cases"):
      import calendars.gregorianCalendar

      test(m"Subtract a date from itself yields zero"):
        2024-Jul-15 - (2024-Jul-15)
      . assert(_ == (Quanta(0): Quanta[Days[1]]))

      test(m"Cross-year date difference"):
        2025-Jan-1 - (2024-Jan-1)
      . assert(_ == (Quanta(366): Quanta[Days[1]]))

      test(m"Date less-than"):
        (2024-Jan-1) < (2024-Jan-2)
      . assert(_ == true)

      test(m"Date greater-than"):
        (2024-Jan-2) > (2024-Jan-1)
      . assert(_ == true)

      test(m"Date less-than-or-equal (equal)"):
        (2024-Jan-1) <= (2024-Jan-1)
      . assert(_ == true)

      test(m"Date greater-than-or-equal (equal)"):
        (2024-Jan-1) >= (2024-Jan-1)
      . assert(_ == true)

      test(m"Date.addDays positive"):
        (2024-Jan-1).addDays(31)
      . assert(_ == 2024-Feb-1)

      test(m"Date.addDays negative"):
        (2024-Feb-1).addDays(-31)
      . assert(_ == 2024-Jan-1)

      test(m"yearDay for Jan 1"):
        (2024-Jan-1).yearDay
      . assert(_ == 1)

      test(m"yearDay for Dec 31 in leap year"):
        (2024-Dec-31).yearDay
      . assert(_ == 366)

      test(m"yearDay for Mar 1 in leap year"):
        (2024-Mar-1).yearDay
      . assert(_ == 61)

      test(m"yearDay for Mar 1 in non-leap year"):
        (2023-Mar-1).yearDay
      . assert(_ == 60)

      test(m"weekday of 2024-Jan-1 is Monday"):
        (2024-Jan-1).weekday
      . assert(_ == Mon)

      test(m"weekday of 2025-Mar-15 is Saturday"):
        (2025-Mar-15).weekday
      . assert(_ == Sat)

      test(m"date is on weekend"):
        import hebdomads.europeanHebdomad
        (2025-Mar-15).weekend
      . assert(_ == true)

      test(m"date is not on weekend"):
        import hebdomads.europeanHebdomad
        (2025-Mar-17).weekend
      . assert(_ == false)

    suite(m"WorkingDays edge cases"):
      given Holidays = Holidays(List
        ( Holiday(2025-Jan-1, t"New Year's Day"),
          Holiday(2025-Dec-25, t"Christmas Day"),
          Holiday(2025-Dec-26, t"Boxing Day") ))

      test(m"WorkingDays(0) on a Saturday bumps to Monday"):
        import hebdomads.europeanHebdomad
        2025-Mar-15 + WorkingDays(0)
      . assert(_ == 2025-Mar-17)

      test(m"WorkingDays(0) on a Sunday bumps to Monday"):
        import hebdomads.europeanHebdomad
        2025-Mar-16 + WorkingDays(0)
      . assert(_ == 2025-Mar-17)

      test(m"WorkingDays(0) on a holiday bumps to next working day"):
        import hebdomads.europeanHebdomad
        2025-Jan-1 + WorkingDays(0)
      . assert(_ == 2025-Jan-2)

      test(m"WorkingDays(0) on a normal weekday is the same day"):
        import hebdomads.europeanHebdomad
        2025-Mar-17 + WorkingDays(0)
      . assert(_ == 2025-Mar-17)

      test(m"WorkingDays across consecutive holidays"):
        import hebdomads.europeanHebdomad
        2025-Dec-24 + WorkingDays(2)
      . assert(_ == 2025-Dec-30)

    suite(m"ISO 8601 parse error cases"):
      import instantDecodables.iso8601InstantDecodable

      test(m"Non-digit at year start raises"):
        capture(t"abcd-01-01".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Truncated year raises"):
        capture(t"20".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Trailing junk after seconds raises"):
        capture(t"2024-01-01T12:00:00garbage".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Bad week-date letter raises"):
        capture(t"2024-X21-1".decode[Instant])
      . matches:
          case _: TimeError =>

    suite(m"RFC 1123 parse error cases"):
      import instantDecodables.rfc1123InstantDecodable

      test(m"Lowercase day name raises"):
        capture(t"sun, 06 Nov 1994 08:49:37 GMT".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Wrong month abbreviation raises"):
        capture(t"Sun, 06 Jux 1994 08:49:37 GMT".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Missing trailing GMT raises"):
        capture(t"Sun, 06 Nov 1994 08:49:37".decode[Instant])
      . matches:
          case _: TimeError =>

      test(m"Truncated input raises"):
        capture(t"Sun, ".decode[Instant])
      . matches:
          case _: TimeError =>

    suite(m"Instant arithmetic"):
      test(m"Instant + Hour quantity"):
        Instant(0L) + 1*Hour
      . assert(_ == Instant(3600000L))

      test(m"Instant + Minute quantity"):
        Instant(0L) + 30*Minute
      . assert(_ == Instant(1800000L))

      test(m"Instant + Second quantity"):
        Instant(0L) + 5*Second
      . assert(_ == Instant(5000L))

      test(m"Instant - Instant gives Duration in seconds"):
        val d: Quantity[Seconds[1]] = Instant(3600000L) - Instant(0L)
        d.value
      . assert(_ == 3600.0)

      test(m"Instant - Quantity gives Instant"):
        Instant(3600000L) - 1*Hour
      . assert(_ == Instant(0L))

      test(m"Instant ordering: less than"):
        Instant(0L) < Instant(1L)
      . assert(_ == true)

      test(m"Instant ordering: greater than or equal"):
        Instant(1L) >= Instant(1L)
      . assert(_ == true)

      test(m"Instant.Min less than Instant.Max"):
        Instant.Min < Instant.Max
      . assert(_ == true)

      test(m"Instant.long round-trip"):
        Instant(12345L).long
      . assert(_ == 12345L)

    suite(m"Duration construction"):
      test(m"Duration(1000L) is one second"):
        Duration(1000L).value
      . assert(_ == 1.0)

      test(m"Duration(0L) is zero"):
        Duration(0L).value
      . assert(_ == 0.0)

      test(m"Duration round-trip via 60_000 ms is 60 s"):
        Duration(60_000L).value
      . assert(_ == 60.0)

    suite(m"Timespan and calendar arithmetic"):
      import calendars.gregorianCalendar

      test(m"n*Radix builds a single-radix timespan"):
        (3*Month).months
      . assert(_ == 3)

      test(m"Combining timespans unions their radix counts"):
        val span = 2*Year + 3*Month + 14*Day
        (span.years, span.months, span.days)
      . assert(_ == (2, 3, 14))

      test(m"Subtracting timespans"):
        ((5*Month) - (2*Month)).months
      . assert(_ == 3)

      test(m"Multiplying a timespan by an integer"):
        (3*Month*4).months
      . assert(_ == 12)

      test(m"Adding whole days to a date needs no policy"):
        2024-Jan-1 + 3*Day
      . assert(_ == 2024-Jan-4)

      test(m"Adding weeks to a date"):
        2024-Jan-1 + 2*Week
      . assert(_ == 2024-Jan-15)

      test(m"Adding months to a date"):
        import monthEnds.clampMonthEnd
        2024-Jan-15 + 2*Month
      . assert(_ == 2024-Mar-15)

      test(m"Jan 31 + 1 month clamps to Feb 29 in a leap year"):
        import monthEnds.clampMonthEnd
        2024-Jan-31 + 1*Month
      . assert(_ == 2024-Feb-29)

      test(m"Jan 31 + 1 month overflows to Mar 2"):
        import monthEnds.overflowMonthEnd
        2024-Jan-31 + 1*Month
      . assert(_ == 2024-Mar-2)

      test(m"Feb 29 + 1 year clamps to Feb 28 in a non-leap year"):
        import monthEnds.clampMonthEnd
        2024-Feb-29 + 1*Year
      . assert(_ == 2025-Feb-28)

      test(m"Adding a negative year"):
        import monthEnds.clampMonthEnd
        2024-Mar-15 + (-1)*Year
      . assert(_ == 2023-Mar-15)

      test(m"A regular timespan adds to an instant as physical seconds"):
        Instant(0L) + (1*Hour + 30*Minute)
      . assert(_ == Instant(5400000L))

      test(m"Adding a month to a timestamp keeps the time of day"):
        import monthEnds.clampMonthEnd
        Timestamp(2024-Jan-31, Clockface(10, 15, 0)) + 1*Month
      . assert(_ == Timestamp(2024-Feb-29, Clockface(10, 15, 0)))

      test(m"Adding an hour across midnight carries into the next day"):
        Timestamp(2024-Jan-1, Clockface(23, 30, 0)) + 1*Hour
      . assert(_ == Timestamp(2024-Jan-2, Clockface(0, 30, 0)))

      test(m"Adding days and hours to a timestamp"):
        Timestamp(2024-Jan-1, Clockface(10, 0, 0)) + (2*Day + 3*Hour)
      . assert(_ == Timestamp(2024-Jan-3, Clockface(13, 0, 0)))

      test(m"Adding a day to a zoned moment ignores DST (keeps wall-clock time)"):
        val moment = Timestamp(2024-Mar-9, Clockface(12, 0, 0)).in(tz"America/New_York")
        (moment + 1*Day).time
      . assert(_ == Clockface(12, 0, 0))

      test(m"Adding 24 hours to a zoned moment honours DST (gains an hour)"):
        val moment = Timestamp(2024-Mar-9, Clockface(12, 0, 0)).in(tz"America/New_York")
        (moment + 24*Hour).time
      . assert(_ == Clockface(13, 0, 0))

    suite(m"Coptic calendar"):
      import calendars.copticCalendar

      test(m"2000-01-01 Gregorian is 22 Koiak 1716 in the Coptic calendar"):
        val date = { import calendars.gregorianCalendar; 2000-Jan-1 }
        (copticCalendar.annual(date)(), copticCalendar.mensual(date), copticCalendar.diurnal(date)())
      . assert(_ == (1716, CopticMonth.Koiak, 22))

      test(m"A Coptic date round-trips through its Julian day number"):
        val date = unsafely(Date(Year(1716), CopticMonth.Koiak, Day(22)))
        (copticCalendar.annual(date)(), copticCalendar.mensual(date), copticCalendar.diurnal(date)())
      . assert(_ == (1716, CopticMonth.Koiak, 22))

      test(m"Adding a Coptic month advances within the Coptic calendar"):
        import monthEnds.clampMonthEnd
        val date = unsafely(Date(Year(1716), CopticMonth.Koiak, Day(22)))
        copticCalendar.mensual(date + 1*CopticMonth)
      . assert(_ == CopticMonth.Tobi)

      test(m"A Coptic month after Nasie wraps to the next year"):
        import monthEnds.clampMonthEnd
        val date = unsafely(Date(Year(1716), CopticMonth.Nasie, Day(5)))
        val result = date + 1*CopticMonth
        (copticCalendar.annual(result)(), copticCalendar.mensual(result))
      . assert(_ == (1717, CopticMonth.Thout))

      test(m"A Coptic leap year has 366 days"):
        copticCalendar.daysInYear(Year(1719))
      . assert(_ == 366)

      test(m"Islamic months cannot be added in a Coptic context"):
        demilitarize:
          unsafely(Date(Year(1716), CopticMonth.Thout, Day(1))) + 1*IslamicMonth
      . assert(_.nonEmpty)

    suite(m"Islamic calendar"):
      import calendars.islamicCalendar

      test(m"2000-01-01 Gregorian is 24 Ramadan 1420 in the Islamic calendar"):
        val date = { import calendars.gregorianCalendar; 2000-Jan-1 }
        ( islamicCalendar.annual(date)(),
          islamicCalendar.mensual(date),
          islamicCalendar.diurnal(date)() )
      . assert(_ == (1420, IslamicMonth.Ramadan, 24))

      test(m"An Islamic date round-trips through its Julian day number"):
        val date = unsafely(Date(Year(1420), IslamicMonth.Ramadan, Day(24)))
        ( islamicCalendar.annual(date)(),
          islamicCalendar.mensual(date),
          islamicCalendar.diurnal(date)() )
      . assert(_ == (1420, IslamicMonth.Ramadan, 24))

      test(m"Adding an Islamic month advances within the Islamic calendar"):
        import monthEnds.clampMonthEnd
        val date = unsafely(Date(Year(1420), IslamicMonth.Ramadan, Day(24)))
        islamicCalendar.mensual(date + 1*IslamicMonth)
      . assert(_ == IslamicMonth.Shawwal)

      test(m"An Islamic month after Dhu al-Hijjah wraps to the next year"):
        import monthEnds.clampMonthEnd
        val date = unsafely(Date(Year(1420), IslamicMonth.DhuAlHijjah, Day(1)))
        val result = date + 1*IslamicMonth
        (islamicCalendar.annual(result)(), islamicCalendar.mensual(result))
      . assert(_ == (1421, IslamicMonth.Muharram))

      test(m"An Islamic leap year has 355 days"):
        islamicCalendar.daysInYear(Year(1420))
      . assert(_ == 355)

      test(m"Coptic months cannot be added in an Islamic context"):
        demilitarize:
          unsafely(Date(Year(1420), IslamicMonth.Muharram, Day(1))) + 1*CopticMonth
      . assert(_.nonEmpty)

    suite(m"Ethiopian calendar"):
      import calendars.ethiopianCalendar

      test(m"2000-01-01 Gregorian is 22 Tahsas 1992 in the Ethiopian calendar"):
        val date = { import calendars.gregorianCalendar; 2000-Jan-1 }
        ( ethiopianCalendar.annual(date)(),
          ethiopianCalendar.mensual(date),
          ethiopianCalendar.diurnal(date)() )
      . assert(_ == (1992, EthiopianMonth.Tahsas, 22))

      test(m"A month after Pagume wraps to the next Ethiopian year"):
        import monthEnds.clampMonthEnd
        val result = unsafely(Date(Year(1992), EthiopianMonth.Pagume, Day(5))) + 1*EthiopianMonth
        (ethiopianCalendar.annual(result)(), ethiopianCalendar.mensual(result))
      . assert(_ == (1993, EthiopianMonth.Meskerem))

    suite(m"Persian calendar"):
      import calendars.persianCalendar

      test(m"2000-01-01 Gregorian is 11 Dey 1378 in the Persian calendar"):
        val date = { import calendars.gregorianCalendar; 2000-Jan-1 }
        ( persianCalendar.annual(date)(),
          persianCalendar.mensual(date),
          persianCalendar.diurnal(date)() )
      . assert(_ == (1378, PersianMonth.Dey, 11))

      test(m"A Persian date round-trips through its Julian day number"):
        val date = unsafely(Date(Year(1403), PersianMonth.Farvardin, Day(1)))
        ( persianCalendar.annual(date)(),
          persianCalendar.mensual(date),
          persianCalendar.diurnal(date)() )
      . assert(_ == (1403, PersianMonth.Farvardin, 1))

    suite(m"Indian National calendar"):
      import calendars.{gregorianCalendar, indianCalendar}

      test(m"1 Chaitra 1879 Saka is 22 March 1957"):
        val date = unsafely(Date(using indianCalendar)(Year(1879), IndianMonth.Chaitra, Day(1)))
        ( gregorianCalendar.annual(date)(),
          gregorianCalendar.mensual(date),
          gregorianCalendar.diurnal(date)() )
      . assert(_ == (1957, Mar, 22))

      test(m"1 Chaitra of a Gregorian-leap year starts on 21 March"):
        val date = unsafely(Date(using indianCalendar)(Year(1942), IndianMonth.Chaitra, Day(1)))
        ( gregorianCalendar.annual(date)(),
          gregorianCalendar.mensual(date),
          gregorianCalendar.diurnal(date)() )
      . assert(_ == (2020, Mar, 21))

      test(m"1 Vaishakha 1879 Saka is 21 April 1957"):
        val date = unsafely(Date(using indianCalendar)(Year(1879), IndianMonth.Vaishakha, Day(1)))
        ( gregorianCalendar.annual(date)(),
          gregorianCalendar.mensual(date),
          gregorianCalendar.diurnal(date)() )
      . assert(_ == (1957, Apr, 21))

    suite(m"Hebrew calendar"):
      import calendars.hebrewCalendar

      test(m"2000-01-01 Gregorian is 23 Tevet 5760 in the Hebrew calendar"):
        val date = { import calendars.gregorianCalendar; 2000-Jan-1 }
        ( hebrewCalendar.annual(date)(),
          hebrewCalendar.mensual(date),
          hebrewCalendar.diurnal(date)() )
      . assert(_ == (5760, HebrewMonth.Tevet, 23))

      test(m"A Hebrew date round-trips through its Julian day number"):
        val date = unsafely(Date(Year(5784), HebrewMonth.Nisan, Day(15)))
        ( hebrewCalendar.annual(date)(),
          hebrewCalendar.mensual(date),
          hebrewCalendar.diurnal(date)() )
      . assert(_ == (5784, HebrewMonth.Nisan, 15))

      test(m"5784 is a leap year with 13 months"):
        hebrewCalendar.monthsInYear(Year(5784))
      . assert(_ == 13)

      test(m"5783 is a common year with 12 months"):
        hebrewCalendar.monthsInYear(Year(5783))
      . assert(_ == 12)

      test(m"Adding a month from Adar I reaches Adar II in a leap year"):
        import monthEnds.clampMonthEnd
        val date = unsafely(Date(Year(5784), HebrewMonth.Adar, Day(1)))
        hebrewCalendar.mensual(date + 1*HebrewMonth)
      . assert(_ == HebrewMonth.AdarSheni)

    suite(m"French Republican calendar"):
      test(m"18 Brumaire An VIII is 9 November 1799"):
        import calendars.{gregorianCalendar, frenchRepublicanCalendar}
        val date =
          unsafely(Date(using frenchRepublicanCalendar)
              (Year(8), FrenchRepublicanMonth.Brumaire, Day(18)))

        ( gregorianCalendar.annual(date)(),
          gregorianCalendar.mensual(date),
          gregorianCalendar.diurnal(date)() )
      . assert(_ == (1799, Nov, 9))

    suite(m"Leap-second strategies"):
      val newYear = 1483228800000L // 2017-01-01 00:00:00 UTC, just after the 2016 leap second

      test(m"Leap seconds are ignored by default"):
        summon[LeapSeconds.Strategy].tai(newYear)
      . assert(_ == newYear)

      test(m"The step strategy counts elapsed leap seconds"):
        import leapSeconds.step
        summon[LeapSeconds.Strategy].tai(newYear) - newYear
      . assert(_ == LeapSeconds.tai(newYear) - newYear)

      test(m"Across the 2016 leap second the TAI offset gains one second"):
        LeapSeconds.tai(newYear) - LeapSeconds.tai(newYear - 60000L)
      . assert(_ == 61000L)

      test(m"The smear strategy is chosen by import"):
        import leapSeconds.smear
        summon[LeapSeconds.Strategy].tai(newYear)
      . assert(_ == LeapSeconds.smearTai(newYear))

      test(m"Smearing matches discrete TAI far from any leap second"):
        val midYear = 1490000000000L // 2017-03-20, no nearby leap second
        LeapSeconds.smearTai(midYear) == LeapSeconds.tai(midYear)
      . assert(_ == true)

    suite(m"ISO-8601 week dates"):
      import calendars.gregorianCalendar

      test(m"2000-01-01 is in week-year 1999, week 52"):
        val date = 2000-Jan-1
        (WeekDate.weekYear(date)(), WeekDate.weekOfYear(date))
      . assert(_ == (1999, 52))

      test(m"2000-01-03 (Monday) starts week-year 2000, week 1"):
        val date = 2000-Jan-3
        (WeekDate.weekYear(date)(), WeekDate.weekOfYear(date))
      . assert(_ == (2000, 1))

      test(m"WeekDate(1999, 52, Sat) is 2000-01-01"):
        WeekDate(Year(1999), 52, Sat)
      . assert(_ == 2000-Jan-1)

      test(m"A week date round-trips a date"):
        val date = 2021-Jul-15
        WeekDate(WeekDate.weekYear(date), WeekDate.weekOfYear(date), date.weekday) == date
      . assert(_ == true)

      test(m"2004 has 53 ISO weeks"):
        WeekDate.weekOfYear(2004-Dec-31)
      . assert(_ == 53)

    suite(m"Year-offset calendars"):
      test(m"2000 Gregorian is 2543 in the Buddhist calendar"):
        import calendars.buddhistCalendar
        val date = { import calendars.gregorianCalendar; 2000-Jun-1 }
        buddhistCalendar.annual(date)()
      . assert(_ == 2543)

      test(m"2000 Gregorian is year 89 in the Minguo calendar"):
        import calendars.minguoCalendar
        val date = { import calendars.gregorianCalendar; 2000-Jun-1 }
        minguoCalendar.annual(date)()
      . assert(_ == 89)

    suite(m"Calendar display"):
      test(m"A Coptic date displays with its month name"):
        import calendars.copticCalendar
        copticCalendar.format(unsafely(Date(Year(1716), CopticMonth.Koiak, Day(22))))
      . assert(_ == t"22 Koiak 1716")

      test(m"Islamic month names are polished"):
        IslamicMonth.RabiAlAwwal.show
      . assert(_ == t"Rabi I")

      test(m"An Islamic date displays with its month name"):
        import calendars.islamicCalendar
        islamicCalendar.format(unsafely(Date(Year(1445), IslamicMonth.Ramadan, Day(15))))
      . assert(_ == t"15 Ramadan 1445")

      test(m"The Hebrew leap month displays as Adar II"):
        import calendars.hebrewCalendar
        hebrewCalendar.format(unsafely(Date(Year(5784), HebrewMonth.AdarSheni, Day(1))))
      . assert(_ == t"1 Adar II 5784")

    suite(m"Clock"):
      test(m"Clock.fixed returns the same instant"):
        val clock = Clock.fixed(Instant(12345L))
        (clock(), clock())
      . assert(_ == (Instant(12345L), Instant(12345L)))

      test(m"now() uses the implicit clock"):
        given Clock = Clock.fixed(Instant(99999L))
        now()
      . assert(_ == Instant(99999L))

      test(m"today() with fixed clock and UTC"):
        given Clock = Clock.fixed(Instant(0L))
        given Timezone = tz"UTC"
        given calendar: RomanCalendar = calendars.gregorianCalendar
        today()
      . assert(_ == 1970-Jan-1)

      test(m"Clock.offset reflects an offset"):
        val before = java.lang.System.currentTimeMillis
        val clock = Clock.offset(60*Second)
        val result: Instant = clock()
        val after = java.lang.System.currentTimeMillis
        result.long >= (before + 60_000L) && result.long <= (after + 60_001L)
      . assert(_ == true)

    suite(m"Period operations"):
      test(m"instant ~ instant constructs a Period"):
        val period = Instant(0L) ~ Instant(1000L)
        (period.start.long, period.finish.long)
      . assert(_ == (0L, 1000L))

      test(m"Period.duration returns the time difference"):
        val period = Instant(0L) ~ Instant(3_600_000L)
        period.duration.value
      . assert(_ == 3600.0)

      test(m"Period.duration is zero for zero-length period"):
        val period = Instant(1000L) ~ Instant(1000L)
        period.duration.value
      . assert(_ == 0.0)

      test(m"Period constructed via Period(start, finish)"):
        val duration: Duration = 3600*Second
        val period = Period(Instant(0L), Instant(0L) + duration)
        (period.start.long, period.finish.long)
      . assert(_ == (0L, 3_600_000L))

      test(m"Period.intersect of identical periods"):
        val a = Instant(0L) ~ Instant(1000L)
        a.intersect(a).let { p => (p.start.long, p.finish.long) }
      . assert(_ == (0L, 1000L))

      test(m"Period.intersect of disjoint periods is Unset"):
        val a = Instant(0L) ~ Instant(1000L)
        val b = Instant(2000L) ~ Instant(3000L)
        a.intersect(b)
      . assert(_ == Unset)

      test(m"Period.intersect with partial overlap"):
        val a = Instant(0L) ~ Instant(2000L)
        val b = Instant(1000L) ~ Instant(3000L)
        a.intersect(b).let { p => (p.start.long, p.finish.long) }
      . assert(_ == (1000L, 2000L))

      test(m"Period.intersect when one fully contains the other"):
        val a = Instant(0L) ~ Instant(3000L)
        val b = Instant(1000L) ~ Instant(2000L)
        a.intersect(b).let { p => (p.start.long, p.finish.long) }
      . assert(_ == (1000L, 2000L))

      test(m"Period.intersect at exact boundary returns Unset"):
        val a = Instant(0L) ~ Instant(1000L)
        val b = Instant(1000L) ~ Instant(2000L)
        a.intersect(b)
      . assert(_ == Unset)

      test(m"Period.union of overlapping periods is one period"):
        val a = Instant(0L) ~ Instant(2000L)
        val b = Instant(1000L) ~ Instant(3000L)
        a.union(b).map { p => (p.start.long, p.finish.long) }
      . assert(_ == Set((0L, 3000L)))

      test(m"Period.union of disjoint periods is two periods"):
        val a = Instant(0L) ~ Instant(1000L)
        val b = Instant(2000L) ~ Instant(3000L)
        a.union(b).size
      . assert(_ == 2)

    suite(m"Moment and Timezone"):
      import calendars.gregorianCalendar

      test(m"Instant 0 in UTC has the correct date and time fields"):
        val moment = Instant(0L).in(tz"UTC")
        (moment.date, moment.time)
      . assert(_ == (1970-Jan-1, Clockface(0, 0, 0)))

      test(m"Moment.instant round-trips an instant in UTC"):
        val original = Instant(1_700_000_000_000L)
        original.in(tz"UTC").instant
      . assert(_ == Instant(1_700_000_000_000L))

      test(m"Moment.timestamp drops the timezone"):
        Instant(0L).in(tz"UTC").timestamp
      . assert(_ == Timestamp(1970-Jan-1, Clockface(0, 0, 0)))

      test(m"Same instant in London during winter is GMT (offset 0)"):
        val winter = t"2024-01-15T12:00:00Z".decode[Instant](using instantDecodables.iso8601InstantDecodable)
        winter.in(tz"Europe/London").time.hour
      . assert(_ == 12)

      test(m"Same instant in London during summer is BST (offset +1)"):
        val summer = t"2024-07-15T12:00:00Z".decode[Instant](using instantDecodables.iso8601InstantDecodable)
        summer.in(tz"Europe/London").time.hour
      . assert(_ == 13)

      test(m"New York winter offset (UTC-5)"):
        val winter = t"2024-01-15T12:00:00Z".decode[Instant](using instantDecodables.iso8601InstantDecodable)
        winter.in(tz"America/New_York").time.hour
      . assert(_ == 7)

      test(m"New York summer offset (UTC-4)"):
        val summer = t"2024-07-15T12:00:00Z".decode[Instant](using instantDecodables.iso8601InstantDecodable)
        summer.in(tz"America/New_York").time.hour
      . assert(_ == 8)

      test(m"Timestamp.instant round-trips through London"):
        given Timezone = tz"Europe/London"
        val ts = Timestamp(2024-Jan-15, Clockface(12, 0, 0))
        ts.instant.in(tz"Europe/London").time.hour
      . assert(_ == 12)

      // On 2024-10-27 London clocks go back at 02:00 BST, so 01:30 local occurs twice: first at
      // 00:30 UTC (BST), then again at 01:30 UTC (GMT).
      test(m"DST fall-back: earlier overlap occurrence round-trips through London"):
        import instantDecodables.iso8601InstantDecodable
        val earlier = t"2024-10-27T00:30:00Z".decode[Instant]
        earlier.in(tz"Europe/London").instant == earlier
      . assert(_ == true)

      test(m"DST fall-back: later overlap occurrence round-trips through London"):
        import instantDecodables.iso8601InstantDecodable
        val later = t"2024-10-27T01:30:00Z".decode[Instant]
        later.in(tz"Europe/London").instant == later
      . assert(_ == true)

      test(m"DST fall-back: later overlap occurrence is flagged Second"):
        import instantDecodables.iso8601InstantDecodable
        t"2024-10-27T01:30:00Z".decode[Instant].in(tz"Europe/London").occurrence
      . assert(_ == Occurrence.Second)

      test(m"DST fall-back: earlier overlap occurrence is flagged First"):
        import instantDecodables.iso8601InstantDecodable
        t"2024-10-27T00:30:00Z".decode[Instant].in(tz"Europe/London").occurrence
      . assert(_ == Occurrence.First)

      test(m"The two overlap occurrences ground one hour apart"):
        val london = tz"Europe/London"
        val clock = Clockface(1, 30, 0)
        val earlier = Moment(2024-Oct-27, clock, london).instant
        val later = Moment(2024-Oct-27, clock, london, Occurrence.Second).instant
        later.long - earlier.long
      . assert(_ == 3600000L)

      // On 2024-03-31 London clocks jump 01:00 GMT → 02:00 BST, so 01:30 local never happens.
      test(m"Spring-forward gap pushes forward by default"):
        import instantDecodables.iso8601InstantDecodable
        val grounded = Moment(2024-Mar-31, Clockface(1, 30, 0), tz"Europe/London").instant
        grounded == t"2024-03-31T01:30:00Z".decode[Instant]
      . assert(_ == true)

      test(m"Spring-forward gap can push backward"):
        import instantDecodables.iso8601InstantDecodable
        import gapPolicies.pushBackward
        val grounded = Moment(2024-Mar-31, Clockface(1, 30, 0), tz"Europe/London").instant
        grounded == t"2024-03-31T00:30:00Z".decode[Instant]
      . assert(_ == true)

      test(m"Spring-forward gap can be rejected"):
        import gapPolicies.rejectGap
        capture(Moment(2024-Mar-31, Clockface(1, 30, 0), tz"Europe/London").instant)
      . assert(_ == TimeError(_.Gap))

      test(m"Timezone(t\"NotARealZone\") raises TimezoneError"):
        capture(Timezone(t"NotARealZone"))
      . matches:
          case _: TimezoneError =>

      test(m"tz\"NotARealZone\" is a compile error"):
        demilitarize(tz"NotARealZone")
      . assert(_.nonEmpty)

    suite(m"ts interpolator"):
      import calendars.gregorianCalendar

      test(m"A bare year produces a Year"):
        val year: Year = ts"2024"
        year
      . assert(_ == Year(2024))

      test(m"A year and month produces a Monthstamp"):
        val monthstamp: Monthstamp = ts"2012-11"
        monthstamp
      . assert(_ == Monthstamp(Year(2012), Nov))

      test(m"A calendar date produces a Date"):
        val date: Date = ts"2020-12-31"
        date
      . assert(_ == 2020-Dec-31)

      test(m"A zoneless date-time produces a Timestamp"):
        val timestamp: Timestamp = ts"2023-05-28T14:30:59"
        timestamp
      . assert(_ == Timestamp(2023-May-28, Clockface(14, 30, 59)))

      test(m"A date-time without seconds produces a Timestamp"):
        val timestamp: Timestamp = ts"2023-05-28T14:30"
        timestamp
      . assert(_ == Timestamp(2023-May-28, Clockface(14, 30, 0)))

      test(m"A zoned date-time with Z produces a Moment"):
        val moment: Moment = ts"1994-11-06T08:49:37Z"
        moment.instant
      . assert(_ == Instant(784111777000L))

      test(m"A zoned date-time with an offset produces a Moment"):
        val moment: Moment = ts"2023-05-28T14:30:59+02:00"
        moment.instant
      . assert(_ == Instant(1685277059000L))

      test(m"An RFC 1123 timestamp produces a Moment"):
        val moment: Moment = ts"Sun, 06 Nov 1994 08:49:37 GMT"
        moment.instant
      . assert(_ == Instant(784111777000L))

      test(m"A zoned leap second produces a Moment flagged Inserted"):
        val moment: Moment = ts"2016-12-31T23:59:60Z"
        moment.leap
      . assert(_ == Leap.Inserted)

      test(m"A zoned leap second grounds to the following second"):
        val moment: Moment = ts"2016-12-31T23:59:60Z"
        moment.instant
      . assert(_ == Instant(1483228800000L))

      test(m"A zoned leap second renders as :60"):
        val moment: Moment = ts"2016-12-31T23:59:60Z"
        moment.show
      . assert(_ == t"2016-12-31T23:59:60Z")

      test(m"A zoneless leap second is a compile error"):
        demilitarize(ts"2016-12-31T23:59:60")
      . assert(_.nonEmpty)

      test(m"A leap second not at :59 is a compile error"):
        demilitarize(ts"2016-12-31T23:30:60Z")
      . assert(_.nonEmpty)

      test(m"A year literal does not typecheck as a Monthstamp"):
        demilitarize:
          val monthstamp: Monthstamp = ts"2024"
      . assert(_.nonEmpty)

      test(m"An unparseable timestamp is a compile error"):
        demilitarize(ts"not-a-timestamp")
      . assert(_.nonEmpty)

      test(m"An empty timestamp is a compile error"):
        demilitarize(ts"")
      . assert(_.nonEmpty)

      test(m"A month above 12 is a compile error"):
        demilitarize(ts"2024-13-15")
      . assert(_.nonEmpty)

      test(m"A nonexistent day is a compile error"):
        demilitarize(ts"2024-06-31")
      . assert(_.nonEmpty)

      test(m"An hour above 23 is a compile error"):
        demilitarize(ts"2024-06-17T25:00:00")
      . assert(_.nonEmpty)

      test(m"An invalid weekday name is a compile error"):
        demilitarize(ts"Xyz, 17 Jun 2024 14:30:45 GMT")
      . assert(_.nonEmpty)

      test(m"A substitution is a compile error"):
        demilitarize(ts"${2024}")
      . assert(_.nonEmpty)

    suite(m"TaiInstant and leap-second conversion"):
      import calendars.gregorianCalendar
      import instantDecodables.iso8601InstantDecodable

      test(m"Instant 0 (UNIX epoch) has 10-second TAI offset"):
        LeapSeconds.tai(0L) - 0L
      . assert(_ == 10_000L)

      test(m"Just before Jan 1 1973 (after June 1972 leap) has +11 offset"):
        val justBefore = 94694399999L
        LeapSeconds.tai(justBefore) - justBefore
      . assert(_ == 11_000L)

      test(m"Just after Jan 1 1973 (after Dec 1972 leap) has +12 offset"):
        val justAfter = 94694400001L
        LeapSeconds.tai(justAfter) - justAfter
      . assert(_ == 12_000L)

      test(m"Instant in 2017 has +37 offset"):
        val later = t"2017-06-15T00:00:00Z".decode[Instant](using instantDecodables.iso8601InstantDecodable)
        LeapSeconds.tai(later.long) - later.long
      . assert(_ == 37_000L)

      test(m"Instant.tai counts leap seconds under the step strategy"):
        import leapSeconds.step
        val later = t"2017-06-15T00:00:00Z".decode[Instant]
        later.tai.long - later.long
      . assert(_ == 37_000L)

      test(m"TaiInstant.instant inverts Instant.tai under the smear strategy"):
        import leapSeconds.smear
        val instant = t"2017-06-15T00:00:00Z".decode[Instant]
        instant.tai.instant == instant
      . assert(_ == true)

      test(m"unsmearTai inverts smearTai across a leap window"):
        val nearLeap = 1483228800000L
        LeapSeconds.unsmearTai(LeapSeconds.smearTai(nearLeap)) == nearLeap
      . assert(_ == true)

      test(m"An inserted leap second grounds to the following second"):
        val leapMoment = Moment(2016-Dec-31, Clockface(23, 59, 59), tz"UTC", leap = Leap.Inserted)
        leapMoment.instant == t"2017-01-01T00:00:00Z".decode[Instant]
      . assert(_ == true)

      test(m"An inserted leap second's TAI is one second before the following second"):
        import leapSeconds.step
        val leapMoment = Moment(2016-Dec-31, Clockface(23, 59, 59), tz"UTC", leap = Leap.Inserted)
        val nextSecond = Moment(2017-Jan-1, Clockface(0, 0, 0), tz"UTC")
        nextSecond.tai.long - leapMoment.tai.long
      . assert(_ == 1000L)

    suite(m"Horology sexagesimal"):
      val horology = Horology.sexagesimal

      test(m"addPrimary adds hours"):
        horology.addPrimary(Clockface(0, 0, 0), Base24(5))
      . assert(_ == Clockface(5, 0, 0))

      test(m"addSecondary adds minutes within an hour"):
        horology.addSecondary(Clockface(0, 30, 0), Base60(15))
      . assert(_ == Clockface(0, 45, 0))

      test(m"addSecondary carries minutes into hours"):
        horology.addSecondary(Clockface(0, 30, 0), Base60(45))
      . assert(_ == Clockface(1, 15, 0))

      test(m"addSecondary day-overflow wraps via Base24 modulo"):
        horology.addSecondary(Clockface(23, 30, 0), Base60(45))
      . assert(_ == Clockface(0, 15, 0))

      test(m"addTertiary adds seconds within a minute"):
        horology.addTertiary(Clockface(0, 0, 30), Base60(20))
      . assert(_ == Clockface(0, 0, 50))

      test(m"addTertiary carries seconds into minutes"):
        horology.addTertiary(Clockface(0, 0, 45), Base60(30))
      . assert(_ == Clockface(0, 1, 15))

      test(m"addTertiary cascading carry into hours"):
        horology.addTertiary(Clockface(0, 59, 59), Base60(2))
      . assert(_ == Clockface(1, 0, 1))

    suite(m"Julian calendar"):
      test(m"Year 1900 is a leap year in the Julian calendar"):
        calendars.julianCalendar.leapYear(Year(1900))
      . assert(_ == true)

      test(m"Year 2000 is a leap year in the Julian calendar"):
        calendars.julianCalendar.leapYear(Year(2000))
      . assert(_ == true)

      test(m"Year 1901 is not a leap year"):
        calendars.julianCalendar.leapYear(Year(1901))
      . assert(_ == false)

      test(m"Year 1900 has 366 days under the Julian calendar"):
        calendars.julianCalendar.daysInYear(Year(1900))
      . assert(_ == 366)

    suite(m"Timestamp"):
      import calendars.gregorianCalendar

      test(m"Timestamp Showable produces 'time, date'"):
        import dateFormats.iso8601DateFormat
        import timeFormats.iso8601TimeFormat
        Timestamp(2024-Jan-15, Clockface(14, 30, 59)).show
      . assert(_ == t"14:30:59, 2024-01-15")

      test(m"Timestamp.year accessor"):
        Timestamp(2024-Mar-15, Clockface(0, 0, 0)).year
      . assert(_ == Year(2024))

      test(m"Timestamp.month accessor"):
        Timestamp(2024-Mar-15, Clockface(0, 0, 0)).month
      . assert(_ == Mar)

      test(m"Timestamp.day accessor"):
        Timestamp(2024-Mar-15, Clockface(0, 0, 0)).day()
      . assert(_ == 15)

      test(m"Timestamp.hour/minute/second accessors"):
        val ts = Timestamp(2024-Mar-15, Clockface(14, 30, 59))
        (ts.hour, ts.minute, ts.second)
      . assert(_ == (14, 30, 59))

      test(m"Timestamp.monthstamp"):
        val ms = Timestamp(2024-Mar-15, Clockface(0, 0, 0)).monthstamp
        (ms.year(), ms.month)
      . assert(_ == (2024, Mar))

      test(m"Timestamp.decode 'YYYY-MM-DDTHH:MM:SS'"):
        t"2024-01-15T14:30:59".decode[Timestamp]
      . assert(_ == Timestamp(2024-Jan-15, Clockface(14, 30, 59)))

      test(m"Timestamp.decode 'YYYY-MM-DD HH:MM:SS'"):
        t"2024-01-15 14:30:59".decode[Timestamp]
      . assert(_ == Timestamp(2024-Jan-15, Clockface(14, 30, 59)))

      test(m"Timestamp.decode rejects malformed input"):
        capture(t"not-a-timestamp".decode[Timestamp])
      . matches:
          case _: TimestampError =>

      test(m"Timestamp.decode rejects invalid month"):
        capture(t"2024-13-15T14:30:59".decode[Timestamp])
      . matches:
          case _: TimestampError =>

      test(m"Timestamp difference decomposes into days/hours/minutes/seconds"):
        val a = Timestamp(2024-Jan-10, Clockface(8, 0, 0))
        val b = Timestamp(2024-Jan-12, Clockface(11, 30, 15))
        val span = b - a
        (span.days, span.hours, span.minutes, span.seconds.value)
      . assert(_ == (2, 3, 30, 15.0))

      test(m"Timestamp difference of equal timestamps is zero"):
        val a = Timestamp(2024-Jan-10, Clockface(8, 0, 0))
        val span = a - a
        (span.days, span.hours, span.minutes, span.seconds.value)
      . assert(_ == (0, 0, 0, 0.0))

      test(m"Timestamp difference is sign-consistent when negative"):
        val a = Timestamp(2024-Jan-10, Clockface(8, 0, 0))
        val b = Timestamp(2024-Jan-12, Clockface(11, 30, 15))
        val span = a - b
        (span.days, span.hours, span.minutes, span.seconds.value)
      . assert(_ == (-2, -3, -30, -15.0))

      test(m"a Date is a Timestamp (usable where a Timestamp is expected)"):
        def jdnOf(timestamp: Timestamp): Int = timestamp.jdn
        val date: Date = 2024-Jan-15
        jdnOf(date) == date.jdn
      . assert(_ == true)

    suite(m"TZDB parser"):
      given TimeEvent is Loggable = new Loggable:
        type Self = TimeEvent
        def log(level: Level, timestamp: Long, event: => TimeEvent): Unit = ()

      test(m"parseFile on a non-existent file raises NoTzdbFile"):
        capture(Tzdb.parseFile(t"this-does-not-exist"))
      . matches:
          case _: TzdbError =>

      test(m"parses a single Rule line"):
        val lines = Stream(t"Rule\tUS\t2007\tmax\t-\tMar\tSun>=8\t2:00\t1:00\tD")
        Tzdb.parse(t"inline", lines).headOption
      . matches:
          case Some(_: Tzdb.Entry.Rule) =>

      test(m"parses a single Link line"):
        val lines = Stream(t"Link\tEurope/London\tEurope/Belfast")
        Tzdb.parse(t"inline", lines).headOption
      . matches:
          case Some(_: Tzdb.Entry.Link) =>

      test(m"parses a leap line with normal-time"):
        val lines = Stream(t"Leap\t1972\tJun\t30\t23:59:59\t+\tS")
        Tzdb.parse(t"inline", lines).headOption
      . matches:
          case Some(_: Tzdb.Entry.Leap) =>

      test(m"leap line with 60-second time raises TzdbError"):
        val lines = Stream(t"Leap\t1972\tJun\t30\t23:59:60\t+\tS")
        capture(Tzdb.parse(t"inline", lines))
      . matches:
          case _: TzdbError =>


      test(m"unparseable Rule raises UnexpectedRule"):
        val lines = Stream(t"Rule\tonly")
        capture(Tzdb.parse(t"inline", lines))
      . matches:
          case _: TzdbError =>

      test(m"unparseable Link raises UnexpectedLink"):
        val lines = Stream(t"Link\tonly")
        capture(Tzdb.parse(t"inline", lines))
      . matches:
          case _: TzdbError =>
